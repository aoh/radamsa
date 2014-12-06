;;;
;;; XMLish parse tree mutator
;;;

(define-library (rad xp)

   (export 
      xp-mutate)

   (import
      (owl base)
      (rad shared)
      (owl parse))

    (begin

      ;;;
      ;;; Parsing (bytes → nodes, but no tree structure)
      ;;;

      ; XP = #(bytes (byte ...))       -- nothing interesting structurally
      ;    | #(open name attrs)        -- was <foo attrs>
      ;    | #(open-single name attrs) -- was <foo attrs />
      ;    | #(close name)             -- </foo>
      ;    | #(tag name attrs XP)      -- was <foo attrs> XP </foo>, after shrubbing
      ;    | #(plus XP1 XP2)           -- XP1 XP2, used in mutations to generate multiple AST nodes 

      ;; convert (... (a . b) ...) → (... a b ...)
      (define (unquote listish)
         (foldr
            (λ (this tail)
               (if (pair? this)
                  (ilist (car this) (cdr this) tail)
                  (cons this tail)))
            null listish))

      (define xp-whitespace
         (get-any-of
            (get-imm #\space)
            (get-imm #\newline)))

      (define xp-optwhite
         (get-greedy* xp-whitespace))
     
      (define xp-string-delim
         (get-any-of
            (get-imm #\")
            (get-imm #\')))

      (define xp-alnum
         (get-byte-if
            (λ (x)
               (cond
                  ((> x 96) (< x 123)) ;; a-z
                  ((> x 64) (< x 91))  ;; A-Z
                  ((> x 47) (< x 58))  ;; 0-9
                  (else #false)))))

      (define xp-label 
         (get-greedy+ xp-alnum))

      (define (get-quoted-upto delim)
         (let-parses
            ((chars
               (get-greedy*
                  (get-either
                     (let-parses
                        ((quote (get-imm #\\))
                         (byte get-byte))
                        (cons #\\ byte))
                     (get-byte-if (λ (x) (not (eq? x delim)))))))
             (end (get-imm delim)))
            (append chars (list delim))))

      (define xp-attr-value
         (get-either
            (let-parses
               ((delim xp-string-delim)
                (chars (get-quoted-upto delim)))
               (cons delim chars))
            xp-label))

     (define xp-attr
         (let-parses
            ((skip xp-optwhite)
             (name xp-label)
             (skip xp-optwhite)
             (val
               (get-either
                  (let-parses
                     ((skip (get-imm #\=))
                      (skip xp-optwhite)
                      (val xp-attr-value))
                     val)
                  (get-epsilon #false))))
            (cons name (if val (unquote val) val))))

      ;; part after <
      (define xp-tag-open
         (let-parses
            ((tag xp-label)
             (attrs (get-greedy* xp-attr))
             (skip xp-optwhite)
             (tag-type 
               (get-any-of
                  (let-parses ((skip (get-imm #\>))) 'open)
                  (let-parses ((skip (get-imm #\/)) (skip (get-imm #\>))) 'open-single))))
            (tuple tag-type tag attrs)))

      ;; part at < 
      (define xp-tag-open/close
         (let-parses
            ((skip (get-imm #\<))
             (val 
               (get-either
                  (let-parses
                     ((skip (get-imm #\/))
                      (name xp-label)
                      (skip xp-optwhite)
                      (skip (get-imm #\>)))
                     (tuple 'close name))
                  xp-tag-open)))
            val))

      ;; something that likely doesn't improve tag parsing success here
      (define (uninteresting? x)
         (cond
            ((eq? x #\") #false)
            ((eq? x #\<) #false)
            (else #true)))

      (define xp-uninteresting
         (let-parses 
            ((bs (get-greedy* (get-byte-if uninteresting?))))
            (tuple 'bytes bs)))

      ;; byte + bytes -> bytes', byte + node  -> ((bytes (byte)) node)
      (define (add byte node)
         (if byte
            (tuple-case node
               ((bytes lst)
                  (tuple 'bytes (cons byte lst)))
               (else
                  (list (tuple 'bytes byte) node)))
            node))

      (define nothing 
         (tuple 'bytes null))

      (define xp-parser 
         (get-greedy* 
            (get-any-of
               xp-tag-open/close
               (let-parses
                  ((this get-byte)
                   (boring xp-uninteresting))
                  (add this boring)))))

      ;; note - this is actually more like a lexer
      (define (xp-parse lst)  
         (try-parse xp-parser lst "bug" "bug - everything should be parseable" #false))


      ;;;
      ;;; List interning
      ;;;

      ; we want eq?-unique tags and attributes to be able to use them as keys in ff 
      ; and compare quickly in parser. these are like symbols, but we neither have 
      ; nor want the symbol interner here.

      ;; store = ff of byte → tails, null → bytelist (if any)

      ;; tree (byte ...) → #false | (byte ...)'
      (define (tree-lookup tree lst)
         (cond
            ((eq? tree #empty) #false)
            ((null? lst) (get tree null #false))
            (else 
               (tree-lookup (get tree (car lst) #empty) (cdr lst)))))
   
      (define (tree-store tree lst)
         (define (store tree left)
            (if (null? left)
               (put tree null lst)
               (put tree (car left)
                  (store (get tree (car left) #empty) (cdr left)))))
         (store tree lst))

      (define (intern tree lst)
         (let ((val (tree-lookup tree lst)))
            (if val
               (values tree val)
               (values (tree-store tree lst) lst))))


      ;;;
      ;;; Tag- and attribute name interning
      ;;;

      ;; use the list interner to make tag- and attribute names eq?-unique

      (define (fold2 op st l)
         (if (null? l)
            (values st l)
            (lets 
               ((st a  (op st (car l)))
                (st as (fold2 op st (cdr l))))
               (values st (cons a as)))))

      ;; tree nodes → tree' nodes'
      (define (intern-tags tree node)
         (cond
            ((null? node)
               (values tree node))
            ((pair? node)
               (lets 
                  ((tree a (intern-tags tree (car node)))
                   (tree b (intern-tags tree (cdr node))))
                  (values tree (cons a b))))
            ((tuple? node)
               (tuple-case node
                  ((bytes lst)
                     (values tree node))
                  ((open tag attrs)
                     (lets 
                        ((tree tag (intern tree tag))
                         (tree attrs
                            (fold2 (λ (t at) (lets ((t a (intern t (car at)))) (values t (cons a (cdr at))))) tree attrs)))
                        (values tree (tuple 'open tag attrs))))
                  ((open-single tag attrs)
                     (lets 
                        ((tree tag (intern tree tag))
                         (tree attrs
                            (fold2 (λ (t at) (lets ((t a (intern t (car at)))) (values t (cons a (cdr at))))) tree attrs)))
                        (values tree (tuple 'open-single tag attrs))))
                  ((close tag)
                     (lets ((tree tag (intern tree tag)))
                        (values tree (tuple 'close tag))))
                  (else
                     (print "what fine node: " node)
                     (exit-owl 127))))
            (else
               (print "what fine node: " node)
               (exit-owl 127))))


      ;;;
      ;;; Compacting byte runs
      ;;;

      ;; #(bytes A) + #(bytes B) = #(bytes (append A B))

      ;; merge adjecent (bytes ...) nodes 
      (define (merge-bytes nodes)
         (if nodes
            (foldr
               (λ (node tail)
                  (tuple-case node
                     ((bytes lst)
                        (if (null? tail)
                           (cons node tail)
                           (tuple-case (car tail)
                              ((bytes lstp)
                                (cons (tuple 'bytes (append lst lstp)) (cdr tail)))
                              (else
                                 (cons node tail)))))
                     (else
                        (cons node tail))))
               null nodes)
            nodes)) ;; later this would be a bug in parser


      ;;; 
      ;;; Connecting tag openings to closes 
      ;;; 

      ;; reverse-nodes tag → open-tag|#false content-nodes|#false remaining-reverse-nodes|#false
      (define (find-open rnodes tag)
         (let loop ((ns rnodes) (out null))
            (if (null? ns)
               (values #false #false #false)
               (let ((this (car ns)))
                  (if (eq? (ref this 1) 'open)
                     (if (eq? (ref this 2) tag)
                        (values this out (cdr ns))
                        (loop (cdr ns) (cons this out)))
                     (loop (cdr ns) (cons this out)))))))

      (define (shrub nodes done)
         (if (null? nodes)
            (reverse done)
            (let ((node (car nodes)))
               (tuple-case node
                  ((bytes lst)
                     (shrub (cdr nodes) (cons node done)))
                  ((open tag attrs)
                     (shrub (cdr nodes) (cons node done)))
                  ((close tag)
                     (lets ((open content donep (find-open done tag)))
                        (if open
                           (lets ((skip tag attrs open))
                              (shrub (cdr nodes)
                                 (cons (tuple 'tag tag attrs content) donep)))
                           (shrub (cdr nodes) (cons node done)))))
                  (else
                     (shrub (cdr nodes) (cons node done)))))))

      (define (shrubberies nodes)
         (shrub nodes null))


      ;;;
      ;;; Collecting basic tag info
      ;;;
   
      (define open-only   0) ;; <p>, or just tag opening seen for now
      (define open-single 1) ;; <p />
      (define open-full   2) ;; <foo> ... </foo>

      ;; add a tag and update type if necessary
      ; st is a ff of tag → (tagtype . attrs)
      ; attrs is a ff of attribute → some-seen-value
      (define (add-tag st tag type)
         (let ((val (getf st tag)))
            (if val
               (if (lesser? type (car val))
                  (put st tag (cons type (cdr val))) ;; update tag type to higher ones (typically 0 -> 2)
                  st)
               (begin
                  ;(print "saw tag " (list->string tag) " for the first time")
                  (put st tag (cons type #empty))))))

      (define (numberish? x)
         (if x 
            (m/^[0-9]+$/ x)
            x))

      (define (number->base10 n)
         (if (eq? n 0)
            (list #\0)
            (let loop ((n n) (out null))
               (if (eq? n 0)
                  out
                  (lets ((q r (quotrem n 10)))
                     (loop q (cons (+ r #\0) out)))))))

      (define (gennum n)
         (λ (rs) 
            (lets ((rs n (mutate-num rs n)))
               (values rs (number->base10 n)))))

      ;; todo: store attribute value(s) more intelligently
      ;; add tag and attrs if any
      ; attrs = ((label . value) ...), both are just byte lists
      (define (add-tag-attrs st tag attrs type)
         (let ((st (add-tag st tag type)))
            (if (null? attrs)
               st
               (lets
                  ((info (getf st tag))
                   (type known info)
                   (attrs 
                     (fold 
                        (λ (known attr) 
                           (put known (car attr) 
                              (if (numberish? (cdr attr))
                                 (gennum (string->number (list->string (cdr attr)) 10))
                                 (cdr attr))))
                        known attrs)))
                  (fupd st tag (cons type attrs))))))

      (define (store-tags st ns)
         (define (store st n)
            (tuple-case n
               ((bytes lst) st)
               ((open tag attrs)
                  (add-tag-attrs st tag attrs open-only))
               ((close tag)
                  (add-tag-attrs st tag null open-full)) ;; implied opening somewhere
               ((open-single tag attrs)
                  (add-tag-attrs st tag attrs open-single))
               ((tag name attrs data)
                  (add-tag-attrs (fold store st data) name attrs open-full))
               (else
                  (print-to stderr "tag-info: bad node " n))))
         (fold store st ns))


      ;;;
      ;;; All passes (byte list → AST node list + info collection/interning)
      ;;;

      (define (print-tag-info tags)
         (print "-----------------------------8<-----------------------------")
         (print "What I know so far:")
         (ff-fold
            (λ (_ tag info)
               (lets ((type attrs info))
                  (print " - '" (list->string tag) "' is a tag of type " (if (= type open-only) 'open-only (if (= type open-single) 'open-single 'full)))
                  (ff-fold 
                     (λ (_ tag val)
                        (for-each display (list "    + attribute '" (list->string tag) "'"))
                        (if val
                           (if (function? val) 
                              (print ", function generator in use")
                              (print ", e.g. '" (list->string val) "'"))
                           (print ", no value seen")))
                     _ attrs)))
            42 tags)
         (print "-----------------------------8<-----------------------------"))

      (define (xp-process tree tags lst)
         (lets
            ((nodes (xp-parse lst))           ;; bytes → (node ...)
             (nodes (merge-bytes nodes))      ;; connect adjecent byte nodes (may be split by parser)
             (tree nodes (intern-tags tree nodes)) ;; equal? tags and attributes are now eq? - TODO pass rs and reservoir sample attrs
             (nodes (shrubberies nodes))     ;; .. #(open X attrs) ... #(close X) ..  → .. #(tag X attrs (...)) .. 
             (tags (store-tags tags nodes)))
            ;(print-tag-info tags)
            (values tree tags nodes)))


      ;;;
      ;;; Rendering (compact)
      ;;;

      (define (render-attrs attrs tail)
         (if (null? attrs)
            tail
            (lets
               ((this (car attrs))
                (name value this)
                (tail (render-attrs (cdr attrs) tail)))
               (cons #\space
                  (append name
                     (if value
                        (cons #\= (append value tail))
                        tail))))))

      (define (render-open-tag name attrs tail)
         (cons #\<
            (append name
               (render-attrs attrs
                  (cons #\> tail)))))
      
      (define (render-open-single-tag name attrs tail)
         (cons #\<
            (append name
               (render-attrs attrs
                  (ilist #\space #\/ #\> tail)))))

      (define (render-close-tag name tail)
         (ilist #\< #\/
            (append name (ilist #\> tail))))

      (define (xp-render exp)
         (define (ren exp tail)
            (cond
               ((list? exp)
                  (foldr
                     (λ (thing tail) (ren thing tail))
                     tail exp))
               ((tuple? exp)
                  (tuple-case exp
                     ((bytes bs)
                        (append bs tail))
                     ((tag name attrs content)
                        (render-open-tag name attrs
                           (ren content
                              (render-close-tag name tail))))
                     ((open name attrs)
                        (render-open-tag name attrs tail))
                     ((open-single name attrs)
                        (render-open-single-tag name attrs tail))
                     ((close name)
                        (render-close-tag name tail))
                     ((plus a b)
                        (ren a (ren b tail)))
                     (else
                        (print*-to stderr (list "WTN IS " exp "???"))
                        (exit-owl 127))))
               (else
                  (print*-to stderr (list "WTN IS " exp "???"))
                  (exit-owl 126))))
         (ren exp null))




      ;;;
      ;;; AST mutations
      ;;;

      ;; pred → ((node ...) → (node' ...)) which satisfy pred
      (define (ast-find pred) 
         (λ (nodes)
            (define (find out n)
               (let ((out (if (pred n) (cons n out) out)))
                  (tuple-case n
                     ((plus a b)
                        (find (find out a) b))
                     ((tag name attrs content)
                        (fold find out content))
                     (else out))))
            (if (pair? nodes)
               (fold find null nodes)
               (find null nodes))))
     
      (define ast-opens
         (ast-find 
            (λ (x) 
               (let ((t (ref x 1))) 
                  (or (eq? t 'open) (eq? t 'tag) (eq? t 'open-single))))))

      (define ast-tags ;; only tags with content and an explicit end
         (ast-find (λ (x) (eq? (ref x 1) 'tag))))

      (define ast-bytes
         (ast-find (λ (x) (eq? (ref x 1) 'bytes))))

      (define ast-any
         (ast-find (λ (x) x)))

      (define ast-bytes-tags
         (ast-find (λ (x) (let ((t (ref x 1))) (or (eq? t 'bytes) (eq? t 'tag))))))

      ;; editor :: node → node' | node, no recursive edit of node'
      (define (ast-edit ns editor)
         (define (edit n)
            (let ((np (editor n)))
               (if (eq? n np)
                  (tuple-case n
                     ((tag name attrs content)
                        (tuple 'tag name attrs 
                           (map edit content)))
                     (else n))
                  np)))
         (if (pair? ns)
            (map edit ns)
            (edit ns)))

      ;;;
      ;;; MUTATIONS ---------------------------------------------------------
      ;;;

      ;; swap two nodes (including content)
      (define (xp-swap rs tags ns)
         (let ((opts (ast-opens ns)))
            (if (or (null? opts) (null? (cdr opts)))
               (values rs tags ns) ;; no two nodes to swap
               (lets ;; pick 2 distinct nodes at random
                  ((rs opts (random-permutation rs opts))
                   (a opts opts)
                   (b opts opts))
                  (values rs tags
                     (ast-edit ns
                        (λ (node) 
                           (cond 
                              ((eq? node a) b) 
                              ((eq? node b) a) 
                              (else node)))))))))

      ;; note: tags is not empty, because mutator is only called if there is at 
      ;; least one open-tag in the data, which is stored to tags prior to mutation
      (define (random-tag rs tags)
         (rand-elem rs (keys tags)))

      (define (random-attrs rs attrs)
         (let loop ((rs rs) (picked null) (attrs (ff->list attrs)))
            (if (null? attrs)
               (values rs picked)
               (lets ((rs a (rand rs 3))) ;; fixed 33% attribute copying probability for now
                  (if (eq? a 0)
                     (let ((node (car attrs))) ;; (label . value)
                        (if (function? (cdr node))
                           ;; generate a value
                           (lets ((rs value ((cdr node) rs)))
                              (loop rs (cons (cons (car node) value) picked) (cdr attrs)))
                           ;; copy a static value
                           (loop rs (cons (car attrs) picked) (cdr attrs))))
                     (loop rs picked (cdr attrs)))))))

      (define (generate-node rs tags node)
         (lets 
            ((rs tag (random-tag rs tags))
             (info (getf tags tag))
             (type attrs info)
             (rs attrs (random-attrs rs attrs)))
            ;(print "generating a '" (list->string tag) "'-tag with attrs " attrs)
            (values rs
               (cond
                  ((eq? type open-only) ;; <foo src=baz>
                     ;; goes always to left for now, so cannot be after a tag at end
                     (tuple 'plus 
                        (tuple 'open tag attrs)
                        node))
                  ((eq? type open-single)
                     (tuple 'plus
                        (tuple 'open-single tag attrs)
                        node))
                  ((eq? type open-full)
                     (tuple 'tag tag attrs (list node)))
                  (else
                     (error "generate-node: unexpected node type " type))))))

      ;; generate a tag next to or around a leaf byte sequence
      (define (xp-insert rs tags ns)
         (lets 
            ((opts (ast-bytes-tags ns))
             (opts (if (null? opts) (ast-any ns) opts))
             (rs target (rand-elem rs opts))
             (rs new (generate-node rs tags target))
             (edit (λ (x) (if (eq? x target) new x))))
            (values rs tags (ast-edit ns edit))))

      ;; duplicate a node
      (define (xp-dup rs tags ns)
         (lets ((opens (ast-opens ns)))
            (if (null? opens)
               (values rs tags ns)
               (lets
                  ((rs target (rand-elem rs opens))
                   (edit (λ (x) (if (eq? x target) (tuple 'plus x x) x))))
                  (values rs tags (ast-edit ns edit))))))

      ;; each pump-step can grow a data block by a little less than 
      ;; its size, so a max of 15 bits can lead to an occasional <128MB
      ;; output (intentionally)

      (define max-pump-limit #b100000000000000)

      (define (pumps rs)
         (let loop ((rs rs) (max 2))
            (lets ((x rs (uncons rs #false)))
               (if (or (eq? 0 (fxband x 1)) (eq? max max-pump-limit))
                  (lets ((rs n (rand rs max)))
                     (values rs (+ n 2)))
                  (loop rs (<< max 1))))))

      ;; choose one, preferably from the beginning
      (define (pick-some rs lst)
         (let loop ((rs rs) (this (car lst)) (lst (cdr lst)))
            (if (null? lst) 
               (values rs this)
               (lets ((d rs (uncons rs #false)))
                  (if (eq? 0 (fxband d 1))
                     (values rs this)
                     (loop rs (car lst) (cdr lst)))))))

      ;; repeat a path in parse tree
      (define (xp-pump rs tags ns)
         (lets
            ((opts (ast-tags ns))
             (opts (sort (λ (a b) (lesser? b a)) opts))) ;; hack warning, this moves with parent nodes to top
            (if (null? opts)
               (values rs tags ns) ;; nothing to pump
               (lets
                  ((rs start (pick-some rs opts)) ;; start node of path copying
                   (targets (ast-bytes-tags start))
                   (targets (keep (λ (x) (not (eq? x start))) targets))) ;; don't target self (no changes)
                  (if (null? targets) ;; don't want to target closes or opens, could break too much structure
                     (values rs tags ns)
                     (lets 
                        ((rs end (rand-elem rs targets))
                         (rs n (pumps rs)))
                        ;(print "Pumping " n " times path between '" (list->string (xp-render start)) "' up to '" (list->string (xp-render end)) "'")
                        (let loop ((n n) (branch start))
                           (if (eq? n 0)
                              (values rs tags
                                 (ast-edit ns (λ (x) (if (eq? x start) branch x))))
                              (loop (- n 1)
                                 (ast-edit start (λ (x) (if (eq? x end) branch x))))))))))))

      (define (repeat-node rs node)
         (lets 
            ((rs n (pumps rs)))
            (values rs 
               (let loop ((out node) (n n))
                  (if (eq? n 0)
                     out
                     (loop (tuple 'plus node out) (- n 1)))))))

      ;; repeat potentially many times
      (define (xp-repeat rs tags ns)
         (lets ((opts (ast-opens ns)))
            (if (null? opts)
               (values rs tags ns)
               (lets
                  ((rs target (rand-elem rs opts))
                   (rs many (repeat-node rs target))
                   (edit (λ (x) (if (eq? x target) many x))))
                  (values rs tags (ast-edit ns edit))))))

      (define (random-mutator rs)
         (lets 
            ((rs x (rand rs 6)))
            (cond
               ((eq? x 0) (values rs xp-swap 'xp-swap))         ;; two nodes exchange positions
               ((eq? x 1) (values rs xp-dup 'xp-dup))           ;; node node is duplicated
               ((eq? x 2) (values rs xp-pump 'xp-pump))         ;; parse tree path is repeated
               ((eq? x 3) (values rs xp-repeat 'xp-repeat))     ;; one node is cloned many times
               (else      (values rs xp-insert 'xp-insert)))))  ;; a new nodes is born

      (define (mutate rs tags nodes)
         (lets 
            ((rs muta op-name (random-mutator rs))
             (rs tags ns (muta rs tags nodes)))
            (if (eq? ns nodes) ;; mutation failed, try something else
               (mutate rs tags nodes)
               (values rs tags ns op-name))))

      ;;;
      ;;; Entry
      ;;;

      ;; if length is greater than one, there must be a tag, because adjecent byte sequences are merged
      (define (taggy? nodes)
         (cond
            ((null? nodes) #false)
            ((null? (cdr nodes)) #false)
            (else #true)))

      ;; (mutator are rs ll meta) → mutator' rs' ll' meta' delta
      ;                    .------------> tag/attribute name interning store
      ;                    |     .------> information about seen tags and their attributes
      (define (xp-mutator tree tags)
         (λ (rs ll meta)
            (lets ((tree tags nodes (xp-process tree tags (vector->list (car ll)))))
               (if (taggy? nodes)
                  (lets 
                     ((rs tags nodes op-name (mutate rs tags nodes))
                      (lst (xp-render nodes)))
                     (values (xp-mutator tree tags) rs 
                        (flush-bvecs lst (cdr ll)) 
                        (inc meta op-name)
                        +1))
                  (values (xp-mutator tree tags) rs ll meta -1))))) ;; Doesn't look too XMLish to me

      (define xp-mutate 
         (xp-mutator #empty #empty))))




;      ;;; Rendering (cute overload indented output, mainly useful for debugging)
;      ;;;
;
;      (define (indent i tail)
;         (if (eq? i 0)
;            tail
;            (ilist 32 32 (indent (- i 1) tail))))
;
;      (define (render-attrs attrs tail)
;         (if (null? attrs)
;            tail
;            (lets
;               ((this (car attrs))
;                (name value this)
;                (tail (render-attrs (cdr attrs) tail)))
;               (cons #\space
;                  (append name
;                     (if value
;                        (cons #\= (append value tail))
;                        tail))))))
;
;      (define (render-open-tag name attrs tail)
;         (cons #\<
;            (append name
;               (render-attrs attrs
;                  (ilist #\> #\newline tail)))))
;      
;      (define (render-open-single-tag name attrs tail)
;         (cons #\<
;            (append name
;               (render-attrs attrs
;                  (ilist #\space #\/ #\> #\newline tail)))))
;
;      (define (render-close-tag name tail)
;         (ilist #\< #\/
;            (append name (ilist #\> #\newline tail))))
;
;      (define (pretty-render exp)
;         (define (ren exp i tail)
;            (cond
;               ((list? exp)
;                  (foldr
;                     (λ (thing tail) (ren thing i tail))
;                     tail exp))
;               ((tuple? exp)
;                  (tuple-case exp
;                     ((bytes bs)
;                        (indent i
;                           (append bs 
;                              (cons #\newline tail))))
;                     ((tag name attrs content)
;                        (indent i
;                           (render-open-tag name attrs
;                              (ren content (+ i 1)
;                                 (indent i
;                                    (render-close-tag name tail))))))
;                     ((open name attrs)
;                        (indent i
;                           (render-open-tag name attrs tail)))
;                     ((open-single name attrs)
;                        (indent i
;                           (render-open-single-tag name attrs tail)))
;                     ((close name)
;                        (indent i
;                           (render-close-tag name tail)))
;                     ((plus a b)
;                        (ren a i (ren b i tail)))
;                     (else
;                        (print*-to stderr (list "WTN IS " exp "???"))
;                        (exit-owl 127))))
;               (else
;                  (print*-to stderr (list "WTN IS " exp "???"))
;                  (exit-owl 126))))
;         (ren exp 0 null))
;      
;      (define (pretty-print exp)
;         (let ((output (pretty-render exp)))
;            (write-vector (list->vector output) stdout)))
