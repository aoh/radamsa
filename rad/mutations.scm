;;;
;;; Stream Mutations
;;;

(define-library (rad mutations)

   (import 
      (owl base)
      (rad generic)
      (rad shared))

   (export 
      *mutations*
      default-mutations
      string->mutators          ;; cmdline-string → (mutator-node ...)
      mutators->mutator)        ;; rs (mutator-node ...) → rs' mutator

   (begin

      (define min-score 2)   ;; occurrence-priority = score*priority / total
      (define max-score 10)

      (define p-weakly-usually 11/20)

      ;;;
      ;;; Random delta generators for random priority steppers
      ;;;

      ;; random delta for brownian steppers
      (define (rand-delta rs)
         (lets ((digit rs (uncons rs #f)))
            (if (eq? 0 (fxband digit 1))
               (values rs +1)
               (values rs -1))))

      ;; random delta with a slight positive bias
      (define (rand-delta-up rs)
         (lets ((rs occ (rand-occurs? rs p-weakly-usually)))
            (if occ
               (values rs +1)
               (values rs -1))))

      ;; quick peek if the data looks possibly binary
      ;; quick stupid version: ignore UTF-8, look for high bits
      (define (binarish? lst)
         (let loop ((lst lst) (p 0))
            (cond
               ((eq? p 8) (stderr-probe "BINARY: NO" #false))
               ((null? lst) (stderr-probe "BINARY: NO" #false))
               ((eq? (car lst) 0) (stderr-probe "BINARY: YES" #true))
               (else
                  (if (eq? 0 (fxband 128 (car lst)))
                     (loop (cdr lst) (+ p 1))
                     (stderr-probe "BINARY: YES" #true))))))

      ;; val++ in ff, or insert 1
      (define (inc ff key)
         (let ((val (getf ff key)))
            (if val
               (fupd ff key (+ val 1))
               (put ff key 1))))

      ;; (byte ...) bvec-ll → (bvec .. . bvec-ll)
      (define (flush-bvecs byte-list tail)
         (let loop ((len (length byte-list)) (lst byte-list))
            (if (< len avg-block-size)
                (cons (list->byte-vector lst) tail)
                (let
                  ((hd (take lst avg-block-size))
                   (tl (drop lst avg-block-size)))
                  (cons (list->byte-vector hd)
                     (loop (- len avg-block-size) tl))))))

      ;; clone a byte vector to a list and edit at given position (using primops since this is heavily used)
      (define (edit-byte-vector bvec edit-pos fn)
         (let ((len (sizeb bvec)))
            (if (eq? len 0)
               bvec
               (let loop ((pos (- len 1)) (out null))
                  (let ((val (refb bvec pos)))
                     (if (eq? pos edit-pos)
                        (if (eq? pos 0)
                           (list->byte-vector (fn val out))
                           (lets ((pos _ (fx- pos 1)))
                              (loop pos (fn val out))))
                        (if (eq? pos 0)
                           (list->byte-vector (cons val out))
                           (lets ((pos _ (fx- pos 1)))
                              (loop pos (cons val out))))))))))



      ;;;
      ;;; Number Mutator
      ;;;

      (define (digit-val d)
         (cond
            ((lesser? d 48) #false)
            ((lesser? 57 d) #false)
            (else (- d 48))))

      ; → digit|F tail 
      (define (get-num lst)
         (let loop ((lst lst) (n 0) (digits 0))
            (cond
               ((null? lst)
                  (if (eq? digits 0)
                     (values #false null)
                     (values n null)))
               ((digit-val (car lst)) =>
                  (λ (d) (loop (cdr lst) (+ d (* n 10)) (+ digits 1))))
               ((eq? digits 0)
                  (values #false lst))
               (else
                  (values n lst)))))

      ;; copy from pos up to end (not countin it)
      (define (copy-range pos end tail)
         (if (eq? pos end)
            tail
            (cons (car pos) (copy-range (cdr pos) end tail))))

      (define interesting-numbers 
         (list->vector 
            (fold 
               (λ (o x) 
                  (let ((x (<< 1 x)))
                     (ilist (- x 1) x (+ x 1) o)))
               null
               '(1 7 8 15 16 31 32 63 64 127 128))))

      ;; fixme: simple placeholder
      (define (mutate-num rs num)
         (lets ((rs n (rand rs 12)))
            (cond
               ((eq? n 0)  (values rs (+ n 1)))
               ((eq? n 1)  (values rs (- n 1)))
               ((eq? n 2)  (values rs 0)) ;; todo, pack funny nums to a list and reduce opts
               ((eq? n 3)  (values rs 1))
               ((eq? n 4)  (rand-elem rs interesting-numbers))
               ((eq? n 5)  (rand-elem rs interesting-numbers))
               ((eq? n 6)  (rand-elem rs interesting-numbers))
               ((eq? n 7)  (lets ((rs x (rand-elem rs interesting-numbers))) (values rs (+ num x))))
               ((eq? n 8)  (lets ((rs x (rand-elem rs interesting-numbers))) (values rs (- x num))))
               ((eq? n 9)  (lets ((rs m (rand rs (* n 2)))) (values rs (- n m))))
               (else  
                  (lets
                     ((rs n (rand-range rs 1 129))
                      (rs n (rand-log rs n))
                      (rs s (rand rs 3))) ;; add more likely 
                     (values rs
                        ((if (eq? s 0) - +) num n)))))))

      (define (mutate-a-num rs lst nfound)
         (if (null? lst)
            (lets ((rs which (rand rs nfound)))
               ;; choose which to mutate (or 0 if none)
               (values rs which null))
            (lets ((valp lstp (get-num lst)))
               (if valp
                  (lets ((rs which tail (mutate-a-num rs lstp (+ nfound 1))))
                     (if (eq? which 0) ;; i won
                        (lets
                           ((rs new (mutate-num rs valp))
                            (new-lst (render new tail)))
                           (values rs -1 (render new tail)))
                        (values rs (- which 1)
                           (copy-range lst lstp tail))))
                  (lets ((rs which tail (mutate-a-num rs (cdr lst) nfound)))
                     (values rs which (cons (car lst) tail)))))))

      ;; todo: fixed scores, should be randomized
      (define (sed-num rs ll meta) ;; edit a number
         (lets
            ((lst (vec->list (car ll)))
             (rs n lst (mutate-a-num rs lst 0))
             (bin? (binarish? lst))
             (lst (flush-bvecs lst (cdr ll))))
            (cond
               ((eq? n 0) 
                  ;; low priority negative, because could be textual with less frequent numbers
                  (lets ((rs n (rand rs 10)))
                     (if (eq? n 0)
                        (values sed-num rs lst meta -1)
                        (values sed-num rs lst meta 0))))
               (bin?
                  ;; found, but this looks a bit binary
                  (values sed-num rs lst (inc meta 'muta-num) -1))
               (else
                  ;; found and looks ok
                  (values sed-num rs lst (inc meta 'muta-num) +2)))))


      ;;;
      ;;; Byte-level Mutations
      ;;;

      ;; todo: swap to generals

      (define (sed-byte-drop rs ll meta) ;; drop byte
         (lets 
            ((rs p (rand rs (sizeb (car ll))))
             (rs d (rand-delta rs)))
            (values sed-byte-drop rs 
               (cons (edit-byte-vector (car ll) p (λ (old tl) tl)) (cdr ll))
               (inc meta 'byte-drop) d)))
      
      (define (sed-byte-inc rs ll meta) ;; increment a byte value mod 256
         (lets 
            ((rs p (rand rs (sizeb (car ll))))
             (rs d (rand-delta rs)))
            (values sed-byte-inc rs 
               (cons (edit-byte-vector (car ll) p (λ (old tl) (cons (band 255 (+ old 1)) tl))) (cdr ll))
               (inc meta 'byte-inc) d)))
      
      (define (sed-byte-dec rs ll meta) ;; increment a byte value mod 256
         (lets 
            ((rs p (rand rs (sizeb (car ll))))
             (rs d (rand-delta rs)))
            (values sed-byte-dec rs 
               (cons (edit-byte-vector (car ll) p (λ (old tl) (cons (if (= old 0) 255 (- old 1)) tl))) (cdr ll))
               (inc meta 'byte-dec) d)))

      (define (sed-byte-flip rs ll meta) ;; flip a bit in a byte
         (lets 
            ((rs p (rand rs (sizeb (car ll))))
             (rs pos (rand rs 8))
             (rs d (rand-delta rs))
             (b (<< 1 pos)))
            (values sed-byte-flip rs 
               (cons (edit-byte-vector (car ll) p (λ (old tl) (cons (bxor b old) tl))) (cdr ll))
               (inc meta 'byte-inc) d)))

      (define (sed-byte-insert rs ll meta) ;; insert a byte
         (lets 
            ((rs p (rand rs (sizeb (car ll))))
             (rs d (rand-delta rs))
             (rs b (rand rs 256)))
            (values sed-byte-insert rs 
               (cons (edit-byte-vector (car ll) p (λ (old tl) (ilist b old tl))) (cdr ll))
               (inc meta 'byte-insert) d)))
    
      ;; rs → rs' n
      (define (repeat-len rs)
         (let loop ((rs rs) (limit #b10))
            (lets ((rs x (rand rs 2)))
               (cond
                  ((eq? x 0) (rand rs limit))
                  ((= limit #x20000) (rand rs limit)) ; max 128k
                  (else (loop rs (<< limit 1)))))))
      
      (define (push n x lst)
         (if (eq? n 0)
            lst
            (push (- n 1) x (cons x lst))))

      (define (sed-byte-repeat rs ll meta) ;; insert a byte
         (lets 
            ((rs p (rand rs (sizeb (car ll))))
             (rs d (rand-delta rs))
             (rs n (repeat-len rs)))
            (values sed-byte-repeat rs 
               (cons (edit-byte-vector (car ll) p (λ (old tl) (push n old (cons old tl)))) (cdr ll))
               (inc meta 'byte-repeat) d)))
      
      (define (sed-byte-random rs ll meta) ;; swap a byte
         (lets 
            ((rs p (rand rs (sizeb (car ll))))
             (rs d (rand-delta rs))
             (rs b (rand rs 256)))
            (values sed-byte-random rs 
               (cons (edit-byte-vector (car ll) p (λ (old tl) (cons b tl))) (cdr ll))
               (inc meta 'byte-random) d)))
      
      (define (sed-byte-perm rs ll meta) ;; permute a few bytes
         (lets 
            ((rs p (rand rs (sizeb (car ll))))
             (rs d (rand-delta rs)))
            (values sed-byte-perm rs 
               (cons 
                  (edit-byte-vector (car ll) p 
                     (λ (old tl)
                        (lets
                           ((lst (cons old tl))
                            (rs n (rand-range rs 2 20)) ;; fixme: magic constant, should use something like repeat-len
                            (head (take lst n))
                            (tail (drop lst n))
                            (rs head (random-permutation rs head)))
                           (append head tail))))
                  (cdr ll))
               (inc meta 'byte-perm) d)))



      ;;;
      ;;; Shared sequences
      ;;;

      ;; (a b ...) → (a+a b ...)
      (define (sed-fuse-this rs ll meta) ;; jump between two shared suffixes in the block
         (lets
            ((lst (vector->list (car ll)))
             (rs lst (list-fuse rs lst lst))
             (rs d (rand-delta-up rs)))
            (values sed-fuse-this rs 
               (flush-bvecs lst (cdr ll))
               (inc meta 'fuse-this)
               d)))

      ;; lst → a b, a ++ b == lst, length a = length b +/- 1
      (define (split lst)
         (define (walk t h out)
            (if (null? h)
               (values (reverse out) t)
               (let ((h (cdr h)))
                  (if (null? h)
                     (values (reverse out) t)
                     (walk (cdr t) (cdr h) (cons (car t) out))))))
         (walk lst lst null))

      ;; (a b ...) → (a+b+a b ...)
      (define (sed-fuse-next rs ll meta)
         (lets
            ((al1 al2 (split (vector->list (car ll))))
             (b ll (uncons (cdr ll) (car ll))) ;; next or current
             (bl (vector->list b))
             (rs abl (list-fuse rs al1 bl))
             (rs abal (list-fuse rs abl al2))
             (rs d (rand-delta-up rs)))
            (values sed-fuse-next rs 
               (flush-bvecs abal ll) ;; <- on avg 1x, max 2x block sizes
               (inc meta 'fuse-next)
               d)))

      (define (sed-fuse-old rs ll meta)
         (define (remember block)
            (λ (rs ll meta)
               (lets
                  ((al1 al2 (halve (vector->list (car ll))))
                   (ol1 ol2 (halve (vector->list block)))
                   (rs a (list-fuse rs al1 ol1)) ; a -> o
                   (rs b (list-fuse rs ol2 al2)) ; o -> a
                   (rs d (rand-delta-up rs)))
                  (values (remember (car ll)) rs 
                     (flush-bvecs a (flush-bvecs b ll)) ;; <- on avg 1x, max 2x block sizes
                     (inc meta 'fuse-old)
                     d))))
         ((remember (car ll)) rs ll meta))


      ;;;
      ;;; Byte Sequences
      ;;;

      ;; stutter 

      (define (sed-seq-repeat rs ll meta)
         (lets ((n (sizeb (car ll))))
            (if (< n 2)
               (values sed-seq-repeat rs ll meta 0)
               (lets
                  ((rs start (rand-range rs 0 (- n 1)))
                   (rs end (rand-range rs (+ start 1) n))
                   (pre (map (λ (p) (refb (car ll) p)) (iota 0 1 start)))
                   (post (map (λ (p) (refb (car ll) p)) (iota end 1 (sizeb (car ll)))))
                   (stut (list->byte-vector (map (λ (p) (refb (car ll) p)) (iota start 1 end))))
                   (rs n (rand-log rs 10)) ; max 2^10 = 1024 stuts
                   (n (max 2 n))
                   (rs delta (rand-delta rs))
                   (stuts
                     (fold
                        (λ (tl n) (cons stut tl))
                        (if (null? post)
                           (cdr ll)
                           (cons (list->byte-vector post) (cdr ll)))
                        (iota 0 1 n)))
                   (ll (if (null? pre) stuts (cons (list->byte-vector pre) stuts))))
                  (values sed-seq-repeat rs ll (inc meta 'seq-repeat) delta)))))


      ;;;
      ;;; Lines
      ;;;

      ;; bvec → ((... 10) .. (... [10])), cut after newlines
      (define (lines bvec)
         (let loop ((lst (vector->list bvec)) (buff null) (out null))
            (if (null? lst)
               (if (null? buff)
                  (reverse out)
                  (reverse (cons (reverse buff) out)))
               (lets ((hd lst lst))
                  (if (eq? hd 10) ;; newline
                     (loop lst null (cons (reverse (cons 10 buff)) out))
                     (loop lst (cons hd buff) out))))))

      ;; #u8[byte ...] → ((byte ... 10) ...) | #false, if this doesn't look like line-based text data
      (define (try-lines bvec)
         (lets ((ls (lines bvec)))
            (cond
               ((null? ls) ;; no data
                  #false)
               ((binarish? (car ls)) ;; first line (start of block) looks binary
                  #false)
               (else ls))))

      (define (unlines ls) 
         (foldr append null ls))

      (define (line-op op name)
         (define (self rs ll meta)
            (let ((ls (try-lines (car ll))))
               (if ls
                  (lets ((rs ls (op rs ls)))
                     (values self rs 
                        (flush-bvecs (unlines ls) (cdr ll))
                        (inc meta name) 1))
                  (values self rs ll meta -1))))
         self)

      (define sed-line-del (line-op list-del 'line-del))
      (define sed-line-dup (line-op list-dup 'line-dup))
      (define sed-line-clone (line-op list-clone 'line-clone))
      (define sed-line-repeat (line-op list-repeat 'line-repeat))
      (define sed-line-swap (line-op list-swap 'line-swap))
      (define sed-line-perm (line-op list-perm 'line-perm))


      ;;;
      ;;; UTF-8
      ;;;

      (define funny-unicode
         (list->tuple
            (append
               (list ;; some manual ones
                  (list 239 191 191)     ;; 65535
                  (list 240 144 128 128) ;; 65536
                  (list #xef #xbb #xbf)  ;; the canonical utf8 bom
                  (list #xfe #xff)       ;; utf16 be bom
                  (list #xff #xfe)       ;; utf16 le bom
                  (list 0 0 #xff #xff)   ;; ascii null be
                  (list #xff #xff 0 0)   ;; ascii null le
                  (list 43 47 118 56)    ;; and some others from wikipedia
                  (list 43 47 118 57) (list 43 47 118 43) (list 43 47 118 47)
                  (list 247 100 76) (list 221 115 102 115) (list 14 254 255) (list 251 238 40)
                  (list 251 238 40 255) (list 132 49 149 51))
               (map ;; some valid points and ranges
                  (λ (codepoint)
                     (render (list->string (list codepoint)) null)) ;; <- make UTF-8 repr
                  (fold
                     (λ (tl node)
                           (if (pair? node) ;; inclusive range
                              (append (iota (car node) 1 (+ (cdr node) 1)) tl)
                              (cons node tl)))
                     null
                     '((#x0009 . #x000d) #x00a0 #x1680 #x180e (#x2000 . #x200a) #x2028 #x2029 #x202f #x205f
                       #x3000 (#x200e . #x200f) (#x202a . #x202e) (#x200c . #x200d) #x0345 #x00b7 (#x02d0 . #x02d1)
                       #xff70 (#x02b0 . #x02b8) #xfdd0 #x034f (#x115f . #x1160) (#x2065 . #x2069) #x3164 #xffa0
                       #xe0001 (#xe0020 . #xe007f) (#x0e40 . #x0e44) #x1f4a9))))))

      (define (sed-utf8-widen rs ll meta)
         (lets 
            ((rs p (rand rs (sizeb (car ll))))
             (rs d (rand-delta rs))
             (ll
               (cons
                  (edit-byte-vector (car ll) p
                     (λ (old tl)
                        ;; assuming we hit a 6-bit ascii char, make it unnecessarily wide
                        ;; which might confuse a length calculation
                        (if (eq? old (band old #b111111))
                           (ilist #b11000000 (bor old #b10000000) tl)
                           ;; fixme: find the next valid point (if any) and do this properly
                           (cons old tl))))
                  (cdr ll))))
            (values sed-utf8-widen rs ll (inc meta 'utf8-widen) d)))

      ;; insert UTF-8 that might be mishandled
      (define (sed-utf8-insert rs ll meta)
         (lets
            ((rs p (rand rs (sizeb (car ll)))) 
             (rs d (rand-delta rs))
             (rs bytes (rand-elem rs funny-unicode)))
            (values sed-utf8-insert rs 
               (cons (edit-byte-vector (car ll) p (λ (old tl) (append bytes (cons old tl)))) (cdr ll))
               (inc meta 'utf8-insert) d)))


      ;;;
      ;;; Guessed Parse-tree Mutations
      ;;;

      (define usual-delims ; <- could be settable from command line
         (list->ff
            '((40 . 41)    ; ()
              (91 . 93)    ; []
              (60 . 62)    ; <>
              (123 . 125)  ; {}
              (34 . 34)    ; ""
              (39 . 39)))) ; ''

      ;; → lst #false = ran out of data trying to parse up to close, but lst is the same with partial parsing
      ;; → lst tail-lst = did successfully parse up to close. ready node is lst, tail is following data
      (define (grow lst close rout)
         (if (null? lst)
            (values (reverse rout) #false) ;; out of data, didn't find close. return partial parse.
            (lets ((hd lst lst))
               (cond
                  ((eq? hd close)
                     ;; match complete, return with rest of list
                     (values (reverse (cons close rout)) lst))
                  ((get usual-delims hd #false) =>
                     (λ (next-close)
                        (lets ((this lst (grow lst next-close null)))
                           (if lst ;; we didn't run out of data and this is a single tree node
                              (grow lst close (cons (cons hd this) rout))
                              ;; we ran out of data. this is a list of partial parses (having the data of
                              ;; lst after hd in some form) which we want to preserve as tail
                              (values (append (reverse rout) (cons hd this)) #false)))))
                  (else ;; add one byte to this node
                     (grow lst close (cons hd rout)))))))

      ;; count how many list nodes are in a tree structure
      (define (count-nodes lst)
         (let loop ((lst lst) (n 0))
            (cond
               ((null? lst) n)
               ((pair? (car lst))
                  (loop (cdr lst)
                     (loop (car lst) (+ n 1))))
               (else
                  (loop (cdr lst) n)))))

      ;; lst → a list of lists (not counting tails) in lst, when walked recursively, not counting lst itself
      (define (sublists lst)
         (let loop ((lst lst) (found null))
            (if (null? lst)
               found
               (let ((hd (car lst)))
                  (if (pair? hd)
                     (loop (cdr lst)
                        (loop hd (cons hd found)))
                     (loop (cdr lst) found))))))

      (define (pick-sublist rs lst)
         (let ((subs (sublists lst)))
            (if (null? subs)
               (values rs #false)
               (lets ((rs n (rand rs (length subs))))
                  (values rs (lref subs n))))))

      ;; replace the node (sub . tail) with (op (sub . tail))
      (define (edit-sublist lst sub op)
         (if (pair? lst)
            (if (eq? (car lst) sub)
               (op lst)
               (cons (edit-sublist (car lst) sub op)
                     (edit-sublist (cdr lst) sub op)))
            lst))

      ;; lst (ff of node → (node → node)) → lst' ; <- could also be done with a recursive-mapn
      (define (edit-sublists lst opff)
         (if (pair? lst)
            (let ((hd (car lst)))
               (if (pair? hd)
                  (let ((maybe-op (get opff hd #false)))
                     (if maybe-op
                        (cons (maybe-op (car lst))
                           (edit-sublists (cdr lst) opff))
                        (cons (edit-sublists (car lst) opff)
                           (edit-sublists (cdr lst) opff))))
                  (cons (car lst)
                     (edit-sublists (cdr lst) opff))))
            lst))

      (define (partial-parse lst abort)
         (if (binarish? lst)
            (abort)
            (let loop ((lst lst) (rout null))
               (if (null? lst)
                  (reverse rout)
                  (lets ((closep (get usual-delims (car lst) #false)))
                     (if closep
                        (lets
                           ((hd (car lst))
                            (this lst (grow (cdr lst) closep null)))
                           (if lst
                              (loop lst (cons (cons hd this) rout))
                              (append (reverse rout) (cons hd this))))
                        (loop (cdr lst) (cons (car lst) rout))))))))

      (define (flatten node tl)
         (cond
            ((null? node) tl)
            ((pair? node) (flatten (car node) (flatten (cdr node) tl)))
            (else (cons node tl))))


      (define (sed-tree-op op name)
         (define (self rs ll meta)
            (lets/cc5 ret
               ((abort (λ () (ret self rs ll meta -1)))
                (meta (inc meta name))
                (lst (partial-parse (vector->list (car ll)) abort))
                ;(_ (if (not (equal? (vector->list (car ll)) (flatten lst null))) (error "partial parse bug: " (list (car ll) 'parses 'to lst))))
                (rs sub (pick-sublist rs lst)) ;; choose partially parsed node to mutate ;; fixme: not checked for F
                (lst (edit-sublist lst sub op)))
               (values self rs
                  (flush-bvecs (flatten lst null) (cdr ll))
                  meta +1)))
         self)

      ;; overwrite one node with one of the others
      (define (sed-tree-swap-one rs ll meta)
         (lets/cc5 ret
            ((abort (λ () (ret sed-tree-swap-one rs ll meta -1)))
             (lst (partial-parse (vector->list (car ll)) abort)) ;; (byte|node ...)
             (subs (sublists lst))
             (n (length subs)))
            (if (length< subs 2)
               ;; drop priority, nothing cool here
               (values sed-tree-swap-one rs ll meta -1)
               (lets
                  ((rs toswap (reservoir-sample rs subs 2))
                   (rs toswap (random-permutation rs toswap)) ;; not random for l=2
                   (a (car toswap))
                   (b (cadr toswap))
                   (lst (edit-sublist lst a (λ (node) (cons b (cdr node))))))
                  (values sed-tree-swap-one rs
                     (flush-bvecs (flatten lst null) (cdr ll))
                     (inc meta 'tree-swap-one)
                     +1)))))

      ;; pairwise swap of two nodes
      (define (sed-tree-swap-two rs ll meta)
         (lets/cc5 ret
            ((abort (λ () (ret sed-tree-swap-two rs ll meta -1)))
             (lst (partial-parse (vector->list (car ll)) abort)) ;; (byte|node ...)
             (subs (sublists lst)))
            (if (length< subs 2)
               ;; drop priority, nothing cool here
               (values sed-tree-swap-two rs ll meta -1)
               (lets
                  ((rs toswap (reservoir-sample rs subs 2))
                   (a (car toswap))
                   (b (cadr toswap))
                   (mapping (list->ff (list (cons a (λ (x) b)) (cons b (λ (x) a)))))
                   (lst (edit-sublists lst mapping)))
                  (values sed-tree-swap-two rs
                     (flush-bvecs (flatten lst null) (cdr ll))
                     (inc meta 'tree-swap-two)
                     +1)))))

      (define sed-tree-del (sed-tree-op (λ (node) null) 'tree-del))

      (define sed-tree-dup (sed-tree-op (λ (node) (cons (car node) node)) 'tree-dup))


      ;;; tree stutter

      (define (repeat-path parent child n)
         (if (< n 2)
            parent ; <- causes at least 1 extra path to be made, saves one useless replace cycle
            (edit-sublist parent child
               (λ (here) (cons (repeat-path parent child (- n 1)) (cdr here))))))

      (define (choose-child rs node)
         (let ((subs (sublists node)))
            (if (null? subs)
               (values rs #false)
               (rand-elem rs subs))))

      (define (choose-stutr-nodes rs subs)
         (if (null? subs)
            (values rs #false #false) ;; no child nodes
            (lets ((rs childp (choose-child rs (car subs))))
               (if childp
                  (values rs (car subs) childp)
                  (choose-stutr-nodes rs (cdr subs))))))

      (define (sed-tree-stutter rs ll meta)
         (lets/cc5 ret
            ((abort (λ () (ret sed-tree-stutter rs ll meta -1)))
             (lst (partial-parse (vector->list (car ll)) abort)) ;; (byte|node ...)
             (subs (sublists lst))
             (rs subs (random-permutation rs subs))
             (rs parent child (choose-stutr-nodes rs subs))
             (rs n-reps (rand-log rs 10)))
            (if parent
               (lets
                  ((lst
                     (edit-sublist lst child
                        (λ (node) (cons (repeat-path parent child n-reps) (cdr node))))))
                  (values sed-tree-stutter rs 
                     (flush-bvecs (flatten lst null) (cdr ll))
                     (inc meta 'tree-stutter)
                     +1))
               (values sed-tree-stutter rs ll meta -1))))

      ; [i]nsert
      ; [r]epeat
      ; [d]rop
      ; [s]wap/[s]tutter/[s]urf <- replace with [j]ump?

      (define *mutations*
         (list

            ;; [b]yte (single)
            (tuple "bd" sed-byte-drop "drop a byte")
            (tuple "bf" sed-byte-flip "flip one bit")
            (tuple "bi" sed-byte-insert "insert a random byte")
            (tuple "br" sed-byte-repeat "repeat a byte")
            (tuple "bp" sed-byte-perm "permute some bytes")
            (tuple "bei" sed-byte-inc "increment a byte by one")
            (tuple "bed" sed-byte-dec "decrement a byte by one")
            (tuple "ber" sed-byte-dec "swap a byte with a random one")

            ;; [s]equence of bytes

            (tuple "sr" sed-seq-repeat "repeat a sequence of bytes")

            ;; token

            ;; line
            
            (tuple "ld" sed-line-del "delete a line")
            (tuple "lr2" sed-line-dup "duplicate a line")
            (tuple "li" sed-line-clone "clone and insert it nearby")
            (tuple "lr" sed-line-repeat "repeat a line")
            (tuple "ls" sed-line-swap "swap two lines")
            (tuple "lp" sed-line-perm "swap order of lines")

            ;; tree

            (tuple "td" sed-tree-del "delete a node")
            (tuple "tr2" sed-tree-dup "duplicate a node")
            (tuple "ts1" sed-tree-swap-one "swap one node with another one")
            (tuple "ts2" sed-tree-swap-two "swap two nodes pairwise")
            (tuple "tr"  sed-tree-stutter "repeat a path of the parse tree")

            ;; utf-8

            (tuple "uw" sed-utf8-widen  "try to make a code point too wide")
            (tuple "ui" sed-utf8-insert "insert funny unicode")

            ;; special
            (tuple "num" sed-num "modify a textual number")

            ;; suffix (aka fuse)
            (tuple "ft" sed-fuse-this "jump to a similar position in block")
            (tuple "fn" sed-fuse-next "likely clone data between similar positions")
            (tuple "fo" sed-fuse-old "fuse previously seen data elsewhere")

            ))

      (define default-mutations
         "ft=2,fo=2,fn,num=3,td,tr2,ts1,tr,ts2,ld,lr2,li,ls,lp,lr,sr,bd,bf,bi,br,bp,bei,bed,ber,uw,ui")

      (define (name->mutation str)
         (or (choose *mutations* str)
             (begin
               (print*-to stderr (list "Unknown mutation: " str))
               #false)))

      ;;                                     ,---> is randomized after knowing seed
      ;; #f | (name . priority) → #f | #(max-score priority mutafn name)
      (define (priority->fuzzer node)
         (cond
            ((not node) #false)
            ((name->mutation (car node)) => 
               (λ (func) (tuple max-score (cdr node) func (car node))))
            (else #false)))

      ; limit to [min-score .. max-score]
      (define (adjust-priority pri delta)
         (if (eq? delta 0)
            pri
            (max min-score (min max-score (+ pri delta)))))

      ;; rs pris → rs' pris'
      (define (weighted-permutation rs pris)
         ;; show a sorted probability distribution 
         (stderr-probe
             ;(lets ((probs (sort car> (map (λ (x) (cons (* (ref x 1) (ref x 2)) (ref x 4))) pris)))
             ;       (all (fold + 0 (map car probs))))
             ;      (print*-to stderr (list "probs: " (map (λ (node) (cons (floor (/ (* (car node) 100) all)) (cdr node))) probs))))
            (sort car> (map (λ (x) (cons (ref x 1) (ref x 4))) pris))
            (lets    
               ((rs ppris ; ((x . (pri . fn)) ...)
                  (fold-map
                     (λ (rs node)
                        (lets
                           ((limit (* (ref node 1) (ref node 2))) ;; score * priority
                            (rs pri (rand rs limit)))
                           (values rs (cons pri node))))
                     rs pris))
                (ppris (sort car> ppris)))
               (values rs (map cdr (sort car> ppris))))))

      ;; Mutators have a score they can change themselves (1-100) and a priority given by 
      ;; the user at command line. Activation probability is (score*priority)/SUM(total-scores).

      ;; (#(score priority mutafn name) ...) → merged-mutafn :: rs ll meta → merged-mutafn' rs' ll' meta'
      (define (mux-fuzzers fs)
         (λ (rs ll meta)
            (let loop ((ll ll)) ;; <- force up to a data node
               (cond
                  ((pair? ll)
                     (lets ((rs pfs (weighted-permutation rs fs))) ;; (#(score priority mutafn name) ...)
                        (let loop ((pfs pfs) (out null) (rs rs)) ;; try each in order
                           (if (null? pfs) ;; no mutation worked
                              (values (mux-fuzzers out) rs ll meta)
                              (lets
                                 ((node (car pfs))
                                  (mscore mpri mfn mname node)
                                  (mfn rs mll mmeta delta 
                                    ((stderr-probe (list 'trying mname) mfn) rs ll meta))
                                  (out ;; always remember whatever was learned
                                    (cons (tuple (adjust-priority mscore delta) mpri mfn mname) out)))
                                 (if (equal? (car ll) (car mll)) 
                                    ;; try something else if no changes, but update state
                                    (loop (cdr pfs) out rs)
                                    (stderr-probe
                                       (list 'used mname) ; <- allow tracing and counting easily via stderr while testing
                                       (values (mux-fuzzers (append out (cdr pfs))) rs mll mmeta))))))))
                  ((null? ll)
                     (values (mux-fuzzers fs) rs ll meta))
                  (else 
                     (loop (ll)))))))

      ;; randomize mutator scores 
      (define (mutators->mutator rs mutas)
         (let loop ((rs rs) (mutas mutas) (out null))
            (if (null? mutas)
               (stderr-probe  
                  (map 
                     (λ (node) (list (ref node 4) 'score (ref node 1) 'pri (ref node 2)))
                     (sort (λ (a b) (> (* (ref a 1) (ref a 2)) (* (ref b 1) (ref b 2)))) out))
                  (values rs (mux-fuzzers out)))
               (lets ((rs n (rand rs max-score)))
                  (loop rs (cdr mutas) (cons (set (car mutas) 1 (max 2 n)) out))))))

      ;; str → (mutator-node ...) | #f, parse cmdline arg but no randomization yet (seed may not be known)
      (define (string->mutators str)
         (lets
            ((ps (map c/=/ (c/,/ str))) ; ((name [priority-str]) ..)
             (ps (map selection->priority ps))
             (fs (map priority->fuzzer ps)))
            (if (all self fs) 
               fs
               #false)))

))
