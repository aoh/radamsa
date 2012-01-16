#!/usr/bin/ol --run

;;;
;;; Radamsa - a general purpose test data fuzzer
;;;

;; Radamsa 0.1 (memory hog)  -> stable 0.1.9 
;; Radamsa 0.2 (pipe stream) -> stable 0.2.3
;; Radamsa 0.3 (funny fold)  <- we're about to go here next

;; fuzzer node info → ff of (name → str, description → text, meta → meta)

(define version-str "Radamsa 0.2.3")

(define avg-block-size 4096)

(import-old lib-args)

;; benchmarking & debugging utils
   
(define (flush-bvecs byte-list tail)
   (let loop ((len (length byte-list)) (lst byte-list))
      (if (< len avg-block-size)
          (cons (list->byte-vector lst) tail)
          (let 
            ((hd (take lst avg-block-size))
             (tl (drop lst avg-block-size)))
            (cons (list->byte-vector hd)
               (loop (- len avg-block-size) tl))))))

(define (renderer ob tl) 
   (render render ob tl))

(define  (length< lst n)
   (cond
      ((eq? n 0) False)
      ((null? lst) True)
      (else (length< (cdr lst) (- n 1)))))

(define (car> a b) (> (car a) (car b)))

(define (timed-thunk msg thunk)
   (lets
      ((pre (time-ms))
       (res (thunk))
       (elapsed (- (time-ms) pre)))
      (mail stderr
         (foldr renderer '(10) 
            (list "BENCH: " msg ": " elapsed "ms")))
      res))

(define-syntax timed-not ;; output timings to stderr
   (syntax-rules ()
      ((timed comment code)
         (timed-thunk comment (λ () code)))))

(define-syntax timed ;; kill the extra code
   (syntax-rules ()
      ((timed-not comment code) code)))

;; todo: stream muxer: cat a b → a ++ b, seen some bugs that needed this

(define rval-failed-to-start-modules 1)
(define rval-crashed 127) ;; in-owl thread crash after detecting an error

(define (car> a b) (> (car a) (car b)))

(define (pop-rand rs)
   (lets ((digit rs (uncons rs F))) rs))

;; force and send a stream to a port
(define (mail-blocks str port n)
   (cond
      ((pair? str)
         (lets
            ((block str str)
             (size (sizeb block)))
            (if (not (eq? size 0))
               (mail port block))
            (mail-blocks str port (+ n size))))
      ((null? str) n)
      (else (mail-blocks (str) port n))))

;; ll port → ok? n-written, temporarily always ok
(define (send-stream data port)
   (let ((res (mail-blocks data port 0)))
      (flush-port port) ; <- only necessary for stdout, since others are autoflushed on close soon
      (if res
         (values True res)
         (values False False))))

;; functions that get metadata about generated data
(define (record-meta/stderr n name info len)
   (mail stderr (foldr renderer '(10) (list n ": " name " -> " len "b"))))

;; functions that get metadata about generated data
(define (record-meta/verbose-stderr n name info len)
   (mail stderr (foldr renderer '(10) (list " " n ": " name " " info ": " len "b"))))

(define (string->vec str)
   (list->byte-vector (render render str null)))

(define (record-meta/not n name info len) 42)

;; todo: pass the meta recorder here
(define (get-stdout-fd n)
   (pair 
      (λ (ll record-meta)
         ;(record-meta n "" (car ll) "to-be-written/stdout") ;; <- when testing new crash-prone ones
         (lets ((ok? w (send-stream (cdr ll) stdout)))
            (record-meta n "stdout" (car ll) w)))
      (get-stdout-fd (+ n 1))))

(define (render-ip bvec)
   (runes->string
      (foldr
         (λ (byte tl)
            (render render byte (if (null? tl) tl (cons 46 tl))))
         null bvec)))

(define (render-ip-port bvec port)
   (runes->string
      (foldr
         (λ (byte tl)
            (render render byte (if (null? tl) tl (cons 46 tl))))
         (cons 58 (render render port null))
         bvec)))

;; todo: parameterize writers to take the meta recorder
(define (file-writer pat)
   (λ (nth)
      (λ (node record-meta)
         (lets ((info ll node))
            (define path
               (runes->string
                  (str-foldr
                     (λ (char tl)
                        (cond
                           ((null? tl)
                              (cons char tl))
                           ((eq? char 37) ; %
                              (case (car tl)
                                 ((110) (render render nth (cdr tl))) ; %n
                                 ;((102) (render render (get info 'name "anonimasu") (cdr tl))) ; %f
                                 (else (error "Unknown pattern in output path: " (list->string (cons char tl))))))
                           (else (cons char tl))))
                     null pat)))
            (define port 
               (open-output-file path))
            ;(record-meta nth path info "to-be-written") ;; <- when testing new crash-prone ones
            (if port
               (lets ((ok? written (send-stream ll port)))
                  (if ok?
                     (record-meta nth path info written)
                     (record-meta nth path info False))
                  (close-port port))
               (error "Cannot write to " path))))))

(define (in-range? min max)
   (λ (x) (and (<= min x) (<= x max))))

(define (make-tcp-connection ip port)
   (λ (n)
      (λ (node record-meta)
         (let loop ()
            (lets ((status wrote (tcp-send ip port (cdr node))))
               (cond
                  ((eq? status 'ok)
                     (record-meta n (render-ip-port ip port) (car node) wrote))
                  ((eq? status 'connect-error) ;; other end not ready to receive connection
                     (wait 100)
                     (loop))
                  ((and (eq? n 0) (eq? status 'write-error))
                     ;; hung up before accepting any data. retry.
                     ;; added to avoid a weird 'connection reset by peer' happening in unit tests.
                     (loop))
                  (else
                     ;; write error byt we did write something
                     (show "WRITE ERROR AFTER " wrote)
                     (record-meta n (render-ip-port ip port) (car node) 
                        status ;; <- could say "disconnected after n"
                        ))))))))

(define (serve-node numbered)
   (λ (node record-meta)
      ;; todo: do a few unblocking reads because the client might block until a request is received
      (lets 
         ((nth cli numbered)
          (ip fd cli))
         (lets 
            ((ip fd cli)
             (ok? np (blocks->socket (cdr node) fd))) ; <- closes fd, not blocks->fd due to win32 incompability
            (record-meta nth (render-ip ip) (car node) np)))))

;; str → ((info . fd) ...) | False
(define (make-output-stream str)
   (cond
      ((equal? str "-")
         (get-stdout-fd 1))
      ((m/^:[0-9]+$/ str)
         (let ((port (string->integer (s/^:// str))))
            (if (and port (>= port 0) (< port 65536))
               (let ((clis (tcp-clients port)))
                  (if clis
                     (lmap serve-node (lzip cons (lnums 1) clis))
                     False))
               (begin
                  (show "Bad port " port)
                  False))))
      ((m/^[0-9]{1,3}(\.[0-9]{1,3}){3}:[0-9]+$/ str)
         (lets
            ((ip-port (c/:/ str)) ;; todo: later these become 2-valued functions
             (port (string->integer (cadr ip-port)))
             (addr (map string->integer (c/\./ (car ip-port)))))
            (cond
               ((not (all (in-range? 0 255) addr))
                  (show "Bad ip address: " addr)
                  null)
               ((not ((in-range? 0 65535) port))
                  (show "Bad port: " port)
                  False)
               (else
                  (lmap (make-tcp-connection (list->vector addr) port)
                     (lnums 1))))))
      (else
         (lmap (file-writer str) (lnums 1)))))

(define basic-cl-opts
   `((help "-h" "--help" comment "Show this thing.")
     (output "-o" "--output" has-arg default "-" cook ,make-output-stream
         comment "Where to write the generated data?")
     (count "-n" "--count" cook ,string->integer check ,(λ (x) (> x 0))
         default "1" comment "How many outputs to generate?")
     (seed "-s" "--seed" has-arg comment "Random seed (any string).")
     (verbose "-v" "--verbose" comment "Be verbose when running.")
     (version "-V" "--version" comment "Show version information.")
     (examples "-E"  "--examples" comment "Show usage examples.")
     (advanced "-H"  "--big-help" comment "Show more options.")
))

(define (choose-type str)
   (cond
      ((equal? str "binary") 'binary)
      ((equal? str "text") 'text)
      (else 
         (print "The type must be either binary or text")
         F)))

(define (valid-probability? x)
   (and (number? x) (>= x 0) (< x 1)))

;; todo: -f | --fuzzers should probably be -m | --mutations, and muxers correspondingly -M | --muxers
(define advanced-cl-opts
   (append basic-cl-opts
      `((text-fuzzers "-t" "--text-fuzzers" has-arg default "b.*=1,s.*=2,sr=4,u.*=3,l.*=3,t.*=4,num=8"
            comment "Which local mutations to enable for textual data? (pass help to list options)")
        (binary-fuzzers "-b" "--binary-fuzzers" has-arg default "b.*=4,s.*=3,u.*"
            comment "Which local mutations to enable for binary data? (pass help to list options)")
        (list-fuzzers "-L" "--list-fuzzers" comment "Show a list of mutations, muxers and streamers.")
        (type       "-T" "--type" has-arg cook ,choose-type
            comment "choose manually what the data looks like (text or binary)")
        (generators "-g" "--generators" has-arg default "sample=100,flat,random" 
            comment "Which stream generators to run? (pass help to list options)")
        (polymerase "-p" "--polymerase" has-arg default "od=4,nd" ; prefer one mutation per fuzzer 
            comment "Which local mutation patterns to use? (pass help to list options)")
        (muxers "-m" "--muxers" has-arg default "swap=4,cat,min,max,avg" 
            comment "Which stream muxers to use? (pass help to list options)")
        (muxer-polymerase "-P" "--muxer-polymerase" has-arg default "once" 
            comment "Which muxer mutator patterns to use? (pass help to list options)")
        (seek "-S" "--seek" cook ,string->integer check ,(λ (x) (>= x 0))
            comment "Skip n outputs." default "0") ;; temporarily disabled due to a bug (likely fd leak)
        (multi-prob "-M" "--multi-prob" cook ,(λ (x) (string->number x F)) default "1/12" check ,valid-probability?
            comment "probability of using multiple data sources to make a single output (0 to always use one)")
        )))

(define command-line-rules
   (cl-rules advanced-cl-opts))

(define usage-text "Usage: radamsa [option] [sample-path] ...")

(define (print-usage-text advanced?)
   (print usage-text)
   (print-rules (if advanced? command-line-rules (cl-rules basic-cl-opts))))

;; () → string
(define (urandom-seed)
   (let ((fd (open-input-file "/dev/urandom"))) ;; False if not there
      (if fd
         (let ((data (interact fd 8)))
            (close-port fd)
            (if (vector? data)
               (list->string 
                  (render render (fold (λ (n d) (+ d (<< n 8))) 0 data) null))
               False))
         False)))

;; () → string (decimal number)
(define (time-seed)
   (list->string (render render (time-ms) null)))

;; get a seed if none was given explicitly (use /dev/urandom or current time in ms)
(define (add-seed opts)
   (put opts 'seed
      (or (urandom-seed) (time-seed))))

;; str → integer
(define (string->seed str)
   (if (string? str)
      (fold (λ (seed digit) (* (+ digit 1) (+ seed 1))) 1 (str-iter str))
      (error "bad seed: " str)))

;; str → str | (name . priority)
(define (try-split-priority str)
   (lets 
      ((parts (c/=/ str))
       (matcher (car parts)))
      (cond
         ((= (length parts) 1)
            ;; no priority given. default to 1 (lowest).
            (cons 1 matcher))
         ((= (length parts) 2)
            (let ((pri (string->integer (cadr parts))))
               (if (and pri (> pri 0))
                  (cons pri matcher)
                  str)))
         (else str))))

(define (split-priorities selection opts paths fail)
   (lets
      ((mods (keep (λ (x) (> (string-length x) 0)) (c/,/ selection))) ;; split
       (pris (map try-split-priority mods))
       (bads (keep string? pris)))
      (if (null? bads)
         pris
         (begin
            (for-each (λ (x) (show "Bad module selection: " x)) bads)
            (print "Modules selection is a comma-separated list of module names optionally followed by weights.")
            (print "For example -e foo,bar,baz=2 will use baz about as often as foo and bar combined.")
            (print "Use $ radamsa -L to list options")
            (fail False)))))

(define (iterate thing)
   (pair thing (iterate thing)))

;; iterate the same thing
(define (simple-node name value)
   (let ((node (cons name value)))
      (iterate node)))

;; iterate a fuzzer function that acts like a lfold over random states
;; rs name value → (name . ((name . (val rs)) (name . (var . rs')) ...))
(define (rs-fold-node name value)
   (cons name
      (λ (rs paths opts)
         (let loop ((rs rs))
            (pair (cons name (value rs))
               (loop (pop-rand rs)))))))

(define (byte-vector->list bvec)
   (let ((n (sizeb bvec)))
      (if (eq? n 0)
         null
         (let loop ((pos (- n 1)) (out null))
            (if (eq? pos 0)
               (cons (refb bvec pos) out)
               (lets ((posp _ (fx- pos 1)))
                  (loop posp (cons (refb bvec pos) out))))))))

;;; LL mutations -----------------------------------------------------------

(define-module lib-ll-mutations

   (export ll-mutations)

   ;; todo: add zero width joiners, different length leading blank padding bytes etc
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
                  (render render (list->string (list codepoint)) null)) ;; <- make UTF-8 repr
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

   (define (digit-val d)
      (cond
         ((lesser? d 48) F)
         ((lesser? 57 d) F)
         (else (- d 48))))

   ; → digit|F tail
   (define (get-num lst)
      (let loop ((lst lst) (n 0) (digits 0))
         (cond
            ((null? lst)
               (if (eq? digits 0)
                  (values F null)
                  (values n null)))
            ((digit-val (car lst)) =>
               (λ (d) (loop (cdr lst) (+ d (* n 10)) (+ digits 1))))
            ((eq? digits 0)
               (values F lst))
            (else
               (values n lst)))))

   ;; copy from pos up to end (not countin it)
   (define (copy-range pos end tail)
      (if (eq? pos end)
         tail
         (cons (car pos) (copy-range (cdr pos) end tail))))

   ;; fixme: simple placeholder
   (define (mutate-num rs num)
      (lets ((rs n (rand rs 16)))
         (cond
            ((eq? n 0)  (values rs (+ n 1)))
            ((eq? n 1)  (values rs (- n 1)))
            ((eq? n 2)  (values rs 0)) ;; todo, pack funny nums to a list and reduce opts
            ((eq? n 3)  (values rs 1))
            ((eq? n 4)  (values rs #xff))
            ((eq? n 5)  (values rs #x100))
            ((eq? n 6)  (values rs #xffff))
            ((eq? n 7)  (values rs #x10000))
            ((eq? n 8)  (values rs #x7fffffff))
            ((eq? n 9)  (values rs #x80000000))
            ((eq? n 10) (values rs #xffffffff)) ;; todo 64-bit also when using a list
            ((eq? n 11) (values rs #x100000000))
            ((eq? n 12)
               (lets ((rs m (rand rs (* n 2))))
                  (values rs (- n m))))
            (else
               (lets
                  ((rs n (rand-range rs 1 129))
                   (rs n (rand-log rs n)))
                  (values rs (+ num n)))))))

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
                         (new-lst (render render new tail)))
                        (values rs -1 (render render new tail)))
                     (values rs (- which 1)
                        (copy-range lst lstp tail))))
               (lets ((rs which tail (mutate-a-num rs (cdr lst) nfound)))
                  (values rs which (cons (car lst) tail)))))))

   ;; todo: pack stream fuzzers to the list itself
   (define (sed-num rs)
      (λ (ll)
         (lets
            ((lst (vec->list (car ll)))
             (rs n lst (mutate-a-num rs lst 0)))
            (cons (list->byte-vector lst) (cdr ll)))))

   (define (sed-star rs) ;; test fuzzer, replaces a character with *
      (λ (ll)
         (lets ((rs p (rand rs (sizeb (car ll)))))
            (cons (edit-byte-vector (car ll) p (λ (old tl) (cons 42 tl))) (cdr ll)))))

   (define (sed-drop rs) ;; drop byte
      (λ (ll)
         (lets ((rs p (rand rs (sizeb (car ll)))))
            (cons (edit-byte-vector (car ll) p (λ (old tl) tl)) (cdr ll)))))

   (define (sed-inc rs) ;; increment a byte mod 256
      (λ (ll)
         (lets ((rs p (rand rs (sizeb (car ll)))))
            (cons (edit-byte-vector (car ll) p (λ (old tl) (cons (band 255 (+ old 1)) tl))) (cdr ll)))))

   (define (sed-flip rs) ;; flip a bit
      (λ (ll)
         (lets 
            ((rs p (rand rs (sizeb (car ll))))
             (rs b (rand rs 8)))
            (cons (edit-byte-vector (car ll) p (λ (old tl) (cons (bxor (<< 1 b) old) tl))) (cdr ll)))))

	(define (pump val n lst)
		(if (eq? n 0) 
			lst
			(pump val (- n 1) (cons val lst))))

	(define (sed-byte-repeat rs) ;; repeat a single byte
      (λ (ll)
         (lets 
            ((rs p (rand rs (sizeb (car ll))))
				 (rs bits (rand rs 21))         ;; 0-20, avg 10
				 (rs bits (rand rs (+ bits 1))) ;; 0-20, avg 5
				 (rs n (rand rs (<< 1 bits))))   ;; 0-1MB, avg 16 bytes
            (cons 
					(edit-byte-vector (car ll) p 
						(λ (old tl) 
							(pump old n (cons old tl))))
					(cdr ll)))))

   ;;; sed-tree-* utils

   (define usual-delims ; <- could be settable from command line
      (list->ff
         '((40 . 41)    ; ()
           (91 . 93)    ; []
           (60 . 62)    ; <>
           (123 . 125)  ; {}
           (34 . 34)    ; ""
           (39 . 39)))) ; ''
        
   ;; → lst False = ran out of data trying to parse up to close, but lst is the same with partial parsing
   ;; → lst tail-lst = did successfully parse up to close. ready node is lst, tail is following data
   (define (grow lst close rout)
      (if (null? lst)
         (values (reverse rout) False) ;; out of data, didn't find close. return partial parse.
         (lets ((hd lst lst))
            (cond
               ((eq? hd close)
                  ;; match complete, return with rest of list
                  (values (reverse (cons close rout)) lst))
               ((get usual-delims hd F) =>
                  (λ (next-close)
                     (lets ((this lst (grow lst next-close null)))
                        (if lst ;; we didn't run out of data and this is a single tree node
                           (grow lst close (cons (cons hd this) rout))
                           ;; we ran out of data. this is a list of partial parses (having the data of 
                           ;; lst after hd in some form) which we want to preserve as tail
                           (values (append (reverse rout) (cons hd this)) False)))))
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
            (values rs False)
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
               (let ((maybe-op (get opff hd F)))
                  (if maybe-op
                     (cons (maybe-op (car lst))
                        (edit-sublists (cdr lst) opff))
                     (cons (edit-sublists (car lst) opff)
                        (edit-sublists (cdr lst) opff))))
               (cons (car lst)
                  (edit-sublists (cdr lst) opff))))
         lst))

   (define (partial-parse lst)
      (let loop ((lst lst) (rout null))
         (if (null? lst)
            (reverse rout)
            (lets ((closep (get usual-delims (car lst) F)))
               (if closep
                  (lets 
                     ((hd (car lst))
                      (this lst (grow (cdr lst) closep null)))
                     (if lst
                        (loop lst (cons (cons hd this) rout))
                        (append (reverse rout) (cons hd this))))
                  (loop (cdr lst) (cons (car lst) rout)))))))

   (define (flatten node tl)
      (cond
         ((null? node) tl)
         ((pair? node) (flatten (car node) (flatten (cdr node) tl)))
         (else (cons node tl))))

   (define (sed-tree-op op)
      (λ (rs)
         (λ (ll)
            (lets
               ((lst (partial-parse (byte-vector->list (car ll))))
                ;(_ (if (not (equal? (byte-vector->list (car ll)) (flatten lst null))) (error "partial parse bug: " (list (car ll) 'parses 'to lst))))
                (rs sub (pick-sublist rs lst)) ;; choose partially parsed node to mutate ;; fixme: not checked for F
                (lst (edit-sublist lst sub op)))
               (cons (list->byte-vector (flatten lst null)) (cdr ll)))))) ;; fixme; check for overgrown leaves (or crash with F on output)

   ;; todo: make these 3 with (sublists → mapping) → (rs → (ll → ...))

   (define (sed-tswap rs)
      (λ (ll)
         (call/cc 
            (λ (ret)
               (lets
                  ((skip (λ () (ret ll)))
                   (lst (partial-parse (byte-vector->list (car ll)))) ;; (byte|node ...)
                   (subs (sublists lst)) ;; find all nodes recursively
                   (subs (if (length< subs 3) (skip) subs))
                   (rs toswap (random-subset rs subs)) ;; choose which to swap <- make a special case for swapping 2?
                   (toswap (cons (car subs) toswap))
                   (rs arr (random-permutation rs subs)) ;; permute targets
                   (mapping (fold (λ (map pair) (put map (car pair) (λ (x) (cdr pair)))) F (zip cons toswap arr))) ;; aim
                   (lst (edit-sublists lst mapping))) ;; fire
                  (cons (list->byte-vector (flatten lst null)) (cdr ll)))))))

   (define (sed-tswap-two rs)
      (λ (ll)
         (call/cc
            (λ (ret)
               (lets 
                  ((skip (λ () (ret ll)))
                   (lst (partial-parse (byte-vector->list (car ll)))) ;; (byte|node ...)
                   (subs (sublists lst))
                   (subs (if (length< subs 3) (skip) subs))
                   (rs toswap (reservoir-sample rs subs 2))
                   (a (car toswap)) 
                   (b (cadr toswap))
                   ;(_ (show " swapping " (list a 'and b)))
                   (mapping (list->ff (list (cons a (λ (x) b)) (cons b (λ (x) a)))))
                   (lst (edit-sublists lst mapping)))
                  (cons (list->byte-vector (flatten lst null)) (cdr ll)))))))

   ;; tree stutter buraum

   (define (repeat-path parent child n)
      (if (< n 2)
         parent ; <- causes at least 1 extra path to be made, saves one useless replace cycle
         (edit-sublist parent child
            (λ (here) (cons (repeat-path parent child (- n 1)) (cdr here))))))
 
   (define (choose-child rs node)
      (let ((subs (sublists node)))
         (if (null? subs)
            (values rs False)
            (rand-elem rs subs))))

   (define (choose-stutr-nodes rs subs abort)
      (if (null? subs) 
         (abort) ;; no child nodes about
         (lets ((rs childp (choose-child rs (car subs))))
            (if childp
               (values rs (car subs) childp)
               (choose-stutr-nodes rs (cdr subs) abort)))))

   (define (sed-tstut rs)
      (λ (ll)
         (call/cc
            (λ (ret)
               (lets 
                  ((skip (λ () (ret ll)))
                   (lst (partial-parse (byte-vector->list (car ll)))) ;; (byte|node ...)
                   (subs (sublists lst))
                   (rs subs (random-permutation rs subs))
                   (rs parent child (choose-stutr-nodes rs subs skip))
                   (rs n-reps (rand-log rs 10))
                   (lst 
                     (edit-sublist lst child 
                        (λ (node) (cons (repeat-path parent child n-reps) (cdr node))))))
                  (flush-bvecs (flatten lst null) (cdr ll)))))))

   (define (print-rewrite a b)
      (lets
         ((a (flatten a null))
          (b (flatten b null))
          (sa (list->string a))
          (sb (list->string b)))
         (print-to "REWRITE FROM --------------------------------------------------------------------------" stderr)
         (print-to (or sa (list 'bad 'data a)) stderr)
         (print "")
         (print-to "REWRITE TO   --------------------------------------------------------------------------" stderr)
         (print-to (or sb (list 'bad 'data b)) stderr)
         (print "")
         (print-to "---------------------------------------------------------------------------------------" stderr)))

   (define (sed-tswap-one rs)
      (λ (ll)
         (call/cc
            (λ (ret)
               (lets 
                  ((skip (λ () (ret ll)))
                   (lst (partial-parse (byte-vector->list (car ll)))) ;; (byte|node ...)
                   (subs (sublists lst))
                   (subs (if (length< subs 3) (skip) subs))
                   (rs toswap (reservoir-sample rs subs 2))
                   (a (car toswap)) 
                   (b (cadr toswap))
                   ;(_ (print-rewrite a b))
                   ;(_ (show " swapping " (list a 'and b)))
                   (mapping (list->ff (list (cons a (λ (x) b)) (cons b (λ (x) a)))))
                   (lst (edit-sublist lst a (λ (node) (cons b (cdr node))))))
                  (cons (list->byte-vector (flatten lst null)) (cdr ll)))))))

   ;; duplicate a single branch
   (define sed-tdup (sed-tree-op (λ (node) (cons (car node) node))))

   ;; duplicate a branch and whatever comes before the next one (if any)
   (define sed-tdup-run 
      (sed-tree-op 
         (λ (node) 
            (cons (car node)
               (let loop ((lst (cdr node)))
                  (if (or (null? lst) (pair? (car lst)))
                     node
                     (cons (car lst) (loop (cdr lst)))))))))

   ;; duplicate a branch and whatever comes before the next one (if any)
   ;; → swapped lst' (returning the original one if out of data)
   (define (swap-next lst node)
      (if (null? lst) 
         (values node lst)
         (lets ((hd lst lst))
            (if (pair? hd) ;; drop here
               (values hd (cons node lst))
               (lets ((node lst (swap-next lst node)))
                  (values node (cons hd lst)))))))

   ;; swap a node with the next one at current level (if any)
   (define sed-tswap-level
      (sed-tree-op 
         (λ (node) 
            (lets ((swapd lst (swap-next (cdr node) (car node))))
               (cons swapd lst)))))

   ;; delete the node from a tree
   (define sed-tdel (sed-tree-op (λ (node) (cdr node))))


   ;;; sed-surf utils

   (define (match-prefix lst p)
      (cond
         ((null? p) True)
         ((null? lst) False)
         ((eq? (car lst) (car p))
            (match-prefix (cdr lst) (cdr p)))
         (else False)))

   (define (occurrences lst pat)
      (let loop ((lst lst) (found null) (pos 0))
         (cond
            ((null? lst) found)
            ((match-prefix lst pat)
               (loop (cdr lst) (cons pos found) (+ pos 1)))
            (else
               (loop (cdr lst) found (+ pos 1))))))

   (define (add-suffix ff ls)
      (if (null? ls)
         ff
         (lets ((x ls ls))
            (put ff x
               (cons ls (get ff x null))))))

   (define (note-substring occurrences rbytes others)
      (let ((score (* (- occurrences 1) (* (- (length rbytes) 1) 2))))
         (if (< score 1)
            others
            (cons (cons score rbytes) others))))

   ;; → ((score reverse-bytes ..) ...)
   (define (radix-walk lsts path n found)
      (cond
         ((null? lsts) found)
         ((null? (cdr lsts)) found)
         ((eq? n 0) found)
         (else
            (ff-fold
               (λ (found char tails)
                  (radix-walk tails (cons char path) (- n 1)
                     (note-substring (length tails) (cons char path) found)))
               found
               (fold add-suffix False lsts)))))

   ;; list of list suffixes
   (define (suffixen lst)
      (if (null? lst)
         null
         (cons lst (suffixen (cdr lst)))))

   (define (car> a b) (> (car a) (car b)))

   ;; ((i . x) (j . z) ...) n → z
   (define (priority-pick l n)
      (let ((m (caar l)))
         (if (<= n m)
            (cdar l)
            (priority-pick (cdr l) (- n m)))))

   ;; todo: make surf max jump size a command line flag
   (define (frequent-substring rs lst)
      (lets
         ((rs max-depth (rand-range rs 4 20)) ; <- 32 gives a max 100ms search
          (subs (timed "radix-walk" (radix-walk (suffixen lst) null max-depth null)))
          (subs (timed "sorting" (sort car> subs)))
          (rs n (rand rs (fold + 0 (map car subs)))))
         (if (null? subs) ;; <- blank → null
            (values rs null)
            (values rs (reverse (priority-pick subs n))))))

   ;; poss quaranteed to have >= 2 elems
   (define (choose-jump rs poss len)
      (lets
         ((rs poss (random-permutation rs poss))
          (from (car poss))
          (to (cadr poss)))
         (if (= (+ from len) (- to 1)) ;; wouldn't change the data
            (values to from)
            (values from to))))

   (define (sed-surf rs) ;; jump between two share suffixes in the block
      (λ (ll)
         (lets
            ((lst (byte-vector->list (car ll)))
             (rs root (frequent-substring rs lst))
             (poss (occurrences lst root)))
            ;; a jump list of one element is constructed for blocks with just one byte
            ;(show "SURF: jumping " (list 'from from 'to to 'using root))
            (if (> (length poss) 1)
               (lets 
                  ((from to (choose-jump rs poss (length root)))
                   (block (car ll))
                   (get (λ (p) (refb block p)))
                   (pre (list->byte-vector (map get (iota 0 1 from))))
                   (post (list->byte-vector (map get (iota to 1 (sizeb block))))))
                  (ilist pre post (cdr ll)))
               ll))))

   (define (sed-stut rs) ;; repeat a section
      (λ (ll)
         (lets ((n (sizeb (car ll))))
            (if (< n 4)
               ll
               (lets
                  ((rs start (rand-range rs 0 (- n 2)))
                   (rs end (rand-range rs (+ start 1) n))
                   (pre (map (λ (p) (refb (car ll) p)) (iota 0 1 start)))
                   (post (map (λ (p) (refb (car ll) p)) (iota end 1 (sizeb (car ll)))))
                   (stut (list->byte-vector (map (λ (p) (refb (car ll) p)) (iota start 1 end))))
                   (rs n (rand-log rs 10)) ; max 2^10 = 1024 stuts
                   (stuts
                     (fold
                        (λ (tl n) (cons stut tl))
                        (if (null? post)
                           (cdr ll)
                           (cons (list->byte-vector post) (cdr ll)))
                        (iota 0 1 n))))
                  (if (null? pre)
                     stuts
                     (cons (list->byte-vector pre) stuts)))))))

   (define (sed-dec rs) ;; decrement a byte mod 256
      (λ (ll)
         (lets ((rs p (rand rs (sizeb (car ll)))))
            (cons 
               (edit-byte-vector (car ll) p 
                  (λ (old tl) (if (eq? old 0) 255 (cons (- old 1) tl))))
               (cdr ll)))))

   (define (sed-rnd rs) ;; swap random byte
      (λ (ll)
         (lets 
            ((rs p (rand rs (sizeb (car ll))))
             (rs new (rand rs 256)))
            (cons 
               (edit-byte-vector (car ll) p (λ (old tl) (cons new tl)))
               (cdr ll)))))
   
   (define (sed-ins rs) ;; insert random byte
      (λ (ll)
         (lets 
            ((rs p (rand rs (sizeb (car ll))))
             (rs new (rand rs 256)))
            (cons 
               (edit-byte-vector (car ll) p (λ (old tl) (ilist new old tl)))
               (cdr ll)))))

   (define (sed-dup rs) ;; duplicate a byte
      (λ (ll)
         (lets ((rs p (rand rs (sizeb (car ll)))))
            (cons 
               (edit-byte-vector (car ll) p 
                  (λ (old tl) (ilist old old tl)))
               (cdr ll)))))

   (define (sed-perm rs) ;; permute a few bytes
      (λ (ll)
         (lets ((rs p (rand rs (sizeb (car ll)))))
            (cons 
               (edit-byte-vector (car ll) p 
                  (λ (old tl) 
                     (lets 
                        ((lst (cons old tl))
                         (rs n (rand-range rs 2 20))
                         (head (take lst n))
                         (tail (drop lst n))
                         (rs head (random-permutation rs head)))
                        (append head tail))))
               (cdr ll)))))

   ;; todo: stub, parse the (or next first valid, if any) code point and edit separately later
   (define (sed-utf8-edit rs)
      (λ (ll)
         (lets ((rs p (rand rs (sizeb (car ll)))))
            (cons 
               (edit-byte-vector (car ll) p 
                  (λ (old tl) 
                     ;; assuming we hit a 6-bit ascii char, make it unnecessarily wide
                     (ilist #b11000000 (bor old #b10000000) tl)))
               (cdr ll)))))

   ;; insert UTF-8 that might be mishandled
   (define (sed-utf8-insert rs)
      (λ (ll)
         (lets
            ((rs p (rand rs (sizeb (car ll))))
             (rs bytes (rand-elem rs funny-unicode)))
            (cons
               (edit-byte-vector (car ll) p (λ (old tl) (append bytes (cons old tl))))
               (cdr ll)))))

   ;; bvec → ((... 10) .. (... [10])), cut after newlines
   (define (lines bvec)
      (let loop ((lst (byte-vector->list bvec)) (buff null) (out null))
         (if (null? lst) 
            (if (null? buff)
               (reverse out)
               (reverse (cons (reverse buff) out)))
            (lets ((hd lst lst))
               (if (eq? hd 10) ;; newline
                  (loop lst null (cons (reverse (cons 10 buff)) out))
                  (loop lst (cons hd buff) out))))))

   ;; delete a random line 
   (define (sed-ldel rs)
      (λ (ll)
         (lets 
            ((ls (lines (car ll)))
             (len (length ls)))
            (if (> len 2) ;; we want at least [possibly-partial a b possibly-partial]
               (lets ((rs a (rand-range rs 1 (- len 1)))) ;; one of the middle ones 
                  (cons (list->byte-vector (foldr append null (ldel ls a))) (cdr ll)))
               ll)))) ;; do nothing if we don't get full lines in this block

   (define (sed-ldup rs)
      (λ (ll)
         (lets 
            ((ls (lines (car ll)))
             (len (length ls)))
            (if (> len 2) ;; we want at least [possibly-partial a b possibly-partial]
               (lets ((rs a (rand-range rs 1 (- len 1)))) ;; one of the middle ones 
                  (cons (list->byte-vector (foldr append null (ledn ls a (λ (x) (cons (car x) x))))) (cdr ll)))
               ll)))) ;; do nothing if we don't get full lines in this block

   ;; instert a random line to a random position
   (define (sed-lins rs)
      (λ (ll)
         (lets 
            ((ls (lines (car ll)))
             (len (length ls)))
            (if (> len 2) ;; we want at least [possibly-partial a b possibly-partial]
               (lets 
                  ((rs from (rand-range rs 1 (- len 1)))
                   (rs to (rand-range rs 1 (- len 1)))) ;; fixme: growth -> may become too large
                  (cons (list->byte-vector (foldr append null (lins ls to (lref ls from)))) (cdr ll)))
               ll))))

   ;; swap two consecutive lines
   (define (sed-lswapc rs)
      (λ (ll)
         (lets 
            ((ls (lines (car ll)))
             (len (length ls)))
            (if (> len 3) ;; we want at least [possibly-partial a b possibly-partial]
               (lets 
                  ((rs pos (rand-range rs 1 (- len 2))))
                  (cons 
                     (list->byte-vector 
                        (foldr append null 
                           (ledn ls pos (λ (x) (ilist (cadr x) (car x) (cddr x))))))
                     (cdr ll)))
               ll))))
   ;; 

   ; (name . starter), starter : rs paths opts → ((info . func) ...)
   (define ll-mutations   
      (list
         ;; for testing
         (rs-fold-node "test" sed-star) ;; <- adds a * character somewhere. for testing distrib and ll polymerase.

         ;; byte level mutations
         (rs-fold-node "bei" sed-inc)   ;; byte edit / increment
         (rs-fold-node "bed" sed-dec)   ;; byte edit / decrement
         (rs-fold-node "ber" sed-rnd)   ;; byte edit / randomize
         (rs-fold-node "bef" sed-flip)  ;; byte edit / flip bit 
         (rs-fold-node "bc"  sed-drop)  ;; byte cut
         (rs-fold-node "bd"  sed-dup)   ;; byte dup
         (rs-fold-node "bp"  sed-perm)  ;; byte perm
         (rs-fold-node "bi"  sed-ins)         ;; byte insert
         (rs-fold-node "br"  sed-byte-repeat) ;; byte repeat

			;; byte sequence level mutations
         (rs-fold-node "sr" sed-stut)   ;; sequence repeat
         (rs-fold-node "sj" sed-surf)   ;; sequence jump (surf with suffix)

         ;; text / line-oriented 
         (rs-fold-node "ldel" sed-ldel) ;; swap a node with another one at the same level (make arbitrary swap later)
         (rs-fold-node "ldup" sed-ldup) ;; clone a line
         (rs-fold-node "lins" sed-lins) ;; insert a random line
         (rs-fold-node "lswapc" sed-lswapc) ;; swap two consecutive lines

         ;; text / partial parse tree 
         (rs-fold-node "td1" sed-tdup)        ;; tree duplicate 
         (rs-fold-node "td2" sed-tdup-run)    ;; tree duplicate
         (rs-fold-node "tc" sed-tdel)         ;; tree cut
         (rs-fold-node "tsl" sed-tswap-level) ;; tree swap (within level)
         (rs-fold-node "ts"  sed-tswap)       ;; tree swap
         (rs-fold-node "tsp" sed-tswap-two)   ;; tree swap (2 pairwise)
         (rs-fold-node "tsn" sed-tswap-one)   ;; tree swap (1 node)
         (rs-fold-node "trs"  sed-tstut)      ;; tree repeat (stutter)

         ;; text / UTF-8
         (rs-fold-node "ui" sed-utf8-insert) ;; UTF-8 insert
         (rs-fold-node "ue" sed-utf8-edit)   ;; UTF-8 edit

         ;; text / numbers
         (rs-fold-node "num" sed-num)

         (cons "crash" 'mr-crasher)))
)

;;;
;;; LL mutation patterns
;;;

(define-module lib-ll-polymerase
   (export ll-polymerase)

   ;; todo: other local mutation patterns missing

   ;; make at least one mutation, with decreasing prob, if ll is nonempty
   ;; call (cont rs tail) after mutation
   (define (once-dec rs mutafn ll cont)
      (lets 
         ((hd ll (uncons ll F))
          (rs ip (rand rs 32))) ;; initial inverse mutation probability
         (if hd
            (let loop ((rs rs) (ll ll) (last hd) (ip ip))
               (lets ((rs n (rand rs ip)))
                  (if (eq? n 0)
                     (cont rs (mutafn (cons last ll)))
                     (lets ((next ll (uncons ll F)))
                        (if next
                           (cons last 
                              (loop rs ll next (+ ip 1)))
                           (cont rs (mutafn (list last))))))))
            ll)))

   (define (k2 a b) b)

   ;; once decreasing
   (define (od rs)
      (λ (mutagen ll) 
         (once-dec rs mutagen ll k2)))

   ;; n mutations, decreasing decreasing probability
   (define (nd rs)
      (λ (mutagen ll)
         (let loop ((rs rs) (ll ll) (ip 1))
            (once-dec rs mutagen ll
               (λ (rs ll)
                  (lets ((rs n (rand rs ip)))
                     ;(show " - n mutate: " (list n '/ ip))
                     (cond
                        ;((eq? n 10) ll) ;; max mutation limit, enable if turns out to be necessar. (maybe in stutr-style stuff)
                        ((eq? n 0) (loop rs ll (+ ip 1))) ;; keep mutatin
                        (else ll)))))))) ;; done mutatin

   (define ll-polymerase 
      (list
         (rs-fold-node "od" od)
         (rs-fold-node "nd" nd))))




;;;
;;; LL generators
;;;

(define-module lib-ll-generators

   (export 
      ll-generators
      stream-sample
      stream-blocks)

   (define max-block-size (* 2 avg-block-size))

   ;; bvec|F bvec → bvec
   (define (merge head tail)
      (if head
         (list->vector (vec-foldr cons (vec-foldr cons null tail) head))
         tail))

   (define (rand-block-size rs)
      (lets ((rs n (rand rs max-block-size)))
         (values rs (max n 8))))

   ;; todo: might make sense to grow the initial block size limits as 2^n up to final limit
   ;; stream data from port as desired sized blocks (which are deterministic unlike the sizes we happen to read) and close it at eof
   (define (stream-blocks rs port)
      (lets ((rs first (rand-block-size rs)))
         (let loop ((rs rs) (last F) (wanted first)) ;; 0 = block ready (if any)
            (let ((block (interact port wanted)))
               (cond
                  ((eof? block) ;; end of stream
                     (if (not (eq? port stdin)) (close-port port))
                     (if last (list last) null))
                  ((not block) ;; read error
                     ;; later carp via whatever logging system is used to stderr
                     (if (not (eq? port stdin)) (close-port port))
                     (if last (list last) null))
                  ((eq? (sizeb block) (+ wanted 1))
                     ;; a block of required (deterministic) size is ready
                     (lets
                        ((block (merge last block))
                         (rs next (rand-block-size rs)))
                        (pair block (loop rs F next))))
                  (else
                     (loop rs (merge last block) 
                        (- wanted (sizeb block)))))))))

   (define (open-sample-port path)
      (cond
         ((equal? path "-") stdin)
         (else (open-input-file path))))

   ;; todo: make the first block a special case? <- or grow the block limit
   (define (stream-sample rs path)
      (let ((port (open-sample-port path)))
         (if port
            (stream-blocks rs port) ; <- use also for stdin streaming
            (begin
               (mail stderr (foldr renderer '(10) (list "Cannot read '" path "' - acting as if it were blank.")))
               null))))

   ;; v all these go to an initial fold over the samples and always make the stdin stream node 

   (define (sgen-sample rs paths opts)
      (lets ((rs order (random-permutation rs paths)))
         ;; permute once per round
         (let loop ((rs rs) (order order))
            (cond
               ((null? order)
                  (sgen-sample rs paths F)) ;; repermute 
               ((string? (car order)) ;; a path
                  (pair
                     (cons
                        (list 'sample (car order)) ;; save sample name to metadata
                        (λ () (stream-sample rs (car order))))
                     (loop (pop-rand rs) (cdr order))))
               (else ;; head is at least partially preread data (meaning stdin atm)
                  (pair (car order)
                     (loop rs (cdr order))))))))

   (define (repeat thing n)
      (let loop ((n n) (out null))
         (if (eq? n 0)
            out
            (loop (- n 1) (cons thing out)))))

   (define (repeat-blocks pre n tail)
      (if (= n 0)
         (list tail)
         (pair pre (repeat-blocks pre (- n 1) tail))))

   (define (rand-length rs)
      (lets
         ((rs nbits (rand rs 18))
          (rs n (rand rs (<< 1 (max nbits 6)))))
         (values rs n)))

   (define (sgen-flat rs paths opts)
      (let loop ((rs rs))
         (lets
            ((rs base (rand rs 256))
             (rs len (rand-length rs))
             (blocks (div len avg-block-size)))
            (pair
               (cons 
                  (list 'flat base len)
                  (repeat-blocks
                     (if (> blocks 0)
                        (list->byte-vector (repeat base avg-block-size))
                        'nothing)
                     blocks
                     (list->byte-vector (repeat base (- len (* blocks avg-block-size))))))
               (loop rs)))))

   (define (sgen-rrep rs paths opts)
      (let loop ((rs rs))
         (lets
            ((rs nalpha (rand rs 1024)) ;; alphabet size
             (rs letters (random-numbers rs 256 nalpha)) ;; alphabet (may have repetition)
             (alpha (list->vector letters)) ;; index 0-(nalpha-1)
             (rs len (rand-log rs 10)) ;; block size to repeat
             (rs poss (random-numbers rs nalpha len))
             (bytes (map (λ (p) (vec-ref alpha p)) poss))
             (block (list->byte-vector bytes))
             (rs n (rand-log rs 6))) ;; repeat block how many times
            (pair
               (cons 'rrep (map (λ (p) block) (iota 0 1 n)))
               (loop rs)))))

   (define (random-bytes rs n out)
      (cond
         ((eq? n 0)
            (values rs out))
         ((eq? n 1) ;; get 8 bits
            (lets ((rs b (rand rs 256)))
               (random-bytes rs 0 (cons b out))))
         (else ;; get and split 16  bits
            (lets ((digit rs (uncons rs F)))
               (random-bytes rs (- n 2)
                  (ilist (band digit 255) (>> digit 8) out))))))

   (define (sgen-random rs paths opts)
      (let loop ((rs rs))
         (lets 
            ((rs len (rand rs 4096))
             (rs bytes (random-bytes rs len null)))
            (pair
               (cons 'random (list (list->byte-vector bytes))) ;; <- could also stream this
               (loop rs)))))

   (define ll-generators
      (list
         (cons "sample" sgen-sample)   ;; stream data from samples
         (cons "flat" sgen-flat)      ;; repeat a single value
         (cons "random" sgen-random)  ;; genrate random data (radamsa -n 100000000000000 -f flip -g random --single | pv | dieharder -g 200 -a)
         (cons "rrep" sgen-rrep))))





;;;
;;; LL muxers
;;;

(define-module lib-ll-muxers

   (export ll-muxers)

   (define (bigger-first a b)
      (lets ((sa (sizeb a)) (sb (sizeb b)))
         (if (< sa sb)
            (values b sb a sa)
            (values a sa b sb))))

   (define (zip-blocks op all bll)
      (cond
         ((null? all) (values all bll))
         ((null? bll) (values all bll))
         ((pair? all)
            (if (pair? bll)
               (lets 
                  ((a as b bs (bigger-first (car all) (car bll)))
                   (al (byte-vector->list a))
                   (bl (byte-vector->list b))
                   (merged (list->byte-vector (zip op al bl)))) ;; will cut tail of al
                  (values
                     (if (eq? al bl) ;; unless the tail is a lie
                        (cons merged (cdr all))
                        (ilist merged
                           (list->byte-vector (map (λ (p) (refb a p)) (iota bs 1 as)))
                           (cdr all)))
                     bll))
               (zip-blocks op all (bll))))
         (else 
            (zip-blocks op (all) bll))))

   (define ll-muxers ;; todo: many to be added in 0.2.1
      (list
         (cons "swap" ;; swap streams
            (λ (rs paths opts)
               (simple-node "swap"
                  (λ (a b) (values b a)))))
         (cons "cat" ;; catenate streams
            (λ (rs paths opts)
               (simple-node "cat"
                  (λ (a b) (values (lappend a b) null)))))
         (cons "min" ;; get minimum for a block
            (λ (rs paths opts)
               (simple-node "min"
                  (λ (a b) 
                     (zip-blocks min a b)))))
         (cons "max" ;; get minimum for a block
            (λ (rs paths opts)
               (simple-node "max"
                  (λ (a b) (zip-blocks max a b)))))
         (cons "avg"
            (λ (rs paths opts)
               (simple-node "avg"
                  (λ (a b) (zip-blocks (λ (a b) (>> (+ a b) 1)) a b))))))))

;;;
;;; LL muxer patterns
;;;

(define-module lib-ll-muxer-polymerase

   (export ll-muxer-polymerase)

   ;; mux-step - walk the two lazy streams (whose length we don't know) and apply muxer once 
   ;; at some position, and then pass control to cont. blocks of a are emitted as output and 
   ;; b is dropped, speeds being roughly equivalent.

   ; → (cont ll ll)
   (define (mux-step rs all bll muxer ip cont)
      (define (finish as bs)
         (cond
            ((null? as) (cont null bs))
            ((null? bs) (cont as null))
            (else 
               (lets ((as bs (muxer as bs))) 
                  (cont as bs)))))
      (lets 
         ((a as (uncons all F))
          (b bs (uncons bll F)))
         (cond
            ((not a) (finish all bll)) ;; no base stream to mux
            ((not b) (finish all bll)) ;; no data to mux against
            (else
               (let loop ((rs rs) (a a) (as as) (b b) (bs bs))
                  (lets ((rs n (rand rs ip)))
                     (if (eq? n 0)
                        (finish (cons a as) (cons b bs))
                        (lets ((rs n (rand rs 2)))
                           (if (eq? n 0)
                              (lets ((aa as (uncons as F)))
                                 (if aa
                                    (pair a (loop rs aa as b bs)) ;; more a follows, emit and continue
                                    (finish (list a) (cons b bs)))) ;; out of a
                              (lets ((bb bs (uncons bs F)))
                                 (if bb
                                    (loop rs a as bb bs) ;; more b follows, drop b an continue
                                    (loop rs a as b null)))))))))))) ;; out of b, keep last block

   (define (fast-forward ll)
      (cond
         ((pair? ll) (fast-forward (cdr ll)))
         ((null? ll) ll)
         (else (fast-forward (ll)))))

   (define (mux-once rs all bll muxer ip)
      (mux-step rs all bll muxer ip 
         (λ (a b) 
            (fast-forward b)   ;; read the b stream to be dropped in order to close the fd
            a)))

   (define ll-muxer-polymerase
      (list
         (rs-fold-node "once" 
            (λ (rs)
               (λ (muxer all bll)
                  (lets
                     ((rs bits (rand rs 8))
                      (rs ip (rand rs (<< 1 bits)))) ;; inverse probability
                     (mux-once rs all bll muxer ip))))))))


(import-old lib-ll-mutations        ll-mutations)                 ;; functions to make a mutation on a stream
(import-old lib-ll-polymerase       ll-polymerase)                ;; functions to apply mutations on a stream
(import-old lib-ll-generators       ll-generators stream-blocks)  ;; functions to make data streams
(import-old lib-ll-muxers           ll-muxers)                    ;; functions to merge two streams
(import-old lib-ll-muxer-polymerase ll-muxer-polymerase)          ;; functions to apply stream merges



;;; Master polymerase --------------------------------------------------------------

;; grab some streams from which to derive a case
(define (choose-streams rs gens multi-prob)
   (let loop ((rs rs) (gens gens) (taken null))
      (lets ((rs occ? (rand-occurs? rs multi-prob)))
         (if (or occ? (null? taken))
            (lets ((this gens (uncons gens F)))
               (loop rs gens (cons this taken)))
            (values rs gens taken)))))

(define (plain-sample? node) 
   (and (pair? (car node))
      (eq? (caar node) 'sample)))

;; merger many data streams to one using the fuzzers and muxers
(define (master-merge rs muxs fuzs lls)
   (lets ((rs n (rand rs 5)))
      (cond
         ((eq? n 0) ;; permute
            (lets ((rs lls (random-permutation rs lls)))
               (master-merge rs muxs fuzs lls)))
         ((eq? n 1) ;; add a stream fuzzer to first
            (lets ((fuzz fuzs (uncons fuzs F)))
               (master-merge rs muxs fuzs 
                  (cons (fuzz (car lls)) (cdr lls)))))
         ((eq? n 2) ;; merge two streams (if more than one)
            (if (not (null? (cdr lls)))
               (lets 
                  ((mux muxs (uncons muxs F))
                   (a (car lls)) (b (cadr lls)))
                  (master-merge rs muxs fuzs
                     (cons (mux a b) (cddr lls))))
               (master-merge rs muxs fuzs lls)))
         ((and (null? (cdr lls)) (not (plain-sample? (car lls)))) ;; else try exit
            (values rs muxs fuzs (car lls)))
         (else ;; else keep muxin
            (master-merge rs muxs fuzs lls)))))

;; pick some stream generators and merge them with random muxers and mutators
(define (master-polymerase rs gens muxs fuzs multi-prob)
   (lets
      ((rs gens tomux (choose-streams rs gens multi-prob)) ;; choose some streams to use
       ;(_ (print-to  (list "Stream count " (length tomux)) stderr)) ;; while testing distributions
       (rs muxs fuzs node (master-merge rs muxs fuzs tomux))) ;; combine them to one stream
      (pair node (master-polymerase rs gens muxs fuzs multi-prob))))

; node : rs paths opts → ((info . value) ...)

(define max-seed #x1000000000000)

(define (pop-at opts n) ; (... (pri . ((info . val) ...)) ...) n → matching (info . val) x (... (pri . ...) ...)
   (lets
      ((node (car opts))
       (pri ll node))
      (if (< n pri)
         (lets ((next ll (uncons ll F)))
            (values next (cons (cons pri ll) (cdr opts))))
         (lets ((grabbed opts (pop-at (cdr opts) (- n pri))))
            (values grabbed (cons node opts))))))

(define (mux rs opts lim)
   (lets 
      ((rs n (rand rs lim))
       (val opts (pop-at opts n)))
      (pair val (mux rs opts lim))))

;; mux streams probabilistically according to priorities
(define (make-muxer rs opts)
   (if (null? (cdr opts))
      (values rs (cdar opts)) ;; no need to mux
      (lets
         ((rs seed (rand rs max-seed))
          (opts (sort car> opts))
          (total (fold + 0 (map car opts))))
         (values rs (mux (seed->rands seed) opts total)))))

;; warn and exit via error
(define (no-such-module name mods)
   (show "I don't have anything matching " name)
   (error "The valid options are " (map car mods)))

;; start node streams, each with their own rs, and mux them according to priorities
; → rs' ((info . val) ...)
(define (start rs mods pris paths opts note)
   (let loop ((rs rs) (pris pris) (out null))
      (if (null? pris)
         (begin
            (note (list "started: " (sort car> (map (λ (x) (cons (car x) (caadr x))) out))))
            (make-muxer rs out))
         (lets
            ((rs seed (rand rs max-seed))
             (match (string->complete-match-regex (cdar pris)))
             (matched (keep (λ (node) (match (car node))) mods))
             (started (map (λ (node) ((cdr node) (seed->rands seed) paths opts)) matched))
             (started (map (λ (ll) (cons (caar pris) ll)) started))) ;; add the priority
            (note (list "Started " (map car matched) " for str '" (cdar pris) "' w/ pri " (caar pris)))
            (if (null? started)
               (no-such-module (cdar pris) mods)
               (loop rs (cdr pris) (append started out)))))))

;; merge muxer polymerase stream and muxer stream to a stream muxer stream
(define (muxer-stream pols muxs)
   (lets
      ((pol pols (uncons pols F))
       (mux muxs (uncons muxs F)))
      (pair
         (λ (aln bln)
            (lets
               ((a-info a-data aln)
                (b-info b-data bln))
               (cons
                  (list (car pol) (car mux) a-info b-info) ;; (pattern muxer str-a str-b)
                  (λ () ((cdr pol) (cdr mux) a-data b-data)))))
         (muxer-stream pols muxs))))

;; ditto, but for stream muxers which take just one stream as argument
(define (fuzzer-stream pols fuzs)
   (lets
      ((pol pols (uncons pols F))
       (fuz fuzs (uncons fuzs F)))
      (pair
         (λ (aln)
            (lets ((a-info a-data aln))
               (cons
                  (list (car pol) (car fuz) a-info) ;; (pattern muxer str-a)
                  (λ () ((cdr pol) (cdr fuz) a-data)))))
         (fuzzer-stream pols fuzs))))

(define (force-and-forget-n ll n)
   (if (> n 0)
      (lets ((this ll (uncons ll F)))
         (this)
         (force-and-forget-n ll (- n 1)))))
     
(define examples 
"Typical usage:
   Fuzz file.foo to fuzzed.foo
      $ cat file.foo | radamsa > fuzzed.foo

   Same using explicitly given path
      $ radamsa -o /tmp/fuzzed.foo file.foo

   Same making 10 files (%d is replaced with the number of the file)
      $ radamsa -n 10 -o /tmp/fuzzed-%n.foo file.foo

   Same but use multiple samples from foo-files/*.foo
      $ radamsa -n 10 -o /tmp/fuzzed-%n.foo foo-files/*.foo")

   (define (preread-stdin rs)
      (force (stream-blocks rs stdin)))

;; force up to n nodes of a lazy list
(define (force-n ll n)
   (cond
      ((= n 0) ll)
      ((null? ll) ll)
      ((pair? ll) (cons (car ll) (force-n (cdr ll) (- n 1))))
      (else (force-n (ll) n))))

;; (tag . block-stream)
(define (stdin-stream rs force?)
   (cons "standard-input" 
      (let ((ll (stream-blocks rs stdin)))
         (if force?
            (force ll)         ;; O(n) - preread if we need to make more than 1 output
            (force-n ll 2))))) ;; O(1) - lazy read and on-line fuzzing if we make just 1 output, preread a few KB

;; if "-" argument is given (at least once), add a byte stream node and remove all occurrences of "-"
;; would be a map, but there can be more than one "-"
(define (maybe-convert-stdin paths seed n)
   (let loop ((out null) (paths paths) (added? False))
      (cond
         ((null? paths) out)
         ((string-eq? "-" (car paths))
            (if added? ;; drop multiple ones
               (loop out (cdr paths) T)
               (loop 
                  (cons (stdin-stream (seed->rands seed) (> n 1)) out) ; <- maybe use sample-stream here
                  (cdr paths) T)))
         (else
            (loop (cons (car paths) out) (cdr paths) added?)))))

(define (print-stderr stuff)
   (mail stderr
      (foldr (λ (a b) (render render a b)) '(10) stuff)))

(define compile-time (time-ms))

(define (print-stderr/time stuff)
   (mail stderr
      (foldr 
         (λ (a b) (render render a b))
         '(10) 
         (ilist (- (time-ms) compile-time) ": " stuff))))
   
(define (peek-data thing)
   (cond
      ((string? thing) ;; sample path
         (let ((fd (open-input-file thing)))
            (if fd
               (let ((data (interact fd 1024))) ;; todo: fast forward some
                  (close-port fd)
                  (if data
                     (byte-vector->list data)
                     null))
               null)))
      ((pair? thing) ;; stdin which has some data already
         (lets ((tag ll thing))
            (cond
               ((pair? ll) (byte-vector->list (car ll)))
               ((null? ll) null)
               (else (error "unforced stdin node: " thing)))))
      (else
         (error "peek-data: what is " thing))))

;; just check common ASCII range, ignores UTF-8 (todo soon)
;; less options for ascii, so binary will usually be noted as such
(define (byte-textuality b)
   (if (lesser? b 127)
      (if (lesser? b 32)
         -1 ;; ignores newlines and tabs atm
         +1)
      -1))

(define (textuality lst)
   (let ((score (fold (λ (sum byte) (+ sum (byte-textuality byte))) 0 lst)))
      ;(print-stderr (list " - score " score))
      (cond
         ((> score 0) 1)
         ((null? lst) 0)
         (else       -1))))

;; peek at some samples and return a textuality score (n > 0 = textual)
(define (classify-content paths) 
   (lets
      ((scores (map (o textuality peek-data) (take paths 10)))
       (sum (fold + 0 scores))
       (verdict (if (> sum 0) 'text 'binary)))
      ;(print-stderr (list "The data is " verdict " based on scores " scores))
      verdict))

;; verbosity → (args → _), verbosity is number of -v:s given
(define (debug-printer verbosity)
   (if (< verbosity 3)
      (λ (x) x)
      (λ (x) (print-stderr (cons "debug: " x)))))

;; limiting number of samples if there are much more than we want to generate
(define (limit-samples rs paths n)
   (let ((n-samples (length paths)))
      (cond
         ((null? paths) ;; not applicable
            (values rs paths))
         ((eq? n 1)
            (lets ((rs n (rand rs n-samples)))
               (values rs (list (lref paths n)))))
         ((< (* n 10) n-samples) ;; n << n-samples
            (reservoir-sample rs paths n))
         (else
            (values rs paths)))))
 
(define (print-options)
   (print
"These are the options accepted by manual mutation selections. This is
mainly useful for testing and debugging. Normally you should just 
use the default automatic settings. You can see the default settings
with $ radamsa --big-help.")
   (show " - stream mutations: " (map car ll-mutations))
   (show " - mutation patterns: " (map car ll-polymerase))
   (show " - generators: " (map car ll-generators))
   (show " - muxers: " (map car ll-muxers))
   (show " - muxer patterns: " (map car ll-muxer-polymerase)))
   

(define (start-radamsa opts paths)
   ;(print-stderr/time '("arguments parsed"))
   (call/cc
      (λ (ret)
         (let ((fail (λ (why rval) (print why) (ret rval))))
            (cond
               ((get opts 'help F) (print-usage-text F) 0)
               ((get opts 'advanced F) (print-usage-text T) 0)
               ((get opts 'examples F) (print examples) 0)
               ((get opts 'version F)
                  (print version-str)
                  0)
               ((not (get opts 'seed F))
                  ;; make a time-based seed unless one is given explicitly
                  ;(print-stderr/time '("added seed, restart"))
                  (start-radamsa (add-seed opts) paths))
               ((getf opts 'list-fuzzers)
                  (print-options)
                  0)
               (else
                  (lets
                     ((seed (string->seed (get opts 'seed F)))
                      (rs (seed->rands seed))
                      (note (debug-printer (get opts 'verbose 0)))
                      (paths ;; add stdin as a sample if no files given and no blank requested
                        (if (and (null? paths) (m/sample/ (get opts 'generators "")))
                           '("-") paths))
                      (n (get opts 'count F))
                      ;(_ (print-stderr/time '("limiting samples")))
                      ;(rs paths (limit-samples rs paths n)) 
                      ;(_ (print-stderr/time '("limited, converting stdin")))
                      (paths   (maybe-convert-stdin paths seed n))
                      (type    (or (get opts 'type F) (classify-content paths)))
                      (_ (note (list "data classification is " type)))
                      ;(_       (print-stderr (list "Data type is " type)))
                      (fuzzer-selection (if (eq? type 'binary) 'binary-fuzzers 'text-fuzzers))
                      ;(_ (print-stderr/time '("starting generators")))
                      (f-pris  (split-priorities (get opts fuzzer-selection F) opts paths fail))
                      (g-pris  (split-priorities (get opts 'generators F) opts paths fail))
                      (p-pris  (split-priorities (get opts 'polymerase F) opts paths fail))
                      (m-pris  (split-priorities (get opts 'muxers F) opts paths fail))
                      (mp-pris (split-priorities (get opts 'muxer-polymerase F) opts paths fail))
                      ;; todo: map sample source generator to samples
                      ;(_ (print-stderr/time '("generators running, checking for stdin")))
                      ;(_ (print-stderr/time '("generator startup")))
                      (rs gens (start rs ll-generators g-pris paths opts note))
                      ;(_ (print-stderr/time '("starting others")))
                      (rs fuzs (start rs ll-mutations f-pris paths opts note))
                      (rs pols (start rs ll-polymerase p-pris paths opts note))
                      (fuzs (fuzzer-stream pols fuzs)) ;; fuse available polymerase with stream fuzzers
                      (rs muxs (start rs ll-muxers m-pris paths opts note))
                      (rs mpol (start rs ll-muxer-polymerase mp-pris paths opts note))
                      (muxs (muxer-stream mpol muxs)) ;; fuze available muxer polymerase with muxers
                      ;(_ (print-stderr/time '("making master")))
                      (have-stdin? (mem equal? paths "-"))
                      (multi-prob 
                        (if have-stdin? 
                           0 ;; don't use multiple streams when working on-line on stdin
                           (get opts 'multi-prob F)))
                      (_ (note (list "multi-prob is " multi-prob)))
                      (master master-polymerase)
                      (lls (master rs gens muxs fuzs multi-prob))
                      (record-meta 
                        (let ((v (get opts 'verbose 0)))
                           (cond
                              ((eq? v 0) record-meta/not)
                              ((eq? v 1) record-meta/stderr)
                              (else      record-meta/verbose-stderr))))
                      (thunks ;; (thunk ...), calling causes io 
                        (lzip (λ (a b) (λ () (wait 100) (a b record-meta))) ;; <- wait in order to allow some time to write
                           (get opts 'output F)
                           lls))
                      (thunks (drop thunks (get opts 'seek 0))) ;; fast forward if --seek
                      )
                     ;(print-stderr/time '("generating data"))
                     (force-and-forget-n thunks n)
                     ;(print-stderr/time '("done"))
                     0)))))))

   
   
(define (toy-meta)
   (bind (wait-mail)
      (λ (from msg)
         (mail from 'anon)
         (toy-meta))))

(define (bummer)
   (lets
      ((s "        ") (a " *    *    *")
       (n (list->string (list 10)))
       (b "   *  *  *") (c "* * BOOOM *  *"))
      (mail stderr (list 27 35 55 27 91 53 109))
      (print-to 
         (foldr string-append "" 
            (list n n s a n s b n s c n s b n s a n))
         stderr)
      (mail stderr (list 27 91 48 109 10))))

(define (crash-msg) ; $ figlet -f isometric1.flf crash
   (mail stderr (string->list "
      ___           ___           ___           ___           ___     
     /\\  \\         /\\  \\         /\\  \\         /\\  \\         /\\__\\    
    /::\\  \\       /::\\  \\       /::\\  \\       /::\\  \\       /:/  /    
   /:/\\:\\  \\     /:/\\:\\  \\     /:/\\:\\  \\     /:/\\ \\  \\     /:/__/     
  /:/  \\:\\  \\   /::\\~\\:\\  \\   /::\\~\\:\\  \\   _\\:\\~\\ \\  \\   /::\\  \\ ___ 
 /:/__/ \\:\\__\\ /:/\\:\\ \\:\\__\\ /:/\\:\\ \\:\\__\\ /\\ \\:\\ \\ \\__\\ /:/\\:\\  /\\__\\
 \\:\\  \\  \\/__/ \\/_|::\\/:/  / \\/__\\:\\/:/  / \\:\\ \\:\\ \\/__/ \\/__\\:\\/:/  /
  \\:\\  \\          |:|::/  /       \\::/  /   \\:\\ \\:\\__\\        \\::/  / 
   \\:\\  \\         |:|\\/__/        /:/  /     \\:\\/:/  /        /:/  /  
    \\:\\__\\        |:|  |         /:/  /       \\::/  /        /:/  /   
     \\/__/         \\|__|         \\/__/         \\/__/         \\/__/    

"))) 

(define apology 
   '("Radamsa stopped because something bad was about to happen. Sorry."
     "If you want to help fix this, drop by at #radamsa on freenode, file"
     "a bug at http://code.google.com/p/ouspg/issues/entry or send email to"
     "aki.helin@ee.oulu.fi. Adding the following text and instructions how"
     "to reproduce this bug will help."))

;; todo: cut down a and b representation length if they are too large
(define (boom err-code a b)
   (crash-msg)
   (for-each (λ (x) (print-to x stderr)) apology)
   (print-to (list "Info: " (list 'err-code 'a a 'b b)) stderr))

(λ (args)
   ;(print-stderr/time '("lisp side running"))
   (fork-server 'meta toy-meta)
   (fork-linked-server 'radamsa
      (λ () (process-arguments (cdr args) command-line-rules usage-text start-radamsa)))
   (let ((res (wait-mail)))
      (exit-owl
         (tuple-case res
            ((radamsa msg)
               (tuple-case msg
                  ((finished rval x t) rval)
                  ((error cont msg reason) (show msg reason) 1)
                  ((crashed err-code a b) (boom err-code a b) 127)
                  (else (show "Abnormal exit: " res) 2)))
            (else
               (print "Abnormal condition.")
               3)))))

