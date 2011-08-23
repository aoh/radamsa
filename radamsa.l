;;;
;;; Radamsa - a general purpose test data generator
;;;

; block-stream      : () | (bvec . block-stream)
; stream-fuzzer     : opts → samples → info (block-stream → block-stream)
; stream-generator  : opts → samples → info (block-stream ...)
; stream-muxer      : opts → samples → info (block-stream block-stream → block-stream) 
; generate          : opts → samples → info (block-stream ...)
; step              : fuzz-node → block-stream fuzz-node

; start-ll-fuzzers    : rs opts samples ll-fuzzers → priority-list
; start-ll-muxers     : rs opts samples ll-muxers  → priority list
; start-ll-generators : rs opts samples ll-muxers  → priority list

; ll-genrs = ((info . ll) ...)
; ll-mutas = ((info . ll) ...)
; ll-muxrs = ((info . ll) ...)

; ll-zipper rs merge str-a str-b → str
;   zipper-fn rs sa sb cont → (block block ... . (cont sx sy))
;   (not sure yet)

; there are many telomerasen
; telomerase rs muta str → str'

; master-telomerase rs i sgens smuxs sfuzs → ((info . block-gen) ...)
;  - pop a sgen
;  - possibly pick stream fuzzers and attach
;  - possibly pop and merge with smux(s)
;  - collect data of each step 
;  - pair (result <- thunk!) and proceed

; fast forward:
;  - just forward the list n times


;; test data generator

(define (enumr n)
   (pair 
      (cons
         (+ n 100000000)
         (list (list->byte-vector (cons 42 (render render n '(10))))))
   (enumr (+ n 1))))

(define gen-test (enumr 1))

;; test telomerase - add a note

(define (telomerase ll muta) 
   (cons (list->vector (string->list "Telomerase was here: ")) ll))

(define tel-test
   (let loop () (pair (cons "test-telomerase" telomerase) (loop))))

(define test-fuzzs
   (let loop ()
      (pair 
         (cons 'test-fuzzer (λ (a b) a)) (loop))))

;; test 

;; convert content generators and changers to a stream of data streams

(define (master-telomerase opts)
   (define (gen gens muxs fuzzs tels)
      (lets
         ((g-node gens (uncons gens F))
          (m-node fuzzs (uncons fuzzs F))
          (t-node tels (uncons tels F))
          (case-info 
            (foldr (λ (x tl) (render render x tl)) null (list (car g-node) "/" (car t-node) "/" (car m-node))))
          (case-stream
            (λ ()
               ((cdr t-node) (cdr g-node) (cdr m-node)))))
         (pair
            (cons case-info case-stream)
            (gen gens muxs fuzzs tels))))
   gen)

(define (get-stdout-fd)
   (pair (cons "stdout" stdout) 
      (get-stdout-fd)))

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

(define (send-stream data port)
   (let ((res (mail-blocks data port 0)))
      ((if (eq? port stdout) flush-port close-port) port)
      res))

;; info is ((n-bytes-written . printable-metadata) ...)
(define (writer fds lls n)
   (let loop ((lls lls) (fds fds) (infos null) (n n))
      (if (= n 0)
         (values lls fds (reverse infos))
         (lets 
            ((out fds (uncons fds F))
             (name fd out) ;; where to write, name = path, ip etc
             (node lls (uncons lls F)) ;; what to write
             (info data node))
            (if (not fd)
               (error "Failed to write: " name))
            (let ((written (send-stream data fd)))
               ;(print* (list " + " (list->string info) ": " written "b"))
               (loop lls fds (cons (cons written info) infos) (- n 1)))))))

(define test-lls 
   ((master-telomerase 'no-opts-yet)
      gen-test
      'unusedatm
      test-fuzzs
      tel-test))


;; startup 
; grab sample paths (add implicitly - if none)
; make output stream (add implicitly -o - if none)
; get random seed (given | /dev/urandom | time)
; make lls, fuzs, mixs etc by
;  - walk over enabled ones
;  - find entry function from global
;  - apply entry func to seed x args x samples → node stream
;  - add priorities of each based on cmd line args 
;  - do a probabilistic stream merge : rs (... (pri thing ...) ...) → (thing (prob-merge rs' ... (pri ...) ...))
;     + simple version: count sum of priorities, sort by them, make rand in range and see where it hits
;     + can do better with a heap
;  - return the enabled ones

(import lib-args)

(define command-line-rule-exp
   `((help "-h" "--help")
     (output "-o" "--output" has-arg default "-"
         comment "Where to save or send the data (default stdout, see man page for options)")
     (count "-n" "--count" cook ,string->integer check ,(λ (x) (> x 0))
         default "1" comment "how many outputs to generate")
     (seed "-s" "--seed" has-arg comment "random seed")
     (blank "-r" "--tabula-rasa" comment "Do not read any sample files or stdin.")))

(define command-line-rules
   (cl-rules command-line-rule-exp))

(define usage-text "hai ^ - ^")

(define (print-usage-text)
   (print usage-text)
   (print-rules command-line-rules))

;; () → string
(define (urandom-seed)
   (let ((fd (open-input-file "/dev/urandom"))) ;; False if not there
      (if fd
         (let ((data (interact fd 16)))
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

(define (toy-meta)
   (bind (wait-mail)
      (λ (from msg)
         (mail from 'anon)
         (toy-meta))))

;; str → integer
(define (string->seed str)
   (if (string? str)
      (fold (λ (seed digit) (* (+ digit 1) (+ seed 1))) 1 (str-iter str))
      (error "bad seed: " str)))

(define (start-radamsa opts paths)
   (cond
      ((get opts 'help F)
         (print "HALP") 
         0)
      ((not (get opts 'seed F))
         ;; make a time-based seed unless one is given explicitly
         (start-radamsa (add-seed opts) paths))
      (else
         (lets
            ((seed (string->seed (get opts 'seed F)))
             (_ (show " -> seed " seed))
             (paths ;; add stdin as a sample if no files given and no blank requested
               (if (and (null? paths) (not (get opts 'blank F)))
                  '("-") paths))
             (_ (show " -> samples " paths))
             (rs (seed->rands seed))
             (lls fds info-bytes
               (writer get-stdout-fd test-lls (get opts 'count False))))
            (for-each
               (λ (node)
                  (lets ((n-written msg node))
                     (print* (list " + " (list->string msg) ": " n-written "b"))))
               info-bytes)
            0))))

(λ (args)
   (fork-server 'meta toy-meta)
   (fork-linked-server 'radamsa
      (λ () (process-arguments (cdr args) command-line-rules usage-text start-radamsa)))
   (let ((res (wait-mail)))
      (exit-owl
         (tuple-case res
            ((radamsa msg)
               (if (and (tuple? msg) (eq? 'finished (ref msg 1)))
                  0 ;; normal exit
                  (begin
                     (show "error: " msg)
                     2))) ;; something funny (nothing more specific yet)
            (else 1))))) ;; something unexpected






