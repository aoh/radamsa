#!/usr/bin/ol --run

;;;
;;; Radamsa - a general purpose fuzzer
;;;

(import (owl args))

(define version-str "Radamsa 0.3a") ;; aka funny fold
(define usage-text "Usage: radamsa [arguments] [file ...]")

(define max-block-size (* 8 1024)) ; average half of this

;; pat :: rs ll muta meta → ll' ++ (list (tuple rs mutator meta))
(define (pat-once-dec rs ll mutator meta)
   (lets
      ((rs ip (rand rs 24)) ;; initial inverse probability, magic value
       (this ll (uncons ll #false)))
      (if this
         (let loop ((rs rs) (this this) (ll ll) (ip ip))
            (if (function? ll)
               ;; stream more data
               (loop rs this (ll) ip)
               (lets ((rs n (rand rs ip)))
                  (if (or (eq? n 0) (null? ll)) ;; mutation happens to occur, or last place for it
                     (lets 
                        ((meta (put meta 'pattern 'dummy))
                         (ll (cons this ll))
                         (mutator rs ll meta (mutator rs ll meta)))
                        ;; use a cont here later as in 0.2
                        (lappend ll (list (tuple rs mutator meta))))
                     ;; keep moving
                     (pair this
                        (loop rs (car ll) (cdr ll) (+ ip 1)))))))
         ;; no data to work on
         (list (tuple rs mutator meta)))))

(define (rand-block-size rs)
   (lets ((rs n (rand rs max-block-size)))
      (values rs (max n 4))))

;; bvec|F bvec → bvec
(define (merge head tail)
   (if head
      (list->vector (vec-foldr cons (vec-foldr cons null tail) head))
      tail))

(define (stream-port rs port)
   (lets ((rs first (rand-block-size rs)))
      (let loop ((rs rs) (last #false) (wanted first)) ;; 0 = block ready (if any)
         (let ((block (interact port wanted)))
            (cond
               ((eof? block) ;; end of stream
                  (if (not (eq? port stdin)) (close-port port))
                  (if last (list last) null))
               ((not block) ;; read error
                  (if (not (eq? port stdin)) (close-port port))
                  (if last (list last) null))
               ((eq? (sizeb block) (+ wanted 1))
                  ;; a block of required (deterministic) size is ready
                  (lets
                     ((block (merge last block))
                      (rs next (rand-block-size rs)))
                     (pair block (loop rs #false next))))
               (else
                  (loop rs (merge last block)
                     (- wanted (sizeb block)))))))))

;; rs port → rs' (bvec ...), closes port unless stdin
(define (port->stream rs port)
   (lets ((rs seed (rand rs 100000000000000000000)))
      (values rs
         (stream-port (seed->rands seed) port))))

;; dict paths → gen
;; gen :: rs → gen' rs' ll meta
(define (stdin-generator online?)
   (λ (rs)
      (lets 
         ((rs ll (port->stream rs stdin))
          (ll (if online? ll (force-ll ll)))) ;; preread if necessary
         (define (gen rs)
            (values gen rs ll (put #false 'generator 'stdin)))
         (values gen rs ll (put #false 'generator 'stdin)))))

(define (file-streamer paths)
   (lets
      ((paths (list->vector paths))
       (n (vec-len paths)))
      (define (gen rs)
         (lets
            ((rs n (rand rs n))
             (path (vec-ref paths n))
             (port (open-input-file path)))
            (if port
               (lets ((rs ll (port->stream rs port)))
                  (values gen rs ll 
                     (list->ff (list '(generator . file) (cons 'source path)))))
               (begin   
                  (print*-to (list "Warning: failed to open given sample path " path) stderr)
                  (gen rs)))))
      gen))

;; output :: (ll' ++ (#(rs mutator meta))) fd → rs mutator meta (n-written | #f), handles port closing &/| flushing
(define (output ll fd)
   (let loop ((ll ll) (n 0))
      (lets ((x ll (uncons ll #false)))
         (cond
            ((not x)
               (error "output:" "no trailing state"))
            ((byte-vector? x)
               (mail fd x)
               (loop ll (+ n (vec-len x))))
            ((tuple? x)
               ((if (eq? fd stdout) flush-port close-port) fd)
               (lets ((rs muta meta x))
                  (values rs muta meta n)))
            (else
               (error "output: bad node: " x))))))

(define (selection->priority lst)
   (let ((l (length lst)))
      (cond
         ((= l 2) ; (name pri-str)
            (let ((pri (string->number (cadr lst) 10)))
               (cond
                  ((not pri)
                     (print*-to (list "Bad priority: " (cadr lst)) stderr)
                     #false)
                  ((< pri 0) ;; allow 0 to set a fuzzer off
                     (print*-to (list "Inconceivable: " (cadr lst)) stderr)
                     #false)
                  (else
                     (cons (car lst) pri)))))
         ((= l 1)
            (cons (car lst) 1))
         (else
            (print*-to (list "Too many things: " lst) stderr)
            #false))))

;; mutation :: rs ll meta → mutation' rs' ll' meta' delta
(define (test-mutation rs ll meta)
   (lets
      ((bvec (car ll))
       (rs p (rand rs (vec-len bvec)))
       (bvec
         (list->byte-vector
            (lset (vector->list bvec) p #\*))))
      (values
         test-mutation
         rs
         (cons bvec (cdr ll))
         (put meta 'test (+ 1 (get meta 'test 0)))
         0)))

(define (name->fuzzer str)
   (cond
      ((equal? str "test")
         test-mutation)
      (else
         (print*-to (list "Unknown mutation: " str) stderr)
         #false)))

;; #f | (name . priority) → #f | (priority . func)
(define (priority->fuzzer node)
   (cond
      ((not node) #false)
      ((name->fuzzer (car node)) => 
         (λ (func) (cons (cdr node) func)))
      (else #false)))

; limit to [2 ... 1000]
(define (adjust-priority pri delta)
   (if (eq? delta 0)
      pri
      (max 1 (min 1000 (+ pri delta)))))

(define (car> a b) 
   (> (car a) (car b)))

;; rs pris → rs' pris'
(define (weighted-permutation rs pris)
   (lets    
      ((rs ppris ; ((x . (pri . fn)) ...)
         (fold-map
            (λ (rs node)
               (lets ((rs pri (rand rs (car node))))
                  (values rs (cons pri node))))
            rs pris)))
      (values rs (map cdr (sort car> ppris)))))

;; ((priority . mutafn) ...) → (rs ll meta → mutator' rs' ll' meta')
(define (mux-fuzzers fs)
   (λ (rs ll meta)
      (let loop ((ll ll)) ;; <- force up to a data node
         (cond
            ((pair? ll)
               (lets ((rs pfs (weighted-permutation rs fs))) ;; ((priority . mutafn) ...)
                  (let loop ((pfs pfs) (out null) (rs rs)) ;; try each in order
                     (if (null? pfs) ;; no mutation worked
                        (values (mux-fuzzers out) rs ll meta)
                        (lets
                           ((mfn rs mll mmeta delta 
                              ((cdar pfs) rs ll meta))
                            (out ;; always remember whatever was learned
                              (cons (cons (adjust-priority (caar pfs) delta) mfn) out)))
                           (if (equal? (car ll) (car mll)) 
                              ;; try something else if no changes, but update state
                              (loop (cdr pfs) out rs)
                              (values (mux-fuzzers (append out pfs)) rs mll mmeta)))))))
            ((null? ll)
               (values (mux-fuzzers fs) rs ll meta))
            (else 
               (loop (ll)))))))

;; str → mutator | #f
(define (string->mutator str)
   (lets
      ((ps (map c/=/ (c/,/ str))) ; ((name [priority-str]) ..)
       (ps (map selection->priority ps))
       (fs (map priority->fuzzer ps)))
      (if (all self fs) 
         (mux-fuzzers (sort car> fs))
         #false)))

(define (string->generator-priorities str)
   (lets
      ((ps (map c/=/ (c/,/ str))) ; ((name [priority-str]) ..)
       (ps (map selection->priority ps)))
      (if (all self ps) ps #false)))

(define (priority->generator args fail n)
   ;; → (priority . generator) | #false
   (λ (pri)
      (if pri
         (lets ((name priority pri))
            (cond
               ((equal? name "stdin")
                  ;; a generator to read data from stdin
                  ;; check n and preread if necessary
                  (if (first (λ (x) (equal? x "-")) args #false)
                     ;; "-" was given, so start stdin generator + possibly preread
                     (cons priority
                        (stdin-generator (= n 1)))
                     #false))
               ((equal? name "file")
                  (let ((args (keep (λ (x) (not (equal? x "-"))) args)))
                     (if (null? args)
                        #false ; no samples given, don't start this one
                        (cons priority (file-streamer args)))))
               (else
                  (fail (list "Unknown data generator: " name)))))
         (fail "Bad generator priority"))))

(define (generator-priorities->generator pris args fail n)
   (lets 
      ((gs (map (priority->generator args fail n) pris))
       (gs (keep self gs)))
      (cond
         ((null? gs) (fail "no generators"))
         ((null? (cdr gs)) (cdar gs))
         (else (fail "cannot mux yet")))))
   
(define (string->patterns str)
   (list 'patterns str))

(define (stdout-stream meta)
   (values stdout-stream stdout 
      (put meta 'output 'stdout))) 

;; args → (out :: → out' fd meta) v null | #false
(define (dummy-output-stream arg)
   (if (equal? arg "-")
      stdout-stream
      (begin
         (show "I can't yet output " arg)
         #false)))

(define command-line-rules
   (cl-rules
      `((help "-h" "--help" comment "Show this thing.")
        (output "-o" "--output" has-arg default "-" cook ,dummy-output-stream
            comment "Where to put the generated data")
        (count "-n" "--count" cook ,string->integer check ,(λ (x) (> x 0))
            default "1" comment "How many outputs to generate")
        (seed "-s" "--seed" cook ,string->integer comment "Random seed (number, default random)")
        (mutations "-m" "--mutations" cook ,string->mutator 
            comment "Which mutations to use"
            default "test=42")
        (patterns "-p" "--patterns" cook ,string->patterns
            comment "Which mutation patterns to use"
            default "trololo")
        (generators "-g" "--generators" cook ,string->generator-priorities ; the rest of initialization needs all args
            comment "Which data generators to use"
            default "file,stdin=100")
        (list "-l" "--list" comment "List mutations, patterns and generators")
        (version "-V" "--version" comment "Show version information."))))

;; () → string
(define (urandom-seed)
   (let ((fd (open-input-file "/dev/urandom"))) ;; #false if not there
      (if fd
         (let ((data (interact fd 10)))
            (close-port fd)
            (if (vector? data)
               (vec-fold (λ (n d) (+ d (<< n 8))) 0 data)
               #false))
         #false)))

;; () → string (decimal number)
(define (time-seed)
   (time-ms))

;; dict args → rval
(define (start-radamsa dict paths)
   ;; show command line stuff
   (cond
      ((null? paths)
         ;; fuzz stdin when called as $ cat foo | radamsa | bar -
         (start-radamsa dict (list "-")))
      ((not (getf dict 'seed))
         ;; get a random seed, prefer urandom
         (start-radamsa 
            (put dict 'seed (or (urandom-seed) (time-seed)))
            paths))
      ((getf dict 'help)
         (print usage-text)
         (print-rules command-line-rules)
         0)
      ((getf dict 'list)
         (print "Would list stuff here.")
         0)
      (else
         ;; print command line stuff
         ;(for-each 
         ;   (λ (thing) (print* (list (car thing) ": " (cdr thing))))
         ;   (ff->list 
         ;      (put dict 'samples paths)))
         
         (lets/cc ret
            ((fail (λ (why) (print why) (ret 1)))
             (rs (seed->rands (getf dict 'seed))))
            (let loop 
               ((rs rs)
                (gen 
                  (generator-priorities->generator 
                     (getf dict 'generators) paths fail (getf dict 'count)))
                (muta (getf dict 'mutations))
                (out (get dict 'output 'bug))
                (n (getf dict 'count)))
               (if (= n 0)
                  0
                  (lets
                     ((gen rs ll meta (gen rs))
                      (pat  pat-once-dec)
                      (out fd meta (out meta))
                      (rs muta meta n-written 
                        (output (pat rs ll muta meta) fd)))
                     ;(print "")
                     ;(show " -> meta " meta)
                     ;(show " => " n-written)
                     (loop rs gen muta out (- n 1)))))))))

(λ (args)
   (process-arguments (cdr args) 
      command-line-rules 
      usage-text 
      start-radamsa))

