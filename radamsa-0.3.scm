#!/usr/bin/ol --run

;;; Radamsa - a general purpose fuzzer

;; todo: mutator scale should probably be kept at 1-100 or 1-1000.

(import (owl args))

(define version-str "Radamsa 0.3a") ;; aka funny fold
(define usage-text "Usage: radamsa [arguments] [file ...]")


;;;
;;; Shared Parameters and Functions
;;;

(define-library (radamsa shared)

   (import (owl base))

   (export
      max-block-size
      initial-ip
      remutate-probability
      selection->priority
      choose
      choose-pri
      car>
      )

   (begin
      (define max-block-size (* 8 1024)) ; average half of this
      (define initial-ip 24)             ; initial max 1/n for basic patterns
      (define remutate-probability 2/3)

      ;; (#t(name func short long) ...) name → func | #false
      (define (choose options name)
         (cond
            ((null? options) #false)
            ((equal? name (ref (car options) 1))
               (ref (car options) 2))
            (else
               (choose (cdr options) name))))

      (define (car> a b) 
         (> (car a) (car b)))

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
      ; ((p . a) ...) n → x
      (define (choose-pri l n)
         (let ((this (caar l)))
            (if (< n this)
               (cdar l)
               (choose-pri (cdr l) (- n this)))))
      
))

(import (radamsa shared)) ;; temporarily also at toplevel



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
;;; Mutation patterns
;;;

(define-library (radamsa patterns)

   (import 
      (owl base)
      (radamsa shared))

   (export
      *patterns*
      string->patterns)

   (begin
      
      (define (mutate-once rs ll mutator meta cont)
         (lets
            ((rs ip (rand rs initial-ip)) ;; initial inverse probability, magic value
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
                              (cont ll rs mutator meta))
                           ;; keep moving
                           (pair this
                              (loop rs (car ll) (cdr ll) (+ ip 1)))))))
               ;; no data to work on
               (cont null rs mutator meta))))

      ;; pat :: rs ll muta meta → ll' ++ (list (tuple rs mutator meta))
      (define (pat-once-dec rs ll mutator meta)
         (mutate-once rs ll mutator meta 
            (λ (ll rs mutator meta)
               (lappend ll (list (tuple rs mutator meta))))))

      ;; 1 or more mutations
      (define (pat-many-dec rs ll mutator meta)
         (mutate-once rs ll mutator meta
            (λ (ll rs mutator meta)
               (lets ((rs muta? (rand-occurs? rs remutate-probability)))
                  (if muta? 
                     (pat-many-dec rs ll mutator meta)
                     (lappend ll (list (tuple rs mutator meta))))))))

      (define *patterns*
         (list
            (tuple "od" pat-once-dec 
               "Mutate once" 
               "Make one mutation with gradually lowering probability")
            (tuple "nd" pat-many-dec 
               "Mutate possibly many times" 
               "Make possibly several mutations with gradually lowering probability")))

      (define (priority->pattern pri)
         (let ((func (choose *patterns* (car pri))))
            (if func
               (cons (cdr pri) func)
               (begin
                  (print*-to (list "Unknown pattern: " (cdr pri)) stderr)
                  #false))))

      ;; ((pri . pat) ...) → (rs ll muta meta → <pattern output>)
      (define (mux-patterns ps)
         (lets
            ((ps (sort car> ps))
             (n (fold + 0 (map car ps))))
            (λ (rs ll muta meta)
               (lets ((rs n (rand rs n)))
                  ((choose-pri ps n) rs ll muta meta)))))

      (define (string->patterns str)
         (lets
            ((ps (map c/=/ (c/,/ str))) ; ((name [priority-str]) ..)
             (ps (map selection->priority ps))
             (ps (map priority->pattern ps)))
            (if (all self ps) 
               (mux-patterns ps)
               #false)))
))

(import (radamsa patterns)) ;; temporarily also at toplevel


;;;
;;; Data Generators
;;;

(define-library (radamsa generators)

   (import
      (owl base)
      (radamsa shared))

   (export 
      string->generator-priorities         ;; done at cl args parsing time
      generator-priorities->generator      ;; done after cl args
      )

   (begin

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
               (λ () (stream-port (seed->rands seed) port)))))

      ;; dict paths → gen
      ;; gen :: rs → rs' ll meta
      (define (stdin-generator rs online?)
         (lets 
            ((rs ll (port->stream rs stdin))
             (ll (if online? ll (force-ll ll)))) ;; preread if necessary
            (λ (rs)
               ;; note: independent of rs. could in offline case read big chunks and resplit each.
               ;; not doing now because online case is 99.9% of stdin uses
               (values rs ll (put #false 'generator 'stdin)))))

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
                        (values rs ll 
                           (list->ff (list '(generator . file) (cons 'source path)))))
                     (begin   
                        (print*-to (list "Warning: failed to open given sample path " path) stderr)
                        (gen rs)))))
            gen))

      (define (string->generator-priorities str)
         (lets
            ((ps (map c/=/ (c/,/ str))) ; ((name [priority-str]) ..)
             (ps (map selection->priority ps)))
            (if (all self ps) ps #false)))

      ;; ((pri . gen) ...) → (rs → gen output)
      (define (mux-generators gs)
         (lets
            ((gs (sort car> gs))
             (n (fold + 0 (map car gs))))
            (define (gen rs)
               (lets
                  ((rs n (rand rs n)))
                  ((choose-pri gs n) rs)))
            gen))

      (define (priority->generator rs args fail n)
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
                              (stdin-generator rs (= n 1)))
                           #false))
                     ((equal? name "file")
                        (let ((args (keep (λ (x) (not (equal? x "-"))) args)))
                           (if (null? args)
                              #false ; no samples given, don't start this one
                              (cons priority (file-streamer args)))))
                     (else
                        (fail (list "Unknown data generator: " name)))))
               (fail "Bad generator priority"))))

      (define (generator-priorities->generator rs pris args fail n)
         (lets 
            ((gs (map (priority->generator rs args fail n) pris))
             (gs (keep self gs)))
            (cond
               ((null? gs) (fail "no generators"))
               ((null? (cdr gs)) (cdar gs))
               (else (mux-generators gs)))))

))

(import (radamsa generators))

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
               (flush-port fd)
               (if (eq? fd stdout) (wait 1000) (close-port fd))
               (lets ((rs muta meta x))
                  (values rs muta meta n)))
            (else
               (error "output: bad node: " x))))))

;;;
;;; Stream Mutations
;;;

(define-library (radamsa mutations)

   (import 
      (owl base)
      (radamsa shared))

   (export 
      *mutations*
      string->mutator)

   (begin

      ;; mutation :: rs ll meta → mutation' rs' ll' meta' delta
      (define (add-a rs ll meta)
         (lets
            ((bvec (car ll))
             (rs p (rand rs (vec-len bvec)))
             (bvec
               (list->byte-vector
                  (led (vector->list bvec) p 
                     (λ (x) (if (eq? x 10) x #\a)))))
             (rs delta (rand-range rs -1 2)))
            (values
               add-a
               rs
               (cons bvec (cdr ll))
               (put meta 'test (+ 1 (get meta 'test 0)))
               delta)))

      (define (add-b rs ll meta)
         (lets
            ((bvec (car ll))
             (rs p (rand rs (vec-len bvec)))
             (bvec
               (list->byte-vector
                  (led (vector->list bvec) p 
                     (λ (x) (if (eq? x 10) x #\b)))))
             (rs delta (rand-range rs -1 2)))
            (values
               add-b
               rs
               (cons bvec (cdr ll))
               (put meta 'test (+ 1 (get meta 'test 0)))
               delta)))

      (define *mutations*
         (list
            (tuple "a" add-a "Replace a byte with a" "nothing here")
            (tuple "b" add-b "Replace a byte with b" "nothing here")))

      (define (name->mutation str)
         (or (choose *mutations* str)
             (begin
               (print*-to (list "Unknown mutation: " str) stderr)
               #false)))

      ;; #f | (name . priority) → #f | (priority . func)
      (define (priority->fuzzer node)
         (cond
            ((not node) #false)
            ((name->mutation (car node)) => 
               (λ (func) (cons (cdr node) func)))
            (else #false)))

      ; limit to [2 ... 1000]
      (define (adjust-priority pri delta)
         (if (eq? delta 0)
            pri
            (max 1 (min 100 (+ pri delta)))))

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
                                    (values (mux-fuzzers (append out (cdr pfs))) rs mll mmeta)))))))
                  ((null? ll)
                     (values (mux-fuzzers fs) rs ll meta))
                  (else 
                     (loop (ll)))))))

      ;; str → mutator | #f
      (define (string->mutator str)
         (lets
            ((ps (map c/=/ (c/,/ str))) ; ((name [priority-str]) ..)
             (ps (map selection->priority ps))
             (ps (map (λ (x) (if x (cons (car x) (* (cdr x) 10)) x)) ps)) ; scale priorities to 10x
             (fs (map priority->fuzzer ps)))
            (if (all self fs) 
               (mux-fuzzers (sort car> fs))
               #false)))

))

(import (radamsa mutations)) ;; temporarily at toplevel

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
            default "a,b")
        (patterns "-p" "--patterns" cook ,string->patterns
            comment "Which mutation patterns to use"
            default "od,nd")
        (generators "-g" "--generators" cook ,string->generator-priorities ; the rest of initialization needs all args
            comment "Which data generators to use"
            default "file,stdin=100")
        (metadata "-M" "--metadata" has-arg
            comment "Save metadata about generated files to this file")
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

(define (show-options)
   (print "Mutations (-m)")
   (for-each (λ (opt) (print* (list "  " (ref opt 1) ": " (ref opt 3)))) *mutations*)
   (print "")
   (print "Mutation patterns (-p)")
   (for-each (λ (opt) (print* (list "  " (ref opt 1) ": " (ref opt 3)))) *patterns*)
   (print "")
   (print "Generators (-g)")
   (print " stdin: read data from standard input if no paths are given or - is among them")
   (print " file: read data from given files"))

(define (maybe-meta-logger path fail)
   (if path
      (let ((port (open-output-file path)))
         (if port
            (λ (stuff)
               (if (eq? stuff 'close)
                  (close-port port)
                  (mail port (serialize stuff '(10)))))
            (fail "Cannot open metadata log file")))
      (λ (stuff) stuff)))

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
      ((getf dict 'version)
         (print version-str)
         0)
      ((getf dict 'help)
         (print usage-text)
         (print-rules command-line-rules)
         0)
      ((getf dict 'list)
         (show-options)
         0)
      (else
         (lets/cc ret
            ((fail (λ (why) (print why) (ret 1)))
             (rs (seed->rands (getf dict 'seed)))
             (record-meta (maybe-meta-logger (getf dict 'metadata) fail))
             (n (getf dict 'count))
             (gen 
               (generator-priorities->generator rs
                  (getf dict 'generators) paths fail (getf dict 'count))))
            (let loop 
               ((rs rs)
                (muta (getf dict 'mutations))
                (pat (getf dict 'patterns))
                (out (get dict 'output 'bug))
                (p 1))
               (if (< n p)
                  0
                  (lets
                     ((rs ll meta (gen rs))
                      (meta (put meta 'nth p))
                      (out fd meta (out meta))
                      (rs muta meta n-written 
                        (output (pat rs ll muta meta) fd)))
                     (record-meta meta)
                     (loop rs muta pat out (+ p 1)))))))))

(λ (args)
   (process-arguments (cdr args) 
      command-line-rules 
      usage-text 
      start-radamsa))

