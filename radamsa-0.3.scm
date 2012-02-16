#!/usr/bin/ol --run

;;;
;;; Radamsa - a general purpose fuzzer
;;;

;; todo and think tomorrow:
; - make sure it's easy to add documentation and metadata everywhere from beginning
; - log-growing block sizes up to max running speed?

(import (owl args))

(define version-str "Radamsa 0.3a") ;; aka funny fold
(define usage-text "Usage: radamsa [arguments] [file ...]")

;; muta :: rs ll meta → self' rs' ll' meta'
(define (dummy-mutator rs ll meta)
   (values
      dummy-mutator
      rs
      (cons (list->vector (cons #\X (vector->list (car ll)))) (cdr ll))
      (put meta 'dummies 
         (+ 1 (get meta 'dummies 0)))))

;; pat :: rs ll muta meta → ll' ++ (list (tuple rs mutator meta))
(define (dummy-pattern rs ll mutator meta)
   (lets 
      ((meta (put meta 'pattern 'dummy))
       (mutator rs ll meta 
         (dummy-mutator rs ll meta)))
      (lappend ll (list (tuple rs mutator meta)))))

;; dict paths → gen
;; gen :: rs → gen' rs' ll
(define (dummy-generator dict paths)
   (define (gen rs)
      (lets
         ((rs seed (rand rs #xffffffffffff)))
         (values gen rs
            (let loop ((rs (seed->rands seed)))
               (lets ((rs n (rand rs 10)))
                  (if (eq? n 0)
                     null
                     (lets
                        ((rs lst (random-numbers rs 26 n))  ;; get some alphabet posns
                         (lst (map (λ (x) (+ x #\a)) lst))) ;; scale to a-z
                        (pair
                           (list->vector (cons #\space lst))
                           (loop rs)))))))))
   gen)

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

(define (stdout-stream)
   (values stdout-stream stdout 
      (put #false 'output 'stdout))) 

;; args → (out :: → out' fd meta) v null | #false
(define (dummy-output-stream arg)
   (if (equal? arg "-")
      stdout-stream
      (begin
         (show "I can't yet output " arg)
         #false)))

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

(define (test-mutation a b c) a)

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
         (λ (func) (cons func (cdr node))))
      (else #false)))

(define (car> a b) (> (car a) (car b)))

(define (string->fuzzers str)
   (lets
      ((ps (map c/=/ (c/,/ str))) ; ((name [priority-str]) ..)
       (ps (map selection->priority ps))
       (fs (map priority->fuzzer ps)))
      (if (all self fs) 
         (sort car> fs)
         #false)))

(define command-line-rules
   (cl-rules
      `((help "-h" "--help" comment "Show this thing.")
        (output "-o" "--output" has-arg default "-" cook ,dummy-output-stream
            comment "Where to write the generated data?")
        (count "-n" "--count" cook ,string->integer check ,(λ (x) (> x 0))
            default "1" comment "How many outputs to generate?")
        (seed "-s" "--seed" cook ,string->integer comment "Random seed (number, default random)")
        (fuzzers "-f" "--fuzzers" cook ,string->fuzzers 
            comment "Enabled mutations and initial probabilities"
            default "b*=1,l*=2")
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
         (print (seed->rands (getf dict 'seed)))
         ;; print command line stuff
         (for-each 
            (λ (thing) (print* (list (car thing) ": " (cdr thing))))
            (ff->list 
               (put dict 'samples paths)))
         
         (lets/cc ret
            ((fail (λ (why) (print why) (ret 1)))
             (rs (seed->rands (getf dict 'seed))))
            (let loop 
               ((rs rs)
                (gen (dummy-generator dict paths))
                (out (get dict 'output 'bug))
                (n (getf dict 'count)))
               (if (= n 0)
                  0
                  (lets
                     ((gen rs ll (gen rs)) 
                      (muta dummy-mutator)
                      (pat  dummy-pattern)
                      (out fd meta (out))
                      (rs muta meta n-written 
                        (output (pat rs ll muta meta) fd)))
                     ;(show " -> meta " meta)
                     ;(show " => " n-written)
                     (loop rs gen out (- n 1)))))))))

(λ (args)
   (process-arguments (cdr args) 
      command-line-rules 
      usage-text 
      start-radamsa))

