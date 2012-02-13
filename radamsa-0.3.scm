#!/usr/bin/ol --run

;;;
;;; Radamsa - a general purpose fuzzer
;;;

;; todo and think tomorrow:
; - convert patterns and outputs to rs → self rs val ...
; - make sure it's easy to add documentation and metadata everywhere from beginning
; - use direct IO later. requires only local changes.
; - log-growing block sizes up to max running speed?
; - would be easy to allow dumping smallish resumable (fasl) states. useful?
; - allow saving metadata from the beginning
;   + separately given file? --metadata out.meta
;   + per file? -M | --metadata-suffix .meta

(import (owl args))

(define version-str "Radamsa 0.3a") ;; aka funny fold
(define usage-text "Usage: radamsa [args]")

;; muta :: rs ll meta → self' rs' ll' meta'
(define (dummy-mutator rs ll meta)
   (values
      dummy-mutator
      rs
      (cons (vector 42 42 42) ll)
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
      (values gen rs 
         (list (vector 97 97 97) (vector 98 98 98) (vector 99 99 99) (vector 10))))
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

;; dict args → rval
(define (start-radamsa dict paths)
   ;; show command line stuff
   (for-each 
      (λ (thing) (print* (list (car thing) ": " (cdr thing))))
      (ff->list 
         (put dict 'samples paths)))

   ;; run test
   (lets
      ((rs (seed->rands (time-ms)))
       (gen (dummy-generator dict paths))
       (out (get dict 'output 'bug))
       (rs gen ll (gen rs))
       (muta dummy-mutator)
       (pat  dummy-pattern)
       (out fd meta (out))
       (rs muta meta n (output (pat rs ll muta meta) fd)))
      (show " -> meta " meta)
      (show " => " n))

   ;; done
   0)

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

(define command-line-rules
   (cl-rules
      `((help "-h" "--help" comment "Show this thing.")
        (output "-o" "--output" has-arg default "-" cook ,dummy-output-stream
            comment "Where to write the generated data?")
        (count "-n" "--count" cook ,string->integer check ,(λ (x) (> x 0))
            default "1" comment "How many outputs to generate?")
        (seed "-s" "--seed" has-arg comment "Random seed (any string).")
        (version "-V" "--version" comment "Show version information."))))

(λ (args)
   (process-arguments (cdr args) 
      command-line-rules 
      usage-text 
      start-radamsa))

