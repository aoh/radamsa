#!/usr/bin/ol --run

;;;
;;; Radamsa 
;;;

(import (owl args))

(define version-str "Radamsa 0.3a") ;; aka funny fold
(define usage-text "Usage: radamsa [args]")

(define (start-radamsa dict paths)
   (for-each 
      (λ (thing) (print* (list (car thing) ": " (cdr thing))))
      (ff->list 
         (put dict 'samples paths)))
   0)

(define command-line-rules
   (cl-rules
      `((help "-h" "--help" comment "Show this thing.")
        ;(output "-o" "--output" has-arg default "-" cook ,make-output-stream
        ;    comment "Where to write the generated data?")
        (count "-n" "--count" cook ,string->integer check ,(λ (x) (> x 0))
            default "1" comment "How many outputs to generate?")
        (seed "-s" "--seed" has-arg comment "Random seed (any string).")
        (version "-V" "--version" comment "Show version information."))))

(λ (args)
   (process-arguments (cdr args) 
      command-line-rules 
      usage-text 
      start-radamsa)
   0)


