#!/usr/bin/ol --run

;;;
;;; Radamsa 
;;;

;;;
;;; Todo vs 0.2.x
;;;
; 
; - incremental priority learning
; - support for incremental mutation learning
; - multiple mutation attempts on failures
; - proper per-module documentation (via command line and otherwise)
; - much more mutations, patterns and generators
; 
; mutator:  rs ll meta → muta' rs' ll' meta'    -- merged mutator
;   - contains the individual mutations and priorities?
; pattern: rs ll mutator meta → 
;   a: have fd there and write as you go returning rs' meta' 
;   b: store updated states to end of stream and pop them after writing
; generator: rs dict args → (ll ...) 
;   - unlike in 0.2, make stdin a separate generator to allow separate priority
;   - file streamers ignore it, it either preloads or streams 
;   - turn muxing into separate generators because
;     + rarely useful in practice (incremental learning can do most of the requred stuff already)
;     + simplifies mental model a lot (stream data -> choose where to do stuff -> do stuff there)

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


