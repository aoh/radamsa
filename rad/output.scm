;;;
;;; Radamsa Output
;;;

(define-library (rad output)

   (import
      (owl base)
      (rad shared))

   (export
      output
      string->outputs)

   (begin

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
                     (if (not (eq? fd stdout)) (close-port fd))
                     (lets ((rs muta meta x))
                        (values rs muta meta n)))
                  (else
                     (error "output: bad node: " x))))))

      (define (stdout-stream meta)
         (values stdout-stream stdout 
            (put meta 'output 'stdout))) 

      ;; args → (out :: → out' fd meta) v null | #false
      (define (string->outputs arg)
         (if (equal? arg "-")
            stdout-stream
            (begin
               (show "I can't yet output " arg)
               #false)))
))
