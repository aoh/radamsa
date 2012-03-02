;;;
;;; Radamsa Output
;;;

(define-library (rad output)

   (import
      (owl base)
      (only (owl primop) halt)
      (rad shared))

   (export
      output
      string->outputs)

   (begin

      ;; output :: (ll' ++ (#(rs mutator meta))) fd → rs mutator meta (n-written | #f), handles port closing &/| flushing
      (define (output ll fd)
         (lets ((res n (blocks->fd ll fd)))
            (cond
               ((not res) (halt 111)) ;; ungraceful direct exit on write errors
               ((and (pair? res) (tuple? (car res)))
                  ;; valid exit with latest and greatest states in a tuple
                  (lets
                     ((states (car res))
                      (rs muta meta states))
                     (if (not (eq? fd stdout))
                        (close-port fd)) ;; closes the opened thread also, later not needed
                     (values rs muta meta n)))
               (else
                  (print*-to (list "Invalid data in output: " (car res)) stderr)
                  (wait 100)
                  (halt 121)))))

      (define (stdout-stream meta)
         (values stdout-stream stdout 
            (put meta 'output 'stdout)))

      (define (file-writer pat)
         (define (gen meta)
            (lets 
               ((path ;; <- could also just regex it there
                  (runes->string
                     (str-foldr
                        (λ (char tl)
                           (cond
                              ((null? tl)
                                 (cons char tl))
                              ((eq? char #\%)
                                 (case (car tl)
                                    ((#\n) (render (get meta 'nth 0) (cdr tl)))
                                    (else (error "Unknown pattern in output path: " (list->string (cons char tl))))))
                              (else (cons char tl))))
                        null pat)))
                (port (open-output-file path)))
               (if (not port)
                  (print*-to (list "Warning: cannot write to '" path "'") stderr))
               (values gen port (put (put meta 'output 'file-writer) 'path path))))
         gen)

      ;; args → (out :: → out' fd meta) v null | #false
      (define (string->outputs str)
         (cond
            ((equal? str "-") ;; conventional way to ask for standard output (and input)
               stdout-stream)
            ((m/^:[0-9]+$/ str)
               (print "This version doesn't yet have TCP server mode.")
               #false)
            ((m/^[0-9]{1,3}(\.[0-9]{1,3}){3}:[0-9]+$/ str)
               (print "This version doesn't yet have TCP client mode.")
               #false)
            (else
               (file-writer str))))
))
