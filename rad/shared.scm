
;;;
;;; Shared Parameters and Functions
;;;

(define-library (rad shared)

   (import (owl base))

   (export
      max-block-size
      avg-block-size
      initial-ip
      remutate-probability
      selection->priority
      choose
      choose-pri
      car>
      length<
      stderr-probe
      )

   (begin

      (define avg-block-size 1024)        ; average block size when streaming sample data
      (define initial-ip 24)              ; initial max 1/n for basic patterns
      (define remutate-probability 4/5)   ; probability of each mutation being followed by a new one in nd

      ;; simple runtime event tracing
      (define-syntax stderr-probe
         (syntax-rules ()
            ;((stderr-probe thing value) (begin (print-to stderr thing) value)) ;; probes enabled
            ((stderr-probe thing value) value) ;; probes disabled
      ))

      (define max-block-size (* 2 avg-block-size))

      ;; (< (length l) n) for potentially long lists
      (define (length< l n)
         (cond
            ((eq? n 0) #false)
            ((null? l) #true)
            (else (length< (cdr l) (- n 1)))))

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
                           (print*-to stderr (list "Bad priority: " (cadr lst)))
                           #false)
                        ((< pri 0) ;; allow 0 to set a fuzzer off
                           (print*-to stderr (list "Inconceivable: " (cadr lst)))
                           #false)
                        (else
                           (cons (car lst) pri)))))
               ((= l 1)
                  (cons (car lst) 1))
               (else
                  (print*-to stderr (list "Too many things: " lst))
                  #false))))

      ; ((p . a) ...) n → x
      (define (choose-pri l n)
         (let ((this (caar l)))
            (if (< n this)
               (cdar l)
               (choose-pri (cdr l) (- n this)))))
      
))

