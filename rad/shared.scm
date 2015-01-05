
;;;
;;; Shared Parameters and Functions
;;;

(define-library (rad shared)

   (import (owl base))

   (export
      max-block-size
      avg-block-size
      min-block-size
      initial-ip
      remutate-probability
      selection->priority
      choose
      choose-pri
      car>
      length<
      stderr-probe
      flush-bvecs
      inc
      mutate-num

      exit-write-error
      exit-read-error
      )

   (begin

      (define avg-block-size 2048)        ; average block size when streaming sample data
      (define min-block-size  256)        ; minimum preferred mutation block size
      (define initial-ip 24)              ; initial max 1/n for basic patterns
      (define remutate-probability 4/5)   ; probability of each mutation being followed by a new one in nd

      ;; val++ in ff, or insert 1
      (define (inc ff key)
         (let ((val (getf ff key)))
            (if val
               (fupd ff key (+ val 1))
               (put ff key 1))))

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

      ;; (byte ...) bvec-ll → (bvec .. . bvec-ll)
      (define (flush-bvecs byte-list tail)
         (let loop ((len (length byte-list)) (lst byte-list))
            (if (< len avg-block-size)
                (cons (list->byte-vector lst) tail)
                (let
                  ((hd (take lst avg-block-size))
                   (tl (drop lst avg-block-size)))
                  (cons (list->byte-vector hd)
                     (loop (- len avg-block-size) tl))))))

      (define exit-write-error 1)
      (define exit-read-error 2)

      (define interesting-numbers
         (list->vector
            (fold
               (λ (o x)
                  (let ((x (<< 1 x)))
                     (ilist (- x 1) x (+ x 1) o)))
               null
               '(1 7 8 15 16 31 32 63 64 127 128))))

      ;; fixme: simple placeholder
      (define (mutate-num rs num)
         (lets ((rs n (rand rs 12)))
            (cond
               ((eq? n 0)  (values rs (+ num 1)))
               ((eq? n 1)  (values rs (- num 1)))
               ((eq? n 2)  (values rs 0)) ;; todo, pack funny nums to a list and reduce opts
               ((eq? n 3)  (values rs 1))
               ((eq? n 4)  (rand-elem rs interesting-numbers))
               ((eq? n 5)  (rand-elem rs interesting-numbers))
               ((eq? n 6)  (rand-elem rs interesting-numbers))
               ((eq? n 7)  (lets ((rs x (rand-elem rs interesting-numbers))) (values rs (+ num x))))
               ((eq? n 8)  (lets ((rs x (rand-elem rs interesting-numbers))) (values rs (- x num))))
               ((eq? n 9)  (lets ((rs m (rand rs (* num 2)))) (values rs (- num m))))
               (else
                  (lets
                     ((rs n (rand-range rs 1 129))
                      (rs n (rand-log rs n))
                      (rs s (rand rs 3))) ;; add more likely
                     (values rs
                        ((if (eq? s 0) - +) num n)))))))

))

