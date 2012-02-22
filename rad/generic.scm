;;;
;;; Simple Generic Linear Mutations 
;;;

(define-library (rad generic)

   (import
      (owl base))

   (export
      list-del      ;; rs l → rs' l'
      list-dup      ;; likewise
      list-clone    ;; ditto 
      list-repeat   ;; same
      list-swap     ;; etc

      ;; missing: perm
   )

   (begin

      ;; apply fn to the list at pos (fixnum range)
      (define (edit l pos fn)
         (if (eq? pos 0)
            (fn l)
            (lets ((pos _ (fx- pos 1)))
               (cons (car l) (edit (cdr l) pos fn)))))

      ;; delete a random element
      (define (list-del rs l)
         (if (null? l)
            (values rs l)
            (lets
               ((len (length l))
                (rs p (rand rs len)))
               (values rs (ldel l p)))))

      ;; duplicate a random element
      (define (list-dup rs l)
         (if (null? l)
            (values rs l)
            (lets
               ((len (length l))
                (rs p (rand rs len)))
               (values rs
                  (edit l p
                     (λ (l) 
                        (if (null? l)
                           l
                           (cons (car l) l))))))))

      (define (push e n l)
         (if (eq? n 0)
            l
            (push e (- n 1) (cons e l))))

      ;; repeat an element
      (define (list-repeat rs l)
         (if (null? l)
            (values rs l)
            (lets
               ((len (length l))
                (rs p (rand rs len))
                (rs n (rand-log rs 10))
                (n (max n 2)))
               (values rs
                  (edit l p
                     (λ (l) (if (null? l) l (push (car l) n l))))))))

      ;; clone a value to another position
      (define (list-clone rs l)
         (if (null? l)
            (values rs l)
            (lets
               ((len (length l))
                (rs from (rand rs len))
                (rs to (rand rs len)))
               (values rs (lins l to (lref l from))))))

      ;; clone a value to another position
      (define (list-swap rs l)
         (let ((len (length l)))
            (if (< len 2)
               (values rs l)
               (lets ((rs p (rand rs (- len 1))))
                  (values rs
                     (edit l p 
                        (λ (l) (ilist (cadr l) (car l) (cddr l)))))))))

      ;; clone a value to another position
      (define (list-clone rs l)
         (if (null? l)
            (values rs l)
            (lets
               ((len (length l))
                (rs from (rand rs len))
                (rs to (rand rs len)))
               (values rs (lins l to (lref l from))))))

      (define (show rs op data)
         (lets ((rs out (op rs data)))
            (print* (list " - " data " → " out " using " op))))

      '(lets 
         ((rs (seed->rands (expt (time-ms) 3)))
          (data '(a b c)))
         (show rs list-repeat data)
         (show rs list-del data)
         (show rs list-dup data)
         (show rs list-clone data))
))

