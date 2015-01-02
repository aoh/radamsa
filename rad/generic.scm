;;;
;;; Simple Generic Linear Mutations 
;;;

(define-library (rad generic)

   (import
      (owl base)
      (rad fuse))

   (export
      list-del      ;; rs l → rs' l'
      list-del-seq  ;; rs l → rs' l'
      list-dup      ;; likewise
      list-clone    ;; ditto 
      list-repeat   ;; same
      list-swap     ;; etc
      list-perm     ;
      list-fuse

      st-list-ins     ;; rs st l → rs' st' l'
      st-list-replace ;; rs st l → rs' st' l'
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

      ;; delete a sequence of things
      (define (list-del-seq rs l)
         (let ((len (length l)))
            (if (= len 0)
               (values rs l)
               (lets
                  ((rs start (rand rs len))
                   (rs n (rand rs (- len start))) ;; <- could take min with rand-len
                   (hd tl (split l start)))
                  (values rs (append hd (drop tl n)))))))

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

      ;; swap two adjecent values 
      (define (list-swap rs l)
         (let ((len (length l)))
            (if (< len 2)
               (values rs l)
               (lets ((rs p (rand rs (- len 1))))
                  (values rs
                     (edit l p 
                        (λ (l) (ilist (cadr l) (car l) (cddr l)))))))))

      ;; permute values
      (define (list-perm rs l)
         (lets ((len (length l)))
            (if (< len 3)
               (values rs l)
               (lets 
                  ((rs from (rand rs (- len 3)))
                   (rs a (rand-range rs 2 (- len from)))
                   (rs b (rand-log rs 10))
                   (n (max 2 (min a b)))
                   (hs (take l from))
                   (tl (drop l from))
                   (rs perm (random-permutation rs (take tl n)))
                   (tl (append perm (drop tl n))))
                  (values rs (append hs tl))))))

      ;; connect prefix of al somewhere to bl, and make sure that (list-fuse l l) != l
      (define list-fuse fuse)

      ;; mutations which keep some old elements for mutations in (n-elems elem-1 elem2 ...)
      (define stored-elems 10)
      (define update-prob (<< stored-elems 1))

      (define (step-state rs st l len)
         (if (< (car st) stored-elems)
            ;; add elements until there are enough stored elements
            (lets ((rs p (rand rs len)))
               (step-state rs 
                  (ilist (+ 1 (car st)) 
                         (lref l p)
                         (cdr st))
                  l len))
            (lets 
               ((rs up (rand rs update-prob)))
               (if (< up stored-elems)
                  ;; update a stored element
                  (lets
                     ((rs ep (rand rs len))
                      (new (lref l ep))
                      (st (edit st (+ 1 up) (λ (tl) (cons (lref l ep) (cdr tl)))))) ; +1 for len
                     (values rs st))
                  (values rs st)))))

      (define (pick-state rs st)
         (lets ((rs p (rand rs (car st))))
            (values rs (lref (cdr st) p))))

      (define (st-list-ins rs st l)
         (lets
            ((n (length l))
             (rs st (step-state rs st l n))
             (rs x (pick-state rs st))
             (rs p (rand rs n))
             (lp (edit l p (λ (tl) (cons x tl)))))
            (values rs st lp)))
      
      (define (st-list-replace rs st l)
         (lets
            ((n (length l))
             (rs st (step-state rs st l n))
             (rs x (pick-state rs st))
             (rs p (rand rs n))
             (lp (edit l p (λ (tl) (cons x (cdr tl))))))
            (values rs st lp)))

      ;; st-list-swap st-list-ins 
      ;; 

      ;;;
      ;;; Testing
      ;;;

      (define (print rs op data)
         (lets ((rs out (op rs data)))
            (print* (list " - " data " → " out " using " op))))

))

