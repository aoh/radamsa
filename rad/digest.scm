

(define-library (rad digest)
   
   (import
      (owl base)
      (owl lazy))
   
   (export
      empty-digests
      dget            ;; digests digest -> bool
      dput            ;; digests digest -> digests
      digest)
   
   (begin

      (define empty-digests #empty)
      
      (define (dget tree d)
         (if (eq? tree #empty)
            #false
            (lets ((digit d d))
               (if (null? d)
                  (get tree d #false)
                  (dget (get tree d #empty) d)))))
     
      (define (dput tree d)
         (if (null? (cdr d))
            (put tree (car d) #true)
            (put tree (car d)
               (dput (get tree (car d) #empty) (cdr d)))))
            
      (define (bs->trits bs)
         (if (null? bs)
            null
            (lets 
               ((a bs (uncons bs 0))
                (b bs (uncons bs 0))
                (c bs (uncons bs 0))
                (_ a (fx<< a 16))
                (_ b (fx<< b 8)))
               (pair
                  (fxbor (fxbor a b) c)
                  (bs->trits bs)))))
      
      (define (digest ll)
         (lets ((ll (bs->trits ll))
                (fst ll (uncons ll 0)))
            (let loop ((ll (bs->trits ll)) (a fst) (sum fst) (len 1) (par fst))
               (cond
                  ((pair? ll)
                     (lets ((b (car ll))
                            (sum _ (fx+ sum b))
                            (len _ (fx+ len 1))
                            (par (fxbxor par b)))
                        (loop (cdr ll) b sum len par)))
                  ((null? ll)
                     ;; low -> high entropy
                     (list len fst a sum par))
                  (else
                     (loop (ll) a sum len par))))))))
         
         
         
