

(define-library (rad digest)
   
   (import
      (owl base)
      (owl lazy)
      (owl codec)
      (owl digest)) ;; sha256-raw ll → (fixnum ...)
   
   (export
      string->hash    ;; used for command line argument
      empty-digests
      bytes->trits
      dget            ;; digests digest -> bool
      dput            ;; digests digest -> digests
      digest)
   
   (begin

      (define (empty-digests max)
         (tuple #empty max 0))
         
      (define (dget* tree d)
         (if (eq? tree #empty)
            #false
            (lets ((digit d d))
               (if (null? d)
                  (get tree digit #false)
                  (dget* (get tree digit #empty) d)))))
    
      (define (dget tree d) 
         (dget* (ref tree 1) d))
      
      (define (dput* tree d)
         (if (null? (cdr d))
            (put tree (car d) #true)
            (put tree (car d)
               (dput* (get tree (car d) #empty) (cdr d)))))

      ;; todo: track duplicates and keep top n% across generations
      (define (prune tree size)
         (tuple #empty size 0))
         
      (define (dput cs d)
         (lets ((tree max n cs))
            (if (eq? n max)
               (dput (prune tree max) d)
               (tuple (dput* tree d) max (+ n 1)))))
         
      (define (bs->trits bs)
         (if (null? bs)
            null
            (lets 
               ((a bs (uncons bs 0))
                (b bs (uncons bs a))
                (c bs (uncons bs b))
                (_ a (fx<< a 16))
                (_ b (fx<< b 8)))
               (pair
                  (fxbor (fxbor a b) c)
                  (bs->trits bs)))))

      (define (trit a b c)
         (lets
            ((_ a (fx<< a 16))
             (_ b (fx<< b 8)))
            (fxbor (fxbor a b) c)))
             
      (define (get-trit ll)
         (cond
            ((null? ll) (values 0 0 ll))
            ((pair? ll)
               (lets ((a ll ll))
                  (cond
                     ((null? ll) (values a 1 ll))
                     ((pair? ll)
                        (lets ((b ll ll))
                           (cond
                              ((null? ll) (values (trit a b 0) 2 ll))
                              ((pair? ll) (lets ((c ll ll)) (values (trit a b c) 3 ll)))
                              (else
                                 (lets ((c ll (uncons ll #f)))
                                    (if c (values (trit a b c) 3 ll)
                                          (values (trit a b 0) 2 ll)))))))
                    (else 
                       (get-trit (cons a (ll)))))))
            (else (get-trit (ll)))))

      (define (pollinate a b)
         (lets ((ah a (fx<< a 3))
                (bh b (fx<< b 3))
                (a (fxbor a bh))
                (b (fxbor b ah)))
            (values a b)))
               
      (define (digest ll)
         (lets ((fst len ll (get-trit ll)))
            (let loop ((ll ll) (a fst) (sum fst) (len len) (par fst) (lag 0))
               (if (null? ll)
                  ;(list len fst a sum par lag)
                  (list 
                     (band #xffffff (bor (<< sum 10) len))
                     (if (= fst a) fst (fxbxor fst a))
                     par 
                     lag)
                  (lets ((b n ll (get-trit ll))
                         (sum _ (fx+ sum b))
                         (len _ (fx+ len n))
                         (par (fxbxor par b)))
                        (if (eq? (fxband len #b1) #b1)
                           (lets ((par lag (pollinate par lag)))
                              (loop ll b sum len par lag))
                           (lets ((sum par (pollinate sum par)))
                              (loop ll b sum len par lag))))))))

   (define (hash-stream ll)
      (let ((res (digest ll)))
         (values 
            res                   ;; correct 
            (str res)))) ;; →  trits->hex

   (define (bytes->trits lst)
      (let loop ((lst lst) (trit 0) (n 0))
         (cond
            ((null? lst)
               (if (eq? n 0)
                  null
                  (list trit)))
            ((eq? n 3)
               (cons trit (loop lst 0 0)))
            (else
               (loop (cdr lst)
                  (bor (<< trit 8) (car lst))
                  (+ n 1))))))
               
   (define (hash-sha256 lst)
      (let ((bs (sha256-bytes lst)))
         (values 
            (bytes->trits bs)
            (hex-encode-list bs))))
      
   (define (string->hash s)
      (cond
         ((string-ci=? s "stream") hash-stream)
         ((string-ci=? s "sha256") hash-sha256)
         ((string-ci=? s "sha") hash-sha256)
         (else #f)))
   
      
))
         
         
         
