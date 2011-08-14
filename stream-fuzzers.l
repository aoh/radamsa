;;; 
;;; functions that operate on block streams
;;;

(define-module lib-stream-fuzzers

   (export stream-fuzzers edit-byte-vector byte-vector->list)

   (define (byte-vector->list bvec)
      (let ((n (sizeb bvec)))
         (if (eq? n 0)
            null
            (let loop ((pos (- n 1)) (out null))
               (cond
                  ((eq? pos 0)
                     (cons (refb bvec pos) out))
                  (else
                     (lets ((posp _ (fx- pos 1)))
                        (loop posp (cons (refb bvec pos) out)))))))))

   ;; a silly fixnum/primitive mess, since this is fairly commonly used
   (define (edit-byte-vector bvec edit-pos fn)
      (let ((len (sizeb bvec)))
         (if (eq? len 0)
            bvec
            (let loop ((pos (- len 1)) (out null))
               (let ((val (refb bvec pos)))
                  (if (eq? pos edit-pos)
                     (if (eq? pos 0)
                        (list->byte-vector (fn val out))
                        (lets ((pos _ (fx- pos 1)))
                           (loop pos (fn val out))))
                     (if (eq? pos 0)
                        (list->byte-vector (cons val out))
                        (lets ((pos _ (fx- pos 1)))
                           (loop pos (cons val out))))))))))

   (define (digit d)
      (cond
         ((lesser? d 48) F)
         ((lesser? 57 d) F)
         (else (- d 48))))

   ; → digit|F tail
   (define (get-num lst)
      (let loop ((lst lst) (n 0) (digits 0))
         (cond
            ((null? lst)
               (if (eq? digits 0) 
                  (values F null)
                  (values n null)))
            ((digit (car lst)) =>
               (λ (d) (loop (cdr lst) (+ d (* n 10)) (+ digits 1))))
            ((eq? digits 0)
               (values F lst))
            (else
               (values n lst)))))

   ;; copy from pos up to end (not countin it)
   (define (copy-range pos end tail)
      (if (eq? pos end)
         tail
         (cons (car pos) (copy-range (cdr pos) end tail))))

   ;; fixme: simple placeholder
   (define (mutate-num rs num)
      (lets ((rs n (rand rs 16)))
         (cond
            ((eq? n 0) (values rs (+ n 1)))
            ((eq? n 1) (values rs (- n 1)))
            ((eq? n 2) (values rs 0))
            ((eq? n 3) (values rs 1))
            ((eq? n 4) (values rs #xff))
            ((eq? n 5) (values rs #x100))
            ((eq? n 6) (values rs #xffff))
            ((eq? n 7) (values rs #x10000))
            ((eq? n 8) (values rs #xffffffff))
            ((eq? n 9) (values rs #x100000000))
            (else
               (lets 
                  ((rs n (rand-range rs 1 129))
                   (rs n (rand-log rs n)))
                  (values rs (+ num n)))))))

   (define (mutate-a-num rs lst nfound)
      (if (null? lst)
         (lets ((rs which (rand rs nfound)))
            ;; choose which to mutate (or 0 if none)
            (values rs which null))
         (lets ((valp lstp (get-num lst)))
            (if valp
               (lets ((rs which tail (mutate-a-num rs lstp (+ nfound 1))))
                  (if (eq? which 0) ;; i won
                     (lets 
                        ((rs new (mutate-num rs valp))
                         (new-lst (render render new tail)))
                        (values rs -1 (render render new tail)))
                     (values rs (- which 1)
                        (copy-range lst lstp tail))))
               (lets ((rs which tail (mutate-a-num rs (cdr lst) nfound)))
                  (values rs which (cons (car lst) tail)))))))

   (define (sfuzz-num s r o)
      (tuple 'fuzzer
         (cons 1
            (λ (rs str)
               (lets
                  ((lst (vec->list (car str)))
                   (rs n lst (mutate-a-num rs lst 0)))
                  (values rs
                     ;; fixme: need a (bvec-conss bytes bvec) that splits large lists
                     (cons (list->byte-vector lst) (cdr str))))))))

   ;; put a star to some position
   (define (sfuzz-star s r o)
      (tuple 'fuzzer 
         (cons 1 
            (λ (rs str) 
               (lets ((rs p (rand rs (sizeb (car str)))))
                  (values rs
                     (cons 
                        (edit-byte-vector (car str) p (λ (old tl) (cons 42 tl)))
                        (cdr str))))))))

   ;; todo: add zero width joiners, different length leading blank padding bytes etc
   (define funny-unicode
      (list->tuple
         (append
            (list ;; some manual ones
               (list #xef #xbb #xbf)  ;; the canonical utf8 bom
               (list #xfe #xff)       ;; utf16 be bom
               (list #xff #xfe)       ;; utf16 le bom
               (list 0 0 #xff #xff)   ;; ascii null be
               (list #xff #xff 0 0)   ;; ascii null le
               (list 43 47 118 56)    ;; and some others from wikipedia
               (list 43 47 118 57)
               (list 43 47 118 43)
               (list 43 47 118 47)
               (list 247 100 76)
               (list 221 115 102 115)
               (list 14 254 255)
               (list 251 238 40)
               (list 251 238 40 255)
               (list 132 49 149 51))
            (map ;; some valid points and ranges
               (λ (codepoint) 
                  (render render codepoint null)) ;; <- make UTF-8 repr
               (fold
                  (λ (tl node)
                     (if (pair? node) ;; inclusive range
                        (append (iota (car node) 1 (+ (cdr node) 1)) tl)
                        (cons node tl)))
                  null
                  '((#x0009 . #x000d)    ; control chars
                    #x00a0               ; no-break space
                    #x1680               ; ogham space mark
                    #x180e               ; mongolian vowel separater (...)
                    (#x2000 . #x200a)    ; some spaces
                    #x2028               ; line separator
                    #x2029               ; paragraph separator
                    #x202f               ; narrow no-break space
                    #x205f               ; medium mathematical space (for small vectors)
                    #x3000               ; ideographic space
                    (#x200e . #x200f)    ; bidi stuff
                    (#x202a . #x202e)    ; more bidi stuff
                    (#x200c . #x200d)    ; zero width joining
                    #x0345               ; combining greek yggdrasil
                    #x00b7               ; extender - middle dot
                    (#x02d0 . #x02d1)     ; modifiers
                    #xff70               ; prolonged sound 
                    (#x02b0 . #x02b8)    ; modifier small letters
                    #xfdd0               ; a non-character 
                    #x034f               ; ignorables
                    (#x115f . #x1160)    ; hangul ignorables
                    (#x2065 . #x2069)    ; reserved ignorables
                    #x3164               ; hangul filler
                    #xffa0               ; halfwit hangul filler
                    #xe0001              ; deprecated language tag
                    (#xe0020 . #xe007f)  ; more deprecated 
                    (#x0e40 . #x0e44)    ; something else
                    #x1f4a9              ; some unicode shit
         ))))))

   ;; insert UTF-8 that might be mishandled
   (define (sfuzz-unicode s r o)
      (tuple 'fuzzer
         (cons 1
            (λ (rs str)
               (lets 
                  ((rs p (rand rs (sizeb (car str))))
                   (rs bytes (rand-elem rs funny-unicode)))
                  (values rs
                     (cons
                        (edit-byte-vector (car str) p (λ (old tl) (append bytes (cons old tl))))
                        (cdr str))))))))

   (define (sfuzz-bit s r o)
      (tuple 'fuzzer 
         (cons 1 
            (λ (rs str) 
               (lets 
                  ((rs p (rand rs (sizeb (car str))))
                   (rs b (rand rs 8)))
                  (values rs
                     (cons 
                        (edit-byte-vector (car str) p 
                           (λ (old tl) (cons (bxor old (<< 1 b)) tl)))
                        (cdr str))))))))

   (define (sfuzz-drop s r o)
      (tuple 'fuzzer 
         (cons 1 
            (λ (rs str) 
               (lets 
                  ((rs p (rand rs (sizeb (car str))))
                   (rs b (rand rs 8)))
                  (values rs
                     (cons 
                        (edit-byte-vector (car str) p (λ (old tl) tl))
                        (cdr str))))))))

   (define (sfuzz-inc-byte s r o)
      (tuple 'fuzzer 
         (cons 1 
            (λ (rs str) 
               (lets ((rs p (rand rs (sizeb (car str)))))
                  (values rs
                     (cons 
                        (edit-byte-vector (car str) p 
                           (λ (val tl) (cons (band 255 (+ val 1)) tl)))
                        (cdr str))))))))

   (define (sfuzz-stutter s r o)
      (tuple 'fuzzer 
         (cons 1 
            (λ (rs str) 
               (if (< (sizeb (car str)) 4)
                  (values rs str)
                  (lets 
                     ((rs start (rand-range rs 0 (- (sizeb (car str)) 2)))
                      (rs end (rand-range rs (+ start 1) (sizeb (car str))))
                      (pre (map (λ (p) (refb (car str) p)) (iota 0 1 start)))
                      (post (map (λ (p) (refb (car str) p)) (iota end 1 (sizeb (car str)))))
                      (stut (list->byte-vector (map (λ (p) (refb (car str) p)) (iota start 1 end))))
                      (rs n (rand-log rs 10)) ; max 2^10 = 1024 stuts
                      (stuts 
                        (fold 
                           (λ (tl n) (cons stut tl))
                           (if (null? post)
                              (cdr str)
                              (cons (list->byte-vector post) (cdr str)))
                           (iota 0 1 n))))
                     (values rs
                        (if (null? pre)
                           stuts
                           (cons (list->byte-vector pre) stuts)))))))))

   (define (add-carry lst n)
      (cond
         ((eq? n 0) lst)
         ((null? lst) (list n)) ; <- note, block boundaries cut carry
         (else
            (let ((this (+ (car lst) n)))
               (cons (band this 255)
                  (add-carry (cdr lst) (>> this 8)))))))

   (define (sfuzz-add-carry s r o)
      (tuple 'fuzzer 
         (cons 1 
            (λ (rs str) 
               (lets 
                  ((rs p (rand rs (sizeb (car str))))
                   (rs delta (rand rs 256)))
                  (values rs
                     (cons 
                        (edit-byte-vector (car str) p 
                           (λ (val tl) (add-carry (cons val tl) (max 1 delta))))
                        (cdr str))))))))

   (define (cons-bytes bs tl)
      (if (null? bs)
         tl
         (cons (list->byte-vector bs) tl)))

   ;; jump to a random position in a block (to be compared to sfuzz-jump and sfuzz-perm later)
   (define (sfuzz-rjump s r o)
      (tuple 'fuzzer
         (cons 1 
            (λ (rs str)
               (lets ; → 0..a b..end 
                  ((block (car str))
                   (rs a (rand rs (sizeb block)))
                   (rs b (rand rs (sizeb block)))
                   (as (map (λ (p) (refb block p)) (iota 0 1 a)))
                   (bs (map (λ (p) (refb block p)) (iota b 1 (sizeb block)))))
                  (values rs
                     (cons-bytes as (cons-bytes bs (cdr str)))))))))

   (define stream-fuzzers
      (list
            ;; stream fuzzers
            ;sfuzz-star      ;; adds the *-character somewhere (for testing distributions)
            sfuzz-bit        ;; flips a random bit
            sfuzz-drop       ;; drop a single byte
            sfuzz-inc-byte   ;; increment the value of a byte
            sfuzz-add-carry  ;; add 1-255 with carry to the right
            sfuzz-stutter    ;; stutter a part of the block
            sfuzz-rjump      ;; jump to a random position within a block (will probably be dropped after sfuzz-jump is in place)
            sfuzz-unicode    ;; fun with unicode
            sfuzz-num        ;; modify numbers (if any)
            )))

