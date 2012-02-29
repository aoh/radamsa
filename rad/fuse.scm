;;;
;;; Alignment-based sequence fusing
;;;

(define-library (rad fuse)

   (export fuse)

   (import (owl base))

   (begin

      (define (car> a b) (> (car a) (car b)))

      ;; todo: move suffix mutations to a separate library or to (rad generic)
      (define (match-prefix lst p)
         (cond
            ((null? p) #true)
            ((null? lst) #false)
            ((eq? (car lst) (car p))
               (match-prefix (cdr lst) (cdr p)))
            (else #false)))

      (define (occurrences lst pat)
         (let loop ((lst lst) (found null) (pos 0))
            (cond
               ((null? lst) found)
               ((match-prefix lst pat)
                  (loop (cdr lst) (cons pos found) (+ pos 1)))
               (else
                  (loop (cdr lst) found (+ pos 1))))))

      (define (add-suffix ff ls)
         (if (null? ls)
            ff
            (lets ((x ls ls))
               (put ff x
                  (cons ls (get ff x null))))))

      (define (note-substring occurrences rbytes others)
         (let ((score (* (- occurrences 1) (* (- (length rbytes) 1) 2))))
            (if (< score 1)
               others
               (cons (cons score rbytes) others))))

      ;; → ((score reverse-bytes ..) ...)
      (define (radix-walk lsts path n found)
         (cond
            ((null? lsts) found)
            ((null? (cdr lsts)) found)
            ((eq? n 0) found)
            (else
               (ff-fold
                  (λ (found char tails)
                     (radix-walk tails (cons char path) (- n 1)
                        (note-substring (length tails) (cons char path) found)))
                  found
                  (fold add-suffix #false lsts)))))

      ;; list of list suffixes
      (define (suffixen lst)
         (if (null? lst)
            null
            (cons lst (suffixen (cdr lst)))))

      ;; ((i . x) (j . z) ...) n → z
      (define (priority-pick l n)
         (let ((m (caar l)))
            (if (<= n m)
               (cdar l)
               (priority-pick (cdr l) (- n m)))))


      ;; todo: make surf max jump size a command line flag
      (define (frequent-substring rs lst)
         (lets
            ((rs max-depth (rand-range rs 4 20)) ; <- 32 gives a max 100ms search
             (subs (radix-walk (suffixen lst) null max-depth null))
             (subs (sort car> subs))
             (rs n (rand rs (fold + 0 (map car subs)))))
            (if (null? subs) ;; <- blank → null
               (values rs null)
               (values rs (reverse (priority-pick subs n))))))

      ;; poss quaranteed to have >= 2 elems
      (define (choose-jump rs poss len)
         (lets
            ((rs poss (random-permutation rs poss))
             (from (car poss))
             (to (cadr poss)))
            (if (= (+ from len) (- to 1)) ;; wouldn't change the data
               (values to from)
               (values from to))))

      (define (fuse rs al bl)
         (values rs 
            (append al bl)))

      ;(define (sed-seq-surf rs ll meta) ;; jump between two shared suffixes in the block
      ;   (lets
      ;      ((lst (vector->list (car ll)))
      ;       (rs root (frequent-substring rs lst))
      ;       (poss (occurrences lst root)))
      ;      ;; a jump list of one element is constructed for blocks with just one byte
      ;      ;(show "SURF: jumping " (list 'from from 'to to 'using root))
      ;      (if (> (length poss) 1) ;; extremely likely for non-random data
      ;         (lets
      ;            ((from to (choose-jump rs poss (length root)))
      ;             (block (car ll))
      ;             (get (λ (p) (refb block p)))
      ;             (rs d (rand-delta-up rs))
      ;             (pre (list->byte-vector (map get (iota 0 1 from))))
      ;             (post (list->byte-vector (map get (iota to 1 (sizeb block))))))
      ;            (values sed-seq-surf rs 
      ;               (ilist pre post (cdr ll))
      ;               (inc meta 'seq-surf)
      ;               d))
      ;         (values sed-seq-surf rs ll meta -1))))

))
