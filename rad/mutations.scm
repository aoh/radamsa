;;;
;;; Stream Mutations
;;;

(define-library (rad mutations)

   (import 
      (owl base)
      (rad shared))

   (export 
      *mutations*
      string->mutator)

   (begin

      ;; clone a byte vector to a list and edit at given position (using primops since this is heavily used)
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

      ;; mutation :: rs ll meta → mutation' rs' ll' meta' delta
      (define (add-a rs ll meta)
         (lets
            ((bvec (car ll))
             (rs p (rand rs (vec-len bvec)))
             (bvec
               (list->byte-vector
                  (led (vector->list bvec) p 
                     (λ (x) (if (eq? x 10) x #\a)))))
             (rs delta (rand-range rs -1 2)))
            (values
               add-a
               rs
               (cons bvec (cdr ll))
               (put meta 'test (+ 1 (get meta 'test 0)))
               delta)))

      (define (add-b rs ll meta)
         (lets
            ((bvec (car ll))
             (rs p (rand rs (vec-len bvec)))
             (bvec
               (list->byte-vector
                  (led (vector->list bvec) p 
                     (λ (x) (if (eq? x 10) x #\b)))))
             (rs delta (rand-range rs -1 2)))
            (values
               add-b
               rs
               (cons bvec (cdr ll))
               (put meta 'test (+ 1 (get meta 'test 0)))
               delta)))

      (define *mutations*
         (list
            (tuple "a" add-a "Replace a byte with a" "nothing here")
            (tuple "b" add-b "Replace a byte with b" "nothing here")))

      (define (name->mutation str)
         (or (choose *mutations* str)
             (begin
               (print*-to (list "Unknown mutation: " str) stderr)
               #false)))

      ;; #f | (name . priority) → #f | (priority . func)
      (define (priority->fuzzer node)
         (cond
            ((not node) #false)
            ((name->mutation (car node)) => 
               (λ (func) (cons (cdr node) func)))
            (else #false)))

      ; limit to [2 ... 1000]
      (define (adjust-priority pri delta)
         (if (eq? delta 0)
            pri
            (max 1 (min 100 (+ pri delta)))))

      ;; rs pris → rs' pris'
      (define (weighted-permutation rs pris)
         (lets    
            ((rs ppris ; ((x . (pri . fn)) ...)
               (fold-map
                  (λ (rs node)
                     (lets ((rs pri (rand rs (car node))))
                        (values rs (cons pri node))))
                  rs pris)))
            (values rs (map cdr (sort car> ppris)))))

      ;; ((priority . mutafn) ...) → (rs ll meta → mutator' rs' ll' meta')
      (define (mux-fuzzers fs)
         (λ (rs ll meta)
            (let loop ((ll ll)) ;; <- force up to a data node
               (cond
                  ((pair? ll)
                     (lets ((rs pfs (weighted-permutation rs fs))) ;; ((priority . mutafn) ...)
                        (let loop ((pfs pfs) (out null) (rs rs)) ;; try each in order
                           (if (null? pfs) ;; no mutation worked
                              (values (mux-fuzzers out) rs ll meta)
                              (lets
                                 ((mfn rs mll mmeta delta 
                                    ((cdar pfs) rs ll meta))
                                  (out ;; always remember whatever was learned
                                    (cons (cons (adjust-priority (caar pfs) delta) mfn) out)))
                                 (if (equal? (car ll) (car mll)) 
                                    ;; try something else if no changes, but update state
                                    (loop (cdr pfs) out rs)
                                    (values (mux-fuzzers (append out (cdr pfs))) rs mll mmeta)))))))
                  ((null? ll)
                     (values (mux-fuzzers fs) rs ll meta))
                  (else 
                     (loop (ll)))))))

      ;; str → mutator | #f
      (define (string->mutator str)
         (lets
            ((ps (map c/=/ (c/,/ str))) ; ((name [priority-str]) ..)
             (ps (map selection->priority ps))
             (ps (map (λ (x) (if x (cons (car x) (* (cdr x) 10)) x)) ps)) ; scale priorities to 10x
             (fs (map priority->fuzzer ps)))
            (if (all self fs) 
               (mux-fuzzers (sort car> fs))
               #false)))

))
