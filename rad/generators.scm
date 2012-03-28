;;;
;;; Data Generators
;;;

(define-library (rad generators)

   (import
      (owl base)
      (rad shared)
      (only (owl primop) halt))

   (export 
      string->generator-priorities         ;; done at cl args parsing time
      generator-priorities->generator      ;; done after cl args
      )

   (begin

      (define (rand-block-size rs)
         (lets ((rs n (rand rs max-block-size)))
            (values rs (max n 4))))

      ;; bvec|F bvec → bvec
      (define (merge head tail)
         (if head
            (list->vector (vec-foldr cons (vec-foldr cons null tail) head))
            tail))

      (define (stream-port rs port)
         (lets ((rs first (rand-block-size rs)))
            (let loop ((rs rs) (last #false) (wanted first)) ;; 0 = block ready (if any)
               (let ((block (get-block port wanted)))
                  (cond
                     ((eof? block) ;; end of stream
                        ;(if (not (eq? port stdin)) (close-port port))
                        (if (not (eq? port stdin)) (fclose port))
                        (if last (list last) null))
                     ((not block) ;; read error
                        ;(if (not (eq? port stdin)) (close-port port))
                        (if (not (eq? port stdin)) (fclose port))
                        (if last (list last) null))
                     ((eq? (sizeb block) wanted)
                        ;; a block of required (deterministic) size is ready
                        (lets
                           ((block (merge last block))
                            (rs next (rand-block-size rs)))
                           (pair block (loop rs #false next))))
                     (else
                        (loop rs (merge last block)
                           (- wanted (sizeb block)))))))))

      ;; rs port → rs' (bvec ...), closes port unless stdin
      (define (port->stream rs port)
         (lets ((rs seed (rand rs 100000000000000000000)))
            (values rs
               (λ () (stream-port (seed->rands seed) port)))))

      ;; dict paths → gen
      ;; gen :: rs → rs' ll meta
      (define (stdin-generator rs online?)
         (lets 
            ((rs ll (port->stream rs stdin))
             (ll (if online? ll (force-ll ll)))) ;; preread if necessary
            (λ (rs)
               ;; note: independent of rs. could in offline case read big chunks and resplit each.
               ;; not doing now because online case is 99.9% of stdin uses
               (values rs ll (put #false 'generator 'stdin)))))

      (define (random-block rs n out)
         (if (eq? n 0)
            (values rs (list->byte-vector out))
            (lets ((digit rs (uncons rs #f)))
               (random-block rs (- n 1) (cons (fxband digit 255) out)))))

      (define (random-stream rs)
         (lets 
            ((rs n (rand-range rs 32 max-block-size))
             (rs b (random-block rs n null))
             (rs ip (rand-range rs 1 100))
             (rs o (rand rs ip)))
            (if (eq? o 0) ;; end
               (list b)
               (pair b (random-stream rs)))))
      
      (define (random-generator rs)
         (lets ((rs seed (rand rs 1111111111111111111111111111111111111)))
            (values rs 
               (random-stream (seed->rands seed))
               (put #false 'generator 'random))))

      ;; paths → (rs → rs' ll|#false meta|error-str)
      (define (file-streamer paths)
         (lets
            ((paths (list->vector paths))
             (n (vec-len paths)))
            (define (gen rs)
               ;; todo: approximates failing after trying a random permutation of paths
               (let loop ((rs rs) (tries 0))
                  (if (= tries 100)
                     (values rs #false "Cannot read")
                     (lets
                        ((rs n (rand rs n))
                         (path (vec-ref paths n))
                         (port (open-input-file path)))
                        (if port
                           (lets ((rs ll (port->stream rs port)))
                              (values rs ll 
                                 (list->ff (list '(generator . file) (cons 'source path)))))
                           (begin   
                              (print*-to stderr (list "Warning: failed to open given sample path " path))
                              (loop rs (+ tries 1))))))))
            gen))

      (define (string->generator-priorities str)
         (lets
            ((ps (map c/=/ (c/,/ str))) ; ((name [priority-str]) ..)
             (ps (map selection->priority ps)))
            (if (all self ps) ps #false)))

      ;; ((pri . gen) ...) → (rs → gen output)
      (define (mux-generators gs)
         (lets
            ((gs (sort car> gs))
             (n (fold + 0 (map car gs))))
            (define (gen rs)
               (lets
                  ((rs n (rand rs n)))
                  ((choose-pri gs n) rs)))
            gen))

      (define (priority->generator rs args fail n)
         ;; → (priority . generator) | #false
         (λ (pri)
            (if pri
               (lets ((name priority pri))
                  (cond
                     ((equal? name "stdin")
                        ;; a generator to read data from stdin
                        ;; check n and preread if necessary
                        (if (first (λ (x) (equal? x "-")) args #false)
                           ;; "-" was given, so start stdin generator + possibly preread
                           (cons priority
                              (stdin-generator rs (eq? n 1)))
                           #false))
                     ((equal? name "file")
                        (let ((args (keep (λ (x) (not (equal? x "-"))) args)))
                           (if (null? args)
                              #false ; no samples given, don't start this one
                              (cons priority (file-streamer args)))))
                     ((equal? name "random")
                        (cons priority random-generator))
                     (else
                        (fail (list "Unknown data generator: " name)))))
               (fail "Bad generator priority"))))

      (define (generator-priorities->generator rs pris args fail n)
         (lets 
            ((gs (map (priority->generator rs args fail n) pris))
             (gs (keep self gs)))
            (cond
               ((null? gs) (fail "no generators"))
               ((null? (cdr gs)) (cdar gs))
               (else (mux-generators gs)))))

))
