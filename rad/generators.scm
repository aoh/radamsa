;;;
;;; Data Generators
;;;

(define-library (rad generators)

   (import
      (owl base)
      (owl sys)
      (rad shared)
      (rad fuse)
      (only (owl primop) halt))

   (export 
      string->generator-priorities         ;; done at cl args parsing time
      generator-priorities->generator      ;; done after cl args
      )

   (begin

      (define (rand-block-size rs)
         (lets ((rs n (rand rs max-block-size)))
            (values rs (max n min-block-size))))

      ;; bvec|F bvec → bvec
      (define (merge head tail)
         (if head
            (list->vector (vec-foldr cons (vec-foldr cons null tail) head))
            tail))

      (define (finish rs len)
         null)

      ;; store length so that extra data can be generated in case of no or very 
      ;; little sample data, which would cause one or very few possible outputs

      (define (stream-port rs port)
         (lets ((rs first (rand-block-size rs)))
            (let loop ((rs rs) (last #false) (wanted first) (len 0)) ;; 0 = block ready (if any)
               (let ((block (get-block port wanted)))
                  (cond
                     ((eof? block) ;; end of stream
                        (if (not (eq? port stdin)) (fclose port))
                        (if last
                           (cons last (finish rs (+ len (sizeb last))))
                           (finish rs len)))
                     ((not block) ;; read error, could be treated as error
                        (if (not (eq? port stdin)) (fclose port))
                        (if last (list last) null))
                     ((eq? (sizeb block) wanted)
                        ;; a block of required (deterministic) size is ready
                        (lets
                           ((block (merge last block))
                            (rs next (rand-block-size rs)))
                           (pair block (loop rs #false next (+ len (sizeb block))))))
                     (else
                        (loop rs (merge last block)
                           (- wanted (sizeb block))
                           len)))))))

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
               (values rs ll (put empty 'generator 'stdin)))))

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
         (lets ((rs seed (rand rs #x1000000000000000000)))
            (values rs 
               (random-stream (seed->rands seed))
               (put empty 'generator 'random))))

      (define (fatal-read-error path)
         (if (dir->list path)
            (print*-to stderr (list "Error: failed to open '" path "'. Please use -r if you want to include samples from directories."))
            (print*-to stderr (list "Error: failed to open '" path "'")))
         (halt exit-read-error))
         
      ;; paths → (rs → rs' ll|#false meta|error-str)
      (define (file-streamer paths)
         (lets
            ((n (vec-len paths)))
            (define (gen rs)
               (lets
                  ((rs n (rand rs n))
                   (path (vec-ref paths n))
                   (port (open-input-file path)))
                  (if port
                     (lets ((rs ll (port->stream rs port)))
                        (values rs ll 
                           (list->ff (list '(generator . file) (cons 'source path)))))
                     (fatal-read-error path))))
            gen))

      (define (consume ll)
         (cond
            ((null? ll) ll)
            ((pair? ll) (consume (cdr ll)))
            (else (consume (ll)))))
         
      (define (walk-to-jump rs ip a as b bs)
         (lets
            ((finish
               (λ () 
                  (lets
                     ((a (vector->list a))
                      (b (vector->list b))
                      (rs ab (fuse rs a b)))
                     (consume as)
                     (cons (list->byte-vector ab) bs))))
             (rs n (rand rs ip))
             (ip (+ ip 1)))
             (if (eq? n 0)
               (finish)
               (lets ((aa as (uncons as #false)))
                  (if aa
                     (lets ((rs n (rand rs 3)))
                        (if (eq? n 0)
                           (cons a 
                              (walk-to-jump rs ip aa as b bs))
                           (lets ((bb bs (uncons bs #false)))
                              (if bb
                                 (cons a 
                                    (walk-to-jump rs ip aa as bb bs))
                                 (finish)))))
                     (finish))))))

      (define (jump-somewhere rs la lb)
         (lets ((a as (uncons la #false)))
            (if a
               (lets ((b bs (uncons lb #false)))
                  (if b
                     (walk-to-jump rs 3 a as b bs)
                     (cons a as)))
               lb)))

      (define (jump-streamer paths)
         (lets ((n (vec-len paths)))
            (define (gen rs)
               (lets
                  ((rs ap (rand rs n))
                   (rs bp (rand rs n))
                   (a (vec-ref paths ap))
                   (b (vec-ref paths bp))
                   (pa (open-input-file a))
                   (pb (open-input-file b)))
                  (cond
                     ((not pa) (fatal-read-error a))
                     ((not pb) (fatal-read-error b))
                     (else
                        (lets
                           ((rs lla (port->stream rs pa))
                            (rs llb (port->stream rs pb))
                            (rs seed (rand rs #xfffffffff)))
                           (values rs
                              (jump-somewhere (seed->rands seed) lla llb)
                              (list->ff
                                 (list 
                                    '(generator . jump)
                                    (cons 'head a)
                                    (cons 'tail b)))))))))
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
            (lets
               ((paths (keep (λ (x) (not (equal? x "-"))) args))
                (paths (if (null? paths) #false (list->vector paths))))
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
                           (if paths
                              (cons priority (file-streamer paths))
                              #false))
                        ((equal? name "jump")
                           (if paths
                              (cons priority
                                 (jump-streamer paths))
                              #false))
                        ((equal? name "random")
                           (cons priority random-generator))
                        (else
                           (fail (list "Unknown data generator: " name)))))
                  (fail "Bad generator priority")))))

      (define (generator-priorities->generator rs pris args fail n)
         (lets 
            ((gs (map (priority->generator rs args fail n) pris))
             (gs (keep self gs)))
            (cond
               ((null? gs) (fail "no generators"))
               ((null? (cdr gs)) (cdar gs))
               (else (mux-generators gs)))))

))
