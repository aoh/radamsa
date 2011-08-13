
(define-module lib-stream-generators

   (export stream-generators)

   ;;;
   ;;; Stream generators
   ;;;

   ;; sample stream parameters
   (define max-block-size 8192)
   (define min-block-size 4)

   ;; stream generators are things that create a lazy stream of data consisting of 
   ;; blocks of raw data. these are then possibly edited by the stream fuzzers and 
   ;; muxers to produce the final data.

   ; thread-init → #(streamer|muxer|fuzzer Node)
   ; streamer → Node = #(n-written/priority 1/priority Opts ((block ...) ...))
   ;            Opts = ff of option → value
   ;               options: fuzz = yes|no|maybe [maybe], allow attaching stream fuzzers and muxers to stream?
   ;                        name = <value>, fills the %s (if any) of output path

   ;; todo: some of the test generators make a byte stream instead of a bvec stream
   ;; todo: byte/bvec stream chunker
   ;; todo: s/muxer/global/, being a fuzzer that is always applied to the whole stream (or maybe merge with stream fuzzers and give different opts?)

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


   ;;; flat data stream

   (define (repeat thing n)
      (let loop ((n n) (out null))
         (if (eq? n 0)
            out
            (loop (- n 1) (cons thing out)))))

   (define (repeat-blocks pre n tail)
      (print (list 'repeat-blocks n))
      (show " pre " (sizeb pre))
      (show " tail " (sizeb tail))
      (if (= n 0)
         (list tail)
         (pair pre (repeat-blocks pre (- n 1) tail))))

   (define (rnd-length rs)
      (lets 
         (
          ; (rs nbits (rnd rs 20)) ;; max 1MB files
          (rs nbits (rnd rs 15))
          (rs n (rnd rs (<< 1 (max nbits 6)))))
         (values rs n)))

   (define (sgen-flat s r o)
      (tuple 'streamer 
         (tuple 0 50 
            (list->ff '((name . flat)))
            (let loop ((rs r))
               (lets 
                  ((rs base (rnd rs 256))
                   (rs len (rnd-length rs))
                   (blocks (div len *block-size*)))
                  (print (list 'loop 'base base 'len len 'blocks blocks))
                  (pair
                     (repeat-blocks 
                        (if (> blocks 0)
                           (list->byte-vector (repeat base *block-size*))
                           'nothing)
                        blocks
                        (list->byte-vector (repeat base (- len (* blocks *block-size*)))))
                     (loop rs)))))))

   (define (sgen-rrep s r o)
      (tuple 'streamer
         (tuple 0 50
            (list->ff '((name . rrep)))
            (let loop ((rs r))
               (lets
                  ((rs nalpha (rnd rs 1024)) ;; alphabet size
                   (rs letters (random-numbers rs 256 nalpha)) ;; alphabet (may have repetition)
                   (alpha (list->vector letters)) ;; index 0-(nalpha-1)
                   (rs len (rnd-log rs 10)) ;; block size to repeat
                   (rs poss (random-numbers rs nalpha len))
                   (bytes (map (λ (p) (vec-ref alpha p)) poss))
                   (block (list->byte-vector bytes))
                   (rs n (rnd-log rs 6))) ;; repeat block how many times
                  ;; ok to make in memory, because this is just ~3n+len/W words
                  (pair (map (λ (p) block) (iota 0 1 n)) (loop rs)))))))


   ;; prng stream generator

   (define (random-stream rs len n out)
      (cond
         ((eq? n *block-size*)
            (pair (list->byte-vector out)
               (random-stream rs len 0 null)))
         ((eq? len 0)
            (if (null? out)
               null
               (list (list->byte-vector out))))
         (else
            (lets ((rs b (rnd rs 256)))
               (random-stream rs (- len 1) (+ n 1) (cons b out))))))

   (define (sgen-random s r o)
      ;; todo: add more special random genearators separately (alphabet selection etc)
      (tuple 'streamer
         (tuple 0 50
            (list->ff '((name . random)))
            (let loop ((rs r))
               (lets 
                  ((rs seed (rnd rs 100000000000000000000000))
                   (rs len (rnd-length rs)))
                  (pair
                     (random-stream (seed->rands seed) len 0 null)
                     (loop rs)))))))


   ;; simple stepper

   (define (stepper-ll pos del step)
      (let loop ((n del))
         (if (eq? n 0)
            (pair pos (stepper-ll (band 255 (+ pos step)) del step))
            (cons pos (loop (- n 1))))))

   (define (stepper pos del step len)
      (let loop ((pos 0) (rout null) (ll (stepper-ll pos del step)))
         (cond
            ((= pos len)
               (list (list->byte-vector (reverse rout))))
            ((pair? ll)
               (if (eq? 0 (band pos 4095))
                  (pair (list->byte-vector (reverse rout))
                     (loop (+ pos 1) (list (car ll)) (cdr ll)))
                  (loop (+ pos 1) (cons (car ll) rout) (cdr ll))))
            (else (loop pos rout (ll))))))
            
   (define (sgen-step s r o)
      (tuple 'streamer 
         (tuple 0 40 
            (list->ff '((name . step)))
            (let loop ((rs r))
               (lets 
                  ((rs pos  (rnd rs 256))
                   (rs del  (rnd rs 256))
                   (rs del  (rnd rs (+ del 1)))
                   (rs del  (rnd rs (+ del 1)))
                   (rs step (rnd rs 256))
                   (rs len (rnd-length rs)))
                  (pair (stepper pos del step len) (loop rs)))))))


   ;;; byte sequence enumerator

   (define (incl l)
      (if (null? l) 
         (list 0)
         (let ((d (car l)))
            (if (eq? d 255)
               (cons 0 (incl (cdr l)))
               (cons (+ d 1) (cdr l))))))

   (define (sgen-enumerate s r o)
      (tuple 'streamer
         (tuple 0 30
            (list->ff '((fuzz . no) (name . enumerate))) ;; don't fuzz, because we want all unique byte sequences
            (let loop ((l null))
               (pair (list (list->byte-vector l)) 
                  (loop (incl l)))))))
      

   ;;; stream samples as such, requires fuzzing

   ;; bvec|F bvec → bvec
   (define (merge head tail)
      (if head
         (list->vector (vec-foldr cons (vec-foldr cons null tail) head))
         tail))

   (define limit (+ max-block-size 1))

   (define (rnd-block-size rs)
      (lets
         ((rs n (rnd rs limit))
          (n (max n 4)))
         (values rs n)))

   ;; stream data from port as *block-size* sized blocks and close it at eof
   (define (stream-blocks rs port)
      (lets ((rs first (rnd-block-size rs)))
         (let loop ((rs rs) (last F) (wanted first)) ;; 0 = block ready (if any)
            (let ((block (interact port wanted)))
               (cond
                  ((eof? block) ;; end of stream
                     (if (not (eq? port stdin)) (close-port port))
                     (if last (list last) null))
                  ((not block) ;; read error
                     ;; later carp via whatever logging system is used to stderr
                     (if (not (eq? port stdin)) (close-port port))
                     (if last (list last) null))
                  ((eq? (sizeb block) (+ wanted 1))
                     ;; a block of required (deterministic) size is ready
                     (lets
                        ((block (merge last block))
                         (rs next (rnd-block-size rs)))
                        (pair block (loop rs F next))))
                  (else
                     (loop rs (merge last block) (- wanted (sizeb block)))))))))

   (define (open-sample-port path)
      (cond
         ((equal? path "-") stdin)
         (else (open-input-file path))))

   ;; todo: make the first block a special case?
   (define (stream-sample rs path)
      (let ((port (open-sample-port path)))
         (if port
            (stream-blocks rs port) ; <- use also for stdin streaming
            null))) ; <- report to stderr later

   (define (sample-stream rs paths)
      (let loop ((rs rs) (ps paths))
         (if (null? ps)
            (loop rs paths)
            (lets ((rs seed (rnd rs #x100000000)))
               (pair 
                  (stream-sample (seed->rands seed) (car ps))
                  (loop rs (cdr ps)))))))

   (define (sgen-sample s r o)
      (if (null? s)
         False
         (tuple 'streamer
            (tuple 0 1 
               (list->ff '((fuzz . yes) (name . "sample")))
               (sample-stream r s)))))

   (define stream-generators
      (list
         sgen-sample      ;; read sample files and stream them out as such with varying block sizes (requires fuzzing)
         sgen-rrep
         ;sgen-flat
         sgen-enumerate
         sgen-step
         sgen-random
         )))
