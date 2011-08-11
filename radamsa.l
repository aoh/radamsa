;;;
;;; Radamsa - a general purpose test data generator
;;;
;
; Copyright (c) 2009-2011 Aki Helin
;
; Permission is hereby granted, free of charge, to any person obtaining a
; copy of this software and associated documentation files (the "Software"),
; to deal in the Software without restriction, including without limitation
; the rights to use, copy, modify, merge, publish, distribute, sublicense,
; and/or sell copies of the Software, and to permit persons to whom the
; Software is furnished to do so, subject to the following conditions
;
; The above copyright notice and this permission notice shall be included
; in all copies or substantial portions of the Software.
;
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
; DEALINGS IN THE SOFTWARE.
;

;; todo: randomize stream fuzzer selection (needed especially when generating less files than there are stream fuzzers)
;; todo: randomize block sizes
;; todo: add different mutators (1, at most n, reducing, ...)
;; todo: add info about used streams and fuzzers to to %i
;; todo: have fuzzers adjust their priorities (and whether they run at all) by looking at the generic and their own opts
;; todo: have the stream reader merge partial reads in order to be deterministic when given the same data and seed in different sized chunks
;; todo: output verbosity as in radamsa, but be quiet by default and have multiple -v:s to increase verbosity
;; todo: add progress meters to stream writer (n written, rough kb/s)
;; todo: have the stream writer sync the writes every n blocks (or bytes) to avoid generating all of the data to memory when it is being consumed slower than generated (pipe, tcp connection etc)
;; todo: have the stream writer stop writing on first error and just fast forward the stream 

;; stream fuzzers: rjump, sjump, permu, cutup, drop, permute, ...

;;;
;;; Static parameters
;;;

(define version-str "Radamsa v0.2c")

(define *block-size* (* 1024 4)) ;; input block size, usually also the block size being written at each step

;;
;;; Dependencies
;;;

(import lib-args)



;;;
;;; Utils
;;;

;; str → n | False, n > 0
(define (string->natural x)
   (let ((num (string->integer x)))
      (cond
         ((not num) False)
         ((< num 0) False)
         (else num))))

;; str → integer
(define (string->seed str)
   (if (string? str)
      (fold (λ (seed digit) (* (+ digit 1) (+ seed 1))) 1 (str-iter str))
      (error "bad seed: " str)))

(define (fatal-error reason info exit-val)
   (show reason info)
   (exit-owl exit-val))

;; fixme: add a custom bvec-ranges
;; bvec-range      start end → bvec' | False
;; bvec-range-list start end → (byte ..) | False

(define blank-bvec (vector))

(define (render-ip-address bvec)
   (runes->string
      (foldr 
         (λ (byte tl) 
            (render render byte (if (null? tl) tl (cons 46 tl))))
         null bvec)))



;;;
;;; Stream generators
;;;

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
               (pair
                  (repeat-blocks 
                     (list->byte-vector (repeat base *block-size*))
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

;; a silly io issue, ports read n+1 bytes to be able to read the full 65536 with max fixnum arguemnt 65535
(define sp (- *block-size* 1))

;; bvec|F bvec → bvec
(define (merge head tail)
   (if head
      (list->vector (vec-foldr cons (vec-foldr cons null tail) head))
      tail))

;; stream data from port as *block-size* sized blocks and close it at eof
(define (stream-blocks port)
   (let loop ((last F) (wanted sp))
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
               ;; last and this have *block-size* bytes
               (let ((block (merge last block)))
                  (pair block (loop F sp))))
            (else
               (loop (merge last block) (- wanted (sizeb block))))))))

(define (open-sample-port path)
   (cond
      ((equal? path "-") stdin)
      (else (open-input-file path))))

;; todo: make the first block a special case?
(define (stream-sample path)
   (let ((port (open-sample-port path)))
      (if port
         (stream-blocks port) ; <- use also for stdin streaming
         null))) ; <- report to stderr later

(define (sample-stream paths)
   (let loop ((ps paths))
      (if (null? ps)
         (loop paths)
         (pair (stream-sample (car ps)) (loop (cdr ps))))))

(define (sgen-sample s r o)
   (if (null? s)
      False
      (tuple 'streamer
         (tuple 0 1 
            (list->ff '((fuzz . yes) (name . "sample")))
            (sample-stream s)))))



;;;
;;; Fuzzer modules
;;;

(define (car> a b) (> (car a) (car b)))

; (priority . fuzzer-fn) (total-priority (pri . fuzzer) ...) → (total' (pri . fuzzer) ...)
(define (add-fuzzer fuzzer lst)
   (cond
      ((null? lst)
         (add-fuzzer fuzzer (list 0)))
      ((not (and (pair? fuzzer) (number? (car fuzzer)) (function? (cdr fuzzer))))
         (error "unexpected fuzzer result: " fuzzer))
      (else
         (cons (+ (car fuzzer) (car lst))
            (sort car> (cons fuzzer (cdr lst)))))))


;; todo: keep a lookahead of 1 block and make sure at least one mutation is done at at the end
;; todo: allow varying ip and related things (borwnian ip min baseline, etc)

(define (stream-walker rs ll fn ip muta?)
   (cond
      ((pair? ll) 
         (if muta?
            ;; something already mutated -> follow probabilities
            (lets ((rs n (rnd rs ip))) 
               (if (eq? n 0) ;; mutation occurs with prob 1/ip 
                  (lets ((rs ll (fn rs ll)))
                     (stream-walker rs ll fn (+ ip 1) T)) ;; decrease mutation probability whenever it happens
                  (pair (car ll)
                     (stream-walker rs (cdr ll) fn ip muta?))))
            ;; no mutations yet -> check that more data follows or mutate this
            (lets 
               ((a ll ll)
                (b ll (uncons ll F)))
               (if b
                  ;; more data follows, continue (prefix unchanged so far)
                  (pair a (stream-walker rs (cons b ll) fn ip F))
                  ;; force one mutation to last block if no changes so far
                  (lets ((rs ll (fn rs (list a))))
                     (stream-walker rs ll fn ip T))))))
      ((null? ll) 
         ll)
      (else 
         (stream-walker rs (ll) fn ip muta?))))
         
;; start apply the fuzzer functoin to stream at random places lazily
(define (attach-fuzzer rs str f)
   (lets ;; reseed the fuzzer using the old one
      ((rs seed (rnd rs 10000000000000))
       (rs (seed->rands seed))
       (rs iprob (rnd rs 10))) ;; <- use rnd 10 and rnd-nbit here (should be independent of stream length, which is not always known)
      (stream-walker rs str f iprob F)))

(define (pick-fuzzer l n)
   (if (null? l) 
      (λ (rs ll) (values rs ll)) ;; identity fuzzer in case we have none to apply
      (let ((n (- n (caar l))))
         (if (< n 0)
            (cdar l)
            (pick-fuzzer (cdr l) n)))))

;; note, fs may be blank (= (0)) here
(define (choose-fuzzer rs fs)
   (lets ((rs n (rnd rs (car fs))))
      (values rs (pick-fuzzer (cdr fs) n))))

(define (attach-n-fuzzers fs rs data n)
   (if (= n 0)
      data
      (lets ((rs f (choose-fuzzer rs fs)))
         (attach-n-fuzzers fs rs 
            (attach-fuzzer rs data f)
            (- n 1)))))

(define (n-fuzzers rs)
   (let loop ((rs rs) (n 0))
      (lets ((rs x (rnd rs 2)))
         (if (eq? x 0)
            (loop rs (+ n 1))
            (values rs n)))))

; fuzzers = fs = opt arg → result
;  opt = 'add <fuzzer> → fs' 
;      = 'fuzz <stream> → fs' <stream>'

;; a function (usually carried in fs parameter) which has the stream fuzzers
(define (make-fuzzer-store rs)
   (let loop ((fs (list 0)) (rs rs))
      (λ (opt arg)
         (cond
            ((eq? opt 'add) ;; include a new fuzzer
               ;(show " *** adding fuzzer " arg)
               (loop (add-fuzzer arg fs) rs))
            ((eq? opt 'fuzz)
               (lets 
                  ((opts data arg)
                   (rs nfuzzers
                     (case (get opts 'fuzz 'maybe)
                        ((yes) (lets ((rs n (n-fuzzers rs))) (values rs (max n 1))))
                        ((no) (values rs 0))
                        (else (n-fuzzers rs)))))
                  ;; pick some fuzzers and attach them to the data stream
                  ;(show nfuzzers " fuzzers")
                  (values (loop fs rs)
                     (tuple opts ;; <- add info about which fuzzers are active later
                        (attach-n-fuzzers fs rs data nfuzzers)))))
            (else
               (error "fuzzers got funny op " opt))))))
              

;;;
;;; Stream fuzzers
;;;

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
   (lets ((rs n (rnd rs 10)))
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
               ((rs n (rnd rs 129))
                (rs n (rnd-log rs n)))
               (values rs (+ num n)))))))

(define (mutate-a-num rs lst nfound)
   (if (null? lst)
      (lets ((rs which (rnd rs nfound)))
         ;; choose which to mutate (or 0 if none)
         (values rs which null))
      (lets ((valp lstp (get-num lst)))
         (if valp
            (lets ((rs which tail (mutate-a-num rs lstp (+ nfound 1))))
               (if (eq? which 0) ;; i won, insert 42
                  (lets ((rs new (mutate-num rs valp)))
                     (values rs -1 (render render new tail)))
                  (values rs (- which 1)
                     (copy-range lst lstp tail))))
            (lets ((rs which tail (mutate-a-num rs (cdr lst) nfound)))
               (values rs which (cons (car lst) tail)))))))

'(lets 
   ((rs pos lst 
      (mutate-a-num (seed->rands (expt (time-ms) 3)) 
         (string->list " 11 22")
         0)))
   (print (list->string lst)))

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
            (lets ((rs p (rnd rs (sizeb (car str)))))
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
               ((rs p (rnd rs (sizeb (car str))))
                (rs bytes (rnd-elem rs funny-unicode)))
               (values rs
                  (cons
                     (edit-byte-vector (car str) p (λ (old tl) (append bytes (cons old tl))))
                     (cdr str))))))))

(define (sfuzz-bit s r o)
   (tuple 'fuzzer 
      (cons 1 
         (λ (rs str) 
            (lets 
               ((rs p (rnd rs (sizeb (car str))))
                (rs b (rnd rs 8)))
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
               ((rs p (rnd rs (sizeb (car str))))
                (rs b (rnd rs 8)))
               (values rs
                  (cons 
                     (edit-byte-vector (car str) p (λ (old tl) tl))
                     (cdr str))))))))

(define (sfuzz-inc-byte s r o)
   (tuple 'fuzzer 
      (cons 1 
         (λ (rs str) 
            (lets ((rs p (rnd rs (sizeb (car str)))))
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
                  ((rs start (rnd-range rs 0 (- (sizeb (car str)) 2)))
                   (rs end (rnd-range rs (+ start 1) (sizeb (car str))))
                   (pre (map (λ (p) (refb (car str) p)) (iota 0 1 start)))
                   (post (map (λ (p) (refb (car str) p)) (iota end 1 (sizeb (car str)))))
                   (stut (list->byte-vector (map (λ (p) (refb (car str) p)) (iota start 1 end))))
                   (rs n (rnd-log rs 10)) ; max 2^10 = 1024 stuts
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
               ((rs p (rnd rs (sizeb (car str))))
                (rs delta (rnd rs 256)))
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
                (rs a (rnd rs (sizeb block)))
                (rs b (rnd rs (sizeb block)))
                (as (map (λ (p) (refb block p)) (iota 0 1 a)))
                (bs (map (λ (p) (refb block p)) (iota b 1 (sizeb block)))))
               (values rs
                  (cons-bytes as (cons-bytes bs (cdr str)))))))))

; s r o → (ff of id → captured-state)
(define (start-fuzzer-modules samples rst opts)
   (fold
      (λ (out op)
         ;; for a thread to handle initialization and store the thread id (which is the op)
         (fork-linked-server op (λ () (op samples rst opts)))
         (wait 1)
         (put out op (catch-thread op)))
      False
      (list
         ;; stream generators
         sgen-sample      ;; read sample files and stream them out as such with varying block sizes (requires fuzzing)
         sgen-rrep
         sgen-flat
         sgen-enumerate
         sgen-step
         sgen-random
         
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


;;;
;;; Radamsa
;;;

;; compute and send a stream to a port
(define (mail-blocks str port n)
   (cond
      ((pair? str)
         (lets 
            ((block str str)
             (size (sizeb block)))
            (if (not (eq? size 0))
               (mail port block))
            (mail-blocks str port (+ n size))))
      ((null? str) n)
      (else (mail-blocks (str) port n))))

(define (send-stream data port)
   (let ((res (mail-blocks data port 0)))
      ((if (eq? port stdout) flush-port close-port) port)
      res))

;; insert stream to correct position (insertion sort with first and second keys (priority affects when equal scores))
(define (insert ss str)
   (cond 
      ((null? ss) (list str))
      ((< (ref str 1) (ref (car ss) 1))
         (cons str ss))
      ((and (= (ref str 1) (ref (car ss) 1))
         (<= (ref str 2) (ref (car ss) 2)))
         (cons str ss))
      (else
         (cons (car ss)
            (insert (cdr ss) str)))))

;; (stream ..) → (opts . data) (stream' ...)
(define (choose-stream ss) 
   ;(show "choosing stream " ss)
   (lets
      ((stream (car ss)) ;; sorted
       (score rpri opts strs stream)
       (str strs (uncons strs null)))
      (values
         (insert (cdr ss) (tuple (+ score rpri) rpri opts strs))
         (cons opts str))))

(define (choose-muxers ss ms base) 
   (values ms base))

;; could be called mutators, but that might be too easy to confuse with muxers
;; build a single mutator function, or have them work via a single proxy function?
(define (choose-fuzzers fs base)
   (show " - fuzzers are " fs)
   (values fs base))

;; s m f → s' m' f' data-stream
(define (generate-stream ss ms fs)
   (lets
      ((ss base (choose-stream ss))            ;; (opts . data)
       (ms muxed (choose-muxers ss ms base))
       (fs output (fs 'fuzz muxed)))
      (values ss ms fs output)))

;; todo: check what is caught if if the thread has already finished (ok to be False if they are not started)

(define (catch-threads ff)
   (ff-fold
      (λ (ff k _)
         (fupd ff k (catch-thread k)))
      ff ff))

;; let the initializing threads outside to play for a while
(define (step-threads ss ms fs init rounds)
   (if init
      (begin
         ;; release threads to run
         (ff-fold 
            (λ (_ id thread) 
               ;(show " - releasing " id)
               ;(show "   + state " thread)
               (if thread  ; <- in case the thread has terminated before it was caucth
                  (release-thread thread)))
            42 init)
         ;; let them run
         (wait rounds)
         ;; grab the results
         (let loop ((ss ss) (ms ms) (fs fs) (init init))
            (let ((env ((if (null? ss) wait-mail check-mail))))
               (if env
                  (lets ((from msg env))
                     ;(show "radamsa: mail from " from)
                     (tuple-case msg
                        ((finished result x y)
                           ;; thread is done
                           (let ((init (del init from)))
                              (if result
                                 (tuple-case result
                                    ((streamer node) (loop (insert ss node) ms fs init))
                                    ((muxer node) (loop ss (cons node ms) fs init))
                                    ((fuzzer node) (loop ss ms (fs 'add node) init))
                                    (else (error "strange thread message " msg)))
                                 ;; voluntary shutdown
                                 (loop ss ms fs init))))
                        ((crashed a b c)
                           (error "thread crash " (list a b c)))
                        (else
                           (error "bad thread mail " msg))))
                  (let ((init (catch-threads init)))
                     (values ss ms fs init))))))
      (values ss ms fs init)))
      
      
(define (writer-stdout nth)
   (λ (info signal)
      ;; info in signal can be used for verbosity, path etc
      (send-stream signal stdout)))

;; todo: really a fold over os
(define (radamsa ss ms fs init n os)
   (lets ((writer os (uncons os F)))
      (if writer
         (lets
            ((ss ms fs init (step-threads ss ms fs init 100))
             (ss ms fs data (generate-stream ss ms fs)))
            (writer (ref data 1) (ref data 2))
            (radamsa ss ms fs init (+ n 1) os))
         'ok))) ;; out of os

;; rs → 3 separate rand streams
(define (make-rand-streams rs)
   (lets
      ((rs sa (rnd-nbit rs 128))
       (rs sb (rnd-nbit rs 128))
       (rs sc (rnd-nbit rs 128)))
      (values (seed->rands sa) (seed->rands sb) (seed->rands sc))))

(define (file-writer mkpath)
   (λ (nth)
      (λ (info signal)
         (let ((path (mkpath nth info signal)))
            (if path
               (let ((port (open-output-file path)))
                  (if (not port) ;; exit run
                     (fatal-error "Cannot write to " path 1))
                  (for-each display (list " - " path ": "))
                  (let ((n-written (send-stream signal port)))
                     (if n-written
                        (show n-written "b")
                        (print "x_X"))))
               (show " - o_O " nth))))))

(define (get-tcp-connection sock)
   (λ (n)
      (λ (info signal)
         (let ((cli (interact sock 'accept)))
            (if cli
               (begin
                  ;; show unformatted ip address
                  (show " tcp <- " (render-ip-address (ref (interact cli 'info) 2)))
                  (send-stream signal cli))
               False)))))
      
      
;; command line string → (nth-file info signal → path)
(define (output-path-maker str)
   (λ (nth info signal)
      (call/cc
         (λ (ret)
            (runes->string
               (str-foldr
                  (λ (char tl)
                     (cond
                        ((null? tl) 
                           (cons char tl))
                        ((eq? char 37)
                           (case (car tl)
                              ((110) (render render nth (cdr tl))) ; %n
                              ((102) (render render (get info 'name "anonimasu") (cdr tl))) ; %f
                              (else (ret False))))
                        (else (cons char tl))))
                  null str))))))

(define (in-range? min max)
   (λ (x) (and (<= min x) (<= x max))))

(define (make-tcp-connection ip port)
   (λ (n)
      (λ (info signal)
         (let loop ((conn (open-connection ip port)))
            (if conn
               (begin
                  (show " tcp -> " (render-ip-address (ref (ref (interact conn 'info) 2) 2)))
                  (send-stream signal conn))
               (begin
                  ;; fixme: fix sleep!
                  (! 1000) 
                  (loop (open-connection ip port))))))))

(define (open-generic-stream str opts)
   (let ((n (get opts 'count 1)))
      (cond
         ((equal? str "-")
            (lmap writer-stdout
               (liota 1 1 (+ n 1))))
         ((/^:[0-9]+$/ str)
            (let ((port (string->integer (s/^:// str))))
               (if (and port (>= port 0) (< port 65536))
                  (let ((socket (open-socket port)))
                     (if socket
                        (lmap (get-tcp-connection socket) (liota 1 1 (+ n 1)))
                        (begin
                           (show " - failed to open port " port)
                           null)))
                  (begin
                     (show " - bad port " port)
                     null))))
         ((/^[0-9]{1,3}(\.[0-9]{1,3}){3}:[0-9]+$/ str)
            (lets
               ((ip-port (c/:/ str))
                (port (string->integer (cadr ip-port)))
                (addr (map string->integer (c/\./ (car ip-port)))))
               (cond
                  ((not (all (in-range? 0 255) addr))
                     (show "bad ip address: " addr)
                     null)
                  ((not ((in-range? 0 65535) port))
                     (show "bad port: " port))
                  (else
                     (lmap (make-tcp-connection (list->vector addr) port)
                        (liota 1 1 (+ n 1)))))))
         (else
            (let ((pat (output-path-maker str)))
               (if pat
                  (lmap (file-writer pat) (liota 1 1 (+ n 1)))
                  (begin
                     (show "bad output pattern: " str)
                     null)))))))

(define (start-outputs opts paths)
   (lets
      ((out-str (get opts 'output "-"))
       (outs (c/,/ out-str))
       (count  (get opts 'count 1)))
      (if (= (length outs) 1)
         (open-generic-stream (car outs) opts)
         (begin
            (show "no multiple outputs yet in radamsa 0.2. requested " outs)
            null))))


;;;
;;; Command line stuff
;;;

(define usage-text "Usage: radamsa [args] -o <where> [sample] ...")

(define what-are-radamsa

"Radamsa -- a semi-intelligent sample-based black-box fuzzer.
Copyright (c) 2009-2011 Aki Helin

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the \"Software\"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.

This program is being written as part of the Protos Genome Project at Oulu
University Secure Programming Group (OUSPG).

Report bugs and other worries to ouspg@ee.oulu.fi or add an issue to:
  http://code.google.com/p/ouspg/issues/list.

More information is available at:
  http://code.google.com/p/ouspg/wiki/Radamsa
")

(define command-line-rule-exp
   `((help "-h" "--help")
     (output "-o" "--output" has-arg default "-" 
         comment "where to put the data (default stdout)")
     (count "-n" "--count" cook ,string->natural check ,(λ (x) (> x 0))
         default "1" comment "how many cases to generate")
     (seed "-s" "--seed" has-arg comment "random seed")
     (about "-A" "--about")
     (version "-V" "--version")
     (continuous "-c" "--continuous" has-arg 
      comment "enable continuous generation (see man page for details)")
     (subset "-S" "--subset" cook ,string->natural 
         comment "take a random subset of n samples")
     (verbose "-v" "--verbose")))
 
(define command-line-rules
   (cl-rules command-line-rule-exp))

(define (print-usage-text)
   (print usage-text)
   (print-rules command-line-rules)
   (print "Examples: None yet for 0.2 series. Sorry.
"))


;;;
;;; Startup
;;;

;; todo: could have a --seed-file [/dev/urandom]

;; () → string
(define (urandom-seed)
   (let ((fd (open-input-file "/dev/urandom"))) ;; False if not there
      (if fd
         (let ((data (interact fd 31)))
            (close-port fd)
            (if (vector? data)
               (list->string
                  (render render (fold (λ (n d) (+ d (<< n 8))) 0 data) null))
               False))
         False)))

;; () → string (decimal number)
(define (time-seed)
   (list->string (render render (time-ms) null)))

;; get a seed if none was given explicitly (use /dev/urandom or current clock)
(define (add-seed opts)
   (put opts 'seed 
      (or (urandom-seed) (time-seed))))

;; todo: only use sample streams when writing 1 file or using stdin as sample
;; todo: add using stdin as a sample 
;; todo: maybe force generating just one case when using stdin as a sample

(define (maybe-subset paths opts)
   (let ((n (get opts 'subset F)))
      (if n
         (let ((paths (take paths n)))
            ;(print " - chose subset ")
            ;(for-each print paths)
            paths)
         paths)))

(define (start-radamsa opts paths)
   (cond
      ((get opts 'help F) 
         (print-usage-text) 
         0)
      ((get opts 'about F) 
         (print "that") 
         0)
      ((get opts 'version F) 
         (print version-str) 
         0)
      ((not (get opts 'seed F)) 
         ;; make a time-based seed unless one is given explicitly
         (start-radamsa (add-seed opts) paths))
      (else
         (lets 
            ((seed (string->seed (get opts 'seed F)))
				 (rs (seed->rands seed))
				 (rs paths (random-permutation rs paths))
             (paths (maybe-subset paths opts))
             (rsa rsb rsc (make-rand-streams rs))
             (init (start-fuzzer-modules paths rsa opts))
             (os (start-outputs opts paths))
             (ss ms fs init ;; allow some exra initialization time
               (step-threads null null (make-fuzzer-store rsb) init 1000))
             (rval (radamsa ss ms fs init 1 os)))
            (if (eq? rval 'ok)
               0
               (begin
                  (show "nonzero exit: " rval)
                  rval))))))

;(start-radamsa False '())

(define (toy-meta)
   (bind (wait-mail)
      (λ (from msg)
         (mail from 'anon)
         (toy-meta))))

(λ (args) 
   (fork-linked-server 'radamsa 
      (λ () (process-arguments (cdr args) command-line-rules usage-text start-radamsa)))
   (fork-server 'meta toy-meta)
   (let ((res (wait-mail)))
      (exit-owl
         (tuple-case res
            ((radamsa msg)
               (if (and (tuple? msg) (eq? 'finished (ref msg 1)))
                  0 ;; normal exit
                  (begin
                     (show "error: " msg)
                     2))) ;; something funny (nothing more specific yet)
            (else 1))))) ;; something unexpected

