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

;; todo: add different stream function applyers (1, at most n, reducing prob, ...)
;; todo: randomize stream fuzzer selection (needed especially when generating less files than there are stream fuzzers)
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


,require "stream-generators.l"
(import lib-stream-generators)

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
      ((rs seed (rnd rs #xffffffffffffffff))
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



;;; Load stream fuzzers

,require "stream-fuzzers.l"

(import lib-stream-fuzzers)


;;; Fork threads to initialize all the modules

; s r o → (ff of id → captured-state)
(define (start-fuzzer-modules samples rst opts)
   (fold
      (λ (out op)
         ;; for a thread to handle initialization and store the thread id (which is the op)
         (fork-linked-server op (λ () (op samples rst opts)))
         (wait 1)
         (put out op (catch-thread op)))
      False
      (foldr append null
         (list 
            stream-generators
            stream-fuzzers))))


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
     ;(continuous "-c" "--continuous" has-arg 
     ; comment "enable continuous generation (see man page for details)")
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
             (paths (if (null? paths) '("-") paths)) ;; read sample data from stdin if no samples given
				 (rs (seed->rands seed))
				 (rs paths (random-permutation rs paths))
             (paths (maybe-subset paths opts))
             (rsa rsb rsc (make-rand-streams rs))
             (init (start-fuzzer-modules paths rsa opts))
             (os (start-outputs opts paths))
             (ss ms fs init ;; allow some extra initialization time
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

