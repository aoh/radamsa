;;;
;;; Radamsa - a general purpose test data generator
;;;

;; test data generator

(define rval-failed-to-start-modules 1)
(define rval-crashed 127) ;; in-owl thread crash

(define (enumr n)
   (pair 
      (cons
         (+ n 100000000)
         (list (list->byte-vector (cons 42 (render render n '(10))))))
   (enumr (+ n 1))))

(define gen-test (enumr 1))

;; test telomerase - add a note

(define (telomerase ll muta) 
   (cons (list->vector (string->list "Telomerase was here: ")) ll))

(define tel-test
   (let loop () (pair (cons "test-telomerase" telomerase) (loop))))

(define test-fuzzs
   (let loop ()
      (pair 
         (cons 'test-fuzzer (λ (a b) a)) (loop))))

(define (car> a b) (> (car a) (car b)))

;; convert content generators and changers to a stream of data streams

(define (master-telomerase opts)
   (define (gen gens muxs fuzzs tels)
      (lets
         ((g-node gens (uncons gens F))
          (m-node fuzzs (uncons fuzzs F))
          (t-node tels (uncons tels F))
          (case-info 
            (foldr (λ (x tl) (render render x tl)) null (list (car g-node) "/" (car t-node) "/" (car m-node))))
          (case-stream
            (λ ()
               ((cdr t-node) (cdr g-node) (cdr m-node)))))
         (pair
            (cons case-info case-stream)
            (gen gens muxs fuzzs tels))))
   gen)

(define (get-stdout-fd n)
   (pair 
      (cons
         (list->string (foldr (λ (x tl) (render render x tl)) null '(list "stdout-" n)))
         stdout)
      (get-stdout-fd (+ n 1))))

;; force and send a stream to a port
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

;; info is ((n-bytes-written . printable-metadata) ...)
(define (writer fds lls record-meta n)
   (let loop ((lls lls) (fds fds) (n n))
      (if (= n 0)
         (values lls fds)
         (lets 
            ((out fds (uncons fds F))
             (name fd out) ;; where to write, name = path, ip etc
             (node lls (uncons lls F)) ;; what to write
             (info data node))
            (if (not fd)
               (error "Failed to write: " name))
            (let ((written (send-stream data fd)))
               (record-meta info written)
               (loop lls fds (- n 1)))))))

(define (record-meta/stdout bytes len)
   (print* (list " → " (list->string bytes) ": " len)))

(define test-lls 
   ((master-telomerase 'no-opts-yet)
      gen-test
      'unusedatm
      test-fuzzs
      tel-test))

;; startup 
; grab sample paths (add implicitly - if none)
; make output stream (add implicitly -o - if none)
; get random seed (given | /dev/urandom | time)
; make lls, fuzs, mixs etc by
;  - walk over enabled ones
;  - find entry function from global
;  - apply entry func to seed x args x samples → node stream
;  - add priorities of each based on cmd line args 
;  - do a probabilistic stream merge : rs (... (pri thing ...) ...) → (thing (prob-merge rs' ... (pri ...) ...))
;     + simple version: count sum of priorities, sort by them, make rand in range and see where it hits
;     + can do better with a heap
;  - return the enabled ones

(import lib-args)

(define command-line-rule-exp
   `((help "-h" "--help")
     (output "-o" "--output" has-arg default "-"
         comment "Where to write the generated data?")
     (count "-n" "--count" cook ,string->integer check ,(λ (x) (> x 0))
         default "1" comment "How many outputs to generate?")
     (seed "-s" "--seed" has-arg comment "Random seed (any string). Default random.")
     (enable "-e" "--enable" has-arg default "everything" 
         comment "Which modules to run? (pass help to list options)")
     (seek "-S" "--seek" has-arg cook ,string->integer check ,(λ (x) (> x 0))
         comment "Skip start from nth file.")
     (blank "-T" "--tabula-rasa" comment "Do not read any sample data.")))

(define command-line-rules
   (cl-rules command-line-rule-exp))

(define usage-text "Usage: radamsa [option] [sample-path] ...")

(define (print-usage-text)
   (print usage-text)
   (print-rules command-line-rules))

;; () → string
(define (urandom-seed)
   (let ((fd (open-input-file "/dev/urandom"))) ;; False if not there
      (if fd
         (let ((data (interact fd 8)))
            (close-port fd)
            (if (vector? data)
               (list->string
                  (render render (fold (λ (n d) (+ d (<< n 8))) 0 data) null))
               False))
         False)))

;; () → string (decimal number)
(define (time-seed)
   (list->string (render render (time-ms) null)))

;; get a seed if none was given explicitly (use /dev/urandom or current time in ms)
(define (add-seed opts)
   (put opts 'seed
      (or (urandom-seed) (time-seed))))

;; str → integer
(define (string->seed str)
   (if (string? str)
      (fold (λ (seed digit) (* (+ digit 1) (+ seed 1))) 1 (str-iter str))
      (error "bad seed: " str)))

;; str → str | (name . priority)
(define (try-split-priority str)
   (lets ((parts (c/=/ str)))
      (cond
         ((= (length parts) 1)
            ;; no priority given. default to 1 (lowest).
            (cons (car parts) 1))
         ((= (length parts) 2)
            (let ((pri (string->integer (cadr parts))))
               (if (and pri (> pri 0))
                  (cons (car parts) pri)
                  str)))
         (else str))))

(define (start-modules pris opts paths)
   (show "would start modules " pris)
   pris)

;; later: all-fu*-cut

(define (enable-modules selection opts paths)
   (lets
      ((mods (keep (λ (x) (> (string-length x) 0)) (c/,/ selection))) ;; split
       (pris (map try-split-priority mods))
       (bads (keep string? pris)))
      (if (null? bads)
         (start-modules pris opts paths)
         (begin
            (for-each (λ (x) (show "Bad module selection: " x)) bads)
            (print "Modules selection is a comma-separated list of module names optionally followed by weights.")
            (print "For example -e foo,bar,baz=2 will use baz about as often as foo and bar combined.")
            (print "Use $ radamsa -e help to list options")
            False))))

(define ll-fuzzers   
   (cons
      (cons "foo"
         (λ (rs paths opts)
            (print "Fuzzer foo started")
            (λ (ll) (cons (vector 70 79 79) ll))))
      (cons "bar"
         (λ (rs paths opts)
            (print "Fuzzer bar started")
            (λ (ll) (cons (vector 66 65 82) ll))))))


; node : rs paths opts → ((info . value) ...)

(define max-seed #x1000000000000)

(define (pop-at opts n) ; (... (pri . ((info . val) ...)) ...) n → matching (info . val) x (... (pri . ...) ...)
   (lets
      ((node (car opts))
       (pri ll node))
      (if (< n pri)
         (lets ((next ll (uncons ll F)))
            (values next (cons (cons pri ll) (cdr opts))))
         (lets ((grabbed opts (pop-at opts (- n pri))))
            (values grabbed (cons node opts))))))

(define (mux rs opts lim)
   (lets 
      ((rs n (rand rs lim))
       (val opts (pop-at opts n)))
      (pair val (mux rs opts lim))))

;; mux streams probabilistically according to priorities
(define (make-muxer rs opts)
   (lets
      ((rs seed (rand rs max-seed))
       (opts (sort car> opts))
       (total (fold + 0 (map car opts))))
      (values rs (mux (seed->rands seed) opts total))))

(define (find-module mods name)
   (cond
      ((null? mods) F)
      ((equal? (caar mods) name) (cdar mods))
      (else (find-module (cdr mods) name))))

;; start node streams, each with their own rs, and mux them according to priorities
; → rs' ((info . val) ...)
(define (start rs mods pris paths opts)
   (let loop ((rs rs) (pris pris) (out null))
      (cond
         ((null? pris)
            (make-muxer rs opts))
         ((find-module mods (caar pris)) =>
            (λ (modfn)
               (lets ((rs seed (rand rs max-seed)))
                  (loop rs (cdr pris) 
                     (cons
                        (cons (cdar pris) (modfn (seed->rands seed) paths opts))
                        out)))))
         (else
            (error "I don't have a module called " (caar pris))))))

(define (start-radamsa opts paths)
   (cond
      ((get opts 'help F)
         (print-usage-text)
         0)
      ((not (get opts 'seed F))
         ;; make a time-based seed unless one is given explicitly
         (start-radamsa (add-seed opts) paths))
      ((enable-modules (get opts 'enable F) opts paths) =>
         (λ (enabled) ; <- ((str . priority) ...), strs may be anything
            (show "ENABLED: " enabled) 
            (lets
               ((seed (string->seed (get opts 'seed F)))
                (paths ;; add stdin as a sample if no files given and no blank requested
                  (if (and (null? paths) (not (get opts 'blank F)))
                     '("-") paths))
                (rs (seed->rands seed))
                (offset (get opts 'seek 0))
                (lls (drop test-lls offset))
                (fds (drop (get-stdout-fd 0) offset))
                (n (get opts 'count F))
                (lls fds (writer fds lls record-meta/stdout n)))
               0)))
      (else rval-failed-to-start-modules)))

(define (toy-meta)
   (bind (wait-mail)
      (λ (from msg)
         (mail from 'anon)
         (toy-meta))))

(λ (args)
   (fork-server 'meta toy-meta)
   (fork-linked-server 'radamsa
      (λ () (process-arguments (cdr args) command-line-rules usage-text start-radamsa)))
   (let ((res (wait-mail)))
      (exit-owl
         (tuple-case res
            ((radamsa msg)
               (if (and (tuple? msg) (eq? 'finished (ref msg 1)))
                  (ref msg 2) ;; normal radamsa thread exit, return the exit value
                  (begin
                     (show "error: " msg)
                     rval-crashed)))
            (else rval-crashed))))) ;; some other thread is talking to me. equally bad.
