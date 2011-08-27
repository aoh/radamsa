;;;
;;; Radamsa - a general purpose test data generator
;;;

;; todo: muxer and mutator polymerase could be merged to the muxer and mutator streams, since they are always used in pairs 
;; todo: muxers also need a polymerase
;; todo: node infos should be ffs of static values like name, description etc to allow something like $ radamsa --list-modules
;; todo: module startup should use (name-of node) instead of a separate string in the list

(define rval-failed-to-start-modules 1)
(define rval-crashed 127) ;; in-owl thread crash

(define (car> a b) (> (car a) (car b)))

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

(define (record-meta/stdout info len)
   (print* (list " → " info ": " len)))

(import lib-args)

(define command-line-rule-exp
   `((help "-h" "--help")
     (output "-o" "--output" has-arg default "-"
         comment "Where to write the generated data?")
     (count "-n" "--count" cook ,string->integer check ,(λ (x) (> x 0))
         default "1" comment "How many outputs to generate?")
     (seed "-s" "--seed" has-arg comment "Random seed (any string). Default random.")
     (fuzzers "-f" "--fuzzers" has-arg default "foo,bar" 
         comment "Which stream fuzzers to run? (pass help to list options)")
     (generators "-g" "--generators" has-arg default "xs,stars=2" 
         comment "Which stream generators to run? (pass help to list options)")
     (polymerase "-p" "--polymerase" has-arg default "pa,pb" 
         comment "Which stream mutator patterns to use? (pass help to list options)")
     (muxer-polymerase "-P" "--muxer-polymerase" has-arg default "drop" 
         comment "Which muxer mutator patterns to use? (pass help to list options)")
     (muxers "-m" "--muxers" has-arg default "unit" 
         comment "Which stream muxers to use? (pass help to list options)")
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
            (cons 1 (car parts)))
         ((= (length parts) 2)
            (let ((pri (string->integer (cadr parts))))
               (if (and pri (> pri 0))
                  (cons pri (car parts))
                  str)))
         (else str))))

;; later: all-fu*-cut

(define (enable-modules selection opts paths)
   (lets
      ((mods (keep (λ (x) (> (string-length x) 0)) (c/,/ selection))) ;; split
       (pris (map try-split-priority mods))
       (bads (keep string? pris)))
      (if (null? bads)
         pris
         (begin
            (for-each (λ (x) (show "Bad module selection: " x)) bads)
            (print "Modules selection is a comma-separated list of module names optionally followed by weights.")
            (print "For example -e foo,bar,baz=2 will use baz about as often as foo and bar combined.")
            (print "Use $ radamsa -e help to list options")
            False))))

(define (iterate thing)
   (pair thing (iterate thing)))

(define (simple-node name value)
   (let ((node (cons name value)))
      (iterate node)))

; (name . starter), starter : rs paths opts → ((info . func) ...)
(define ll-fuzzers   
   (list
      (cons "foo"
         (λ (rs paths opts)
            (print "Fuzzer foo started.")
            (simple-node "foo-fuzzer"
               (λ (ll) (cons (vector 70 79 79) ll)))))
      (cons "bar"
         (λ (rs paths opts)
            (print "Fuzzer bar started.")
            (simple-node "bar-fuzzer"
               (λ (ll) (cons (vector 66 65 82) ll)))))))

(define sample-gen-name "sample") ; <- named to be able to check for this as a special case (like stdin)

(define ll-generators
   (list
      (cons "stars"
         (λ (rs paths opts)
            (print "Generator stars started.")
            (simple-node "stars"
               (list (vector 42 42 42 42 42 42 42 42 42 42 42 42 42 42 42)))))
      (cons "xs"
         (λ (rs paths opts)
            (print "Generator xs started.")
            (simple-node "xs"
               (list (vector 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88)))))))

(define ll-polymerase 
   (list
      (cons "pa"
         (λ (rs paths opts)
            (print "Polymerase A started")
            (simple-node "pola"
               (λ (ll mutagen)
                  (cons (vector 80 111 108 65 58) ll)))))
      (cons "pb"
         (λ (rs paths opts)
            (print "Polymerase B started")
            (simple-node "polb"
               (λ (ll mutagen)
                  (cons (vector 80 111 108 66 58) ll)))))))

(define ll-muxer-polymerase 
   (list
      (cons "drop"
         (λ (rs paths opts)
            (print "Muxer polymerase drop started.")
            (simple-node "drop"
               (λ (all bll) all))))))

; muxer : ll ll → ll
(define ll-muxers
   (list
      (cons "unit" ;; always pick the first stream
         (λ (rs paths opts)
            (print "Unit muxer started")
            (simple-node "unit"
               (λ (a b) a))))))

;; master polymerase controls how many streams we'll use, and chooses how to mux n fuzz them together

;; grab some streams 
(define (choose-streams rs gens)
   (let loop ((rs rs) (gens gens) (taken null))
      (lets ((rs n (rand rs (* 2 (length taken)))))
         (if (eq? n 0)
            (lets ((this gens (uncons gens F)))
               (loop rs gens (cons this taken)))
            (values rs gens taken)))))

(define (plain-sample? node) 
   (string? (car node))) ;; todo: not really

(define (attach-fuzzer fuzs tels node)
   (lets
      ((m-node fuzs (uncons fuzs F))
       (t-node tels (uncons tels F)))
      (values fuzs tels
         (cons
            (list (car t-node) (car m-node) (car node)) ;; info
            (λ () ((cdr t-node) (cdr node) (cdr m-node))))))) ;; lazy data 

;; merger many data streams to one using the fuzzers and muxers
(define (master-merge rs muxs fuzs tels lls)
   (lets ((rs n (rand rs 5)))
      (cond
         ((eq? n 0) ;; permute
            (lets ((rs lls (random-permutation rs lls)))
               (master-merge rs muxs fuzs tels lls)))
         ((eq? n 1) ;; add a stream fuzzer to first
            (lets ((fuzs tels node (attach-fuzzer fuzs tels (car lls))))
               (master-merge rs muxs fuzs tels (cons node (cdr lls)))))
         ((eq? n 2) ;; merge two streams (if more than one)
            (if (not (null? (cdr lls)))
               (lets 
                  ((m-node muxs (uncons muxs F))
                   (m-info muxer m-node)
                   (a (car lls))
                   (b (cadr lls)))
                  (master-merge rs muxs fuzs tels
                     (cons
                        (cons (list m-info (car a) (car b)) 
                           (λ () (muxer (cdr a) (cdr b))))
                        (cddr lls))))
               (master-merge rs muxs fuzs tels lls)))
         ((and (null? (cdr lls)) (not (plain-sample? (car lls)))) ;; else try exit
            (values rs muxs fuzs tels (car lls)))
         (else ;; else keep muxin
            (master-merge rs muxs fuzs tels lls)))))



         
      

;; todo: needs to act differently if only one sample which is stdin, so some special handling will be needed
(define (master-polymerase rs gens muxs fuzzs tels)
   (lets
      ((rs gens tomux (choose-streams rs gens)) ;; choose streams to fuzz
       (rs muxs fuzs tels node
         (master-merge rs muxs fuzzs tels tomux))) ;; combine them to one stream
      (pair node  ;; <- data to generate
         (master-polymerase rs gens muxs fuzzs tels))))

; node : rs paths opts → ((info . value) ...)

(define max-seed #x1000000000000)

(define (pop-at opts n) ; (... (pri . ((info . val) ...)) ...) n → matching (info . val) x (... (pri . ...) ...)
   (lets
      ((node (car opts))
       (pri ll node))
      (if (< n pri)
         (lets ((next ll (uncons ll F)))
            (values next (cons (cons pri ll) (cdr opts))))
         (lets ((grabbed opts (pop-at (cdr opts) (- n pri))))
            (values grabbed (cons node opts))))))

(define (mux rs opts lim)
   (lets 
      ((rs n (rand rs lim))
       (val opts (pop-at opts n)))
      (pair val (mux rs opts lim))))

;; mux streams probabilistically according to priorities
(define (make-muxer rs opts)
   (if (null? (cdr opts))
      (values rs (cdar opts)) ;; no need to mux
      (lets
         ((rs seed (rand rs max-seed))
          (opts (sort car> opts))
          (total (fold + 0 (map car opts))))
         (values rs (mux (seed->rands seed) opts total)))))

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
            (make-muxer rs out))
         ((find-module mods (cdar pris)) =>
            (λ (modfn)
               (lets 
                  ((rs seed (rand rs max-seed))
                   (nodes (modfn (seed->rands seed) paths opts)))
                  (loop rs (cdr pris) (cons (cons (caar pris) nodes) out)))))
         (else
            (error "I don't have a module called " (caar pris))))))

(define (start-radamsa opts paths)
   (call/cc
      (λ (ret)
         (let ((fail (λ (why rval) (print why) (ret rval))))
            (cond
               ((get opts 'help F)
                  (print-usage-text)
                  0)
               ((not (get opts 'seed F))
                  ;; make a time-based seed unless one is given explicitly
                  (start-radamsa (add-seed opts) paths))
               (else
                  (lets
                     ((f-pris (enable-modules (get opts 'fuzzers F) opts paths)) ; <-- move fail cont here
                      (_ (or f-pris (fail "Bad fuzzers" 1)))                     ; -----^
                      (g-pris (enable-modules (get opts 'generators F) opts paths))
                      (_ (or g-pris (fail "Bad generators" 2)))
                      (p-pris (enable-modules (get opts 'polymerase F) opts paths))
                      (_ (or p-pris (fail "Bad transcribers" 3)))
                      (m-pris (enable-modules (get opts 'muxers F) opts paths))
                      (_ (or m-pris (fail "Bad muxers" 4)))
                      (mp-pris (enable-modules (get opts 'muxer-polymerase F) opts paths))
                      (_ (or mp-pris (fail "Bad muxers polymerase" 4)))
                      (seed (string->seed (get opts 'seed F)))
                      (paths ;; add stdin as a sample if no files given and no blank requested
                        (if (and (null? paths) (not (get opts 'blank F)))
                           '("-") paths))
                      (rs (seed->rands seed))
                      (offset (get opts 'seek 0))
                      (rs fuzs (start rs ll-fuzzers f-pris paths opts))
                      (rs gens (start rs ll-generators g-pris paths opts))
                      (rs pols (start rs ll-polymerase p-pris paths opts))
                      (rs muxs (start rs ll-muxers m-pris paths opts))
                      (rs mpol (start rs ll-muxer-polymerase mp-pris paths opts))
                      (lls (drop (master-polymerase rs gens muxs fuzs pols) offset))
                      (fds (drop (get-stdout-fd 0) offset))
                      (n (get opts 'count F))
                      (lls fds (writer fds lls record-meta/stdout n)))
                     0)))))))

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

