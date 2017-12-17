;;;
;;; Radamsa Output
;;;

(define-library (rad output)

   (import
      (owl base)
      (owl iff)
      (owl io)
      (only (owl primop) halt)
      (rad digest)
      (rad shared))

   (export
      output
      checksummer dummy-checksummer
      dummy-output        ;; construct, but don't write
      string->outputs)    ;; str num → ll of output functions | #false

   (begin

      ;; output :: (ll' ++ (#(rs mutator meta))) fd → rs mutator meta (n-written | #f), handles port closing &/| flushing
      (define (output-to-fd ll fd)
         (lets 
            ((ll n (blocks->port ll fd))
             (ok? (and (pair? ll) (tuple? (car ll)))) ;; all written?
             (lst (force-ll ll))
             (state (llast ll))
             (rs muta meta state))
            (if (not (eq? fd stdout))
               (close-port fd))
            ;; could warn about write errors
            (values rs muta meta n)))

      (define (output ll x)
         (tuple-case x
            ((udp ip port sock)
               (lets 
                  ((rlst (reverse (force-ll ll)))
                   (state (car rlst))
                   ;(real-data (foldr append null (map vector->list (keep vector? (force-ll ll)))))
                   (len (foldr + 0 (map vector-length (cdr rlst))))
                   (data 
                      (fold ;; left to right fold, because reverse
                         (λ (tail block)
                            (vec-foldr cons tail block))
                         null (cdr rlst)))
                   (rs muta meta state))
                  ;(if (not (equal? data real-data)) (error "data mismatch"))
                  (if (> len 65535)
                     ;; definitely too large for a single packet
                     (values rs muta meta 0)
                     (lets ((buffer (list->byte-vector data)))
                        (cond
                           ((send-udp-packet sock ip port buffer)
                              (values rs muta meta 
                                 (vector-length buffer)))
                           (else
                              ;; likely packet size exceeded, so log it as a negative number for debugging
                              (values rs muta meta 
                                 (- 0 (vector-length buffer)))))))))
            (else
               (output-to-fd ll x))))
            
      ;; compute and discard (for fast forwardng)
      (define (dummy-output ll)
        (lets
          ((state (lfold (λ (prev this) this) #f ll))
           (rs muta meta state))
          (values rs muta meta)))

      (define (stream-chunk buff pos tail)
         (if (eq? pos 0)
            (cons (refb buff pos) tail)
            (lets ((next x (fx- pos 1)))
               (stream-chunk buff next
                  (cons (refb buff pos) tail)))))

      (define (output-stream->byte-stream lst)
         (foldr 
            (lambda (node tl)
               (if (vector? node)
                  (lambda ()
                     (let ((len (vector-length node)))
                        (if (eq? len 0)
                           tl
                           (stream-chunk node (- len 1) tl))))
                  tl))
            null lst))

      ;;;
      ;;; Checksum computing and uniqueness filtering
      ;;;
   
      ;; force all and compute payload checksum 
      ;; ll -> forced-ll checksum
      (define (checksummer hash)
         (λ (cs ll)
            (lets
               ((lst (force-ll ll))
                (bs (output-stream->byte-stream lst))
                (csum-trits csum-string (hash bs)))
               (if (dget cs csum-trits)
                  (values lst cs #false)
                  (values lst (dput cs csum-trits) csum-string)))))
      
      ;; dummy checksum, does not not force stream
      (define (dummy-checksummer hash)
         (λ (cs ll)
            (values ll cs "n/a")))

      (define (stdout-stream meta)
         (values stdout-stream stdout 
            (put meta 'output 'stdout)))

      (define (get-natural ll)
         (let loop ((ll ll) (n 0))
            (lets ((x ll (uncons ll #false)))
               (cond
                  ((not x) ;; no more anything, 0 is accepted as n
                     (values ll n))
                  ((and (<= #\0 x ) (<= x #\9))
                     (loop ll (+ (* n 10) (- x #\0))))
                  (else ;; no more numbers
                     (values (cons x ll) n))))))

      (define default-path ".rad")
      (define default-suffix (cdr (string->list default-path)))

      (define (suffix-char? x) 
        (not (has? '(#\. #\/ #\\) x)))

      (define (path-suffix path default) 
        (lets ((hd tl (take-while suffix-char? (reverse (string->list path)))))
          (if (and (pair? tl) (eq? (car tl) #\.))
            (reverse hd)
            default-suffix)))

      (define (source-path meta def)
        (or (get meta 'source #false) ;; file generator
            (get meta 'head #false)   ;; jump generator (head -> tail) 
            def))

      (define (file-writer pat suf)
         (define (gen meta)
            (lets 
               ((path
                  (runes->string
                     (str-foldr
                        (λ (char tl)
                           (cond
                              ((null? tl)
                                 (cons char tl))
                              ((and (eq? char #\%) (pair? tl))
                                 (case (car tl)
                                    ((#\n) (render (get meta 'nth 0) (cdr tl)))
                                    ((#\s) (append (path-suffix (source-path meta default-path) default-path) (cdr tl)))
                                    ((#\p) (append suf (cdr tl)))
                                    ((#\0) ;; %0[0-9]+n -> testcase number with padding
                                       (lets
                                          ((tlp pad (get-natural tl))
                                           (np tlp (uncons tlp #false))) ;; trailing required n
                                          (if (and pad (equal? np #\n))
                                             (lets
                                                ((digits (render (get meta 'nth 0) null))
                                                 (padding 
                                                   (map (λ (x) #\0)
                                                      (iota 0 1 (- pad (length digits)))))
                                                 (chars (append padding digits))
                                                 (chars (drop chars (- (length chars) pad))))
                                                (append chars tlp))
                                             (begin
                                                ;; print warning to stderr becase one likely accidentally %04d or %4n, etc
                                                (print*-to stderr
                                                   (list "Warning: testcase padding should be %0[0-9]+n"))
                                                (cons char tl)))))
                                    (else 
                                       (print*-to stderr
                                          (list "Warning: unknown pattern in output path: '" 
                                             (list->string (list char (car tl)))
                                             "'. Did you mean '%n, %s or %p'?"))
                                       (cons char tl))))
                              (else (cons char tl))))
                        null pat)))
                (port (open-output-file path)))
               (if (not port)
                  (begin
                     (print*-to stderr (list "Error: cannot write to '" path "'"))
                     (halt exit-write-error)))
               (values gen port (put (put meta 'output 'file-writer) 'path path))))
         gen)


      ;;;
      ;;; TCP client mode
      ;;;

      (define (ip->string ip)
         (list->string
            (vec-foldr
               (λ (this tl)
                  (render this
                     (if (null? tl)
                        tl
                        (cons #\. tl))))
               null ip)))

      (define (tcp-client ip port)
         (let ((ips (ip->string ip)))
            (define (gen meta)
               (let loop ((n 1)) ;; try to connect repeatedly
                  (let ((fd (open-connection ip port)))
                     (if fd
                        (values 
                           gen
                           fd
                           (put (put (put meta 'output 'tcp-client) 'ip ips) 'port port))
                        (begin
                           (sleep 100) ;; avoid eating up a whole core if the target is down
                           (if (= 0 (band n #xffff))
                              (print*-to stderr (list n " connection attempts to " ips ":" port)))
                           (loop (+ n 1)))))))
            gen))


      ;;;
      ;;; TCP server mode
      ;;;

      ;; ll → ip fd ll' | #f #f ()
      (define (next-client ll)
         (cond
            ((null? ll)
               (values #f #f null))
            ((pair? ll)
               (let ((this (car ll)))
                  (values (car this) (cdr this) (cdr ll))))
            (else
               (next-client (ll)))))

      (define (tcp-server ll port)
         (λ (meta)
            (lets ((ip fd ll (next-client ll)))
               (values
                  (tcp-server ll port)
                  fd
                  (-> meta
                     (put 'port port)
                     (put 'output 'tcp-server)
                     (put 'ip (ip->string ip)))))))

      ;;; 
      ;;; UDP client
      ;;; 

      (define (make-udp-client ip port)
         (lets 
            ((sock (open-udp-socket 0))
             (node (tuple 'udp ip port sock)))
            (if sock
               (let loop ()
                  (λ (meta)
                     (values (loop) node
                        (-> meta
                           (put 'port port)
                           (put 'output 'udp-client)
                           (put 'ip ip)))))
               (begin
                  (print-to stderr "Failed to created UDP socket")
                  #false))))
      
      ;; o n → (out :: → out' fd meta) v null | #false, where os is string from -o, and n is number from -n
      (define (string->outputs str n suf)
         (cond
            ((equal? str "-") ;; conventional way to ask for standard output (and input)
               stdout-stream)
            ((m/^:[0-9]+(\/(tcp|udp))?$/ str) ;; server mode network output, currently tcp only
               (lets
                   ((udp? (m/udp/ str))
                    (str (s/\/[a-z]+$// str)) ;; drop protocol info if there
                    (port (string->number (s/:// str) 10)))
                  (cond
                     ((not (and (number? port) (< port 65536)))
                        (print-to stderr "Invalid port: " port)
                        (print-to stderr "Port should be an integer below 65536")
                        #false)
                     (udp?
                        (print-to stderr "UDP server mode requested")
                        #false)
                     (else
                        (let ((clis (tcp-clients port)))
                           (if clis
                              (tcp-server clis port)
                              (begin
                                 (print "Couldn't bind to local port " port)
                                 #false)))))))
            ((m/^[0-9]{1,3}(\.[0-9]{1,3}){3}:[0-9]+(\/(tcp|udp))?$/ str) ;; client mode network output
               (lets
                  ((udp? (m/udp/ str))
                   (ip+port (c/:/ (s/\/[a-z]+$// str)))
                   (port (string->number (cadr ip+port) 10))
                   (ss (c/\./ (car ip+port)))
                   (bs (map (λ (x) (string->number x 10)) ss)))
                  (cond
                     ((not (and (all number? bs) 
                                (all (λ (x) (< x 256)) bs)
                                (number? port)
                                (< port 65536)))
                        (print-to stderr "Not a valid target: " str)
                        (print-to stderr "Target should be of the form 192.168.0.1:123 with optional /tcp or /udp suffix")
                        #false)
                     (udp?
                        ;; note: broadcast not currently enabled in socket creation, so no need to check for it here
                        (make-udp-client (list->vector bs) port))
                     (else
                        (tcp-client (list->vector bs) port)))))
            ((and (> n 1) (not (m/%(0[1-9][0-9]*)?n/ str)))
               (print-to stderr "Refusing to overwrite file '" str "' many times. You should add %n or %0[padding]n to the path.")
               #false)
            (else
               (file-writer str suf))))
))
