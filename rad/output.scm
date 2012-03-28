;;;
;;; Radamsa Output
;;;

(define-library (rad output)

   (import
      (owl base)
      (only (owl primop) halt)
      (rad shared))

   (export
      output
      string->outputs)

   (begin

      ;; output :: (ll' ++ (#(rs mutator meta))) fd → rs mutator meta (n-written | #f), handles port closing &/| flushing
      (define (output ll fd)
         (lets 
            ((ll n (blocks->port ll fd))
             (ok? (and (pair? ll) (tuple? (car ll)))) ;; all written?
             (state (lfold (λ (last block) block) #false ll)) ;; find the last tuple
             (rs muta meta state))
            (if (not (eq? fd stdout))
               (close-port fd))
            ;; could warn about write errors
            (values rs muta meta n)))

      (define (stdout-stream meta)
         (values stdout-stream stdout 
            (put meta 'output 'stdout)))

      (define (file-writer pat)
         (define (gen meta)
            (lets 
               ((path ;; <- could also just regex it there
                  (runes->string
                     (str-foldr
                        (λ (char tl)
                           (cond
                              ((null? tl)
                                 (cons char tl))
                              ((eq? char #\%)
                                 (case (car tl)
                                    ((#\n) (render (get meta 'nth 0) (cdr tl)))
                                    (else (error "Unknown pattern in output path: " (list->string (cons char tl))))))
                              (else (cons char tl))))
                        null pat)))
                (port (open-output-file path)))
               (if (not port)
                  (print*-to stderr (list "Warning: cannot write to '" path "'")))
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
                           (interact sleeper-id 2) ;; avoid eating up a whole core if the target is down
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

      (define (tcp-server ll)
         (λ (meta)
            (lets ((ip fd ll (next-client ll)))
               (values
                  (tcp-server ll)
                  fd
                  (put (put meta 'output 'tcp-server) 'ip (ip->string ip))))))

      ;; args → (out :: → out' fd meta) v null | #false
      (define (string->outputs str)
         (cond
            ((equal? str "-") ;; conventional way to ask for standard output (and input)
               stdout-stream)
            ((m/^:[0-9]+$/ str)
               (let ((port (string->number (s/:// str) 10)))
                  (if (and (number? port) (< port 65536))
                     (let ((clis (tcp-clients port)))
                        (if clis
                           (tcp-server clis)
                           (begin
                              (show "Couldn't bind to local port " port)
                              #false)))
                     (begin   
                        (show "Invalid port: " port)
                        #false))))
            ((m/^[0-9]{1,3}(\.[0-9]{1,3}){3}:[0-9]+$/ str)
               (lets
                  ((ip+port (c/:/ str))
                   (port (string->number (cadr ip+port) 10))
                   (ss (c/\./ (car ip+port)))
                   (bs (map (λ (x) (string->number x 10)) ss)))
                  (if (and (all number? bs) 
                           (all (λ (x) (< x 256)) bs)
                           (number? port)
                           (< port 65536))
                     (tcp-client (list->vector bs) port)
                     (begin
                        (show "Not a valid target: " str)
                        #false))))
            (else
               (file-writer str))))
))
