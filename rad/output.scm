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
         (lets ((res n (blocks->port ll fd)))
            (cond
               ((not res) (halt 111)) ;; ungraceful direct exit on write errors
               ((and (pair? res) (tuple? (car res)))
                  ;; valid exit with latest and greatest states in a tuple
                  (lets
                     ((states (car res))
                      (rs muta meta states))
                     (if (not (eq? fd stdout))
                        (close-port fd)) ;; closes the opened thread also, later not needed
                     (values rs muta meta n)))
               (else
                  (print*-to stderr (list "Invalid data in output: " (car res)))
                  (halt 121)))))

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

      (define (tcp-client ip port)
         (print (list "would use " ip " port " port " but won't yet."))
         null)

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

      (define (ip->string ip)
         (list->string
            (vec-foldr
               (λ (this tl)
                  (render this
                     (if (null? tl)
                        tl
                        (cons #\. tl))))
               null ip)))

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
