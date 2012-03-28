;;; 
;;; Radamsa 
;;;

(define-library (rad main)

   (import
      (owl base)
      (owl args)
      (rad generators)
      (rad output)
      (rad patterns)
      (rad mutations)
      (rad shared))

   (export 
      radamsa)

   (begin

      (define (string->count str)
         (cond
            ((mem equal? '("inf" "infinity" "-1" "forever") str)
               'infinity)
            ((string->number str 10) =>
               (λ (n) 
                  (if (> n 0)
                     n
                     #false)))
            (else #f)))

      (define version-str "Radamsa 0.3") ;; aka funny fold

      (define usage-text "Usage: radamsa [arguments] [file ...]")

      (define about-text 

"Radamsa is a general purpose fuzzer. It is intended to be used for
breaking valid sample files in ways that might expose errors in programs
processing them. For more information read the fine manual page or
visit http://code.google.com/p/ouspg/

Radamsa was written by Aki Helin at OUSPG.")

      (define (string->natural str)
         (let ((i (string->integer str)))
            (if (and i (>= i 0))
               i
               #false)))

      (define command-line-rules
         (cl-rules
            `((help "-h" "--help" comment "show this thing")
              (about "-a" "--about" comment "what is this thing?")
              (version "-V" "--version" comment "show program version")
              (output "-o" "--output" has-arg default "-" cook ,string->outputs
                  comment "specify where to put the generated data")
              (count "-n" "--count" cook ,string->count
                  default "1" comment "how many outputs to generate (number or inf)")
              (seed "-s" "--seed" cook ,string->natural comment "random seed (number, default random)")
              (mutations "-m" "--mutations" cook ,string->mutators ;; seed not yet known so intermediate value here
                  comment "which mutations to use"
                  default ,default-mutations) ;; these come from (rad mutations)
              (patterns "-p" "--patterns" cook ,string->patterns
                  comment "which mutation patterns to use"
                  default ,default-patterns)
              (generators "-g" "--generators" cook ,string->generator-priorities ; the rest of initialization needs all args
                  comment "which data generators to use"
                  default "file,stdin=100")
              (metadata "-M" "--meta" has-arg
                  comment "save metadata about generated files to this file")
              (list "-l" "--list" comment "list mutations, patterns and generators")
              (verbose "-v" "--verbose" comment "show progress during generation"))))

      ;; () → string
      (define (urandom-seed)
         (let ((fd (open-input-file "/dev/urandom"))) ;; #false if not there
            (if fd
               (let ((data (get-block fd 10)))
                  (close-port fd)
                  (if (vector? data)
                     (vec-fold (λ (n d) (+ d (<< n 8))) 0 data)
                     #false))
               #false)))

      ;; () → string (decimal number)
      (define (time-seed)
         (time-ms))

      (define (show-options)
         (print "Mutations (-m)")
         (for-each (λ (opt) (print* (list "  " (ref opt 1) ": " (ref opt 3)))) *mutations*)
         (print "")
         (print "Mutation patterns (-p)")
         (for-each (λ (opt) (print* (list "  " (ref opt 1) ": " (ref opt 3)))) *patterns*)
         (print "")
         (print "Generators (-g)")
         (print " stdin: read data from standard input if no paths are given or - is among them")
         (print " file: read data from given files")
         (print " random: generate random data"))

      ;; drop B as implied
      (define (verbose-size n)
         (define (verb n u)
            (if (or (< n 1024) (null? (cdr u)))
               (render n (render (car u) null))
               (verb (div n 1024) (cdr u))))
         (list->string
            (verb n '("" "K" "M" "T" "P"))))

      (define (serialize-meta val)
         (if (ff? val)
            (ff-fold
               (λ (out key val)
                  (render key
                     (ilist #\: #\space
                        ((if (string? val) serialize render)
                           val
                           (if (null? out)
                              '(#\newline)
                              (ilist #\, #\space out))))))
               null val)))

      ;; ... → (ff | seed | 'close → ...)
      (define (maybe-meta-logger path verbose? fail)
         (define verb 
            (if verbose? 
               (λ (x) 
                  (cond
                     ((eq? x 'close) 42)
                     ((getf x 'seed) =>
                        (λ (seed) (print*-to stderr (list "Random seed: " seed))))
                     (else
                        (print*-to stderr 
                           (list " - " (or (getf x 'path) (get x 'ip "output")) 
                              ": " (verbose-size (get x 'length 0)))))))
               (λ (x) x)))
         (cond
            (path
               (let ((port (if (equal? path "-") stdout (open-output-file path))))
                  (if port
                     (λ (stuff)
                        (if (eq? stuff 'close)
                           (if (not (eq? port stdout)) (close-port port))
                           (write-bytes port (serialize-meta stuff)))
                        (verb stuff))
                     (fail "Cannot open metadata log file"))))
            (verbose?
               verb)
            (else 
               (λ (x) x))))

      (define (maybe-printer verbose)
         (if verbose
            (λ (args) (print*-to stderr args))
            (λ (args) args)))

      ;; dict args → rval
      (define (start-radamsa dict paths)
         ;; show command line stuff
         (cond
            ((null? paths)
               ;; fuzz stdin when called as $ cat foo | radamsa | bar -
               (start-radamsa dict (list "-")))
            ((not (getf dict 'seed))
               ;; get a random seed, prefer urandom
               (start-radamsa 
                  (put dict 'seed (or (urandom-seed) (time-seed)))
                  paths))
            ((getf dict 'version)
               (print version-str)
               0)
            ((getf dict 'help)
               (print usage-text)
               (print-rules command-line-rules)
               0)
            ((getf dict 'about)
               (print about-text)
               0)
            ((getf dict 'list)
               (show-options)
               0)
            (else
               (lets/cc ret
                  ((fail (λ (why) (print why) (ret 1)))
                   (rs (seed->rands (getf dict 'seed)))
                   (record-meta 
                     (maybe-meta-logger 
                        (getf dict 'metadata)
                        (getf dict 'verbose)
                        fail))
                   (n (getf dict 'count))
                   (mutas (getf dict 'mutations))
                   (rs muta (mutators->mutator rs mutas))
                   (gen 
                     (generator-priorities->generator rs
                        (getf dict 'generators) paths fail (getf dict 'count))))
                  ;; possibly save the seed to metadata
                  (record-meta (put #false 'seed (getf dict 'seed)))
                  (let loop 
                     ((rs rs)
                      (muta muta)
                      (pat (getf dict 'patterns))
                      (out (get dict 'output 'bug))
                      (p 1))
                     (if (and (number? n) (< n p)) ; n can be 'infinity
                        (begin
                           (record-meta 'close)
                           0)
                        (lets/cc ret
                           ((rs ll meta (gen rs))
                            (_ (if (not ll) (begin (print*-to stderr (list meta)) (ret 2))))
                            (meta (put meta 'nth p))
                            (out fd meta (out meta))
                            (rs muta meta n-written 
                              (output (pat rs ll muta meta) fd))
                            (meta (put meta 'length n-written)))
                           (record-meta meta)
                           (loop rs muta pat out (+ p 1)))))))))

      (define (radamsa args)
         (process-arguments (cdr args) 
            command-line-rules 
            usage-text 
            start-radamsa))))

(import (rad main))

radamsa ;; (arg ...) → int

