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

      (define version-str "Radamsa 0.3a") ;; aka funny fold
      (define usage-text "Usage: radamsa [arguments] [file ...]")

      (define command-line-rules
         (cl-rules
            `((help "-h" "--help" comment "Show this thing.")
              (output "-o" "--output" has-arg default "-" cook ,string->outputs
                  comment "Where to put the generated data")
              (count "-n" "--count" cook ,string->integer check ,(λ (x) (> x 0))
                  default "1" comment "How many outputs to generate")
              (seed "-s" "--seed" cook ,string->integer comment "Random seed (number, default random)")
              (mutations "-m" "--mutations" cook ,string->mutator 
                  comment "Which mutations to use"
                  default ,default-mutations) ;; these come from (rad mutations)
              (patterns "-p" "--patterns" cook ,string->patterns
                  comment "Which mutation patterns to use"
                  default "od,nd")
              (generators "-g" "--generators" cook ,string->generator-priorities ; the rest of initialization needs all args
                  comment "Which data generators to use"
                  default "file,stdin=100")
              (metadata "-M" "--metadata" has-arg
                  comment "Save metadata about generated files to this file")
              (list "-l" "--list" comment "List mutations, patterns and generators")
              (version "-V" "--version" comment "Show version information."))))

      ;; () → string
      (define (urandom-seed)
         (let ((fd (open-input-file "/dev/urandom"))) ;; #false if not there
            (if fd
               (let ((data (interact fd 10)))
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
         (print " file: read data from given files"))

      (define (maybe-meta-logger path fail)
         (if path
            (let ((port (open-output-file path)))
               (if port
                  (λ (stuff)
                     (if (eq? stuff 'close)
                        (close-port port)
                        (mail port (serialize stuff '(10)))))
                  (fail "Cannot open metadata log file")))
            (λ (stuff) stuff)))

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
            ((getf dict 'list)
               (show-options)
               0)
            (else
               (lets/cc ret
                  ((fail (λ (why) (print why) (ret 1)))
                   (rs (seed->rands (getf dict 'seed)))
                   (record-meta (maybe-meta-logger (getf dict 'metadata) fail))
                   (n (getf dict 'count))
                   (gen 
                     (generator-priorities->generator rs
                        (getf dict 'generators) paths fail (getf dict 'count))))
                  (let loop 
                     ((rs rs)
                      (muta (getf dict 'mutations))
                      (pat (getf dict 'patterns))
                      (out (get dict 'output 'bug))
                      (p 1))
                     (if (< n p)
                        0
                        (lets
                           ((rs ll meta (gen rs))
                            (meta (put meta 'nth p))
                            (out fd meta (out meta))
                            (rs muta meta n-written 
                              (output (pat rs ll muta meta) fd)))
                           (record-meta meta)
                           (loop rs muta pat out (+ p 1)))))))))

      (define (radamsa args)
         (process-arguments (cdr args) 
            command-line-rules 
            usage-text 
            start-radamsa))))

(import (rad main))

radamsa

