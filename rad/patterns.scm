
;;;
;;; Shared Parameters and Functions
;;;

;;;
;;; Mutation patterns
;;;

(define-library (rad patterns)

   (import 
      (owl base)
      (rad shared))

   (export
      *patterns*
      default-patterns
      string->patterns)

   (begin
     
      (define max-burst-mutations 16)

      (define (mutate-once rs ll mutator meta cont)
         (lets
            ((rs ip (rand rs initial-ip)) ;; initial inverse probability, magic value
             (this ll (uncons ll #false)))
            (if this
               (let loop ((rs rs) (this this) (ll ll) (ip ip))
                  (if (function? ll)
                     ;; stream more data
                     (loop rs this (ll) ip)
                     (lets ((rs n (rand rs ip)))
                        (if (or (eq? n 0) (null? ll)) ;; mutation happens to occur, or last place for it
                           (lets 
                              ((ll (cons this ll))
                               (mutator rs ll meta (mutator rs ll meta)))
                              (cont ll rs mutator meta))
                           ;; keep moving
                           (pair this
                              (loop rs (car ll) (cdr ll) (+ ip 1)))))))
               ;; no data to work on
               (cont null rs mutator meta))))

      ;; pat :: rs ll muta meta → ll' ++ (list (tuple rs mutator meta))
      (define (pat-once-dec rs ll mutator meta)
         (mutate-once rs ll mutator 
            (put meta 'pattern 'once-dec)
            (λ (ll rs mutator meta)
               (lappend ll (list (tuple rs mutator meta))))))

      ;; 1 or more mutations
      (define (pat-many-dec rs ll mutator meta)
         (mutate-once rs ll mutator 
            (put meta 'pattern 'many-dec)
            (λ (ll rs mutator meta)
               (lets ((rs muta? (rand-occurs? rs remutate-probability)))
                  (if muta? 
                     (pat-many-dec rs ll mutator meta)
                     (lappend ll (list (tuple rs mutator meta))))))))

      (define (pat-burst rs ll mutator meta)
         (mutate-once rs ll mutator
            (put meta 'pattern 'burst)
            (λ (ll rs mutator meta)
               (let loop ((rs rs) (ll ll) (mutator mutator) (meta meta) (n 1))
                  (lets ((rs p (rand-occurs? rs remutate-probability)))
                     (if (or p (< n 2))
                        ;; note, the ll is pre-evaluated because we already mutated it
                        (lets ((mutator rs ll meta (mutator rs ll meta)))
                           (loop rs ll mutator meta (+ n 1)))
                        (lappend ll (list (tuple rs mutator meta)))))))))
           
      (define *patterns*
         (list
            (tuple "od" pat-once-dec "Mutate once" )
            (tuple "nd" pat-many-dec "Mutate possibly many times" )
            (tuple "bu" pat-burst "Make several mutations closeby once")))

      (define default-patterns "od,nd,bu")

      (define (priority->pattern pri)
         (let ((func (choose *patterns* (car pri))))
            (if func
               (cons (cdr pri) func)
               (begin
                  (print*-to stderr (list "Unknown pattern: " (cdr pri)))
                  #false))))

      ;; ((pri . pat) ...) → (rs ll muta meta → <pattern output>)
      (define (mux-patterns ps)
         (lets
            ((ps (sort car> ps))
             (n (fold + 0 (map car ps))))
            (λ (rs ll muta meta)
               (lets ((rs n (rand rs n)))
                  ((choose-pri ps n) rs ll muta meta)))))

      (define (string->patterns str)
         (lets
            ((ps (map c/=/ (c/,/ str))) ; ((name [priority-str]) ..)
             (ps (map selection->priority ps))
             (ps (map priority->pattern ps)))
            (if (all self ps) 
               (mux-patterns ps)
               #false)))
))

