;;;
;;; Alignment-based sequence fusing
;;;

(define-library (rad fuse)

   (export fuse)

   (import 
      (owl base))

   (begin

      (define search-fuel 100000)

      (define search-stop-ip 8)

      (define (jump lst from to)
         (if (eq? lst from)
            to
            (cons (car lst)
               (jump (cdr lst) from to))))

      (define (suffixes lst)  
         (if (null? lst)
            null
            (cons lst (suffixes (cdr lst)))))

      (define (char-suffixes sufs)
         (fold
            (λ (subs suf)
               (if (null? suf)
                  subs
                  (put subs (car suf)
                     (cons (cdr suf) (get subs (car suf) null)))))
            empty sufs))

      (define (any-position-pair rs nodes)
         (lets 
            ((rs node (rand-elem rs nodes))
             (froms tos node)
             (rs from (rand-elem rs froms))
             (rs to (rand-elem rs tos)))
            (values rs from to)))


      ;;            .------------------------> source list suffixes after a shared prefix
      ;;            |               .--------> target -||-
      ;;            |               v
      ;;            v
      ;; node = ((suffix ...) . (suffix ...))
      (define (split tl node)
         (lets
            ((sas (char-suffixes (car node)))
             (sbs (char-suffixes (cdr node))))
            (ff-fold
               (λ (tl char sufs) ;; all suffixes in a after prefix+char
                  (let ((bs (getf sbs char)))
                     (if bs ;; there were some also in b
                        (cons (cons sufs bs) tl)
                        tl))) ;; nothing shared after char
               tl sas)))

      (define (find-jump-points rs a b)
         (lets
            ((al (suffixes a))
             (bl (suffixes b))
             (nodes (list (cons al bl))))
            (let loop ((rs rs) (nodes nodes) (fuel search-fuel))
               (if (< search-fuel 0)
                  (any-position-pair rs nodes)
                  (lets ((rs x (rand rs search-stop-ip)))
                     (if (eq? x 0)
                        (any-position-pair rs nodes)
                        (let ((nodesp (fold split null nodes)))
                           (if (null? nodesp)
                              (any-position-pair rs nodes)
                              (loop rs nodesp (- fuel (length nodesp)))))))))))

      (define (fuse rs al bl)
         (cond
            ((null? al) (values rs bl))
            ((null? bl) (values rs al))
            (else
               (lets ((rs a b (find-jump-points rs al bl)))
                  (values rs (jump al a b))))))))
