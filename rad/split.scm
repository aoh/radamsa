;;;
;;; Data Splitter
;;;

;; given a chunk of data guess a good byte to split it to fields for further mutations

(define-library (rad split)

   (import
      (owl base))

   (export
      split-lense)

   (begin

      (define (split-lense mutator)
         (λ (rs data meta)
            42))))


