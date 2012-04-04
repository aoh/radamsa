;;;
;;; Data Splitter
;;;

;; given a chunk of data guess a good byte to split it to fields for further mutations

;; options:
; - tree level
;    - use pais from partial parser to guess a parse tree depth (can be positive & negative)
;    - search a plateau, possibly with hills, where there are shared (predetermined?) bytes between sequences
;    - split the nodes and apply a given mutator to their sequence (sans the separators)
;    - fuse them back adding separators as necessary
; - black box
;    - compute a partial lazy suffix tree as needed
;    - look for nodes with are fairly wide and have suffixes in a nice sequence (tm)
;    - split around them and continue as above
; - stupid
;    - split around {#\,, #\space, #\tab, ...} and do as above

;; pros/cons
; stupid: 
;   + trivial to implement
;   - will break nested structures
; tree level:
;   + easy to implement
;   + will work against most usual suspects
;   - won't adapt to unknown targets with similar structure
; black box:
;   + adapts to unknown targets
;   - will break nested structures 

(define-library (rad split)

   (import
      (owl base))

   (export
      split-lense)

   (begin

      (define (split-lense mutator)
         (λ (rs data meta)
            42))))


