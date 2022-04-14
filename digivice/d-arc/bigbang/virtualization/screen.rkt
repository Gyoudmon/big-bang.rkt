#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/class)

(require "../forward/zone.rkt")
(require "../forward/sprite.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Screen<%>
  (Class [surface-ready? (-> Boolean)]
         [shown? (-> Boolean)]

         [actual-size (-> (Instance Game-Zone<%>) (Values Nonnegative-Flonum Nonnegative-Flonum))]
         [view-size (case-> [-> (Values Nonnegative-Flonum Nonnegative-Flonum)]
                            [Nonnegative-Flonum Nonnegative-Flonum -> Void])]
         [min-size (case-> [-> (Values Nonnegative-Flonum Nonnegative-Flonum)]
                           [Nonnegative-Flonum Nonnegative-Flonum -> Void])]
         
         [global->local (->* ((Instance Game-Zone<%>) Flonum Flonum) (Flonum Flonum) (Values Flonum Flonum))]
         [local->global (->* ((Instance Game-Zone<%>) Flonum Flonum) (Flonum Flonum) (Values Flonum Flonum))]

         [begin-update-sequence (-> Void)]
         [in-update-sequence? (-> Boolean)]
         [end-update-sequence (-> Void)]
         [needs-update? (-> Boolean)]
         [notify-graphlet-updated (-> Sprite<%>)]
         [refresh (-> (Instance Game-Zone<%>) Void)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

