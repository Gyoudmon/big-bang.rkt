#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/gui)

(require "forward/zone.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Display<%>
  (Class [get-device (-> (Instance DC<%>))]
         [get-canvas (-> (Instance Canvas<%>))]

         [actual-size (-> (Values Nonnegative-Integer Nonnegative-Integer))]
         [client-size (-> (Values Nonnegative-Integer Nonnegative-Integer))]
         [graphical-minimum-size (-> (Values Nonnegative-Integer Nonnegative-Integer))]
         [required-minimum-size (case-> [-> (Values Nonnegative-Integer Nonnegative-Integer)]
                                        [Nonnegative-Integer Nonnegative-Integer -> Void])]

         [surface-ready? (-> Boolean)]
         [shown? (-> Boolean)]

         [on-navigate (-> Integer Integer Void)]
         [on-elapse (-> Nonnegative-Fixnum Nonnegative-Fixnum Void)]
         [on-elapsed (-> Nonnegative-Fixnum Nonnegative-Fixnum Fixnum Void)]

         [global->local (->* ((Instance Game-Zone<%>) Flonum Flonum) (Flonum Flonum) (Values Flonum Flonum))]
         [local->global (->* ((Instance Game-Zone<%>) Flonum Flonum) (Flonum Flonum) (Values Flonum Flonum))]
         [zone-actual-size (-> (Instance Game-Zone<%>) (Values Nonnegative-Flonum Nonnegative-Flonum))]
         [refresh (-> (Instance Game-Zone<%>) Void)]

         [take-snapshot (-> Any)]
         [save (-> Path-String Void)]))
