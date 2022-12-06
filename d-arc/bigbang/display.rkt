#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/gui/base)

(require "forward/zone.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Display<%>
  (Class [get-device (-> (Instance DC<%>))]
         [get-canvas (-> (Instance Canvas<%>))]

         [actual-size (-> (Values Nonnegative-Flonum Nonnegative-Flonum))]
         [client-size (-> (Values Nonnegative-Flonum Nonnegative-Flonum))]
         [zone-actual-size (-> (Instance Game-Zone<%>) (Values Nonnegative-Flonum Nonnegative-Flonum))]
         [graphical-minimum-size (-> (Values Nonnegative-Flonum Nonnegative-Flonum))]
         [required-minimum-size (case-> [-> (Values Nonnegative-Flonum Nonnegative-Flonum)]
                                        [Nonnegative-Real Nonnegative-Real -> Void])]

         ;;; NOTE
         ; Don't change `surface-ready?` to `ready?`,
         ;   a sprite may also implements this interface,
         ;   where one itself has the method `ready?`
         [surface-ready? (-> Boolean)]
         [shown? (-> Boolean)]

         [on-navigate (-> Integer Integer Void)]
         [on-elapse (-> Nonnegative-Fixnum Nonnegative-Fixnum Void)]
         [on-elapsed (-> Nonnegative-Fixnum Nonnegative-Fixnum Fixnum Void)]

         [global->local (->* ((Instance Game-Zone<%>) Flonum Flonum) (Flonum Flonum) (Values Flonum Flonum))]
         [local->global (->* ((Instance Game-Zone<%>) Flonum Flonum) (Flonum Flonum) (Values Flonum Flonum))]
         [refresh (-> (Instance Game-Zone<%>) Void)]

         [take-snapshot (-> Any)]
         [save (-> Path-String Void)]))
