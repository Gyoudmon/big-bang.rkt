#lang typed/racket/gui

(provide (all-defined-out))

(require "forward/sprite.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Sprite%
  (Class #:implements Sprite<%>
         [get-feature (-> (Listof Symbol))]

         [construct (-> Void)]
         [get-extent (-> Flonum Flonum (Values Nonnegative-Flonum Nonnegative-Flonum))]
         [get-margin (-> Flonum Flonum (Values Flonum Flonum Flonum Flonum))]
         [resize (-> Nonnegative-Flonum Nonnegative-Flonum Void)]
         [update (-> Nonnegative-Fixnum Nonnegative-Fixnum Nonnegative-Fixnum Void)]
         [draw (-> (Instance DC<%>) Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum Void)]
         [draw-progress (-> (Instance DC<%>) Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum Void)]
         [ready? (-> Boolean)]

         [on-key (-> (U Char Symbol) Boolean)]
         [on-tap (-> Flonum Flonum Void)]
         [on-leave (-> Flonum Flonum Void)]
         [on-wheel-translation (-> Flonum Flonum Flonum Boolean Boolean)]
         [on-wheel-zoom (-> Flonum Flonum Flonum Boolean)]

         [on-char (-> (U Char Symbol) (Instance Key-Event%) Boolean)]
         [on-mouse-move (-> Flonum Flonum (Instance Mouse-Event%) Boolean)]
         [on-mouse-drag (-> Flonum Flonum Symbol (Instance Mouse-Event%) Boolean)]
         [on-mouse-press (-> Flonum Flonum Symbol (Instance Mouse-Event%) Boolean)]
         [on-mouse-release (-> Flonum Flonum Symbol (Instance Mouse-Event%) Boolean)]

         [camouflage (-> Boolean Void)]
         [concealled? (-> Boolean)]

         [take-snapshot (-> Any)]
         [save (-> Path-String Void)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define sprite% : Sprite%
  (class object% (super-new)
    (define findable? : Boolean #true)
    
    (define/public (get-feature) '(handles-events))
    
    (define/public (construct) (void))
    (define/public (get-extent local-x local-y) (values 0.0 0.0))
    (define/public (get-margin local-x local-y) (values 0.0 0.0 0.0 0.0))
    (define/public (resize width height) (void))
    (define/public (update count interval uptime) (void))
    (define/public (draw dc x y Width Height) (void))
    (define/public (draw-progress dc x y Width Height) (void))
    (define/public (ready?) #true)

    (define/public (on-key keycode) #false)
    (define/public (on-tap local-x local-y) (void))
    (define/public (on-leave local-x local-y) (void))
    (define/public (on-wheel-translation local-x local-y delta horizontal?) #false)
    (define/public (on-wheel-zoom local-x local-y delta) #false)

    (define/public (on-char keycode keyboard) #false)
    (define/public (on-mouse-move local-x local-y mouse) #false)
    (define/public (on-mouse-drag local-x local-y which mouse) #false)
    (define/public (on-mouse-press local-x local-y which mouse) #false)
    (define/public (on-mouse-release local-x local-y which mouse) #false)

    (define/public (camouflage yes?) (set! findable? (not yes?)))
    (define/public (concealled?) (not findable?))

    (define/public (take-snapshot) (void))
    (define/public (save sprite.png) (void))))
