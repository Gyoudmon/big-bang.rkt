#lang typed/racket/base

(provide (all-defined-out))

(require digimon/dtrace)

(require typed/racket/gui/base)
(require typed/racket/class)

(require racket/flonum)
(require racket/math)

(require "display.rkt")
(require "zone.rkt")

(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Universe%
  (Class #:implements Display<%>
         (init [parent (U (Instance Frame%) (Instance Dialog%)
                          (Instance Panel%) (Instance Pane%))]
               [construct-zones (-> (Listof (Instance Game-Zone%))) #:optional]
               [min-width (Option Nonnegative-Integer) #:optional]
               [min-height (Option Nonnegative-Integer) #:optional]
               [heads-up-zone (Option (Instance Heads-up-Zone%)) #:optional]
               [transparent? Boolean #:optional]
               [/dev/gtrace Logger #:optional])
         [current-zone (-> (Option (Instance Game-Zone%)))]
         [current-zone-index (-> (Option Index))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (bind-zone-ownership stx)
  (syntax-case stx []
    [(_ master gzone)
     (syntax/loc stx
       (set-field! info gzone (make-object game-zone-info% master)))]))

(define-syntax (construct-zone stx)
  (syntax-case stx []
    [(_ gzone reason Width Height)
     (syntax/loc stx
       (with-handlers ([exn? (λ [[e : exn]] (void))])
         (send gzone construct reason Width Height)
         (send gzone load reason Width Height)
         (send gzone reflow Width Height)
         (send gzone notify-surface-ready)))]))

(define-syntax (draw-zone stx)
  (syntax-case stx []
    [(_ gzone dc x y width height)
     (syntax/loc stx
       (with-handlers ([exn? (λ [[e : exn]] (void))])
         (send gzone draw dc x y width height)))]))

(define-syntax (zone-tick stx)
  (syntax-case stx []
    [(_ hup-zone game-zones zidx on-elapse argl ...)
     (syntax/loc stx
       (let ([max-idx (vector-length game-zones)])
         (unless (not hup-zone)
           (send* hup-zone
             (begin-update-sequence)
             (on-elapse argl ...)
             (end-update-sequence)))
         
         (when (< zidx max-idx)
           (send* (vector-ref game-zones zidx)
             (begin-update-sequence)
             (on-elapse argl ...)
             (end-update-sequence))

           (let tick-pzs ([idx : Nonnegative-Fixnum 0])
             (when (< idx zidx)
               (send (vector-ref game-zones idx) on-elapse argl ...)
               (tick-pzs (+ idx 1))))

           (let tick-nzs ([idx : Nonnegative-Fixnum (+ zidx 1)])
             (when (< idx max-idx)
               (send (vector-ref game-zones idx) on-elapse argl ...)
               (tick-nzs (+ idx 1)))))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct zone-margin
  ([top : Nonnegative-Flonum]
   [right : Nonnegative-Flonum]
   [bottom : Nonnegative-Flonum]
   [left : Nonnegative-Flonum])
  #:type-name Zone-Margin
  #:mutable #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define universe% : Universe%
  (class object% (super-new)
    (init parent
          [construct-zones list]
          [min-width #false] [min-height #false]
          [heads-up-zone #false] [transparent? #false]
          [/dev/gtrace /dev/dtrace])

    (define hup-margin : Zone-Margin (zone-margin 0.0 0.0 0.0 0.0))
    (define hup-zone : (Option (Instance Heads-up-Zone%)) heads-up-zone) 

    (define recent-zone-idx : Index 0)
    (define game-zones : (Immutable-Vectorof (Instance Game-Zone%))
      (vector->immutable-vector
       (for/vector : (Vectorof (Instance Game-Zone%)) ([gz (in-list (construct-zones))])
         (bind-zone-ownership this gz)
         gz)))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define physics% : (Class #:implements Canvas%)
      (class canvas%
        (super-new [parent parent] [style (if (not transparent?) null '(transparent))] [enabled #true] [label #false]
                   [min-width min-width] [min-height min-height] [stretchable-width #true] [stretchable-height #true]
                   [horiz-margin 0] [vert-margin 0] [gl-config #false]
                   [paint-callback void])

        (define firstrun? : Boolean #true)

        (define/override (on-paint) (do-paint))
        (define/override (on-size width height) (do-resize (->fl width) (->fl height)))
        (define/override (on-superwindow-show shown?)
          (unless (not shown?)
            (cond [(and firstrun?) (do-construct 'first-run) (set! firstrun? #false)])))
        
        (define/override (on-subwindow-event who mouse) (do-pointer-event mouse))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define/public (get-canvas) physics)
    (define/public (get-device) device)

    (define/public (actual-size) (values (->fl (send physics get-width)) (->fl (send physics get-height))))
    (define/public (client-size) (let-values ([(w h) (send physics get-client-size)]) (values (->fl w) (->fl h))))
    (define/public (graphical-minimum-size) (let-values ([(w h) (send physics get-graphical-min-size)]) (values (->fl w) (->fl h))))
    (define/public required-minimum-size
      (case-lambda
        [() (values (max (->fl (send physics min-width)) 0.0) (max (->fl (send physics min-height)) 0.0))]
        [(mwidth mheight) (send* physics (min-width (exact-round mwidth)) (min-height (exact-round mheight)))]))

    (define/public (surface-ready?) #true)
    (define/public (shown?) (send physics is-shown?))

    (define/public (on-navigate from-idx to-idx) (void))
    (define/public (on-elapse interval uptime) (zone-tick hup-zone game-zones recent-zone-idx on-elapse interval uptime))
    (define/public (on-elapsed interval uptime elapsed) (zone-tick hup-zone game-zones recent-zone-idx on-elapsed interval uptime elapsed))
    
    (define/public (global->local zone x y [xoff 0.0] [yoff 0.0]) (values x y))
    (define/public (local->global zone x y [xoff 0.0] [yoff 0.0]) (values x y))
    (define/public (zone-actual-size zone) (actual-size))
    (define/public (refresh zone) (when (or (eq? zone hup-zone) (eq? zone (current-zone))) (send physics refresh)))

    (define/public (take-snapshot) (void))
    (define/public (save file.png) (void))

    (define/public (current-zone-index)
      (and (< recent-zone-idx (vector-length game-zones))
           recent-zone-idx))

    (define/public (current-zone)
      (and (< recent-zone-idx (vector-length game-zones))
           (vector-ref game-zones recent-zone-idx)))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (do-construct [reason : Symbol]) : Void
      (case reason
        [(first-run)
         (let*-values ([(Width Height) (client-size)]
                       [(_x _y gz:width gz:height) (zone-actual-position+size Width Height)])
           (unless (not hup-zone)
             (construct-zone hup-zone reason Width Height))
           (for ([gzone (in-vector game-zones)])
             (construct-zone gzone reason gz:width gz:height)))]))

    (define (do-resize [width : Nonnegative-Flonum] [height : Nonnegative-Flonum]) : Void
      (define-values (_x _y gz:width gz:height) (zone-actual-position+size width height))

      (unless (not hup-zone)
        (send hup-zone reflow width height))

      (for ([gzone (in-vector game-zones)])
        (send gzone reflow gz:width gz:height)))

    (define (do-paint) : Void
      (define-values (Width Height) (client-size))
      (define-values (gz:x gz:y gz:width gz:height) (zone-actual-position+size Width Height))

      ; only draw the head-up zone, current zone and the zone transferred from

      (let ([zidx (current-zone-index)])
        (unless (not zidx)
          (draw-zone (vector-ref game-zones zidx) device gz:x gz:y gz:width gz:height)))

      (unless (not hup-zone)
        (draw-zone hup-zone device 0.0 0.0 Width Height)))

    (define (do-pointer-event [mouse : (Instance Mouse-Event%)]) : Boolean
      #false)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define physics : (Instance Canvas%) (new physics%))
    (define device : (Instance DC<%>) (send physics get-dc))

    (define (zone-actual-position+size [Width : Nonnegative-Flonum] [Height : Nonnegative-Flonum])
      : (Values Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum)
      (define x : Nonnegative-Flonum (zone-margin-left hup-margin))
      (define y : Nonnegative-Flonum (zone-margin-top hup-margin))
      
      (values x y
              (max (- Width x (zone-margin-right hup-margin)) 0.0)
              (max (- Height y (zone-margin-bottom hup-margin)) 0.0)))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CONSTRUCTOR ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (unless (not hup-zone)
      (bind-zone-ownership this hup-zone)
      (let-values ([(t r b l) (send hup-zone margin-values)])
        (set-zone-margin-top!    hup-margin t)
        (set-zone-margin-right!  hup-margin r)
        (set-zone-margin-bottom! hup-margin b)
        (set-zone-margin-left!   hup-margin l)))

    (send device set-smoothing 'smoothed)))
