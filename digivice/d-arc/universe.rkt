#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/gui)

(require racket/flonum)

(require "display.rkt")
(require "zone.rkt")

(require digimon/digitama/system)
(require plot/bitmap)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Universe%
  (Class #:implements Display<%>
         (init [parent (U (Instance Frame%) (Instance Dialog%)
                          (Instance Panel%) (Instance Pane%))]
               [construct-zones (-> (Listof (Instance Game-Zone%))) #:optional]
               [min-width (Option Nonnegative-Integer) #:optional]
               [min-height (Option Nonnegative-Integer) #:optional]
               [heads-up-zone (Option (Instance Heads-up-Zone%)) #:optional]
               [transparent? Boolean #:optional])
         [current-zone (-> (Option (Instance Game-Zone%)))]
         [current-zone-index (-> (Option Index))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define universe% : Universe%
  (class object% (super-new)
    (init parent
          [construct-zones list]
          [min-width #false] [min-height #false]
          [heads-up-zone #false] [transparent? #false])

    (define hup-zone : (Option (Instance Heads-up-Zone%)) heads-up-zone)
    (define hup-margins : FlVector (flvector 0.0 0.0 0.0 0.0))

    (define zones : (Vectorof (Instance Game-Zone%))
      (for/vector : (Vectorof (Instance Game-Zone%)) ([gz (in-list (construct-zones))])
        (set-field! info gz (make-object game-zone-info% this))
        gz))

    (define recent-zone-idx : Index 0)

    ;;; NOTE
    ; Yes, it's weird here, but Racket class doesn't have the method-like constructor,
    ; instead, the body of class itself is the constructor.
    ; In this situation, the overridden canvas% is just used to register some event listeners,
    ; sounds like a method-like constructor as in Java or .Net.
    (define constructor% : (Class #:implements Canvas%)
      (class canvas%
        (super-new [parent parent] [style (if (not transparent?) null '(transparent))] [enabled #true] [label #false]
                   [min-width min-width] [min-height min-height] [stretchable-width #true] [stretchable-height #true]
                   [horiz-margin 0] [vert-margin 0] [gl-config #false]
                   [paint-callback void])

        (unless (not hup-zone)
          (let-values ([(t r b l) (send hup-zone get-margin)])
            (flvector-set! hup-margins 0 (real->double-flonum t))
            (flvector-set! hup-margins 1 (real->double-flonum r))
            (flvector-set! hup-margins 2 (real->double-flonum b))
            (flvector-set! hup-margins 3 (real->double-flonum l))))

        (define/override (on-paint) (do-paint))
        (define/override (on-subwindow-event who mouse) (do-pointer-event mouse))))

    (define/public (get-canvas) physics)
    (define/public (get-device) device)

    (define/public (actual-size) (values (send physics get-width) (send physics get-height)))
    (define/public (client-size) (send physics get-client-size))
    (define/public (graphical-minimum-size) (send physics get-graphical-min-size))
    (define/public required-minimum-size
      (case-lambda
        [() (values (max (send physics min-width) 0) (max (send physics min-height) 0))]
        [(mwidth mheight) (send* physics (min-width mwidth) (min-height mheight))]))

    (define/public (surface-ready?) #true)
    (define/public (shown?) (send physics is-shown?))

    (define/public (on-navigate from-idx to-idx) (void))
    (define/public (on-elapse [interval : Nonnegative-Fixnum] [uptime : Nonnegative-Fixnum]) (send physics refresh))
    (define/public (on-elapsed [interval : Nonnegative-Fixnum] [uptime : Nonnegative-Fixnum] [elapsed : Fixnum]) (void))
    
    (define/public (global->local zone x y [xoff 0.0] [yoff 0.0]) (values x y))
    (define/public (local->global zone x y [xoff 0.0] [yoff 0.0]) (values x y))
    (define/public (zone-actual-size zone) (values 0.0 0.0))
    (define/public (refresh zone) (void))

    (define/public (take-snapshot) (void))
    (define/public (save file.png) (void))

    (define/public (current-zone-index)
      (and (< recent-zone-idx (vector-length zones))
           recent-zone-idx))

    (define/public (current-zone)
      (and (< recent-zone-idx (vector-length zones))
           (vector-ref zones recent-zone-idx)))

    (define (do-paint) : Void
      (define-values (lx ty rx by) (pea-positions))
      (send device draw-bitmap pea lx ty)

      (unless (not histogram)
        (define-values (glx gty grx gby) (statistics-position))
        (send device draw-bitmap (assert histogram) glx gty)

        (let ([gametes (format "~a + ~a → ~a"
                         (if (not dorminant1?) 'd 'D)
                         (if (not dorminant2?) 'd 'D)
                         (cond [(and dorminant1? dorminant2?) 'DD]
                               [(or dorminant1? dorminant2?) 'Dd]
                               [else 'dd]))])
          (send device set-font gamete-font)
          (define-values (gw gh _d _s) (send device get-text-extent gametes))
          (send device draw-text gametes (/ (+ lx rx (- gw)) 2) (+ by gh))
          (send device set-font stat-font))

        (let* ([ch (send device get-char-height)]
               [DD (vector-ref samples 0)]
               [Dd (vector-ref samples 1)]
               [dd (vector-ref samples 2)]
               [dg (+ DD Dd)]
               [mx (/ (+ glx grx) 2.0)])
          (send device draw-text (format "DD: ~a" DD) (+ glx ch) (+ gby (* ch 1.0)))
          (send device draw-text (format "Dd: ~a" Dd) (+ glx ch) (+ gby (* ch 2.5)))
          (send device draw-text (format "显性: ~a" dg) (+ mx ch) (+ gby (* ch 1.75)))
          (send device draw-text (format "dd: ~a" dd) (+ glx ch) (+ gby (* ch 4.0)))
          (send device draw-text (format "隐性: ~a" dd) (+ mx ch) (+ gby (* ch 4.0)))
          (send device draw-text (format "总次数: ~a" population) (+ glx ch) (+ gby (* ch 6.5)))
          (when (> dd 0)
            (send device draw-text (format "比例: ~a：1" (~r (/ dg dd) #:precision 1)) (+ mx ch) (+ gby (* ch 6.5))))))
      
      (void))

    (define (do-pointer-event [mouse : (Instance Mouse-Event%)]) : Boolean
      (and (send mouse button-up? 'left)
           (let-values ([(lx ty rx by) (pea-positions)]
                        [(mx my) (values (send mouse get-x) (send mouse get-y))])
             (and (< lx mx rx) (< ty my by)
                  (reproduce 1)
                  #true))))

    (define (pea-positions) : (Values Real Real Real Real)
      (define-values (#{Width : Natural} #{Height : Natural}) (client-size))
      (define-values (width height) (values (send pea get-width) (send pea get-height)))
      (define lx : Flonum (- (* Width 0.32) (* width 0.50)))
      (define ty : Flonum (- (* Height 0.32) (* height 0.50)))
      (define rx : Flonum (+ lx width))
      (define by : Flonum (+ ty height))

      (values lx ty rx by))

    (define (statistics-position) : (Values Real Real Real Real)
      (define-values (Width Height) (client-size))
      (define-values (width height) (values (send (assert histogram) get-width) (send (assert histogram) get-height)))
      (define lx : Flonum (- (* Width 0.64) (* width 0.50)))
      (define ty : Flonum (- (* Height 0.32) (* height 0.50)))
      (define rx : Flonum (+ lx width))
      (define by : Flonum (+ ty height))
      
      (values lx ty rx by))
    
    (define physics : (Instance Canvas%) (new constructor%))
    (define device : (Instance DC<%>) (send physics get-dc))
    (define stat-font : (Instance Font%) (send device get-font))
    (define gamete-font : (Instance Font%) (make-font #:size 48))

    (define pea : (Instance Bitmap%) (read-bitmap (digimon-path "stone" "pea.jpeg")))
    (define histogram : (Option (Instance Bitmap%)) #false)
    (define dorminant1? : Boolean #false)
    (define dorminant2? : Boolean #false)
    (define samples : (Vector Natural Natural Natural) (vector 0 0 0))
    (define population : Natural 0)
    (define statistics : (Listof (Mutable-Vector Any (U False Real ivl)))
      (list (vector 'DD 0) (vector 'Dd 0) (vector 'dd 0)))

    (define (reproduce [n : Index]) : Void
      (for ([i (in-range n)])
        (set! dorminant1? (= (random 2) 1))
        (set! dorminant2? (= (random 2) 1))
        
        (let ([idx (cond [(and dorminant1? dorminant2?) 0] [(or dorminant1? dorminant2?) 1] [else 2])])
          (vector-set! samples idx (+ (vector-ref samples idx) 1))))

      (set! population (for/fold ([s : Natural 0]) ([c (in-vector samples)]) (+ s c)))

      (for ([stat (in-list statistics)]
            [c (in-vector samples)])
        (vector-set! stat 1 (/ c population)))
      
      (set! histogram (plot #:x-label "基因型" #:y-label "出现概率" #:title "性状分离的概率模拟实验"
                            #:y-min 0 #:y-max 1
                            #:width (max 1 (quotient (plot-width) 2))
                            #:height (send pea get-height)
                            (list (discrete-histogram statistics)))))))
