#lang typed/racket/gui

(provide (all-defined-out))

(require racket/flonum)

(require digimon/dtrace)
(require digimon/class)

(require "sprite.rkt")
(require "display.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Game-Zone-Info<%>
  (Class (init-field [master (Instance Display<%>)])))

(define-type Game-Zone<%>
  (Class (field [info (Option (Instance Game-Zone-Info<%>))])))

(define-type Graphlet-Info<%>
  (Class (init-field [master (Instance Game-Zone%)])))

(define-type Graphlet%
  (Class #:implements Sprite%
         [master (-> Game-Zone%)]
         [bind-info (-> (Instance Graphlet-Info<%>) Void)]
         
         [colliding-with-mouse? (-> Flonum Flonum Boolean)]
         [own-caret (-> Boolean Void)]
         [has-caret? (-> Boolean)]
         [moor (->* () (Symbol) Void)]

         [available-visible-region (-> Flonum Flonum (Values Nonnegative-Flonum Nonnegative-Flonum))]
         [get-location (->* () (Symbol) (Values Flonum Flonum))]
         [contain-region? (-> Flonum Flonum Flonum Flonum Boolean)]

         [notify-ready (-> Void)]
         [notify-updated (-> Void)]))

(define-type Game-Zone%
  (Class #:implements Game-Zone<%>
         
         [construct (-> Void)]
         [get-extent (-> Flonum Flonum (Values Nonnegative-Flonum Nonnegative-Flonum))]
         [get-margin (-> Flonum Flonum (Values Flonum Flonum Flonum Flonum))]
         [resize (-> Nonnegative-Flonum Nonnegative-Flonum Void)]
         [update (-> Nonnegative-Fixnum Nonnegative-Fixnum Void)]
         [draw (-> (Instance DC<%>) Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum Void)]
         [draw-progress (-> (Instance DC<%>) Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum Void)]
         [ready? (-> Boolean)]

         [begin-update-sequence (-> Void)]
         [end-update-sequence (-> Void)]
         [in-update-sequence? (-> Boolean)]
         [needs-update? (-> Boolean)]
         [notify-graphlet-updated (-> (Instance Sprite%) Void)]

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

         [enable (-> Boolean Void)]
         [get-zone-view (-> (Values Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum))]
         [find-last-graphlet (-> (Option (Instance Graphlet%)))]
         [get-graphlet-rectangle (-> (Instance Graphlet%) [#:global? Boolean] (Values Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum))]
         [get-zone-rectangle (-> [#:global? Boolean] (Values Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum))]
         
         [on-elapse (-> Nonnegative-Fixnum Nonnegative-Fixnum Void)]
         [on-elapsed (-> Nonnegative-Fixnum Nonnegative-Fixnum Fixnum Void)]))

#;(define-type Digivice-Zone%
  (Class #:implements Game-Zone%
         (init-field [alignment (U 'vertical 'horizontal)]
                     [gapsize Nonnegative-Real #:optional]
                     [on-interactive-operation On-Interactive-Operation #:optional])))

(define-type Heads-up-Zone%
  (Class #:implements Game-Zone%
         [get-margin (-> (Values Nonnegative-Real Nonnegative-Real Nonnegative-Real Nonnegative-Real))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define the-dc (make-object bitmap-dc% #false))

(define game-zone-info% : Game-Zone-Info<%>
  (class object% (super-new)
    (init-field master)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define editable-sprite% : Sprite%
  (class sprite% (super-new)))

(define permanent-sprite% : Sprite%
  (class sprite% (super-new)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define game-zone% : Game-Zone%
  (class object% (super-new)
    (field [info #false])
    
    (define/public (construct) (void))
    (define/public (get-extent x y) (values 0.0 0.0))
    (define/public (get-margin x y) (values 0.0 0.0 0.0 0.0))
    (define/public (resize width height) (void))
    (define/public (update interval uptime) (void))
    (define/public (draw dc x y Width Height) (void))
    (define/public (draw-progress dc x y Width Height) (void))
    (define/public (ready?) #true)

    (define/public (begin-update-sequence) (void))
    (define/public (end-update-sequence) (void))
    (define/public (in-update-sequence?) #false)
    (define/public (needs-update?) #false)
    (define/public (notify-graphlet-updated sprite) (void))

    (define/public (on-key keycode) #false)
    (define/public (on-tap x y) (void))
    (define/public (on-leave x y) (void))
    (define/public (on-wheel-translation local_x local_y delta horizontal?) #false)
    (define/public (on-wheel-zoom local_x local_y delta) #false)

    (define/public (on-char keycode keyboard) #false)
    (define/public (on-mouse-move x y mouse) #false)
    (define/public (on-mouse-drag x y which mouse) #false)
    (define/public (on-mouse-press x y which mouse) #false)
    (define/public (on-mouse-release x y which mouse) #false)

    (define/public (enable ?) (void))
    (define/public (get-zone-view) (values 0.0 0.0 0.0 0.0))
    (define/public (find-last-graphlet) #false)
    (define/public (get-graphlet-rectangle g #:global? [global? #false]) (values 0.0 0.0 0.0 0.0))
    (define/public (get-zone-rectangle #:global? [global? #false]) (values 0.0 0.0 0.0 0.0))
         
    (define/public (on-elapse interval uptime) (void))
    (define/public (on-elapsed interval uptime elapsed) (void))))

#;(define game-zone% : Game-Zone%
  (class pasteboard% (super-new)
    (send* this
      (set-selection-visible #false)
      (auto-wrap #true))

    (inherit begin-edit-sequence end-edit-sequence get-dc get-admin)
    (inherit insert find-first-snip find-next-selected-snip get-snip-location)
    (inherit set-selected remove-selected is-selected? no-selected set-caret-owner)
    (inherit dc-location-to-editor-location #| <==> |# global-to-local)
    (inherit editor-location-to-dc-location #| <==> |# local-to-global)

    (define &hover : (Boxof (Option (Instance Snip%))) (box #false))
    
    (define/augment (can-interactive-move? mouse) (inner #false can-interactive-move? mouse))
    (define/augment (can-interactive-resize? mouse) (inner #false can-interactive-resize? mouse))
    (define/augment (can-load-file? filename format) (inner #false can-load-file? filename format))
    (define/augment (can-save-file? filename format) (inner #false can-save-file? filename format))

    (define/public (get-snip-rectangle snip #:global? [dc-coordinate? #false])
      (define &x : (Boxof Real) (box 0.0))
      (define &y : (Boxof Real) (box 0.0))
      (define &width : (Boxof Nonnegative-Real) (box 0.0))
      (define &height : (Boxof Nonnegative-Real) (box 0.0))
      (get-snip-location snip &x &y) ; TODO: why must call (get-extent)?
      (send snip get-extent (or (get-dc) the-dc) (unbox &x) (unbox &y) &width &height)
      (when dc-coordinate? (local-to-global &x &y))
      (if (and (typeof? snip editable-sprite%) (not (is-selected? snip)))
          (match-let-values ([(_ _ _ _ ext-height ext-width) (send snip get-rectangle+extent)])
            (values (real->double-flonum (unbox &x))
                    (real->double-flonum (unbox &y))
                    (flmax (fl- (real->double-flonum (unbox &width)) ext-width) 0.0)
                    (flmax (fl- (real->double-flonum (unbox &height)) ext-height) 0.0)))
          (values (real->double-flonum (unbox &x)) (real->double-flonum (unbox &y))
                  (real->double-flonum (unbox &width)) (real->double-flonum (unbox &height)))))

    (define/public (get-zone-rectangle #:global? [dc-coordinate? #false])
      (define &x0 : (Boxof Real) (box +inf.0))
      (define &y0 : (Boxof Real) (box +inf.0))
      (define &xn : (Boxof Real) (box 0.0))
      (define &yn : (Boxof Real) (box 0.0))
      (let fold-snip ([this-snip (find-first-snip)])
        (if (false? this-snip)
            (let-values ([(w h) (values (- (unbox &xn) (unbox &x0)) (- (unbox &yn) (unbox &y0)))])
              (when dc-coordinate? (local-to-global &x0 &y0))
              (values (real->double-flonum (unbox &x0)) (real->double-flonum (unbox &y0))
                      (flmax (real->double-flonum w) 0.0) (flmax (real->double-flonum h) 0.0)))
            (let-values ([(x y w h) (get-snip-rectangle this-snip #:global? #false)])
              (when (< x (unbox &x0)) (set-box! &x0 x))
              (when (< y (unbox &y0)) (set-box! &y0 y))
              (when (> (fl+ x w) (unbox &xn)) (set-box! &xn (fl+ x w)))
              (when (> (fl+ y h) (unbox &yn)) (set-box! &yn (fl+ y h)))
              (fold-snip (send this-snip next))))))
    
    (define/public (get-zone-view)
      (define &x : (Boxof Real) (box 0.0))
      (define &y : (Boxof Real) (box 0.0))
      (define &width : (Boxof Real) (box 0.0))
      (define &height : (Boxof Real) (box 0.0))
      (define master : (Option (Instance Editor-Admin%)) (get-admin))
      (cond [(false? master) (values 0.0 0.0 0.0 0.0)]
            [else (let ([_ (send master get-view &x &y &width &height #false)])
                    (values (real->double-flonum (unbox &x)) (real->double-flonum (unbox &y))
                            (flmax (real->double-flonum (unbox &width)) 0.0)
                            (flmax (real->double-flonum (unbox &height)) 0.0)))]))

    (define/public (find-last-snip)
      (let try ([this-snip (find-first-snip)])
        (define next-snip (and this-snip (send this-snip next)))
        (if (false? next-snip) this-snip (try next-snip))))

    (define/public (do-interactive-operation snip operation event)
      (and (typeof? snip sprite%)
           (case operation
             [(hover)    (send snip on-hover  #true  event)]
             [(active)   (send snip on-active #true  event)]
             [(deactive) (send snip on-active #false event)]
             [(goodbye)  (send snip on-hover  #false event)]
             [(select)   (send snip on-select #true  event)]
             [(deselect) (send snip on-select #false event)]
             [(insert)   (send snip on-insert this   event)]
             [(delete)   (send snip on-delete this   event)]
             [else #false])))

    (define/public (ordered-insert snip<? snip [x 0] [y 0] #:unique? [unique? #true])
      (let insert/compare-with : Void ([this-snip : (Option (Instance Snip%)) (find-first-snip)])
        (cond [(false? this-snip) (insert snip x y)]
              [else (let ([uuid : Any (snip<? snip this-snip)])
                      (cond [(eq? uuid #true) (insert snip this-snip x y)]
                            [(and uuid unique?) (error 'ordered-insert "snip has already existed: ~a" uuid)]
                            [else (insert/compare-with (send this-snip next))]))])))

    (define/public (enable ?)
      (let try-next : Void ([child : (Option (Instance Snip%)) (find-first-snip)])
        (unless (false? child)
          (cond [(typeof? child sprite%) (send child enable ?)]
                [(typeof? child editable-sprite%) (send child enable ?)])
          (try-next (send child next)))))

    (define/public (find-next-editable-sprite snip)
      (let try : (Option (Instance Editable-Sprite%))
        ([this-snip (if (false? snip) (find-first-snip) (send snip next))])
        (cond [(or (false? this-snip) (typeof? this-snip editable-sprite%)) this-snip]
              [else (try (send this-snip next))])))

    (define/public (find-previous-editable-sprite snip)
      (let try : (Option (Instance Editable-Sprite%))
        ([this-snip (if (false? snip) (find-last-snip) (send snip previous))])
        (cond [(or (false? this-snip) (typeof? this-snip editable-sprite%)) this-snip]
              [else (try (send this-snip previous))])))

    (define/public (find-first-editable-sprite) (find-next-editable-sprite #false))
    (define/public (find-last-editable-sprite) (find-previous-editable-sprite #false))

    (define/override (find-snip editor-x editor-y [after #false])
      (let try : (Option (Instance Snip%)) ([maybe-snip (or after (find-first-snip))])
        (and maybe-snip
             (let-values ([(x y w h) (get-snip-rectangle maybe-snip #:global? #false)])
               (cond [(and (<= x editor-x (+ x w)) (<= y editor-y (+ y h))) maybe-snip]
                     [else (try (send maybe-snip next))])))))
  
    (define/override (adjust-cursor mouse)
      (define maybe-cursor : (U Void (Option (Instance Cursor%)))
        (when (get-admin)
          (define &editor-x : (Boxof Real) (box #| initiated with global x |# (send mouse get-x)))
          (define &editor-y : (Boxof Real) (box #| initiated with global y |# (send mouse get-y)))  
          (global-to-local &editor-x &editor-y)
          (define maybe-snip : (Option (Instance Snip%)) (find-snip (unbox &editor-x) (unbox &editor-y)))
          (unless (false? maybe-snip)
            (get-snip-location maybe-snip &editor-x &editor-y)
            (define-values (dc-x dc-y) (editor-location-to-dc-location (unbox &editor-x) (unbox &editor-y)))
            (send maybe-snip adjust-cursor (or (get-dc) the-dc) dc-x dc-y (unbox &editor-x) (unbox &editor-y) mouse))))
      (if (void? maybe-cursor) (super adjust-cursor mouse) maybe-cursor))
    
    (define/override (on-event mouse)
      (super on-event mouse)
      (define &editor-x : (Boxof Real) (box #| initiated with global x |# (send mouse get-x)))
      (define &editor-y : (Boxof Real) (box #| initiated with global y |# (send mouse get-y)))  
      (global-to-local &editor-x &editor-y)
      (define maybe-snip : (Option (Instance Snip%)) (find-snip (unbox &editor-x) (unbox &editor-y)))
      (let ([hovered : (Option (Instance Snip%)) (unbox &hover)])
        (when (and hovered (not (eq? hovered maybe-snip)))
          (do-interactive-operation hovered 'goodbye mouse))
        (set-box! &hover maybe-snip)
        (when (and maybe-snip (send mouse moving?) (not (send mouse get-left-down)))
          (do-interactive-operation maybe-snip 'hover mouse)))
      (if (or (send mouse leaving?) (nor (typeof? maybe-snip sprite%) (typeof? maybe-snip editable-sprite%)))
          (dtrace-info "kill all tooltips" #:topic 'tooltips #:urgent #false)
          (let*-values ([(_) (get-snip-location maybe-snip &editor-x &editor-y)]
                        [(dc-x dc-y) (editor-location-to-dc-location (unbox &editor-x) (unbox &editor-y))]
                        [(tips) (send maybe-snip get-tooltip (or (get-dc) the-dc) dc-x dc-y (unbox &editor-x) (unbox &editor-y) mouse)])
            (dtrace-info "show/hide tooltip" #:topic 'tooltips #:urgent tips))))

    (define/override (on-default-event mouse)
      (define maybe-snip : (Option (Instance Snip%))
        (let-values ([(editor-x editor-y) (dc-location-to-editor-location (send mouse get-x) (send mouse get-y))])
          (find-snip editor-x editor-y)))
      ;;; TODO: why (button-up?) does not make editable-snip release its caret?
      (when (send mouse button-down? 'left)
        (let deselect-snip : Void ([snip (find-next-selected-snip #false)])
          (unless (false? snip)
            (when (and (typeof? snip editable-sprite%) (not (eq? maybe-snip snip)))
              (remove-selected snip))
            (deselect-snip (find-next-selected-snip snip)))))
      (when (and maybe-snip (not (is-a? maybe-snip permanent-sprite%)))
        (cond [(or (send mouse button-down? 'left)
                   (and (send mouse moving?)
                        (send mouse get-left-down)))
               (cond [(typeof? maybe-snip editable-sprite%) (set-selected maybe-snip)]
                     [else (void (do-interactive-operation maybe-snip 'active mouse))])]
              [(send mouse button-up? 'left)
               (set-selected maybe-snip)
               (void (do-interactive-operation maybe-snip 'deactive mouse))])))

    (define/override (on-char keyboard)
      (cond [(not (eq? (send keyboard get-key-code) #\tab)) (super on-char keyboard)]
            [else (let ([snip (find-next-selected-snip #false)])
                    (define next-snip : (Option (Instance Editable-Sprite%))
                      (if (send keyboard get-shift-down)
                          (if snip (find-previous-editable-sprite snip) (find-last-editable-sprite))
                          (if snip (find-next-editable-sprite snip) (find-first-editable-sprite))))
                    (cond [(false? next-snip) (no-selected)]
                          [else (set-selected next-snip)]))]))

    (define/override (on-default-char keyboard)
      (unless (memq (send keyboard get-key-code)
                    '(left right up down #\backspace #\rubout))
        (super on-default-char keyboard)))
    
    (define/augment (on-insert snip before x y)
      (begin-edit-sequence)
      (inner (void) on-insert snip before x y))
    
    (define/augment (after-insert snip before x y)
      (inner (void) after-insert snip before x y)
      (when (typeof? snip sprite%) (send snip on-insert this))
      (end-edit-sequence))

    (define/augment (after-select snip on?)
      (inner (void) after-select snip on?)
      (when (typeof? snip editable-sprite%)
        (set-caret-owner (and on? snip))
        (define digivice : (Option (Instance Editor<%>)) (send snip get-editor))
        (unless (or (false? digivice) (not on?))
          (send digivice select-all))))
    
    (define/augment (on-delete snip)
      (begin-edit-sequence)
      (inner (void) on-delete snip))
    
    (define/augment (after-delete snip)
      (when (typeof? snip sprite%) (send snip on-delete this))
      (inner (void) after-delete snip)
      (end-edit-sequence))

    (define/public (on-elapse interval uptime)
      (void))

    (define/public (on-elapsed interval uptime elapsed)
      (void))))

#;(define digivice-zone% : Digivice-Zone%
  (class game-zone% (super-new)
    (init-field alignment [gapsize 0] [on-interactive-operation void])

    (inherit get-snip-rectangle move-to move find-last-snip)

    (define/override (do-interactive-operation snip operation event)
      (or (super do-interactive-operation snip operation event)
          (and (on-interactive-operation this snip operation event)
               #true)))

    (define/augment (after-insert snip before x y)
      (define (find-last-movable-snip [this-snip : (Option (Instance Snip%))]) : (Option (Instance Snip%))
        (cond [(false? this-snip) #false]
              [(is-a? this-snip permanent-sprite%) (find-last-movable-snip (send this-snip previous))]
              [else this-snip]))
      (unless (is-a? snip permanent-sprite%)
        (case alignment
          [(vertical)
           (if (false? before)
               (let ([last-one (find-last-movable-snip (send snip previous))])
                 (cond [(false? last-one) (move-to snip 0 0)]
                       [else (let-values ([(_x last-y _w last-height) (get-snip-rectangle last-one)])
                               (move-to snip 0 (+ last-y last-height gapsize)))]))
               (let-values ([(snip-x snip-y _w _h) (get-snip-rectangle before)])
                 (move-to snip snip-x snip-y)
                 (define-values (_x _y _w snip-height) (get-snip-rectangle snip))
                 (let move-down : Void ([this-snip : (Option (Instance Snip%)) before])
                   (unless (false? this-snip)
                     (unless (is-a? this-snip permanent-sprite%)
                       (move this-snip 0 (+ snip-height gapsize)))
                     (move-down (send this-snip next))))))]
          [(horizontal)
           (if (false? before)
               (let ([last-one (find-last-movable-snip (send snip previous))])
                 (cond [(false? last-one) (move-to snip 0 0)]
                       [else (let-values ([(last-x _y last-width _h) (get-snip-rectangle last-one)])
                               (move-to snip (+ last-x last-width gapsize) 0))]))
               (let-values ([(snip-x snip-y _w _h) (get-snip-rectangle before)])
                 (move-to snip snip-x snip-y)
                 (define-values (_x _y snip-width _h) (get-snip-rectangle snip))
                 (let move-right : Void ([this-snip : (Option (Instance Snip%)) before])
                   (unless (false? this-snip)
                     (unless (is-a? this-snip permanent-sprite%)
                       (move this-snip (+ snip-width gapsize) 0))
                     (move-right (send this-snip next))))))]))
      (inner (void) after-insert snip before x y))
    
    (define/augment (after-delete snip)
      (define ?next : (Option (Instance Snip%)) (send snip next))
      (unless (false? ?next)
        (unless (is-a? snip permanent-sprite%)
          (case alignment
            [(vertical)
             (define-values (_x _y _w snip-height) (get-snip-rectangle snip))
             (let move-up : Void ([this-snip : (Option (Instance Snip%)) ?next])
               (unless (false? this-snip)
                 (unless (is-a? this-snip permanent-sprite%)
                   (move this-snip 0 (- (+ snip-height gapsize))))
                 (move-up (send this-snip next))))]
            [(horizontal)
             (define-values (_x _y snip-width _h) (get-snip-rectangle snip))
             (let move-left : Void ([this-snip : (Option (Instance Snip%)) ?next])
               (unless (false? this-snip)
                 (unless (is-a? this-snip permanent-sprite%)
                   (move this-snip (- (+ snip-width gapsize)) 0))
                 (move-left (send this-snip next))))])))
      (inner (void) after-delete snip))
    
    (define/augment (after-select snip on?)
      (do-interactive-operation snip (if on? 'select 'deselect) (current-milliseconds))
      (inner (void) after-select snip on?))))

#;(define heads-up-zone% : Heads-up-Zone%
  (class game-zone% (super-new)
    (define/public (get-margin up right bottom left)
      (void))))
