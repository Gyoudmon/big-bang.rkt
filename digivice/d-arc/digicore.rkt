#lang typed/racket/gui

(provide (all-defined-out))

(require racket/flonum)

(require digimon/exception)
(require digimon/dtrace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Digivice-Operation (U 'select 'deselect 'hover 'active 'deactive 'goodbye))
(define-type Digivice-Event (U Integer (Instance Event%)))
(define-type On-Interactive-Operation (-> (Instance Game-Zone%) (Instance Snip%) Digivice-Operation Digivice-Event Any))

(define-type Game-Snip%
  (Class #:implements Snip%
         [enable (-> Boolean Void)]
         [enabled? (-> Boolean)]
         #;[selected? (-> Boolean)]
         [refresh-now (-> Void)]
         [get-zone-size (-> (Values Nonnegative-Flonum Nonnegative-Flonum))]
         [get-size (-> (Values Nonnegative-Flonum Nonnegative-Flonum))]
         [get-tooltip (-> (Instance DC<%>) Real Real Real Real (Instance Mouse-Event%) Any)]
         [on-insert (-> (Instance Game-Zone%) Void)]
         [on-delete (-> (Instance Game-Zone%) Void)]
         [on-select (-> Boolean Digivice-Event Boolean)]
         [on-hover (-> Boolean Digivice-Event Boolean)]
         [on-active (-> Boolean Digivice-Event Boolean)]

         [on-elapse (-> Nonnegative-Fixnum Nonnegative-Fixnum Void)]
         [on-elapsed (-> Nonnegative-Fixnum Nonnegative-Fixnum Fixnum Void)]))

(define-type Game-Zone%
  (Class #:implements Pasteboard%
         #;[enable (-> Boolean Void)]
         [get-zone-view (-> (Values Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum))]
         [find-last-snip (-> (Option (Instance Snip%)))]
         [get-snip-rectangle (-> (Instance Snip%) [#:global? Boolean] (Values Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum))]
         [get-zone-rectangle (-> [#:global? Boolean] (Values Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum))]
         [do-interactive-operation (-> (Instance Snip%) Digivice-Operation Digivice-Event Boolean)]
         [ordered-insert (->* ((-> (Instance Snip%) (Instance Snip%) Any) (Instance Snip%)) (Real Real #:unique? Boolean) Void)]
         #;[find-next-editor-snip (-> (Option Snip) (Option (Instance Nefertimon-Editor-Snip%)))]
         #;[find-previous-editor-snip (-> (Option Snip) (Option (Instance Nefertimon-Editor-Snip%)))]
         #;[find-first-editor-snip (-> (Option (Instance Nefertimon-Editor-Snip%)))]
         #;[find-last-editor-snip (-> (Option (Instance Nefertimon-Editor-Snip%)))]

         [on-elapse (-> Nonnegative-Fixnum Nonnegative-Fixnum Void)]
         [on-elapsed (-> Nonnegative-Fixnum Nonnegative-Fixnum Fixnum Void)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define the-dc (make-object bitmap-dc% #false))

(define game-snip% : Game-Snip%
  (class snip% (super-new)
    (send* this
      (set-snipclass (new snip-class%))
      (set-flags (cons 'handles-all-mouse-events (send this get-flags))))
    
    (inherit get-admin get-extent)

    (define enabled : Boolean #true)
    
    (define/public (enable ?) (set! enabled ?))
    (define/public (enabled?) enabled)
    (define/public (on-insert zone) (void))
    (define/public (on-delete zone) (void))
    (define/public (on-select ? event) #false)
    (define/public (on-hover ? event) #false)
    (define/public (on-active ? event) #false)
    (define/public (get-tooltip dc x y editor-x editor-y mouse) #false)

    (define/public (on-elapse interval uptime) (void))
    (define/public (on-elapsed interval uptime elapsed) (void))

    
    #;(define/public (selected?)
      (define master : (Option (Instance Snip-Admin%)) (get-admin))
      (and master
           (let ([zone (send master get-editor)])
             (and (pasteboard%? zone)
                  (send zone is-selected? this)))))

    (define/public (refresh-now)
      (define master : (Option (Instance Snip-Admin%)) (get-admin))
      (define editor : (Option (Instance Editor<%>)) (and master (send master get-editor)))
      (unless (false? editor)
        (define-values (w h) (get-size))
        (send editor needs-update this 0 0 w h)))
    
    (define/public (get-zone-size) ; insets have already been subtracted
      (define master : (Option (Instance Snip-Admin%)) (get-admin))
      (cond [(false? master) (values 0.0 0.0)]
            [else (let ([&width : (Boxof Nonnegative-Real) (box 0.0)]
                        [&height : (Boxof Nonnegative-Real) (box 0.0)])
                    (send master get-view-size &width &height)
                    (values (real->double-flonum (unbox &width))
                            (real->double-flonum (unbox &height))))]))

    (define/public (get-size)
      ;;; NOTE
      ; racket/snip has its own caching strategy,
      ; getting size through editor is therefore better than directly invoking (get-extent)  
      (define master : (Option (Instance Snip-Admin%)) (get-admin))
      (define editor : (Option (Instance Editor<%>)) (and master (send master get-editor)))
      (if (false? editor)
          (let ([&width : (Boxof Nonnegative-Real) (box 0.0)]
                [&height : (Boxof Nonnegative-Real) (box 0.0)])
            (get-extent the-dc 0.0 0.0 &width &height)
            (values (real->double-flonum (unbox &width))
                    (real->double-flonum (unbox &height))))
          (let ([dc : (Instance DC<%>) (or (and master (send master get-dc)) the-dc)]
                [&x : (Boxof Real) (box 0.0)]
                [&y : (Boxof Real) (box 0.0)])
            ;;; WARNING: Client may have to call (resized) first
            (send editor get-snip-location this &x &y #false)
            (define-values (x y) (values (unbox &x) (unbox &y)))
            (send editor get-snip-location this &x &y #true)
            (values (flabs (real->double-flonum (- (unbox &x) x)))
                    (flabs (real->double-flonum (- (unbox &y) y)))))))
    
    (define/override (copy)
      (throw exn:fail:unsupported "this feature is disabled!"))
    
    (define/override (write /dev/edtout)
      (throw exn:fail:unsupported "this feature has not been implemented yet!"))))

(define game-zone% : Game-Zone%
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
      (if (and #false #;(nefertimon-editor-snip%? snip) (not (is-selected? snip)))
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
      (and #;(nefertimon-snip%? snip)
           (case operation
             #;[(hover)    (send snip on-hover  #true  event)]
             #;[(active)   (send snip on-active #true  event)]
             #;[(deactive) (send snip on-active #false event)]
             #;[(goodbye)  (send snip on-hover  #false event)]
             #;[(select)   (send snip on-select #true  event)]
             #;[(deselect) (send snip on-select #false event)]
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

    #;(define/public (enable ?)
      (let try-next : Void ([child : (Option (Instance Snip%)) (find-first-snip)])
        (unless (false? child)
          (cond [else #;(nefertimon-snip%? child) (send child enable ?)]
                #;[(nefertimon-editor-snip%? child) (send child enable ?)])
          (try-next (send child next)))))

    #;(define/public (find-next-editor-snip snip)
      (let try : (Option (Instance Nefertimon-Editor-Snip%))
        ([this-snip (if (false? snip) (find-first-snip) (send snip next))])
        (cond [(or (false? this-snip) (nefertimon-editor-snip%? this-snip)) this-snip]
              [else (try (send this-snip next))])))

    #;(define/public (find-previous-editor-snip snip)
      (let try : (Option (Instance Nefertimon-Editor-Snip%))
        ([this-snip (if (false? snip) (find-last-snip) (send snip previous))])
        (cond [(or (false? this-snip) (nefertimon-editor-snip%? this-snip)) this-snip]
              [else (try (send this-snip previous))])))

    #;(define/public (find-first-editor-snip) (find-next-editor-snip #false))
    #;(define/public (find-last-editor-snip) (find-previous-editor-snip #false))

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
        #;(when (and maybe-snip (send mouse moving?) (not (send mouse get-left-down)))
          (do-interactive-operation maybe-snip 'hover mouse)))
      #;(if (or (send mouse leaving?) (nor maybe-snip (is-a? maybe-snip game-snip%) #;(nefertimon-editor-snip%? maybe-snip)))
          (dtrace-info "kill all tooltips" #:topic 'tooltips #:urgent #false)
          (let*-values ([(_) (get-snip-location maybe-snip &editor-x &editor-y)]
                        [(dc-x dc-y) (editor-location-to-dc-location (unbox &editor-x) (unbox &editor-y))]
                        [(tips) (send maybe-snip get-tooltip (or (get-dc) the-dc) dc-x dc-y (unbox &editor-x) (unbox &editor-y) mouse)])
            (dtrace-info "show/hide tooltip" #:topic 'tooltips #:urgent tips))))

    (define/override (on-default-event mouse)
      (define maybe-snip : (Option (Instance Snip%))
        (let-values ([(editor-x editor-y) (dc-location-to-editor-location (send mouse get-x) (send mouse get-y))])
          (find-snip editor-x editor-y)))
      ;;; TODO: why (button-up?) does not make editor-snip release its caret?
      (when (send mouse button-down? 'left)
        (let deselect-snip : Void ([snip (find-next-selected-snip #false)])
          (unless (false? snip)
            (when (and #false #;(nefertimon-editor-snip%? snip) (not (eq? maybe-snip snip)))
              (remove-selected snip))
            (deselect-snip (find-next-selected-snip snip)))))
      (when (and maybe-snip #;(not (is-a? maybe-snip permanent-snip%)))
        (cond [(or (send mouse button-down? 'left)
                   (and (send mouse moving?)
                        (send mouse get-left-down)))
               (cond #;[(nefertimon-editor-snip%? maybe-snip) (set-selected maybe-snip)]
                     [else (void (do-interactive-operation maybe-snip 'active mouse))])]
              [(send mouse button-up? 'left)
               (set-selected maybe-snip)
               (void (do-interactive-operation maybe-snip 'deactive mouse))])))

    (define/override (on-char keyboard)
      (cond [(not (eq? (send keyboard get-key-code) #\tab)) (super on-char keyboard)]
            [else (let ([snip (find-next-selected-snip #false)])
                    #;(define next-snip : (Option (Instance Nefertimon-Editor-Snip%))
                      (if (send keyboard get-shift-down)
                          (if snip (find-previous-editor-snip snip) (find-last-editor-snip))
                          (if snip (find-next-editor-snip snip) (find-first-editor-snip))))
                    (cond [else #;(false? next-snip) (no-selected)]
                          #;[else (set-selected next-snip)]))]))

    (define/override (on-default-char keyboard)
      (unless (memq (send keyboard get-key-code)
                    '(left right up down #\backspace #\rubout))
        (super on-default-char keyboard)))
    
    (define/augment (on-insert snip before x y)
      (begin-edit-sequence)
      (inner (void) on-insert snip before x y))
    
    (define/augment (after-insert snip before x y)
      (inner (void) after-insert snip before x y)
      #;(when (game-snip%? snip) (send snip on-insert this))
      (end-edit-sequence))

    (define/augment (after-select snip on?)
      (inner (void) after-select snip on?)
      #;(when (nefertimon-editor-snip%? snip)
        (set-caret-owner (and on? snip))
        (define digivice : (Option (Instance Editor<%>)) (send snip get-editor))
        (unless (or (false? digivice) (not on?))
          (send digivice select-all))))
    
    (define/augment (on-delete snip)
      (begin-edit-sequence)
      (inner (void) on-delete snip))
    
    (define/augment (after-delete snip)
      #;(when (game-snip%? snip) (send snip on-delete this))
      (inner (void) after-delete snip)
      (end-edit-sequence))

    (define/public (on-elapse interval uptime)
      (void))

    (define/public (on-elapsed interval uptime elapsed)
      (void))))
