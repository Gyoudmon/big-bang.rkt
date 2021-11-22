#lang typed/racket

(provide (all-defined-out))

(require "schema.rkt")
(require "card.rkt")

(define-type Make-Sidebar-Snip (->* () (UUPK String Schema-Status) (Instance Sidebar-Snip%)))

(define-type Sidebar-Snip%
  (Class #:implements/inits Schema-Snip%
         (init-field [$normal Sidebar.CSS]
                     [$selected Sidebar.CSS]
                     [$icons (HashTable Symbol Bitmap)]
                     [figure Bitmap]
                     [tongues (Pairof String String)]
                     [statistics (-> Any)])))

(define-type Sidebar-Zone%
  (Class #:implements Digivice-Zone%
         (init [struct:table Struct-TypeTop])
         (init-field [make-schema-snip Make-Sidebar-Snip])
         (init [do-select (-> (Instance Digivice-Zone%) Snip Any) #:optional]
               [do-deselect (-> (Instance Digivice-Zone%) Snip Any) #:optional])
         [find-schema-snip (-> UUPK (Option (Instance Sidebar-Snip%)))]
         [change-schema-status (->* (UUPK Schema-Status) (String #:label (Option String)) Void)]
         [card-slash (-> String (Instance Schema-Card%) Any (Option UUPK) Void)]
         [insert-schema (-> UUPK String Void)]
         [merge-schema (-> UUPK String (U Schema-Status Any) Void)]
         [delete-schema (-> UUPK Void)]))

(define sidebar-zone% : Sidebar-Zone%
  (class digivice-zone% (inherit find-first-snip ordered-insert delete)
    (init struct:table)
    (init-field make-schema-snip)
    (init [do-select void] [do-deselect void])

    (super-make-object
     'vertical 0
     (λ [[zone : (Instance Digivice-Zone%)] [snip : Snip] [op : Digivice-Operation] [e : Digivice-Event]]
       (cond [(mouse%? e)
              (when (and (symbol=? op 'deactive) (sidebar-snip%? snip))
                (define maybe-cursor : (Option (Instance Cursor%)) (send zone adjust-cursor e))
                (when (eq? maybe-cursor hand.cur)
                  (schema-log-message (make-schema-message #:urgent (get-field uuid snip)
                                                           #:brief `("SELECT * FROM ~a WHERE uuid = $?" ,(object-name struct:table))
                                                           struct:table 'select))))]
            [(eq? op 'select) (do-select zone snip)]
            [(eq? op 'deselect) (do-deselect zone snip)])))
    
    (define/public (find-schema-snip uuid)
      (let try-next ([this-snip (send this find-first-snip)])
        (and (sidebar-snip%? this-snip)
             (or (and (uupk= (get-field uuid this-snip) uuid) this-snip)
                 (try-next (send this-snip next))))))

    (define/public (change-schema-status uuid status [tooltip ""] #:label [label #false])
      (define snip : (Option (Instance Sidebar-Snip%)) (find-schema-snip uuid))
      (unless (false? snip)
        (unless (false? label) (send snip change-caption label))
        (send snip change-status status tooltip)))

    (define/public (card-slash title zone src uuid)
      (send infocard slash title zone src)
      (change-schema-status (or uuid nilpk) 'normal))
    
    (define/public (insert-schema uuid tibt-name)
      (dtrace-debug (format "Loaded ~a" tibt-name) #:topic 'splash)
      (ordered-insert schema-snip<? (make-schema-snip uuid tibt-name 'normal)))
    
    (define/public (merge-schema uuid tibt-name status)
      (define snip : (Option (Instance Sidebar-Snip%)) (find-schema-snip uuid))
      (cond [(false? snip) (ordered-insert schema-snip<? (make-schema-snip uuid tibt-name 'saving))]
            [else (change-schema-status uuid (if status 'normal 'saving) #:label tibt-name)]))
    
    (define/public (delete-schema uuid)
      (define snip : (Option (Instance Sidebar-Snip%)) (find-schema-snip uuid))
      (unless (false? snip) (delete snip)))))

(define/make-is-a? sidebar-snip% : Sidebar-Snip%
  (class schema-snip% (super-new)
    (init-field $normal $selected $icons figure tongues statistics)

    (inherit selected? refresh-now get-message)
    (inherit-field uuid caption status)

    (define figure-size : Real (send figure get-height))
    
    (define/override (adjust-cursor dc x y editor-x editor-y mouse)
      (define-values (mx my) (values (send mouse get-x) (send mouse get-y)))
      (define style : Sidebar.CSS (if (selected?) $selected $normal))
      (cond [(and (<= (+ x (sidebar-horizontal-inset style)) mx (+ x (sidebar-horizontal-inset style) figure-size))
                  (<= (+ y (sidebar-vertical-inset style)) my (+ y (sidebar-vertical-inset style) figure-size)))
             hand.cur]
            [(and (hash-has-key? $icons status)
                  (let-values ([(status.icon icon-x icon-y icon-width icon-height) (get-status-icon-info style x y)])
                    (and (<= icon-x mx (+ icon-x icon-width)) (<= icon-y my (+ icon-y icon-height)))))
             default.cur]
            [else #false]))

    (define/override (get-tooltip dc x y editor-x editor-y mouse)
      (define cursor : (Option (Instance Cursor%)) (adjust-cursor dc x y editor-x editor-y mouse))
      (cond [(eq? cursor hand.cur) (cons (if (nilpk? uuid) (car tongues) (cdr tongues)) 'info)]
            [(and (eq? cursor default.cur) (get-message)) => values]
            [else (let ([stats (statistics)])
                    (cond [(not (string? stats)) (cons caption 'info)]
                          [else (cons (format "~a~n~a" caption stats) 'info)]))]))

    (define/override (get-extent dc x y [&w #false] [&h #false] [&d #false] [&s #false] [&l #false] [&r #false])
      (fill-box! [&w &h] [(sidebar-width $normal) (sidebar-height $normal)])
      (fill-box! [&l &r] = (sidebar-horizontal-inset $normal))
      (fill-box! [&d &s] = (sidebar-vertical-inset $normal)))

    (define/override (draw dc x y left top right bottom dx dy draw-caret)
      (define style : Sidebar.CSS (if (selected?) $selected $normal))
      (define-values (label-width label-height) (send (sidebar-font style) get-text-size caption))
      (define label-x : Real (+ x figure-size (* (sidebar-horizontal-inset style) 2)))
      (define label-y : Real (+ y (* 1/2 (- (sidebar-height style) label-height))))
      (define saved-pen : (Instance Pen%) (send dc get-pen))
      (send dc set-smoothing 'aligned)
      (if (eq? style $selected)
          (send* dc
            (set-pen (sidebar-background-color style) 1 'solid)
            (set-brush (sidebar-background-color style) 'solid)
            (draw-rectangle x y (sidebar-width style) (sidebar-height style)))
          (let ([line-width : Real (- (sidebar-width style) figure-size (* (sidebar-horizontal-inset style) 4))])
            (send* dc
              (set-pen (sidebar-separator-color style) 1 'solid)
              (draw-line label-x (+ y (sidebar-height style) -1) (+ label-x line-width) (+ y (sidebar-height style) -1)))))
      (send* dc
        (set-font (sidebar-font style))
        (set-text-foreground (sidebar-color style))
        (draw-bitmap figure (+ x (sidebar-horizontal-inset style)) (+ y (sidebar-vertical-inset style)))
        (draw-text caption label-x label-y (send (sidebar-font style) should-combine?)))
      (when (hash-has-key? $icons status)
        (define-values (icon icon-x icon-y _w _h) (get-status-icon-info style x y))
        (send dc draw-bitmap icon icon-x icon-y))
      (send dc set-pen saved-pen))

    (define/private (get-status-icon-info [style : Sidebar.CSS] [x : Real] [y : Real])
      : (Values Bitmap Real Real Nonnegative-Real Nonnegative-Real)
      (define icon : Bitmap (hash-ref $icons status))
      (define-values (icon-width icon-height) (bitmap-size icon))
      (values icon
              (+ x (- (sidebar-width style) icon-width (* (sidebar-horizontal-inset style) 2)))
              (+ y (* 1/2 (- (sidebar-height style) icon-height)))
              icon-width
              icon-height))))
