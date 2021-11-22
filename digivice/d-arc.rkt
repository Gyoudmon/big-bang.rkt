#!/bin/sh

#|
# `raco setup` makes it hard to set other options,
# I have to keep the launch command simple.
exec racket -N "`basename $0 .rkt`" -t "$0" -- ${1+"$@"}
|#

#lang typed/racket/gui

(require digimon/system)
(require digimon/tongue)
(require digimon/location)

(require bitmap/base)
(require bitmap/constructor)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(current-digimon "Nefertimon")
(current-digivice (symbol->string (#%module)))

(current-tongue 'zh)
(default-fallback-tongue 'en)
(default-tongue-paths (list (digimon-path 'tongue)))

(define toolbar-background-color : Color (rgb* 'white))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Editor-Frame%
  (Class #:implements Frame%
         (init [label String #:optional] [parent (Option (Instance Frame%)) #:optional]
               [x (Option Positive-Integer) #:optional] [y (Option Positive-Integer) #:optional]
               [width (Option Positive-Integer) #:optional] [height (Option Positive-Integer) #:optional]
               [float? Boolean #:optional])
         (init-field [inset Byte #:optional])
         (field [#%editor (Instance Pasteboard%)]
                [#%canvas (Instance Editor-Canvas%)])))

(define editor-frame% : Editor-Frame%
  (class frame%
    (init [label ""] [parent #false]
          [x #false] [y #false] [width #false] [height #false]
          [float? #false])

    (init-field [inset 8])
    
    (super-new [label label] [parent parent] [x x] [y y] [width width] [height height]
               [style (if (not float?) '() '(no-caption no-system-menu float))]
               [min-width width] [min-height height] [stretchable-width #true] [stretchable-height #true]
               [border 0] [spacing 0] [enabled #true] [alignment '(center top)])

    (field [#%editor (new pasteboard%)]
           [#%canvas (instantiate editor-canvas% (this #%editor '(no-border no-hscroll no-vscroll))
                       [wheel-step #false] [vertical-inset inset] [horizontal-inset inset])])))

(define split-frame%
  (class editor-frame% (super-new)
    (define widget : (Instance Editor-Frame%) (new editor-frame% [width 320] [float? #true]))
    
    (define/override (on-superwindow-show show?)
      (if (not show?) (on-hide) (on-show)))

    (define/override (on-subwindow-event target mouse)
      (if (send widget is-shown?)
          (and (send mouse button-up? 'left)
               (hide-widget))
          (and (or (send mouse moving?) (send mouse button-up? 'left))
               (<= (send mouse get-x) (get-field inset this))
               (show-widget))))

    (define/override (on-move x y)
      (when (send widget is-shown?)
        (locate-widget)))

    (define/override (on-size width height)
      (when (send widget is-shown?)
        (locate-widget)))

    (define/private (on-show) : Void
      (send this maximize #true))

    (define/private (on-hide) : Void
      (send widget show #false))

    (define/private (show-widget) : True
      (send widget show #true)
      (on-move (send this get-x) (send this get-y))
      #true)

    (define/private (hide-widget) : True
      (send widget show #false)
      #true)

    (define/private (locate-widget) : Void
      (define full-height (send this get-height))
      (define-values (_ this-height) (send this get-client-size))
      
      (unless (eq? (send widget get-height) this-height)
        (send widget resize (send widget get-width) this-height))
      
      (send widget move
            (send this get-x)
            (+ (send this get-y) (- full-height this-height))))))



(module+ main
  (parameterize ([current-directory (digimon-path 'digivice)]
                 [current-command-line-arguments (vector)])
    (send (new split-frame% [label "STEM"]) show #true)))
