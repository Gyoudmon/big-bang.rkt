#!/bin/sh

#|
# `raco setup` makes it hard to set other options,
# I have to keep the launch command simple.
exec racket -N "`basename $0 .rkt`" -t "$0" -- ${1+"$@"}
|#

#lang typed/racket/gui

(require digimon/gui)

(require digimon/system)
(require digimon/tongue)
(require digimon/location)

(require bitmap/base)
(require bitmap/constructor)

(require "d-arc/digicore.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(current-digimon "STEM")
(current-digivice (symbol->string (#%module)))

(current-tongue 'zh)
(default-fallback-tongue 'en)
(default-tongue-paths (list (digimon-path 'tongue)))

(define toolbar-background-color : Color (rgb* 'white))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type GameFrame%
  (Class #:implements Frame%
         (init [label String #:optional] [parent (Option (Instance Frame%)) #:optional]
               [x (Option Positive-Integer) #:optional] [y (Option Positive-Integer) #:optional]
               [width (Option Positive-Integer) #:optional] [height (Option Positive-Integer) #:optional]
               [float? Boolean #:optional])
         (init-field [inset Byte #:optional])
         (field [#%editor (Instance Game-Zone%)]
                [#%canvas (Instance Editor-Canvas%)])

         [on-elapse (-> Nonnegative-Fixnum Nonnegative-Fixnum Void)]
         [on-elapsed (-> Nonnegative-Fixnum Nonnegative-Fixnum Fixnum Void)]))

(define game-frame% : GameFrame%
  (class frame%
    (init [label ""] [parent #false]
          [x #false] [y #false] [width #false] [height #false]
          [float? #false])

    (init-field [inset 8])
    
    (super-new [label label] [parent parent] [x x] [y y] [width width] [height height]
               [style (if (not float?) '() '(no-caption no-system-menu float))]
               [min-width width] [min-height height] [stretchable-width #true] [stretchable-height #true]
               [border 0] [spacing 0] [enabled #true] [alignment '(center top)])

    (field [#%editor (new game-zone%)]
           [#%canvas (instantiate editor-canvas% (this #%editor '(no-border no-hscroll no-vscroll))
                       [wheel-step #false] [vertical-inset inset] [horizontal-inset inset])])

    (define/public (on-elapse interval uptime)
      (send #%editor on-elapse interval uptime))

    (define/public (on-elapsed interval uptime elapsed)
      (send #%editor on-elapsed interval uptime elapsed))))

(define split-frame%
  (class game-frame% (super-new)
    (define widget : (Instance GameFrame%) (new game-frame% [width 320] [float? #true]))
    
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
      (send this maximize #true)
      (send timeline start 0))

    (define/private (on-hide) : Void
      (send widget show #false)
      (send timeline stop))

    (define/private (show-widget) : True
      (send widget show #true)
      (on-move (send this get-x) (send this get-y))
      #true)

    (define/private (hide-widget) : True
      (send widget show #false)
      #true)

    (define/private (locate-widget) : Void
      (let-values ([(_ this-height) (send this get-client-size)])
        (unless (eq? (send widget get-height) this-height)
          (send widget resize (send widget get-width) this-height)))

      (let-values ([(x y) (send this client->screen 0 0)]
                   [(x-inset y-inset) (get-display-left-top-inset)])
        (send widget move
              (- x (or x-inset 0))
              (- y (or y-inset 0)))))

    (define (on-elapse [interval : Nonnegative-Fixnum] [uptime : Nonnegative-Fixnum]) : Void
      (send this on-elapse interval uptime)
      (send widget on-elapse interval uptime))

    (define (on-elapsed [interval : Nonnegative-Fixnum] [uptime : Nonnegative-Fixnum] [elapsed : Fixnum]) : Void
      (send this on-elapsed interval uptime elapsed)
      (send widget on-elapsed interval uptime elapsed))

    (define timeline : (Instance GameTimer%) (new game-timer% [on-elapse on-elapse] [on-elapsed on-elapsed]))))



(module+ main
  (parameterize ([current-directory (digimon-path 'digivice)]
                 [current-command-line-arguments (vector)])
    (let ([d-arc (new split-frame% [label (current-digimon)])])
      (send d-arc show #true))))
