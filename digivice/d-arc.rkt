#!/bin/sh

#|
# `raco setup` makes it hard to set other options,
# I have to keep the launch command simple.
exec racket -N "`basename $0 .rkt`" -t "$0" -- ${1+"$@"}
|#

#lang typed/racket/gui

(require digimon/gui)
(require digimon/dtrace)

(require digimon/system)
(require digimon/tongue)
(require digimon/location)
(require digimon/class)
(require digimon/collection)

(require "d-arc/widget.rkt")
(require "d-arc/headsup.rkt")

(require "d-arc/bigbang/universe.rkt")
(require "d-arc/bigbang/zone.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Game-Frame%
  (Class #:implements Frame%
         (init [label String #:optional] [parent (Option (Instance Frame%)) #:optional]
               [x (Option Positive-Integer) #:optional] [y (Option Positive-Integer) #:optional]
               [width Index #:optional] [height Index #:optional]
               [float? Boolean #:optional])
         (init-field [/dev/gtrace Logger #:optional])))

(define-type Game-Darc%
  (Class #:implements/inits Game-Frame%
         (init [construct-zones (-> (Listof (Instance Game-Zone%))) #:optional]
               [heads-up-zone (Option (Instance Heads-up-Zone%)) #:optional]
               [widget-width Index #:optional]
               [gtrace-level (Option Dtrace-Level) #:optional]
               [gtrace-receiver Dtrace-Receiver #:optional])
         (init-field [fullscreen? Boolean #:optional])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define game-frame% : Game-Frame%
  (class frame%
    (init [label ""] [parent #false]
          [x #false] [y #false] [width 0] [height 0]
          [float? #false])
    (init-field [/dev/gtrace /dev/dtrace])
    
    (super-new [label label] [parent parent] [x x] [y y] [width width] [height height]
               [style (if (not float?) '() '(no-caption no-system-menu float))]
               [min-width (and (> width 0) width)] [min-height (and (> height 0) height)]
               [stretchable-width (not float?)] [stretchable-height #true]
               [border 0] [spacing 0] [enabled #true] [alignment '(center top)])

    (define/override (on-subwindow-event target mouse)
      (and (send mouse leaving?)
           (let ([p (send this get-parent)])
             (and (typeof? p game-frame%)
                  (send p show #true) ; `(send p focus)` doesn't work
                  #true))))))

(define game-darc% : Game-Darc%
  (class game-frame% (super-new)
    (init [construct-zones list]
          [heads-up-zone #false]
          [widget-width 0]
          [gtrace-level #false]
          [gtrace-receiver dtrace-event-echo])
    (init-field [fullscreen? #true])

    (inherit-field /dev/gtrace)
    
    (define widget-frame : (Instance Game-Frame%)
      (new game-frame% [parent this] [float? #true]
           [width (if (> widget-width 0) widget-width 320)]
           [/dev/gtrace /dev/gtrace]))

    (define universe : (Instance Universe%)
      (new universe%
           [parent this]
           [construct-zones construct-zones]
           [heads-up-zone heads-up-zone]
           [/dev/gtrace /dev/gtrace]))

    (define widget : (Instance Universe%)
      (new universe%
           [parent widget-frame]
           [construct-zones (λ [] (list (new widget%)))]
           [/dev/gtrace /dev/gtrace]))
    
    (define/override (on-superwindow-show show?)
      (if (not show?) (on-hide) (on-show)))

    (define/override (on-subwindow-event target mouse)
      (if (send widget-frame is-shown?)
          (and (send mouse button-up? 'left)
               (hide-widget))
          (and (or (send mouse moving?) (send mouse button-up? 'left))
               (<= (send mouse get-x) 8)
               (show-widget))))

    (define/override (on-move x y)
      (when (send widget-frame is-shown?)
        (locate-widget)))

    (define/override (on-size width height)
      (when (send widget-frame is-shown?)
        (locate-widget)))

    (define/augment (can-close?)
      (for/fold ([can? #true])
                ([d-arc (in-list (get-top-level-windows))]
                 #:when (not (eq? d-arc this)))
        (send d-arc show #true)
        (bell)
        #false))

    (define/augment (on-close)
      (when (thread? gtrace)
        (log-message /dev/gtrace 'fatal "Done" eof)
        (thread-wait gtrace)))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (on-show) : Void
      (if (not fullscreen?)
          (send this maximize #true)
          (send this fullscreen #true))
      (send timeline start 0))

    (define (on-hide) : Void
      (send widget-frame show #false)
      (send timeline stop))

    (define (show-widget) : True
      (send widget-frame show #true)
      (on-move (send this get-x) (send this get-y))
      #true)

    (define (hide-widget) : True
      (send widget-frame show #false)
      #true)

    (define (locate-widget) : Void
      (let-values ([(_ this-height) (send this get-client-size)])
        (unless (eq? (send widget-frame get-height) this-height)
          (send widget-frame resize (send widget-frame get-width) this-height)))

      (let-values ([(x y) (send this client->screen 0 0)]
                   [(x-inset y-inset) (get-display-left-top-inset)])
        (send widget-frame move
              (- x (or x-inset 0))
              (- y (or y-inset 0)))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (on-elapse [interval : Nonnegative-Fixnum] [uptime : Nonnegative-Fixnum]) : Void
      (send universe on-elapse interval uptime)
      (send widget on-elapse interval uptime))

    (define (on-elapsed [interval : Nonnegative-Fixnum] [uptime : Nonnegative-Fixnum] [elapsed : Fixnum]) : Void
      (send universe on-elapsed interval uptime elapsed)
      (send widget on-elapsed interval uptime elapsed))

    (define timeline : (Instance GameTimer%)
      (new game-timer% [on-elapse on-elapse] [on-elapsed on-elapsed]))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define gtrace : (Option Thread)
      (and gtrace-level
           (thread (make-dtrace-loop (or gtrace-level 'note) /dev/gtrace
                                     #:exit-predicate? eof-object? #:display-exit-message? #false #:silent-topics dtrace-silent-topics
                                     #:false-receiver gtrace-receiver #:dtrace-receiver gtrace-receiver
                                     #:topic-receivers null #:default-receiver gtrace-receiver))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (enter-digimon-zone!)
  
  (current-digivice (symbol->string (#%module)))

  (current-tongue 'zh)
  (default-fallback-tongue 'en)
  (default-tongue-paths (list (digimon-path 'tongue)))
  
  (define d-arc
    (new game-darc% [label (current-digimon)] [fullscreen? #false]
         [heads-up-zone (new headsup%)]
         [/dev/gtrace /dev/dtrace]
         [gtrace-level 'trace]))

  (send d-arc show #true))
