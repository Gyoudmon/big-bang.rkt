#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/class)

(require "bigbang/zone.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define headsup% : Heads-up-Zone%
  (class heads-up-zone% (super-new)
    (define _uptime : Natural 0)
    
    (define/override (draw dc x y width height)
      (send dc draw-text (number->string _uptime) 0 0))

    (define/override (on-elapse interval uptime)
      (set! _uptime uptime))))
