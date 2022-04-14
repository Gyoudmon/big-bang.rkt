#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/class)

(require "bigbang/zone.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define widget% : Game-Zone%
  (class game-zone% (super-new)
    (define/override (draw dc x y width height)
      (send dc draw-line x y width height))))
