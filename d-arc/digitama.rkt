#lang typed/racket/gui

(provide (all-defined-out))
(provide (all-from-out css css/sugar))

(require css)
(require css/sugar)

(require digimon/system)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(default-css-media-type 'screen)
(default-css-media-features
  (let-values ([(w h) (get-display-size)])
    (make-css-media-features #:width (real->double-flonum (or w 0))
                             #:height (real->double-flonum (or h 0))
                             #:aspect-ratio (if (and (exact-positive-integer? w) (exact-positive-integer? h)) (/ w h) 1)
                             #:resolution (real->double-flonum (or (get-display-backing-scale) 1.0))
                             #:update 'fast
                             #:overflow-block 'scroll
                             #:overflow-inline 'scroll
                             #:color (get-display-depth)
                             #:pointer 'fine #:any-pointer 'fine
                             #:hover 'hover #:any-hover 'hover)))

(css-root-element-type (string->symbol (current-digivice)))
(current-css-element-color 'DarkSlateGray)

(default-css-feature-support? ; see /stone/nefertimon.css
  (λ [[desc : Symbol] [value : Any]]
    (implies (equal? (cons 'font-variant-ligatures 'normal)
                     (cons desc value))
             (not (eq? (system-type)
                       'macosx)))))

#|
(define digivice-parsers : CSS-Declaration-Parsers
  (lambda [suitcased-name deprecated!]
    (or (css-font-parsers suitcased-name deprecated!)
        (css-color-property-parsers suitcased-name null)
        (css-simple-box-property-parsers suitcased-name))))

(define digivice-filter : (CSS-Cascaded-Value-Filter (List Font Pen Brush (HashTable Symbol Any)))
  (lambda [declares inherits]
    (call-with-css-box declares inherits
      #:with [digivice-size digivice-size-ref digivice-color-ref digivice-icon-ref digivce-ref]
      (define-values (width height) (digivice-size (css-vw) (css-vh)))
      (list (default-css-font)
            (css-extract-border-pen declares inherits 'top)
            (css-extract-background-brush declares inherits)
            ((inst make-hasheq Symbol Any)
             (list (cons 'width width)
                   (cons 'height height)
                   (cons 'vertical-inset (digivice-size-ref 'padding-top height))
                   (cons 'horizontal-inset (digivice-size-ref 'padding-right width))
                   (cons 'vertical-margin (digivice-size-ref 'margin-top height))
                   (cons 'horizontal-margin (digivice-size-ref 'margin-right width))))))))

(define digivice.css : CSS-StyleSheet
  (with-handlers ([exn? (λ [[e : exn]] (dtrace-warning (exn-message e)) (read-css-stylesheet (exn-message e)))])
    (define path.css : Path (digivice-path #".css" 'stone))
    (define css : CSS-StyleSheet (read-css-stylesheet path.css))
    (bitmap-icon (digimon-path 'icon "CSS.png") #:dtrace (format "Loaded ~a" path.css))
    css))

(define nefertimon.css : (Listof+ CSS-StyleSheet) (list digivice.css))

(define ~root:n : CSS-Subject (make-css-subject #:classes '(digivice) #::classes `(,(current-tongue))))
(define ~root:s : CSS-Subject (struct-copy css-subject ~root:n [:classes `(selected ,(current-tongue))]))
(define-values ($root:n *root:n) (css-cascade nefertimon.css (list ~root:n) digivice-parsers digivice-filter #false))
(define-values ($root:s *root:s) (css-cascade nefertimon.css (list ~root:s) digivice-parsers digivice-filter #false))

(let ([default-face : (U String Symbol) (or (send (car $root:n) get-face) (send (car $root:n) get-family))])
  (default-css-font (car $root:n))
  (default-css-caption-font (make-css-font normal-control-font #:family default-face))
  (default-css-icon-font (make-css-font tiny-control-font #:family default-face))
  (default-css-menu-font (make-css-font menu-control-font #:family default-face))
  (default-css-message-box-font (default-css-font))
  (default-css-small-caption-font (make-css-font small-control-font #:family default-face))
  (default-css-status-bar-font (make-css-font view-control-font #:family default-face))
  (default-css-pen (cadr $root:n))
  (default-css-brush (caddr $root:n)))
|#
