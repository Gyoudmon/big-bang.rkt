#lang info

(define collection "STEM")
(define pkg-desc "STEM Laboratory")

(define version "Baby")
(define pkg-authors '("WarGrey Ju"))

(define digimon-idun "wargrey")

(define compile-omit-paths (list "stone"))
(define test-omit-paths 'all)

(define build-deps '("scribble-lib" "rackunit-lib" "racket-doc"))
(define deps '("base" "git://github.com/wargrey/digimon"
                      "git://github.com/wargrey/css"
                      "git://github.com/wargrey/schema"))

(define gracket-launcher-names (list "d-arc"))
(define gracket-launcher-libraries (list "digivice/d-arc.rkt"))
