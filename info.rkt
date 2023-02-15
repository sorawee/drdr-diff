#lang info
(define collection "drdr-diff")
(define deps '("http-easy-lib"
               "text-table"
               "base"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/drdr-diff.scrbl" ())))
(define pkg-desc "Description Here")
(define version "0.0")
(define pkg-authors '(sorawee))
(define license '(Apache-2.0 OR MIT))
