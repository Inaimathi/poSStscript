#lang scheme
(require "main.ss"
         "syntax.ss"
         "primitives.ss")

(define (text-field point label #:font (a-font (font "Helvetica" 14)) #:width (width #f) #:height (height 20))
      (with (translate (car point) (cdr point))
            (stroke (rect '(0 . 0) width height))
            (with a-font (color "666666")
                  (text '(3 . 3) label))))
      
(provide (all-defined-out))