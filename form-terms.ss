#lang scheme

(require "main.ss"
         mzlib/pregexp)
;; (require "main.ss" "form-terms.ss")
(define (table pt columns)
  (text pt "Hello" #:font (font "Helvetica" 32) #:color "000000"))

(define (text-field point label #:font (label-font (font "Helvetica" 6)) #:width (width #f) #:height (height 20))
  (let* ((font-size (string->number (second (pregexp-match #px"(\\d+?) scalefont" label-font))))
         (box-width (if width width (* 2 font-size (string-length label)))))
    (with (translate (car point) (cdr point))
          (stroke (rect '(0 . 0) box-width height))
          (text '(3 . 3) label #:font label-font #:color "666666"))))
      
(provide (all-defined-out))