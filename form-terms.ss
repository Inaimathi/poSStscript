#lang scheme

(require "main.ss"
         mzlib/pregexp)
;; (require "main.ss" "form-terms.ss")
(define (table pt columns)
  (text pt "Hello" #:font (font "Helvetica" 32) #:color "000000"))

(define (radio-button pt label)
  (let ((radius 5))
    (with (stroke (circle (pt+ pt radius) radius))
          (text (pt+ pt `(,(* 3 radius) . 0)) label #:font (font "Helvetica" 10)))))

(define (check-box pt label)
  (let ((width 10))
    (with (stroke (square pt width))
          (text (pt+ pt `(,(round (* 1.5 width)) . 0)) label #:font (font "Helvetica" 10)))))

(define (text-field pt label #:font (label-font (font "Helvetica" 6)) #:width (width #f) #:height (height 20))
  (let* ((font-size (string->number (second (pregexp-match #px"(\\d+?) scalefont" label-font))))
         (box-width (if width width (* 2 font-size (string-length label)))))
    (with (stroke (rect pt box-width height))
          (text (pt+ pt `(3 . ,(- height (+ 2 font-size)))) label #:font label-font #:color "666666"))))
      
(provide (all-defined-out))