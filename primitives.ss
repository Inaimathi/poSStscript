#lang scheme
(require "syntax.ss")

;;;Point operations (because they're really REALLY common
(def-pt-op pt- -)
(def-pt-op pt+ +)

;;;Primitives
(define (text message)
  (path (format "(~a) show~n" message)))

(define (font name size)
  (format "/~a findfont ~a scalefont setfont~n" name size))

(define (for start step stop . body)
  (format "~a ~a ~a{~n ~a}for~n" start step stop (apply string-append body)))
  
(def-keyword setrgbcolor r g b)
(def-keyword setcymkcolor c m y k)
(def-keyword translate x y)
(def-keyword rotate d)
(def-keyword scale x y)

(def-point-fn arc (pt) radius start-degree end-degree)
(def-point-fn moveto (pt))
(def-point-fn lineto (pt))
(def-point-fn curveto (pt1 pt2 pt3))

(def-block path "closepath" "newpath")
(def-block with "grestore" "gsave")
(def-block fill "fill" "")
(def-block stroke "stroke" "")

(provide (all-defined-out))