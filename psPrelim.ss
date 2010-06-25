;;psPrelim.ss
(require mzlib/defmacro)

(define (shape pts)
  (path (moveto (car pts))
        (apply string-append (map lineto (cdr pts)))))

;; (define (rounded-rect pt width height radius)
;;   (let* ((x (car pt)) (y (cdr pt))
;;         (wx (+ x width)) (hy (+ y height)))
;;     (arc pt 
;;     )

(define (rect pt width height)
  (let* ((x (car pt)) (y (cdr pt))
        (wx (+ x width)) (hy (+ y height)))
    (shape `(,pt ,(cons x hy) ,(cons wx hy) ,(cons wx y)))))

(define (square pt width)
    (shape `(,pt ,(pt+ pt (cons 0 width)) ,(pt+ pt width) ,(pt+ pt (cons width 0)))))

(define (circle pt radius)
  (path (arc pt radius 0 360)))

;;;Utility
(define (ps . body)
  (printf "%!PS~n")
  (printf (apply string-append body))
  (printf "showpage~n"))

(define-macro (def-pt-op name op)
  `(define (,name pt term)
     (cond ((number? term) (cons (,op (car pt) term) (,op (cdr pt) term)))
           ((pair? term) (cons (,op (car pt) (car term)) (,op (cdr pt) (cdr pt))))
           (else pt))))

(define-macro (def-keyword name . args)
  `(define (,name ,@args)
       (format ,(string-append (apply string-append (build-list (length args) (lambda (n) "~a "))) (symbol->string name) "~n")
               ,@args)))

(define-macro (def-point-fn name pts . args)
  (define (pt-args pts)
    (cond ((null? pts) '())
          (else (cons `(car ,(car pts)) (cons `(cdr ,(car pts)) (pt-args (cdr pts)))))))
  `(define (,name ,@pts ,@args)
     (format ,(string-append (apply string-append (build-list (+ (* 2 (length pts)) (length args)) (lambda (n) "~a "))) (symbol->string name) "~n")
             ,@(pt-args pts)
             ,@args)))

(define-macro (def-block name close open)
  `(define (,name . body)
     (string-append (string-append ,open "\n") (apply string-append body) (string-append ,close "\n"))))

;;;Point operations (because they're really REALLY common
(def-pt-op pt- -)
(def-pt-op pt+ +)

;;;Primitives
(define (text message)
  (path (format "(~a) show~n" message)))

(define (font name size)
  (format "/~a findfont ~a scalefont setfont~n" name size))

(define (loop start step stop . body)
  (format "~a ~a ~a{~n ~a}for~n" start step stop (apply string-append body)))
  
(def-keyword setrgbcolor r g b)
(def-keyword cmyk c m y k)
(def-keyword translate x y)
(def-keyword rotate d)
(def-keyword scale x y)

(def-point-fn arc (pt) radius start-degree end-degree)
(def-point-fn moveto (pt))
(def-point-fn lineto (pt))
(def-point-fn curveto (pt1 pt2 pt3))

(def-block path "closepath" "newpath")
(def-block save "grestore" "gsave")
(def-block fill "fill" "")
(def-block stroke "stroke" "")