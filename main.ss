#lang scheme
;;psPrelim.ss
(require mzlib/defmacro)

;;;Syntax declarations
(define (page . body)
  (lambda (page-num page-total b1 b2 b3 b4)
    (string-append (format "%%Page: ~a ~a~n%%BeginPageSetup~n%%PageBoundingBox: ~a ~a ~a ~a~n%%EndPageSetup~n"
                           page-num page-total b1 b2 b3 b4)
                   (apply string-append body)
                   "showpage\n\n")))

(define-macro (ps filename bounding-box . pages)
  (define (apply-page page num)
    `(,page ,num ,(length pages) ,@bounding-box))
  (let ((contents `(string-append
                    (format "%!PS-Adobe-3.0~n%%Pages: ~a~n%%BoundingBox: ~a ~a ~a ~a~n%%DocumentData: Clean7Bit~n%%LanguageLevel: 2~n~n"
                            ,(length pages) ,@bounding-box)
                    ,@(map apply-page pages (build-list (length pages) (lambda (n) (+ 1 n)))))))
    (if filename
        (let ((destination (cond ((string? filename) (string->path filename))
                                 ((path? filename) filename))))
          `(display-to-file ,contents ,destination #:mode 'text #:exists 'replace))
        `(printf ,contents))))

(define-macro (def-pt-op name op)
  `(define (,name pt term)
     (cond ((number? term) (cons (,op (car pt) term) (,op (cdr pt) term)))
           ((pair? term) (cons (,op (car pt) (car term)) (,op (cdr pt) (cdr term))))
           (else pt))))

(define-macro (def-keyword name . args)
  `(define (,name . arguments)
     (if (null? arguments)
         ,(format "~a~n" name)
         (apply (lambda ,args
                  (format ,(string-append (apply string-append (build-list (length args) (lambda (n) "~a "))) (symbol->string name) "~n")
                          ,@args)) arguments))))

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

;;;PostScript Primitive declarations
;;Point operations (because they're really REALLY common
(def-pt-op pt- -)
(def-pt-op pt+ +)

;;Primitives
(define (show msg) (format "(~a) show~n" msg))
(define (charpath flag msg) (format "(~a) ~a charpath~n" msg (if flag "true" "false")))

(define (font name size)
  (format "/~a findfont ~a scalefont setfont~n" name size))

(define (for start step stop . body)
  (format "~a ~a ~a{~n ~a}for~n" start step stop (apply string-append body)))

(def-keyword setlinewidth width)
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

;;;Complex primitives and light abstractions
(define (color . a-color)
  (define (break-string string num)
    (cond ((< (string-length string) num) '())
          (else (cons (substring string 0 num)
                      (break-string (substring string num) num)))))
  (define (digit->color% d)
    (string->number (real->decimal-string (/ (string->number d 16) 255))))
  (let ((c (if (string? (car a-color))
               (map digit->color% (break-string (car a-color) 2))
               a-color)))
    (cond ((= (length c) 3) (apply setrgbcolor c))
          ((= (length c) 4) (apply setcymkcolor c)))))

(define (text pt message
              #:font (a-font (font "Helvetica" 14))
              #:stroke-width (stroke-width #f)
              #:stroke-color (stroke-color #f)
              #:color (a-fill "000000"))
  (with (color a-fill) a-font
        (moveto pt)
        (fill (charpath #t message))
        (moveto pt)
        (if (or stroke-color stroke-width)
            (with (if stroke-color (color stroke-color) "")
                  (if stroke-width (setlinewidth stroke-width) "")
                  (stroke (charpath #f message)))
            "")))

(define (shape pts)
  (path (moveto (car pts))
        (apply string-append (map lineto (cdr pts)))))

(define (curve-shape pts)
  (path (moveto (car pts))
        (apply string-append (map curveto(cdr pts)))))

(define (rounded-rect pt width height radius)
  (let* ((rad (cond ((number? radius) (build-list 4 (lambda (n) radius)))
                    ((= 2 (length radius)) (list (cadr radius) (car radius) (car radius) (cadr radius)))
                    (else (take radius 4))))
         ;;;These are out of order so that the radius can be in clockwise order from the bottom left 
         (r1 (first rad)) (r2 (fourth rad)) (r3 (third rad)) (r4 (second rad))
         (c1 (pt+ pt r1))
         (c2 (pt+ (pt+ pt `(,width . 0)) `(,(- r2) . ,r2)))
         (c3 (pt- (pt+ pt `(,width . ,height)) r3))
         (c4 (pt+ (pt+ pt `(0 . ,height)) `(,r4 . ,(- r4)))))
    (path (arc c1 r1 180 270)
          (arc c2 r2 270 360)
          (arc c3 r3 0 90)
          (arc c4 r4 90 180))))

(define (rect pt width height)
  (let* ((x (car pt)) (y (cdr pt))
         (wx (+ x width)) (hy (+ y height)))
    (shape `(,pt ,(cons x hy) ,(cons wx hy) ,(cons wx y)))))

(define (square pt width)
  (shape `(,pt ,(pt+ pt (cons 0 width)) ,(pt+ pt width) ,(pt+ pt (cons width 0)))))

(define (circle pt radius)
  (path (arc pt radius 0 360)))

(define (table pt columns)
  (text pt "Hello" #:font (font "Helvetica" 32) #:color "000000"))

;; (ps "test0.ps" (0 0 612 792)
;;     (page 
;;      (translate 500 500)
;;      (for 0 10 360
;;           (scale 1.1 1.1)
;;           (with (rotate)
;;                 (stroke (circle '(200 . 300) 100)))))
;;     (page (stroke (circle '(150 . 50) 150)))
;;     (page (stroke (circle '(50 . 0) 200))))

;; (ps "test1.ps" (0 0 612 792)
;;       (page (translate 50 50)
;;             (table '(100 . 100) '(la la la la))
;;             (stroke (square '(0 . 0) 100))))

;; (ps "test2.ps" (0 0 612 792)
;;     (page (text-field '(50 . 50) "Hello")))

(provide (all-defined-out))