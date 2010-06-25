#lang scheme
;;psPrelim.ss
(require mzlib/defmacro
         "syntax.ss"
         "primitives.ss")

(define (shape pts)
  (path (moveto (car pts))
        (apply string-append (map lineto (cdr pts)))))

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
    (path 
     (arc c1 r1 180 270)
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

;; (ps (0 0 612 792)
;;     (page (stroke (circle '(200 . 300) 100)))
;;     (page (stroke (circle '(150 . 50) 150)))
;;     (page (stroke (circle '(50 . 0) 200))))

(provide (all-defined-out))