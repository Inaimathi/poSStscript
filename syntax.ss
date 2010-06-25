#lang scheme
(require mzlib/defmacro)

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
        `(display-to-file ,contents ,(build-path filename) #:mode 'text #:exists 'replace)
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

(provide (all-defined-out))