#lang racket
(require eopl)

(define-datatype ast ast?
  [num (value number?)]
  [add (left ast?) (right ast?)]
  [sub (left ast?) (right ast?)]
  [mul (left ast?) (right ast?)])

(define eval
  (lambda (tree)
    (cases ast tree
      (num (value) value)
      (add (left right) (+ (eval left) (eval right)))
      (sub (left right) (- (eval left) (eval right)))
      (mul (left right) (* (eval left) (eval right))))))

(define keywords    '(+ - *))
(define constructors (list add sub mul))

(define parse
  (lambda (sexp)
    (cond
      [(number? sexp) (num sexp)]
      [(and
        (list? sexp)
        (= 3 (length sexp))
        (memq (first sexp) keywords))
       (let*
           ([keyword     (first sexp)]
            [left        (second sexp)]
            [right       (third sexp)]
            [constructor (list-ref constructors (index-of keywords keyword))])
         (constructor (parse left) (parse right)))]
      [else (error 'parse "invalid input ~a" sexp)])))

(define go
  (lambda (sexp)
    (eval (parse sexp))))
