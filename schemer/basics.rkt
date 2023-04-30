#lang racket

(define (atom? x) (and (not (pair? x)) (not (null? x))))
(define (add1 x) (+ x 1))
(define (sub1 x) (- x 1))

(provide atom?)
(provide add1)
(provide sub1)