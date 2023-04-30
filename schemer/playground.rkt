#lang racket

(define (atom? x) (and (not (pair? x)) (not (null? x))))
(define (add1 x) (+ x 1))
(define (sub1 x) (- x 1))

;CHAPTER 3 ----

(define (lat? l)
  (cond
    ((null? l) #t)
    ((atom? (car l)) (lat? (cdr l)))
    (else #f)))

(define (member? e l)
  (cond
    ((null? l) #f)
    (else (or (eq? e (car l)) (member? e (cdr l))))))

(define (rember e l)
  (cond
    ((null? l) '())
    ((eq? e (car l)) (cdr l))
    (else (cons (car l) (rember e (cdr l))))))

(define (firsts l)
  (cond
    ((null? l) '())
    (else (cons (car (car l)) (firsts (cdr l))))))

(define (insertR new old l)
  (cond
    ((null? l) '())
    ((eq? old (car l)) (cons old (cons new (cdr l))))
    (else (cons (car l) (insertR new old (cdr l))))))

(define (insertL new old l)
  (cond
    ((null? l) '())
    ((eq? old (car l)) (cons new l))
    (else (cons (car l) (insertL new old (cdr l))))))

(define (subst new old l)
  (cond
    ((null? l) '())
    ((eq? old (car l)) (cons new (cdr l)))
    (else (cons (car l) (subst new old (cdr l))))))

(define (subst2 new o1 o2 l)
  (cond
    ((null? l) '())
    ((or (eq? o1 (car l)) (eq? o2 (car l))) (cons new (cdr l)))
    (else (cons (car l) (subst2 new o1 o2 (cdr l))))))

(define (multirember e l)
  (cond
    ((null? l) '())
    ((eq? e (car l)) (multirember e (cdr l)))
    (else (cons (car l) (multirember e (cdr l))))))

(define (multiinsertR new old l)
  (cond
    ((null? l) '())
    ((eq? old (car l)) (cons old (cons new (multiinsertR new old (cdr l)))))
    (else (cons (car l) (multiinsertR new old (cdr l))))))

(define (multiinsertL new old l)
  (cond
    ((null? l) '())
    ((eq? old (car l)) (cons new (cons old (multiinsertL new old (cdr l)))))
    (else (cons (car l) (multiinsertL new old (cdr l))))))

(define (multisubst new old l)
  (cond
    ((null? l) '())
    ((eq? old (car l)) (cons new (multisubst new old (cdr l))))
    (else (cons (car l) (multisubst new old (cdr l))))))

;CHAPTER 4 ----
(define (+ a b)
  (cond
    ((zero? a) b)
    (else (add1 (+ (sub1 a) b)))))

(define (- a b)
  (cond
    ((zero? b) a)
    (else (sub1 (- a (sub1 b))))))

(define (addtup tup)
  (cond
    ((null? tup) 0)
    (else (+ (car tup) (addtup (cdr tup))))))