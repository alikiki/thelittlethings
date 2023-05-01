#lang racket
(require "basics.rkt")

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

(define (* a b)
  (cond
    ((zero? b) 0)
    (else (+ a (* a (sub1 b))))))

(define (tup+ tup1 tup2)
  (cond
    ((null? tup1) tup2)
    ((null? tup2) tup1)
    (else (cons (+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2))))))

(define (> a b)
  (cond
    ((zero? a) #f)
    ((zero? b) #t)
    (else (> (sub1 a) (sub1 b)))))

(define (< a b)
  (cond
    ((zero? b) #f)
    ((zero? a) #t)
    (else (< (sub1 a) (sub1 b)))))

(define (= a b)
  (cond
    ((zero? a) (zero? b))
    ((zero? a) #f)
    (else (= (sub1 a) (sub1 b)))))

(define (up a b)
  (cond
    ((zero? b) 1)
    (else (* a (up a (sub1 b))))))

(define (/ a b)
  (cond
    ((< a b) 0)
    (else (add1 (/ (- a b) b)))))

(define (length lat)
  (cond
    ((null? lat) 0)
    (else (add1 (length (cdr lat))))))

(define (pick n lat)
  (cond
    ((= n 1) (car lat))
    (else (pick (sub1 n) (cdr lat)))))

(define (no-nums lat)
  (cond
    ((null? lat) '())
    ((number? (car lat)) (no-nums (cdr lat)))
    (else (cons (car lat) (no-nums (cdr lat))))))

(define (all-nums lat)
  (cond
    ((null? lat) '())
    ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
    (else (all-nums (cdr lat)))))

(define (eqan? a1 a2)
  (cond
    ((and (number? a1) (number? a2)) (= a1 a2))
    ((or (number? a1) (number? a2)) #f)
    (else (eq? a1 a2))))

(define (occur a lat)
  (cond
    ((null? lat) 0)
    ((eqan? (car lat) a) (add1 (occur a (cdr lat))))
    (else (occur a (cdr lat)))))

(define (one? n) (= n 1))

; rewritten
(define (rempick n lat)
  (cond
    ((one? n) (cdr lat))
    (else (cons (car lat) (rempick (sub1 n) (cdr lat))))))

;CHAPTER 5 ----
(define (rember* a l)
  (cond
    ((null? l) '())
    ((atom? (car l))
     (cond
       ((eq? (car l) a) (cdr l))
       (else (cons (car l) (rember* a (cdr l))))))
    (else (cons (rember* a (car l)) (rember* a (cdr l))))))

(define (insertR* new old l)
  (cond
    ((null? l) '())
    ((atom? (car l))
     (cond
       ((eq? (car l) old) (cons old (cons new (insertR* new old (cdr l)))))
       (else (cons (car l) (insertR* new old (cdr l))))))
    (else (cons (insertR* new old (car l)) (insertR* new old (cdr l))))))

(define (occur* a l)
  (cond
    ((null? l) 0)
    ((atom? (car l))
     (cond
       ((eq? a (car l)) (add1 (occur* a (cdr l))))
       (else (occur* a (cdr l)))))
    (else (+ (occur* a (car l)) (occur* a (cdr l))))))

(define (subst* new old l)
  (cond
    ((null? l) '())
    ((atom? (car l))
     (cond
       ((eq? old (car l)) (cons new (subst* new old (cdr l))))
       (else (cons (car l) (subst* new old (cdr l))))))
    (else (cons (subst* new old (car l)) (subst* new old (cdr l))))))

(define (insertL* new old l)
  (cond
    ((null? l) '())
    ((atom? (car l))
     (cond
       ((eq? old (car l)) (cons new (cons old (insertL* new old (cdr l)))))
       (else (cons (car l) (insertL* new old (cdr l))))))
    (else (cons (insertL* new old (car l)) (insertL* new old (cdr l))))))

(define (member* a l)
  (cond
    ((null? l) #f)
    ((atom? (car l))
     (cond
       ((eq? (car l) a) #t)
       (else (member* a (cdr l)))))
    (else (or (member* a (car l)) (member* a (cdr l))))))

(define (leftmost l)
  (cond
    ((atom? (car l)) (car l))
    (else (leftmost (car l)))))

; eqlist? before rewrite
; (define (eqlist? l1 l2)
;   (cond
;     ((and (null? l1) (null? l2)) #t)
;     ((or (null? l1) (null? l2)) #f)
;     ((and (atom? (car l1)) (atom? (car l2))) (and (eqan? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))
;     ((or (atom? (car l1)) (atom? (car l2))) #f)
;     (else (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))))

; eqlist? after rewrite
(define (eqlist? l1 l2)
  (cond
    ((and (null? l1) (null? l2) #t))
    ((or (null? l1) (null? l2)) #f)
    (else (and (equal? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))))

(define (equal? s1 s2)
  (cond
    ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
    ((or (atom? s1) (atom? s2)) #f)
    (else (eqlist? s1 s2))))

(define (rember-generic s l)
  (cond
    ((null? l) '())
    ((equal? (car l) s) (cdr l))
    (else (cons (car l) (rember-generic s l)))))