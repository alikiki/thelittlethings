#lang racket
(require "basics.rkt")

; i'm really just doing this so i can read the reasoned schemer :)

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

;CHAPTER 6 --

; i think the book gets this wrong...
(define (numbered? aexp)
  (cond
    ((atom? aexp) (number? aexp))
    ((or
      (eq? (car (cdr aexp)) '+)
      (eq? (car (cdr aexp)) '*)
      (eq? (car (cdr aexp)) '^)) (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp))))))
    (else #f)))

(define (value nexp)
  (cond
    ((atom? nexp) nexp)
    ((eq? (car (cdr nexp)) '+) (+ (value (car nexp)) (value (car (cdr (cdr nexp))))))
    ((eq? (car (cdr nexp)) '*) (* (value (car nexp) (value (car (cdr (cdr nexp)))))))
    (else (up (value (car nexp) (value (car (cdr (cdr nexp)))))))))

(define (1st-sub-exp aexp)
  (car (cdr aexp)))

(define (2nd-sub-exp aexp)
  (car (cdr (cdr aexp))))

(define (operator aexp)
  (car aexp))

(define (value-prefix nexp)
  (cond
    ((atom? nexp) nexp)
    ((eq? (operator nexp) '+) (+ (value-prefix (1st-sub-exp nexp)) (value-prefix (2nd-sub-exp nexp))))
    ((eq? (operator nexp) '*) (* (value-prefix (1st-sub-exp nexp)) (value-prefix (2nd-sub-exp nexp))))
    (else (up (value-prefix (1st-sub-exp nexp)) (value-prefix (2nd-sub-exp nexp))))))

(define (1st-sub-exp-infix aexp)
  (car aexp))

(define (2nd-sub-exp-infix aexp)
  (car (cdr (cdr aexp))))

(define (operator-infix aexp)
  (car (cdr aexp)))

(define (value-infix nexp)
  (cond
    ((atom? nexp) nexp)
    ((eq? (operator-infix nexp) '+) (+ (value-infix (1st-sub-exp-infix nexp)) (value-infix (2nd-sub-exp-infix nexp))))
    ((eq? (operator-infix nexp) '*) (* (value-infix (1st-sub-exp-infix nexp)) (value-infix (2nd-sub-exp-infix nexp))))
    (else (up (value-infix (1st-sub-exp-infix nexp)) (value-infix (2nd-sub-exp-infix nexp))))))

(define (zero?-paren n)
  (null? n))

(define (add1-paren n)
  (cons '() n))

(define (sub1-paren n)
  (cdr n))

(define (+-paren m n)
  (cond
    ((zero?-paren n) m)
    (else (add1-paren (+-paren m (sub1-paren n))))))

;CHAPTER 7--

(define (set? lat)
  (cond
    ((null? lat) #t)
    ((member? (car lat) (cdr lat)) #f)
    (else (set? (cdr lat)))))


(define (makeset lat)
  (cond
    ((null? lat) '())
    ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
    (else (cons (car lat) (makeset (cdr lat))))))

(define (makeset2 lat)
  (cond
    ((null? lat) '())
    (else (cons (car lat) (makeset2 (multirember (car lat) (cdr lat)))))))

(define (subset? set1 set2)
  (cond
    ((null? set1) #t)
    ((and (member? (car set1) set2) (subset? (cdr set1) set2)) #t)
    ; ((member? (car set1) set2) (subset? (cdr set1) set2))
    (else #f)))

(define (eqset? set1 set2)
  (and (subset? set1 set2) (subset? set2 set1)))

(define (intersect? set1 set2)
  (cond
    ((null? set1) #f)
    ((or (member? (car set1) set2) (intersect? (cdr set1) set2)) #t)
    (else #f)))

(define (intersect set1 set2)
  (cond
    ((null? set1) '())
    ((member? (car set1) set2) (cons (car set1) (intersect (cdr set1) set2)))
    (else (intersect (cdr set1) set2))))

(define (union set1 set2)
  (cond
    ((null? set1) set2)
    ((member? (car set1) set2) (union (cdr set1) set2))
    (else (cons (car set1) (union (cdr set1) set2)))))

(define (intersectall l-set)
  (cond
    ((null? l-set) '())
    (else (intersect (car l-set) (intersectall (cdr l-set))))))

(define (a-pair? x)
  (cond
    ((or (atom? x) (null? x) (null? (cdr x))) #f)
    (else (null? (cdr (cdr x))))))

(define (first l)
  (car l))

(define (second l)
  (car (cdr l)))

(define (third l)
  (car (cdr (cdr l))))

(define (seconds l)
  (cond
    ((null? l) '())
    (else (cons (second (car l)) (seconds (cdr l))))))

(define (fun? rel)
  (set? (firsts rel)))

(define (revrel rel)
  (cond
    ((null? rel) '())
    (else (cons (cons (second (car rel)) (first (car rel))) (revrel (cdr rel))))))

(define (fullfun? fun)
  (set? (seconds fun)))

(define (one-to-one fun)
  (fun? (revrel fun)))


(define (rember-f test? a l)
  (cond
    ((null? l) '())
    ((test? a (car l)) (cdr l))
    (else (cons (car l) (rember-f test? a (cdr l))))))

(define (eq?-c a)
  (lambda (x) (eq? x a)))

(define (rember-fun test?)
  (lambda (a l)
    (cond
      ((null? l) '())
      ((test? a (car l)) (cdr l))
      (else (cons (car l) ((rember-fun test?) a (cdr l)))))))

(define (insertL-fun test?)
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((test? old (car l)) (cons new (cons old ((insertL-fun test?) new old (cdr l)))))
      (else (cons (car l) ((insertL-fun test?) new old (cdr l)))))))

(define (insertR-fun test?)
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((test? old (car l)) (cons old (cons new ((insertL-fun test?) new old (cdr l)))))
      (else (cons (car l) ((insertR-fun test?) new old (cdr l)))))))

(define (seqL new old l)
  (cons new (cons old l)))

(define (seqR new old l)
  (cons old (cons new l)))

(define (insert-g seq)
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((eq? (car l) old) (seq new old l))
      (else (cons (car l) ((insert-g seq) new old (cdr l)))))))

; (define (insertL) (insert-g seqL))
; (define (insertR) (insert-g seqR))

(define (insertL-gen)
  (insert-g
   (lambda (new old l)
     (cons new (cons old l)))))

(define (seqS new old l)
  (cons new l))

(define (subst-gen) (insert-g seqS))

(define (rember-gen a l) ((insert-g (lambda (new old l) l)) #f a l))

(define (atom-to-function x)
  (cond
    ((eq? x '+) (+))
    ((eq? x '*) (*))
    (else (up))))

(define (value-using-atom2func x)
  (cond
    ((atom? x) x)
    (else ((atom-to-function (operator x))
           (value-using-atom2func (1st-sub-exp x))
           (value-using-atom2func (2nd-sub-exp x))))))

(define (multirember-f test?)
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? a (car lat)) ((multirember-f test?) a (cdr lat)))
      (else (cons (car lat) ((multirember-f test?) a (cdr lat)))))))

(define multirember-eq?-rewrite (multirember-f eq?))

(define (multiremberT test?)
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((test? (car lat)) ((multiremberT test?) (cdr lat)))
      (else (cons (car lat) ((multiremberT test?) (cdr lat)))))))

(define (multiinsertLR new oldL oldR lat)
  (cond
    ((null? lat) '())
    ((eq? oldL (car lat)) (cons new (cons oldL (multiinsertLR new oldL oldR (cdr lat)))))
    ((eq? oldR (car lat)) (cons oldR (cons new (multiinsertLR new oldL oldR (cdr lat)))))
    (else (cons (car lat) (multiinsertLR new oldL oldR (cdr lat))))))

(define (multiinsertLR&co new oldL oldR lat col)
  (cond
    ((null? lat) (col '() 0 0))
    ((eq? (car lat) oldL)
     (multiinsertLR&co new oldL oldR (cdr lat)
                       (lambda (newlat L R)
                         (col (cons new (cons oldL newlat)) (add1 L) R))))
    ((eq? (car lat) oldR)
     (multiinsertLR&co new oldL oldR (cdr lat)
                       (lambda (newlat L R)
                         (col (cons oldR (cons new newlat)) L (add1 R)))))
    (else
     (multiinsertLR&co new oldL oldR (cdr lat)
                       (lambda (newlat L R)
                         (col (cons (car lat) newlat) L R))))))

(define (evens-only* l)
  (cond
    ((null? l) '())
    ((atom? (car l))
     (cond
       ((even? (car l)) (cons (car l) (evens-only* (cdr l))))
       (else (evens-only* (cdr l)))))
    (else (cons (evens-only* (car l)) (evens-only* (cdr l))))))

(define (evens-only&co l col)
  (cond
    ((null? l) (col '() 1 0))
    ((atom? (car l))
     (cond
       ((even? (car l)) (evens-only&co (cdr l)
                                       (lambda (newlat p s)
                                         (col (cons (car l) newlat) (* p (car l)) s))))
       (else (evens-only&co (cdr l)
                            (lambda (newlat p s)
                              (col newlat p (+ s (car l))))))))
    (else (evens-only&co (cdr l)
                         (lambda (newlat p s)
                           (evens-only&co (cdr l)
                                          (lambda (dl dp ds)
                                            (col (cons newlat dl) (* p dp) (+ s ds)))))))))







