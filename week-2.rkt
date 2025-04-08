#lang racket

(define rember
  (lambda (a l)
          (cond
            ((null? l) (list))
            ((eq? a (car l)) (cdr l))
             (else (cons (car l) (rember a (cdr l)))))))

(define firsts
  (lambda (l)
    (cond
      ((null? l) (list))
      (else (cons (car (car l)) (firsts (cdr l)))))))

(define insertR
  (lambda (new old l)
    (cond
      ((null? l) (list))
      ((eq? old (car l)) (cons old (cons new (cdr l))))
      (else (cons (car l) (insertR new old (cdr l)))))))

(define insertL
  (lambda (new old l)
    (cond
      ((null? l) (list))
      ((eq? old (car l)) (cons new (cons old (cdr l))))
      (else (cons (car l) (insertL new old (cdr l)))))))

(define subst
  (lambda (new old l)
    (cond
      ((null? l) (list))
      ((eq? old (car l)) (cons new (cdr l)))
      (else (cons (car l) (subst new old (cdr l)))))))

(define subst2
  (lambda (new o1 o2 l)
    (cond
      ((null? l) (list))
      ((eq? o1 (car l)) (cons new (cdr l)))
      ((eq? o2 (car l)) (cons new (cdr l)))
      (else (cons (car l) (subst2 new o1 o2 (cdr l)))))))

(define multirember
  (lambda (a l)
          (cond
            ((null? l) (list))
            ((eq? a (car l)) (multirember (cdr l)))
             (else (cons (car l) (multirember a (cdr l)))))))

(define multiinsertR
  (lambda (new old l)
    (cond
      ((null? l) (list))
      ((eq? old (car l)) (multiinsertR (cons old (cons new (cdr l)))))
      (else (cons (car l) (multiinsertR new old (cdr l)))))))

(define multiinsertL
  (lambda (new old l)
    (cond
      ((null? l) (list))
      ((eq? old (car l)) (multiinsertL (cons new (cons old (cdr l)))))
      (else (cons (car l) (multiinsertL new old (cdr l)))))))

(define multisubst
  (lambda (new old l)
    (cond
      ((null? l) (list))
      ((eq? old (car l)) (multisubst (cons new (cdr l))))
      (else (cons (car l) (multisubst new old (cdr l)))))))


(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

(define zero?
  (lambda (n)
    (eq? n 0)))

(define add
  (lambda (a b)
    (cond
      ((zero? a) b)
      (else (add (sub1 a) (add1 b))))))

(define minus
  (lambda (a b)
    (cond ((zero? b) a)
          (else (sub1 (minus a (sub1 b)))))))

(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (add (car tup) (addtup (cdr tup)))))))

(define multi
  (lambda (a b)
    (cond ((zero? a) 0)
          (else (add b (multi (sub1 a) b))))))

(define tup+
  (lambda (tupa tupb)
    (cond ((null? tupa) tupb)
          ((null? tupb) tupa)
          (else (cons (add (car tupa) (car tupb)) (tup+ (cdr tupa) (cdr tupb)))))))

(define greater-than
  (lambda (n m)
    (cond ((zero? n) #f)
          ((zero? m) #t) (else (greater-than (sub1 n) (sub1 m))))))

(define less-than
  (lambda (n m)
    (cond ((zero? n) #t)
          ((zero? m) #f) (else (less-than (sub1 n) (sub1 m))))))

(define equal
  (lambda (a b)
    (cond
      ((greater-than a b) #f)
      ((less-than a b) #f)
      (else #t))))

(define length
  (lambda (lat)
    (cond ((null? lat) 0)
          (else (add 1 (length (cdr lat)))))))

(define pick
  (lambda (n l)
    (cond
      ((zero? n) (car l))
      (else (pick (sub1 n) (cdr l))))))


