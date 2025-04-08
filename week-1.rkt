#lang racket

(define is-empty-list
  (lambda (x)
    (and (list? x) (null? x))))

(define atom?
  (lambda (x) 
    (not (or (pair? x) (is-empty-list x)))))

; car

; cdr

; cons

; null?

; atom?

; eq?

(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      (else (and (atom? (car l)) (lat? (cdr l)))))))

(define member?
  (lambda (a l)
    (cond
      ((null? l) #f)
       (else (or (eq? a (car l)) (member? a (cdr l)))))))

  