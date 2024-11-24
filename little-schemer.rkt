#lang racket

(define (atom? a) (symbol? a))

; car

; cdr

; cons

; eq?

; null?

(define lat?
  (lambda (l)
    (cond [(null? l) #t]
          [(atom? (car l)) (lat? (cdr l))]
          [else #f])))

; (list 'a 'b)

(define member-rough?
  (lambda (a lat)
    (cond
      [(null? lat) #f]
      [(eq? a (car lat)) #t]
      [else (member-rough? a (cdr lat))]
      )))

(define member?
  (lambda (a lat)
    (cond
      [(null? lat) #f]
      [else (or (eq? a (car lat)) (member? a (cdr lat)))])))


(define rember
  (lambda (a lat)
    (cond
      [(null? lat) '()]
      [(eq? a (car lat)) (cdr lat)]
      [else (cons (car lat) (rember a (cdr lat)))]
      )))

(define firsts
  (lambda (l)
    (cond
      [(null? l) '()]
      [else (cons (car (car l)) (firsts (cdr l)))])))






