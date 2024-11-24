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


(define insertR
  (lambda (new old lat)
    (cond
      [(null? lat) '()]
      [(eq? old (car lat)) (cons old (cons new (cdr lat)))]
      [else (cons (car lat) (insertR new old (cdr lat)))])))

(define insertL
  (lambda (new old lat)
    (cond
      [(null? lat) '()]
      [(eq? old (car lat)) (cons new (cons old (cdr lat)))]
      [else (cons (car lat) (insertL new old (cdr lat)))])))

(define subst
  (lambda (new old lat)
    (cond
      [(null? lat) '()]
      [(eq? old (car lat)) (cons new (cdr lat))]
      [else (cons (car lat) (subst new old (cdr lat)))])))

(define subst2
  (lambda (new o1 o2 lat)
    (cond
      [(null? lat) '()]
      [(or (eq? o1 (car lat)) (eq? o2 (car lat))) (cons new (cdr lat))]
      [else (cons (car lat) (subst2 new o1 o2 (cdr lat)))])))

(define multirember
  (lambda (a lat)
    (cond
      [(null? lat) '()]
      [(eq? a (car lat)) (multirember a (cdr lat))]
      [else (cons (car lat) (multirember a (cdr lat)))])))

(define multiinsertR
  (lambda (new old lat)
    (cond
      [(null? lat) '()]
      [(eq? (car lat) old) (multiinsertR new old (cons old (cons new) (cdr lat)))]
      [else (cons (car lat) (multiinsertR new old (cdr lat)))])))

(define multiinsertL
  (lambda (new old lat)
    (cond
      [(null? lat) '()]
      [(eq? (car lat) old) (multiinsertR new old (cons new (cons old) (cdr lat)))]
      [else (cons (car lat) (multiinsertR new old (cdr lat)))])))

(define multisubst
  (lambda (new old lat)
    (cond
      [(null? lat) '()]
      [(eq? old (car lat)) (multisubst new old (cons new (cdr lat)))]
      [else (cons (car lat) (subst new old (cdr lat)))])))






