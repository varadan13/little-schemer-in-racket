#lang racket

(define is-empty-list
  (lambda (x)
    (and (list? x) (null? x))))

(define atom?
  (lambda (x) 
    (not (or (pair? x) (is-empty-list x)))))

(define rember*
  (lambda (a l)
    (cond
      [(null? l) (list)]
      [(atom? (car l)) (cond
                         [(eq? (car l) a) (rember* a (cdr l))]
                         [else (cons (car l) (rember* a (cdr l)))])]
      [else (cons (rember* a (car l)) (rember* a (cdr l)))])))

(define insertR*
  (lambda (new old l)
    (cond
     [(null? l) (list)]
     [(atom? (car l)) (cond
                        [(eq? (car l) old) (cons old (cons new (insertR* new old (cdr l))))]
                        [else (cons (car l) (insertR* new old (cdr l)))])]
     [else (cons (insertR* new old (car l)) (insertR* new old (cdr l)))])))