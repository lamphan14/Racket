#lang racket
(define (divisible-by-7? n)
  (if (= 0 ( modulo n 7))
      #t
      #f)
  )

(define function-3
  (lambda(f)(f 3)))

(define (my-map f lst)
  (cond
    [(empty? lst) empty]
    [else (cons (f (first lst))
                (my-map f (rest lst)))]))

(define (zipper lst1 lst2)
     (match* [lst1 lst2]
         [{'() '()} '()]
         [{(cons hd1 tl1) (cons hd2 tl2)}
           (cons (list hd1 hd2)
                      (zipper tl1 tl2))]))

(define (getEvens alist)
          (if (empty? alist)
              '()
              (if (even? (car alist))
                  (cons (car alist) (getEvens (cdr alist)))
                  (getEvens (cdr alist)))))
(define (getOdds alist)
          (if (empty? alist)
              '()
              (if (odd? (car alist))
                  (cons (car alist) (getOdds (cdr alist)))
                  (getOdds (cdr alist)))))

(define (segregate alist) 
        (list (getEvens alist)(getOdds alist)))

(define (upper-threshold lst theshold)
  (cond
    ((null? lst) '())
    ((< (car lst) theshold)
     (cons (car lst)
           (upper-threshold (cdr lst) theshold)))
    (else (upper-threshold (cdr lst) theshold))))

(define (my-flatten sequence)
 (cond ((null? sequence) '())
 ((list? (car sequence)) (append (my-flatten (car sequence))
 (my-flatten (cdr sequence))))
 (else (cons (car sequence) (my-flatten (cdr sequence))))))

 
(define (htdp-sort l)
  (cond
    [(empty? l) l]
    [else (insert (first l) (htdp-sort (rest l)))]))
(define (insert x l)
  (cond
    [(empty? l) (list x)]
    [else (if (<= x (first l)) (cons x l) (cons (first l) (insert x (rest l))))]))
(define (my-sorted? l)
  (cond
    [(empty? (rest l)) #true]
    [else (and (<= (first l) (second l)) (my-sorted? (rest l)))]))

(define my-list-ref 
  (lambda (lst index)
    (if (= index 0)
        (if (null? lst)
            (error "Error: Index out of bound")
            (car lst))
        (if (null? lst) (error "Error: Index out of bound ")
            (my-list-ref (cdr lst) (- index 1))))))
  