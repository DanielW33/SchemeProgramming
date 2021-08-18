#lang racket

;p1 
(define ((funPower f n) x)
  (cond ((equal? n 0) x)
        (else ((funPower f (- n 1)) (f x)))))

  ;((funPower f (- n 1)) x))

(define (encode n)(lambda(f) (lambda(z) (cond ((equal? n 0) z)
                                          (else (((encode (- n 1)) f) (f z)))))))

(define (decode n) (lambda(f) (lambda(n) (f n))))

(define (MULT n m) (lambda(f) (lambda(x) (* f x))))

;p2
(define (dot x y)
  (cond ((null? x) 0)
        (else (+ (* (car x) (car y)) (dot (cdr x) (cdr y))))))

;p3
(define (in e L)
  (cond ((null? L) #f)
        ((list? (car L)) (in e (append(car L) (cdr L))))
        (( equal? (car L) e) #t)
        (else (in e (cdr L)))))
        

;p4
(define (lstOddSum L)
  (cond ((null? L) 0)
        (else (+ (car L) (cond
                           ((null? (cdr L)) 0)
                           (else (lstOddSum (cdr (Skip L)))))))))
(define (Skip L) (cdr L))
                          
;p5
(define trunc "Null") 
        

(define (leq L1 L2)
  (cond ((null? L1) #t)
        (else (search (map (lambda (x y) (<= x y)) L1 L2)))))
;My leq function returns a list of true and false terms, but I was unable to figure out how to complete this without recursion to only get a single term.
;So i had to implement the search function in order to get a single term.
(define (search L)
  (cond ((null? L) #t)
        ((equal? #f (car L)) #f)
        (else (search (cdr L)))))
;((map (lambda (x y) (<= x y)) L1 L2))
;Dup Solution Explanation:
;Method takes in L (List) as an argument, Then uses the foldr procedure
;foldr f x '()
;foldr takes in a function: f
;Ending value x
;'() being any list.
(define (dup L) (foldr (lambda(x y) (cons x (cons x y))) '() L))

;Test code section
;(define x 10)
;(define y (+ x 20))
;(define (f x) (+ x 1) )

;(define ((g x) [y 20])
;  (+ x y))

;(if (< 3 2) 1 (+ 2 3))
;(* 2 (if (< 3 5) 3 "abc"))
;(max 1 2 3)

;Testing Lambda-----------------------------
;((lambda (x) x) 1)
;((lambda (x y z) (+ x y z)) 1 2 3)
;((lambda (f) (f 2)) (lambda (x) (* 2 x)))

;Testing condition---------------------------
;(define (test x)
;  (cond ((number? x) "Number")
;        ((string? x) "String")
;        ((list? x) "List")
;        (else "other")))

;Testing recursion!---------------------
;(define (factorial n)
;  (if (= n 0) 1
;      (* n (factorial (- n 1)))))

;(define (fib n)
;  (cond ((= n 1) 1)
;        ((= n 2) 2)
;        (else (+ (fib (- n 1)) (fib (- n 2))))))

;(define (dotP x y)
;  (cond ((null? x) 0)
;        (else (+ (* (car x) (car y)) (dotP (cdr x) (cdr y))))))

;(define (1stSum lists)
;  (cond ((null? lists) 0)
;        (else (+ (car lists) (1stSum (cdr lists))))))
;Church Encoding Tests--------------------
;Boolean
;(define (True a b) a)
;(define (False a b) a)
;Natural Numbers
;(define (One f x) (f x))
;(define (Two f x) (f (f x)))
;(define (Zero f x) x)
;(define (Add n m)
;  (lambda (f x)
;    (n f (m f x))))

(define (Maptest L) (map sqrt L))  ;Map applies the function sqrt to each value in L
(define (FoldlTest L) (foldl * 1 L)) ;Foldl Applies the function * to the current value in the list. Starting with 1