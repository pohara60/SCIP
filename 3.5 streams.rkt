#lang r5rs

;;; create binding for error (for R5RS)
(define error #f)
;;; capture toplevel continuation
;;;  assign a function to error, allowing a variable number of arguments to
;;;  be passed
(call-with-current-continuation (lambda (k)
                                  (set! error
                                        (lambda error-arguments
                                          (display ">>>> ERROR ")
                                          (newline)
                                          (k error-arguments)))
                                  'done)) 
(define false #f)
(define true #t)

;; Speacial form to construct stream using delay
(define-syntax cons-stream
  (syntax-rules ()
    ((_ a b)
     (cons a (delay b)))))
(define stream-car car)
(define stream-cdr (lambda (s) (force (cdr s))))
(define the-empty-stream '())
(define (stream-null? stream) (eq? stream the-empty-stream))

;; Useful functions
(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))
(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))
(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))
(define (display-stream s)
  (stream-for-each display-line s))
(define (display-line x) (display x) (newline))

(define (stream-for-n proc s n)
  (if (or (< n 1) (stream-null? s))
      'done
      (begin (proc (stream-car s))
             (stream-for-n proc (stream-cdr s) (- n 1)))))
(define (display-stream-n s n)
  (stream-for-n display-line s n))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter
                       pred
                       (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))


;; Exercise 3.50
(define (stream-map-n proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map-n
              (cons proc (map stream-cdr argstreams))))))
(display-stream
 (stream-map-n
  *
  (stream-enumerate-interval 1 5)
  (stream-enumerate-interval 1 5)))

;; Exercise 3.51
(define (show x)
  (display-line x)
  x)
(define x
  (stream-map show
              (stream-enumerate-interval 0 10)))
(stream-ref x 5)
(stream-ref x 7)

;; Infinite streams
(define (fibgen a b) (cons-stream a (fibgen b (+ a b))))
(define fibs (fibgen 0 1))
(display-stream-n fibs 10)

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
(define (divisible? x y) (= (remainder x y) 0))
(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
           (lambda (x)
             (not (divisible? x (stream-car stream))))
           (stream-cdr stream)))))
(define primes (sieve (integers-starting-from 2)))
(display-stream-n primes 10)

;; Recursive streams
(define (add-streams s1 s2) (stream-map-n + s1 s2))
(define powers2 (cons-stream 1 (add-streams powers2 powers2)))
(display-stream-n powers2 10)

(define primes2
  (cons-stream
   2
   (stream-filter prime? (integers-starting-from 3))))
(define (prime? n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n) true)
          ((divisible? n (stream-car ps)) false)
          (else (iter (stream-cdr ps)))))
  (iter primes2))
(define (square n) (* n n))
(display-stream-n primes2 10)