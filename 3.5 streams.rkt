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

; Stream constructor
(define (stream . args)
  (define (add s list)
    (if (null? list)
        s
        (add (cons-stream (car list) s) (cdr list))))
  (add the-empty-stream (reverse args)))

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
;(display-stream
; (stream-map-n
;  *
;  (stream-enumerate-interval 1 5)
;  (stream-enumerate-interval 1 5)))

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
;(display-stream-n fibs 10)

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
;(display-stream-n primes 10)

;; Recursive streams
(define (add-streams s1 s2) (stream-map-n + s1 s2))
(define powers2 (cons-stream 1 (add-streams powers2 powers2)))
;(display-stream-n powers2 10)
(define (add-streams-n . argstreams) (apply stream-map-n + argstreams))


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
;(display-stream-n primes2 10)

;; Exercise 2.54
(define (mul-streams s1 s2) (stream-map-n * s1 s2))
(define integers (integers-starting-from 1))
(define factorials
  (cons-stream
   (stream-car integers)
   (mul-streams factorials (stream-cdr integers))))

;; Exercise 3.55
;(define (partial-sums s)
;  (cons-stream
;   (stream-car s)
;   (add-streams (partial-sums s) (stream-cdr s))))
; To avoid recalculation
(define (partial-sums s) 
   (add-streams s (cons-stream 0 (partial-sums s))))

;; Exercise 3.56
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream
                   s1car
                   (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream
                   s2car
                   (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream
                   s1car
                   (merge (stream-cdr s1)
                          (stream-cdr s2)))))))))
(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor))
              stream))
(define S
  (cons-stream 1
               (merge (scale-stream S 2) (merge (scale-stream S 3) (scale-stream S 5)))))

;; Exercise 3.58
(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

;; Exercise 3.59
;(define (integrate-series-original s)
;  (define (iter s n)
;    (cond ((stream-null? s) the-empty-stream)
;          ((= n 1) (iter (stream-cdr s) (+ n 1)))
;          (else
;           ( let ((c (* (/ 1 n) (stream-car s))))
;              (display "is: ") (display-line c)
;              (cons-stream c (iter (stream-cdr s) (+ n 1)))))))
;  (iter s 1))
(define (integrate-series s)
  (define (display-/ a b)
;    (display "integrate: ") (display a) (display " / ") (display-line b)
    (/ a b))
  (stream-map-n display-/ s integers))
(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

;(define (stream-odd sequence)
;  (define (iter s n)
;    (if (even? n)
;        (cons-stream (stream-car s) (iter (stream-cdr s) (+ n 1)))
;        (cons-stream 0 (iter (stream-cdr s) (+ n 1)))))
;  (iter sequence 0))
;(define (stream-even sequence)
;  (define (iter s n)
;    (if (odd? n)
;        (cons-stream (stream-car s) (iter (stream-cdr s) (+ n 1)))
;        (cons-stream 0 (iter (stream-cdr s) (+ n 1)))))
;  (iter sequence 0))
;(define (alt-negate s)
;  (define (iter s sign)
;    (if (stream-null? s)
;        the-empty-stream
;        (if (= sign 1)
;            (cons-stream (stream-car s) (iter (stream-cdr s) -1))
;            (cons-stream (- 0 (stream-car s)) (iter (stream-cdr s) 1)))))
;  (iter s 1))
;(define cosine-series (alt-negate (stream-odd exp-series)))
;(define sine-series (alt-negate (stream-even exp-series)))

(define sine-series (cons-stream 0 (integrate-series cosine-series)))
(define cosine-series (cons-stream 1 (integrate-series (scale-stream sine-series -1))))

;; Exercise 3.60
; This does not work properly because add-streams only works for equal length streams
(define (mul-series s1 s2)
  (cons-stream
   (* (stream-car s1) (stream-car s2))
   (add-streams
    (add-streams
     (scale-stream (stream-cdr s1) (stream-car s2))
     (scale-stream (stream-cdr s2) (stream-car s1)))
    (cons-stream 0 (mul-series (stream-cdr s1) (stream-cdr s2))))))
(define circle-series 
  (add-streams (mul-series cosine-series cosine-series) 
               (mul-series sine-series sine-series))) 
;(display-stream-n circle-series 30)


;; Exercise 3.61
(define (invert-unit-series s)
  (cons-stream
   1
   (mul-series
    (scale-stream (stream-cdr s) -1)
    (invert-unit-series s))))

;; Fix add-streams
(define (filter predicate list)
  (cond ((null? list)
         '())
        ((predicate (car list))
         (cons (car list) (filter predicate (cdr list))))
        (else
         (filter predicate (cdr list)))))
(define (stream-map-new proc . argstreams)
  (let ((non-null-args (filter (lambda (s) (not (stream-null? s))) argstreams)))
    (if (null? non-null-args)
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car non-null-args))
       (apply stream-map-new
              (cons proc (map stream-cdr non-null-args)))))))
(define (add-streams-new . argstreams) (apply stream-map-new + argstreams))

;; 3.5.3 Exploiting the Stream Paradigm
(define (average a b) (/ (+ a b) 2))
(define (sqrt-improve guess x)
  (average guess (/ x guess)))
(define (sqrt-stream x)
  (define guesses
    (cons-stream
     1.0
     (stream-map (lambda (guess) (sqrt-improve guess x))
                 guesses)))
  guesses)
; Converges on 6th iteration
;(display-stream-n (sqrt-stream 2) 6)

; pi/4 = 1 - 1/3 + 1/5 - 1/7 ...
(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))
(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))
;(display-stream-n pi-stream 30)
(define (stream-asymptote s)
  (define tolerance 0.0001)
  (define max-iterations 10000)
  (define (iter s last iterations)
    (cond ((stream-null? s) last)
          ((= iterations 0) (iter (stream-cdr s) (stream-car s) (+ iterations 1)))
          ((< (abs (- (/ (stream-car s) last) 1)) tolerance)
           (display iterations) (display-line " iterations")
           (stream-car s))
          ((= iterations max-iterations) (error "stream-asymptote: max-iterations exceeded!"))
          (else (iter (stream-cdr s) (stream-car s) (+ iterations 1)))))
  (iter s 0 0))
; (stream-asymptote pi-stream)
; 6367 iterations
; 3.1414356184148753
; (actual 3.14159265359)

; Euler sequence accelerator
(define (euler-transform s)
  (let ((s0 (stream-ref s 0)) ; Sn-1
        (s1 (stream-ref s 1)) ; Sn
        (s2 (stream-ref s 2))) ; Sn+1
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))
; (display-stream-n (euler-transform pi-stream) 5)
; (stream-asymptote (euler-transform pi-stream))
; 11 iterations
; 3.1414796890042562
(define (make-tableau transform s)
  (cons-stream s (make-tableau transform (transform s))))
(define (accelerated-sequence transform s)
  (stream-map stream-car (make-tableau transform s)))
; (display-stream-n (accelerated-sequence euler-transform pi-stream) 5)
; (stream-asymptote (accelerated-sequence euler-transform pi-stream))
; 4 iterations
; 3.1415927140337785

; Exercise 3.64
(define (stream-limit stream tolerance)
  (if (< (abs (- (stream-ref stream 1) (stream-ref stream 0))) tolerance)
      (stream-ref stream 1)
      (stream-limit (stream-cdr stream) tolerance)))
(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))
;(sqrt 2 0.0001)

; Exercise 3.64
(define (ln-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln-summands (+ n 1)))))
(define ln-stream
  (partial-sums (ln-summands 1)))
; (stream-asymptote ln-stream)
;>>>> ERROR 
;("stream-asymptote: max-iterations exceeded!")
; (stream-asymptote (euler-transform ln-stream))
;14 iterations
;0.6931748806748808
; (stream-asymptote (accelerated-sequence euler-transform ln-stream))
;4 iterations
;0.6931471960735491

; Interleave streams
(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))