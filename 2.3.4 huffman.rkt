#lang sicp

(define (make-leaf symbol weight) 
 (list 'leaf symbol weight)) 
(define (leaf? object) 
 (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x)) 
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right) 
 (list left right (append (symbols left) (symbols right)) (+ (weight left) (weight right))))
 
(define (left-branch tree) (car tree)) 
(define (right-branch tree) (cadr tree)) 
(define (symbols tree) (if (leaf? tree) (list (symbol-leaf tree)) (caddr tree))) 
(define (weight tree) (if (leaf? tree) (weight-leaf tree) (cadddr tree)))

(define (decode bits tree) 
 (define (decode-1 bits current-branch) 
  (if (null? bits) 
      '() 
      (let ((next-branch (choose-branch (car bits) current-branch))) 
        (if (leaf? next-branch) (cons (symbol-leaf next-branch) (decode-1 (cdr bits) tree)) 
            (decode-1 (cdr bits) next-branch))))) 
  (decode-1 bits tree))
(define (choose-branch bit branch) 
  (cond ((= bit 0) (left-branch branch)) 
        ((= bit 1) (right-branch branch)) 
        (else (error "bad bit: CHOOSE-BRANCH" bit))))
  
(define (adjoin-set x set) 
  (cond ((null? set) (list x)) 
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs) 
  (if (null? pairs) '() 
      (let ((pair (car pairs))) 
        (adjoin-set (make-leaf (car pair) ; symbol
                               (cadr pair)) ; frequency
                    (make-leaf-set (cdr pairs))))))

(define sample-tree 
  (make-code-tree 
    (make-leaf 'A 4)
    (make-code-tree 
      (make-leaf 'B 2) 
      (make-code-tree 
        (make-leaf 'D 1) 
        (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree)

(define (encode message tree) 
  (if (null? message) 
      '() 
      (append (encode-symbol (car message) tree) 
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree) 
 (define (encode-1 symbol current-branch) 
  (if (leaf? current-branch)
      (if (eq? symbol (symbol-leaf current-branch))
          '()
          (error "bad symbol: ENCODE-1" symbol))
      (if (member symbol (symbols (left-branch current-branch)))
          (cons 0 (encode-1 symbol (left-branch current-branch)))
          (cons 1 (encode-1 symbol (right-branch current-branch))))))
 (encode-1 symbol tree))
(encode-symbol 'B sample-tree)
(decode (encode '(A B C D) sample-tree) sample-tree)

(define (generate-huffman-tree pairs) 
  (successive-merge (make-leaf-set pairs)))
  
(define (successive-merge leaf-set)
  (cond ((eq? (length leaf-set) 0) '())
        ((eq? (length leaf-set) 1) leaf-set)
        (else (successive-merge (cons (make-code-tree (car leaf-set) (cadr leaf-set)) (cddr leaf-set))))))

(make-leaf-set (list '(A 4) '(B 2) '(C 1) '(D 1)))
(generate-huffman-tree (list '(A 4) '(B 2) '(C 1) '(D 1)))
(encode 
  '(GET)
  (generate-huffman-tree (list 
  '(A 2) '(GET 2) '(SHA 3) '(WAH 1) '(BOOM 1) '(JOB 2) '(NA 16) '(YIP 9))))
(define tree
  (generate-huffman-tree (list 
  '(a 2) '(Get 2) '(Sha 3) '(Wah 1) '(boom 1) '(job 2) '(na 16) '(yip 9))))
(decode   
  (encode 
    '(Get a job Sha na na na na na na na na Get a job Sha na na na na na na na na Wah yip yip yip yip yip yip yip yip yip Sha boom)
    tree)
  tree)
  