
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;; question 1
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))


;; question 2
(define (string-append-map xs suffix)
  (map (lambda (str) (string-append str suffix)) xs))


;; question 3
(define (list-nth-mod xs n)
  (cond
    [(< n 0) (error "list-nth-mod: negative number")]
    [(null? xs) ((error "list-nth-mod: empty list"))]
    [else (car (list-tail xs (remainder n (length xs))))]))


;; question 4
(define (stream-for-n-steps stream n)
  (if (<= n 0)
      null
      (letrec ([strval (stream)])
      (cons (car strval)
             (stream-for-n-steps (cdr strval) (- n 1))))))


;; question 5
;; need to warp f with lambda?
(define funny-number-stream
  (letrec ([f (lambda (n)
            (if (= 0 (remainder n 5))
                (cons (- 0 n) (lambda () (f (+ n 1))))
                (cons n (lambda () (f (+ n 1))))))])
  (lambda () (f 1))))


;; question 6
(define dan-then-dog 
  (letrec ([f (lambda (n)
             (if n
                 (cons "dan.jpg" (lambda () (f #f)))
                 (cons "dog.jpg" (lambda () (f #t)))))])
  (lambda () (f #t))))


;; question 7
(define (stream-add-zero s)
  (lambda()
    (letrec ([sval (s)]) (cons (cons 0 (car sval)) (stream-add-zero (cdr sval))))))


;; question 8
(define (cycle-lists xs ys)
 (letrec ([f (lambda (n)
               (cons (cons (list-nth-mod xs n) (list-nth-mod ys n))
                     (lambda() (f (+ n 1)))
                     ))])
  (lambda　() (f 0))))


;; question 9
(define (vector-assoc v lst)
  (letrec ([vlen (vector-length lst)]
           [f (lambda (n)
              (if (equal? vlen n)
                  #f
                  (letrec ([ith (vector-ref lst n)])
                    (if (and (pair? ith) (equal? (car ith) v))
                    ith
                    (f (+ n 1))))))])
  (f 0)))


;; question10
(define (cached-assoc xs n)
  (letrec ([index 0]
           [cache (make-vector n #f)])
  (lambda (v)
    (letrec ([cachefound (vector-assoc v cache)])
       (if cachefound
           cachefound
           (letrec ([curval (assoc v xs)])
             (begin (vector-set! cache index curval)
                    (set! index (remainder (+ index 1) n))
                    curval)))))
    )
)








