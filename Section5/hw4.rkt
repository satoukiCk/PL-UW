
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file
(define ones (lambda () (cons 1 ones)))
(define a 2)
;; put your code below


(define (sequence low high stride)
  (if (>  low  high)
      null
      (cons low (sequence (+ low stride) high stride))))

(define (string-append-map xs suffix)
  (map (lambda (s)
         (string-append s suffix)) xs))

(define (list-nth-mod xs n)
  (cond
    [(negative? n) (error "list-nth-mod: negative number")]
    [(null? xs) (error "list-nth-mod: empty list")]
    [#t (car (list-tail xs (remainder n (length xs))))]))

(define (stream-for-n-steps stream n)
  (letrec ([f (lambda (stream num)
                (let ([pr (stream)])
                  (if (= num 0)
                      null
                      (cons (car pr) (f (cdr pr) (- num 1))))))])
    (f stream n)))

(define funny-number-stream
  (letrec ([f (lambda (x)
                (if (= (remainder x 5) 0)
                 (cons (* -1 x) (lambda () (f (+ x 1))))
                 (cons x (lambda () (f (+ x 1))))))])
    (lambda () (f 1))))

(define dan-then-dog
  (letrec ([f (lambda (x)
                (if (= (remainder x 2) 1)
                    (cons "dan.jpg" (lambda () (f (+ x 1))))
                    (cons "dog.jpg" (lambda () (f (+ x 1))))))])
    (lambda () (f 1))))

(define (stream-add-zero s)
  (letrec ([f (lambda (stream)
               (let ([pr (stream)])
                 (cons(cons 0 (car pr)) (lambda () (f (cdr pr))))))])
    (lambda () (f s))))

(define (cycle-lists xs ys)
  (letrec ([f (lambda (n)
              (cons (cons (list-nth-mod xs (remainder n (length xs))) (list-nth-mod ys (remainder n (length ys))) )
                    (lambda () (f (+ n 1))) ))])
    (lambda () (f 0))))

(define (vector-assoc v vec)
  (letrec ([f (lambda (x)
                (if (equal? v (car (vector-ref vec x)))
                    (vector-ref vec x)
                    (if (= (vector-length vec) (+ x 1))
                        #f
                        (f (+ x 1)))))])
    (f 0)))

(define (cached-assoc xs n)
  (letrec ([memo (make-vector n  (cons #f #f))]
           [nslot 0])
            (lambda (v)
                (let ([ans (vector-assoc v memo)])
                (if ans
                    (cdr ans)
                    (letrec ([new-ans (assoc v xs)])
                      (begin
                          (vector-set! memo nslot v)
                          (set! nslot (remainder (+ 1 nslot) n))
                          new-ans
                          )))))))