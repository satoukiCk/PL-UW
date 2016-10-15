#lang racket
(define fibonacci3
  (letrec([memo null]
          [f (lambda (x)
               (let ([ans (assoc x memo)])
                 (if ans
                     (cdr ans)
                     (let ([new-ans (if (or (= x 1) (= x 2))
                                        1
                                        (+ (f (-x 1))
                                           (f (-x 2))))])
                       (begin
                         (set! memo(cons (cons x new-ans) memo))
                         new-ans)))))])
f))