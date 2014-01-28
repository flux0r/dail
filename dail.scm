#!r6rs

(import (rnrs))

(define (count-spins n x y z)
  (define (spin dist)
    (if (< dist 0)
        (mod dist n)
        dist))
  (let ((dist-x-y (- x y))
        (dist-y-z (- z y)))
    (+ (* 3 n)
       x
       (spin dist-x-y)
       (if (= 0 dist-y-z)
           n
           (spin dist-y-z)))))

(define (string->datum x)
  (call-with-port (open-string-input-port x) read))

(define (readline)
  (string->datum (get-line (current-input-port))))

(define (space? c)
  (= c #\space))

(define (drop-while p xs)
  (if (null? xs)
      ()
      (if (p (car xs))
        (drop-while p (cdr xs))
        xs)))

(define (o f g)
  (lambda (x) (f (g x))))

(define (span p xs)
  (if (null? xs)
    (cons xs xs)
    (if (p (car xs))
      (let ((pr (span p (cdr xs))))
        (cons (cons (car xs) (car pr)) (cdr pr)))
      (cons () xs))))

(define (break p xs)
  (span (o not p) xs))

(define (words xs)
  (let ((xs- (drop-while space? (string->list xs))))
    (if (null? xs-)
      ()
      (let ((pr (break space? xs-)))
        (cons (car pr) (words (cdr pr)))))))

(define (take n xs)
  (cond ((<= n 0) ())
        ((null? xs) ())
        (else (cons (car xs) (take (- n 1) (cdr xs))))))
