(define (make-sieve n)
  (make-vector n #t))

(define (sieve-list s lo)
  (define (iter k result)
    (if (< k 0)
	result
	(if (vector-ref s k)
	    (iter (- k 1) (cons (+ lo k) result))
	    (iter (- k 1) result))))
  (iter (- (vector-length s) 1) '()))

(define (mark-sieve s n)
  (let ((inc n))
    (define (iter k)
      (cond ((>= k (vector-length s)) s)
	    (else
	     (vector-set! s k #f)
	     (iter (+ k inc)))))
    (iter (* n n))))

(define (simple-sieve n)
  (if (< n 2)
      '()
      (let ((s (make-sieve (+ n 1))))
        (define (iter i s)
          (if (> i (sqrt n))
              s
              (iter (+ i 1) (mark-sieve s i))))
        (vector-set! s 0 #f)
        (vector-set! s 1 #f)
        (sieve-list (iter 2 s) 0))))

(define (compute-first-multiple p lo)
  (let ((np (* (floor (/ lo p)) p) ))
    (if (< np lo)
	(+ np p)
	np)))

(define (mark-segment p s lo hi)
  (define (iter n)
    (cond ((>= n hi) s)
	  (else
	   (vector-set! s (- n lo) #f)
	   (iter (+ n p)))))
  (let ((np (compute-first-multiple p lo)))
    (iter np)))

(define (compute-segment p lo hi)
  (let ((s (make-sieve (- hi lo))))
    (define (iter p)
      (cond ((null? p) s)
	    (else
	     (mark-segment (car p) s lo hi)
	     (iter (cdr p)))))
    (iter p)))

(define (segmented-sieve n)
  (if (< n 2)
      '()
      (let ((ssiz (+ (inexact->exact (floor (sqrt n))) 1)))
        (let ((p-list (reverse (simple-sieve ssiz))))
          (define (iter lo hi res)
            (cond ((> lo n) res)
                  (else
                   (let ((seg-p
                          (sieve-list (compute-segment p-list lo hi) lo))
                         (hi
                          (min (+ hi ssiz) (+ n 1))))
                     (define (add-primes p res)
                       (if (null? p)
                           res
                           (add-primes (cdr p) (cons (car p) res))))
                     (iter (+ lo ssiz) hi (add-primes seg-p res))))))
          (reverse (iter (+ ssiz 1) (+ ssiz ssiz 1) p-list))))))