; ;;;;;;;;;;;;;;
; ; Questions ;;
; ;;;;;;;;;;;;;;
; Scheme
(define (cddr s) (cdr (cdr s)))

(define (cadr s) (car (cdr s)))

(define (caddr s) (car (cddr s)))

(define (sign x)
  (cond 
    ((> x 0) 1)
    ((< x 0) -1)
    (else    0)
  )
)

(define (square x) (* x x))

(define (pow b n)
  (cond 
    ((zero? n) 1)
    ((= n 1)   b)
    ((even? n) (square (pow b (/ n 2))))
    (else      (* b (pow b (- n 1))))
  )
)

(define (unique s)
  (if (null? s)
      nil
      (cons (car s)
            (unique
             (filter (lambda (x) (not (equal? x (car s))))
                     (cdr s)
             )
            )
      )
  )
)
