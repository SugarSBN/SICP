(define (constant? x) (number? x))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2) (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2) 
    (let ((a11 (extract-exp a1))
          (a21 (extract-exp a2)))
    (cond ((and (number? a11) (number? a21)) (+ a11 a21))
          ((number? a11) (if (= a11 0) a21 (list '+ a11 a21)))
          ((number? a21) (if (= a21 0) a11 (list '+ a11 a21)))
          (else (list '+ a11 a21)))))

(define (make-product m1 m2) 
    (let ((m11 (extract-exp m1))
          (m21 (extract-exp m2)))
    (cond ((and (number? m11) (number? m21)) (* m11 m21))
          ((number? m11) (if (= m11 0) 0 (if (= m11 1) m21 (list '* m11 m21))))
          ((number? m21) (if (= m21 0) 0 (if (= m21 1) m11 (list '* m11 m21))))
          (else (list '* m11 m21)))))

(define (sum? x)
    (if (list? x)
        (eq? (car x) '+)
        #f))

(define (product? x)
    (if (list? x)
        (eq? (car x) '*)
        #f))

(define (extract-exp s) 
    (cond ((list? s) (if (= 1 (length s)) (car s) s))
          (else s)))

(define (addend s) (car (cdr s)))

(define (augend s) (cdr (cdr s)))

(define (multiplier s) (car (cdr s)))

(define (multiplicand s) (cdr (cdr s)))

(define (deriv exp var)
    (let ((exp1 (extract-exp exp)))
    (cond ((constant? exp1) 0)
          ((variable? exp1) 
            (if (same-variable? exp1 var) 1 0))
          ((sum? exp1) (make-sum (deriv (addend exp1) var)
                                 (deriv (augend exp1) var)))
          ((product? exp1) (make-sum (make-product (multiplier exp1)
                                                  (deriv (multiplicand exp1) var))
                                    (make-product (deriv (multiplier exp1) var)
                                                  (multiplicand exp1)))))))

; 3x^2+4x+1
(define poly2 '(+ (* 3 (* x x)) (+ (* 4 x) 1)))

(deriv poly2 'x)