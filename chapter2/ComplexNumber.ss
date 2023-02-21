
(define (make-rectangular real imag) (cons real imag))
(define (real-part z) (car z))
(define (imag-part z) (cdr z))

(define (make-polar magnitude angle) (cons (* r (cos a)) (* r (sin a))))
(define (magnitude z) (sqrt (+ (square (car z)) (square (cdr z)))))
(define (angle z) (atan (cdr z) (car z)))

(define (+c z1 z2)
    (make-rectangular (+ (real-part z1) (real-part z2))
                      (+ (imag-part z1) (imag-part z2))))
(define (-c z1 z2)
    (make-rectangular (- (real-part z1) (real-part z2))
                      (- (imag-part z1) (imag-part z2))))

(define (*c z1 z2)
    (make-polar (* (magnitude z1) (magnitude z2))
                (+ (angle z1) (angle z2))))

(define (/c z1 z2)
    (make-polar (/ (magnitude z1) (magnitude z2))
                (- (angle z1) (angle z2))))

;-----------------------------------------------------------------------
(define (make-rectangular x y) 
    (define (dispatch m)
        (cond ((eq? m 'real-part) x)
              ((eq? m 'imag-part) y)
              ((eq? m 'magnitude) (sqrt (+ (square x) (square y))))
              ((eq? m 'angle) (atan y x))
              (else (error "Unknown operation -- MAKE-RECTANGULAR" m))))
    dispatch)

(define (make-polar r a)
    (define (dispatch m)
        (cond ((eq? m 'real-part) (* r (cos a)))
              ((eq? m 'imag-part) (* r (sin a)))
              ((eq? m 'magnitude) r)
              ((eq? m 'angle) a)
              (else (error "Unknown operation -- MAKE-POLAR" m))))
    dispatch)

(define a (make-rectangular 1 2))

(a 'magnitude)