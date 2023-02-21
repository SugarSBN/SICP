(define balance 100)

(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))

(withdraw 25)

(withdraw 25)

(withdraw 60)

(withdraw 25)

(define new-withdraw
    (let ((balance 100))
      (lambda (amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount))
                   balance)
            "Insufficient funds"))))

(new-withdraw 30)

(new-withdraw 30)

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))

(define w1 (make-withdraw 100))
(define w2 (make-withdraw 100))

(w1 30)
(w2 40)


(define (make-account balance)
    (define (withdraw amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount))
                   balance)
            "Insufficient funds"))
    (define (deposit amount)
        (begin (set! balance (+ balance amount)))
                balance)
    (define (dispatch m)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request -- MAKE-ACCOUNT"m))))
    dispatch)

(define a (make-account 100))
((a 'withdraw) 30)
((a 'deposit) 40)
((a 'withdraw) 30)

(define b (make-account 100))
((b 'withdraw) 30)


