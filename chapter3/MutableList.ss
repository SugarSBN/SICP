(define x '((a b) c d))

(define y '(e f))

(set-car! x y)

(car x)
(car y)

(set-car! y 'rua)
(car x)
(car y)