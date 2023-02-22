
(define (make-wire)

    (define (call-each procedures)
        (if (null? procedures)
            'done
            (begin ((car procedures))
                    (call-each (cdr procedures)))))

    (let ((signal-value 0) (action-procedures '()))

        (define (set-my-signal! new-value)
            (if (not (= signal-value new-value))
                (begin (set! signal-value new-value)
                       (call-each action-procedures))
                'done))
        
        (define (accept-action-procedure proc)
            (set! action-procedures (cons proc action-procedures))
            (proc))
        
        (define (dispatch m)
            (cond ((eq? m 'get-signal) signal-value)
                  ((eq? m 'set-signal!) set-my-signal!)
                  ((eq? m 'add-action!) accept-action-procedure)
                  (else (error "Unknown operatiorn -- WIRE" m))))
        dispatch))


(define (inverter input output)
    
    (define (logic-not s)
        (cond ((= s 0) 1)
            ((= s 1) 0)
            (else (error "Invalid signal" s))))

    (define (invert-input)
        (let ((new-value (logic-not (get-signal input))))
            (after-delay inverter-delay
                (lambda () (set-signal! output new-value)))))

    (add-action! input invert-input))


(define (and-gate a1 a2 output)

    (define (logic-and x y)
        (cond ((= x 0) 0)
            (else y)))

    (define (and-action-procedure)
        (let ((new-value (logic-and (get-signal a1) (get-signal a2))))
            (after-delay and-gate-delay
                (lambda () (set-signal! output new-value)))))

    (add-action! a1 and-action-procedure)
    (add-action! a2 and-action-procedure))


(define (or-gate a1 a2 output)
    
    (define (logic-or x y)
        (cond ((= x 1) 1)
            (else y)))

    (define (or-action-procedure)
        (let ((new-value (logic-or (get-signal a1) (get-signal a2))))
            (after-delay or-gate-delay
                (lambda () (set-signal! output new-value)))))
    (add-action! a1 or-action-procedure)
    (add-action! a2 or-action-procedure))

(define (get-signal wire) (wire 'get-signal))
(define (set-signal! wire new-value) ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedure) ((wire 'add-action!) action-procedure))

;----------------------------------------
; queue

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))

(define (set-front-ptr! queue front) (set-car! queue front))
(define (set-rear-ptr! queue rear) (set-cdr! queue rear))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

(define (front queue)
    (if (empty-queue? queue)
        (error "FRONT called with an empty queue" queue)
        (car (front-ptr queue))))

(define (insert-queue! queue item)
    (let ((new-pair (cons item ())))
        (cond ((empty-queue? queue) 
                    (set-front-ptr! queue new-pair)
                    (set-rear-ptr! queue new-pair)
                    queue)
              (else
                    (set-cdr! (rear-ptr queue) new-pair)
                    (set-rear-ptr! queue new-pair)
                    queue))))

(define (delete-queue! queue)
    (cond ((empty-queue? queue)
                (error "DELETE-QUEUE! called with an empty queue" queue))
          (else
                (set-front-ptr! queue (cdr (front-ptr queue)))
                queue)))

;----------------------------------------
; agenda

(define (make-time-segment time queue) (cons time queue))

(define (segment-time segment) (car segment))
(define (segment-queue segment) (cdr segment))

(define (make-agenda)
    (list '*agenda* (make-time-segment 0 (make-queue))))

(define (segments agenda) (cdr agenda))

(define (first-segment agenda) (car (segments agenda)))

(define (rest-segments agenda) (cdr (segments agenda)))

(define (set-segments! agenda segments) (set-cdr! agenda segments))

(define (current-time agenda) (segment-time (first-segment agenda)))

(define (empty-agenda? agenda) 
    (and (empty-queue? (segment-queue (first-segment agenda)))
         (null? (rest-segments agenda))))

(define (first-agenda-item agenda)
    (let ((q (segment-queue (first-segment agenda))))
        (if (empty-queue? q)
            (begin (set-segments! agenda (rest-segments agenda))
                   (first-agenda-item agenda))
            (front q))))

(define (remove-first-agenda-item! agenda)
    (delete-queue! (segment-queue (first-segment agenda))))

(define (add-to-agenda! time action agenda)

    (define (insert-new-time! time action segments)
        (let ((q (make-queue)))
            (insert-queue! q action)
            (set-cdr! segments (cons (make-time-segment time q) (cdr segments)))))

    (define (add-to-segments! segments)
        (if (= (segment-time (car segments)) time)
            (insert-queue! (segment-queue (car segments)) action)
            (let ((rest (cdr segments)))
                (cond ((null? rest) 
                        (insert-new-time! time action segments))
                      ((> (segment-time (car rest)) time)
                        (insert-new-time! time action segments))
                      (else (add-to-segments! rest))))))

    (add-to-segments! (segments agenda)))

(define (after-delay delay action)
    (add-to-agenda! (+ delay (current-time the-agenda)) action the-agenda))

(define (propagate)
    (if (empty-agenda? the-agenda)
        'done
        (let ((first-item (first-agenda-item the-agenda)))
             (first-item)
             (remove-first-agenda-item! the-agenda)
             (propagate))))

(define (probe name wire)
    (add-action! wire 
        (lambda ()
            newline
            (display name)
            (display (current-time the-agenda))
            (display " New-value = ")
            (display (get-signal wire)))))

(define the-agenda (make-agenda))

;---------------------------------------------------------------------------------------------

(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define (half-adder a b s c)
    (let ((d (make-wire)) (e (make-wire)))
        (or-gate a b d)
        (and-gate a b c)
        (inverter c e)
        (and-gate d e s)))

(define (full-adder a b cin sum cout)
    (let ((s (make-wire)) (c1 (make-wire)) (c2 (make-wire)))
        (half-adder b cin s c1)
        (half-adder a s sum c2)
        (or-gate c1 c2 cout)))

(define a (make-wire))
(define b (make-wire))
(define cin (make-wire))
(define sum (make-wire))
(define cout (make-wire))

(full-adder a b cin sum cout)
(set-signal! a 1)
(set-signal! b 0)
(set-signal! cin 1)
(propagate)   ; 1 + 0 + 1 = 1, 0
(get-signal sum)
(get-signal cout)