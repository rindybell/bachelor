(define (countdown x)
	(if (= x 1)
	    (return 1)
	    (begin (return x)(countdown (- x 1)))))

(define (dblCountdown x)
	(+ (countdown x) (countdown x)))
(print (dblCountdown 2))
