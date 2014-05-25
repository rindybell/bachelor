(define (countdown x)
	(if (= x 1)
	    (return 1)
	    (begin (return x)(countdown (- x 1)))))

(define (multiCountdown x)
 (* (countdown 2) (countdown 2)))

(print (multiCountdown 3))
