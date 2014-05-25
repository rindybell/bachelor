(define (countdown x)
	(if (= x 1)
	    (return 1)
	    (begin (return x)(countdown (- x 1)))))

(print (countdown 9))
