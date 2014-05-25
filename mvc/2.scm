(define (countup x y)
	(if (>= x y)
	    (return x)
	    (begin (return x)(countup (+ x 1) y))))

(define (times_table x y)
	(if (= y 9)
	       (format #t "~D~%" (* x y))
		(format #t "~D " (* x y))))
		
(times_table (countup 1 9) (countup 1 9))
