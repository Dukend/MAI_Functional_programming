;;; lab1 Egor Kondratev
;;; 1.44

(defun printlevel (current n tmp q1 q2)
	(cond ((= current (1- n)) (prin1 1))
	((< current (1- n))
	(if (= current 0)
	(print 1)
	(prin1 (/ tmp (* q1 q2))))
 	(princ " ")
	(printlevel (1+ current) n tmp (* q1 (1+ current)) (/ q2 (- n (1+ current)))))))

(defun iteration(current q last)
	(printlevel 0 current q 1 q)
	(if (< current last)
		(iteration (1+ current) (* q (1+ current)) last)))

(defun pascal-triangle(n)	
	(iteration 0 1 n))
