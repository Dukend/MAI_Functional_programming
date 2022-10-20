;;; lab5 Egor Kondratev
;;; 5.45

(defclass polynom ()
    ((polunom-symbol :initarg :var1 :reader var1)
     (term-list :initarg :terms :reader terms)
    )
)

(defun make-term (&key order coeff)
    (list order coeff)
)

(defun order (term) (first term))
(defun coeff (term) (second term))

(defgeneric zeropp (arg)
 (:method ((n number))   ; (= n 0)
  (zerop n)))

(defgeneric minuspp (arg)
 (:method ((n number))   ; (< n 0)
  (minusp n)))

(defun mult-list (list)
    (mapcar #'(lambda(x) (reduce '* x)) list)
)

(defun get-last-n-elems (count list)
    (last list count)
)

(defun sum-list (list)
    (reduce '+ list)
)


(defun remove-last-el (list)
    (loop for i on list
        while (rest i)
            collect (first i)
    )
)


(defmethod print-object ((p polynom) stream)
  (format stream "[Polynomial (~s) ~:{~:[~:[+~;-~]~d~[~2*~;~s~*~:;~s^~d~]~;~]~}]"
          (var1 p)
          (mapcar (lambda (term)
                    (list (zeropp (coeff term))
                          (minuspp (coeff term))
                          (if (minuspp (coeff term))
                              (abs (coeff term))
                              (coeff term))
                          (order term)
                          (var1 p)
                          (order term)))
                  (terms p))))





(defun get-zeros (n)
    (make-list n :initial-element '0)
)

(defun cur-coef (cur next tail)
    (cond ((null next) (if (= 0 (order cur)) 
                            (cons (coeff cur) tail)
                            (cons (coeff cur) (append (get-zeros (order cur)) tail)))
          )
          ((= (order cur) (1+ (order next))) (cons (coeff cur) tail))
          (t (cons (coeff cur) (append (get-zeros (1- (- (order cur) (order next)))) tail)))
    )
)

(defun coefs (p)
    (if p (cur-coef (first p) (second p) (coefs (rest p))))
)



(defun combinations (count list)
    (cond
        ((zerop count) '(()))
        ((endp list)   '())
        (t (nconc (mapcar (let ((item (first list)))
                                (lambda (comb) (cons item comb)))
                                (combinations (1- count) (rest list)))
              (combinations count (rest list))))
    )
)

(defun mult (d a j)
    (cond
        ((oddp j) (* d (sum-list (mult-list (combinations j a)))))
        (t (* -1 (* d (sum-list (mult-list (combinations j a))))))
    )
)

(defun listd (j d a i)
    (if d (cons (mult (first d) (get-last-n-elems (- (list-length a) j) a) i) (listd (1+ j) (rest d) a (1- i))))
)

(defun sum-mult (d a)
    (sum-list (listd 0 d a (list-length d)))
)

(defun func (p a)
    (let  ((b (coefs (terms p)))
           (d (list (first (coefs (terms p)))))
           )
        (loop for i in (rest b) 
            do (nconc d (list (+ i (sum-mult d (remove-last-el a)))))
        )
    (reverse d)
    )
)

(defun ex1 ()
    (let ((pol (make-instance 'polynom :var1 'x :terms 
        (list   (make-term :order 5 :coeff -2)
                (make-term :order 3 :coeff 4)
                (make-term :order 1 :coeff -6))))
        (l (list 1 2 3 2 1 1)))

    (print "Polynom:")
    (print pol)
    (print "List:")
    (print l)
    (print "Result:")
    (print (func pol l))
    (values))
)

(defun ex2 ()
    (let ((pol (make-instance 'polynom :var1 'x :terms 
        (list   (make-term :coeff 1 :order 3)
                (make-term :coeff 2 :order 1)
                (make-term :coeff 1 :order 0))))
        (l (list 4 4 4)))

    (print "Polynom:")
    (print pol)
    (print "List:")
    (print l)
    (print "Result:")
    (print (func pol l))
    (values))
)
(defun ex3 ()
    (let ((pol (make-instance 'polynom :var1 'x :terms 
        (list   (make-term :order 2 :coeff 5)
                (make-term :order 1 :coeff 3.3)
                (make-term :order 0 :coeff -7.4))))
        (l (list 1 1 1)))

    (print "Polynom:")
    (print pol)
    (print "List:")
    (print l)
    (print "Result:")
    (print (func pol l))
    (values))
)


;; (ex1)
;; (ex2)
;; (ex3)