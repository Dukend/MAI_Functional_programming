;;; lab2 Egor Kondratev
;;; 2.10

(defun productsum3help (a i j)
    (if (> j 1) (* (productsum3help a (+ i 1)(- j 1))
    (+ (nth i a)(nth (+ i 1) a)(* 2 (nth j a))))
    (+ (nth i a)(nth (+ i 1) a)(* 2 (nth j a))))
)

(defun product-sum3 (a)
    (productsum3help a 0 (- (length a) 1))
)