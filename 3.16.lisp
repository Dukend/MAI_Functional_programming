;;; lab3 Egor Kondratev
;;; 3.16

(defun mprint (m)
  (loop for i below (car (array-dimensions m)) do
        (loop for j below (cadr (array-dimensions m)) do
          (let ((cell (aref m i j)))
            (format t "~a " cell)))
        (format t "~%")))

(defun findel (l elem)
  (when l
    (if (= (car l) elem)
      t
      (findel (cdr l) elem))))

(defun relocation (ans arr lst n m i j) 
  (if (< i n)
    (if (< j m)
      (progn
        (if (and (= (rem (+ i j) 2) 0) (findel lst (aref arr i j)))
          (setf (aref ans i j) 0) 
          (setf (aref ans i j) (aref arr i j)))
        (relocation ans arr lst n m i (+ 1 j)))
      (relocation ans arr lst n m (+ 1 i) 0)
      )
    t))

(defun func (A lis)
  (let* ((n (car (array-dimensions A)))
      (m (car (cdr (array-dimensions A))))
      (ans (make-array (list n m))))
    (relocation ans A lis n m 0 0)
    ans))

;; (mprint (func #2A((1 2 3) (4 5 6) (7 8 9)) '(1 3 5 7 8 9)))
;; (mprint (func #2A((1 2 3 4) (5 6 7 8) (9 10 11 12)) '(1 6 8 9 11)))
