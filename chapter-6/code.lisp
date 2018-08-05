(defun foo (x y z) (+ x y z))

(defun foo (x)
  (format t "Parameter: ~a~%" x)
  (let ((x 2))
    (format t "Outer LET: ~a~%" x)
    (let ((x 3))
      (format t "Inner LET: ~a~%" x))
    (format t "Outer LET: ~a~%" x))
  (format t "Parameter: ~a~%" x))

(dotimes (x 10) (format t "~d " x))

(defun bar (x)
  (let* ((x 10)
         (y (+ x 10)))
    (format t "LET*: x=~a y=~a" x y)))
