(defun foo (a b c) (list a b c))

(funcall #'foo 1 2 3)
(apply #'foo '(1 2 3))


(defun plot (fn min max step)
  (loop for i from min to max by step do
        (loop repeat (funcall fn i) do (format t "*"))
        (format t "~%")))


(plot #'exp 0 4 1/2)

(defun DDD (x) (* 2 x))
(plot #'DDD 0 10 1)
(plot #'(lambda (x) (* 2 x)) 0 10 1)

