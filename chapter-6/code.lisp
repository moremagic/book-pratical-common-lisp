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

; count変数を束縛したLambda関数をダイナミック変数に代入
(defparameter *fn* 
  (let ((count 0))
    #'(lambda () (setf count (1+ count)))))

; count変数を束縛したLambda関数を変数に代入
(setf aaa 
  (let ((count 0))
    #'(lambda () (setf count (1+ count)))))


; lambdaを作り出す関数。
; 毎回違うオブジェクトを作るのでcount変数束縛は全て違うものになる
; (funcall (aaa))
(defun aaa () 
  (let ((count 0))
    #'(lambda () (setf count (1+ count)))))


; 複数のクロージャから一つの変数束縛を補足する
; (funcall (car bbb))
; (funcall (cadr bbb))
; (funcall (caddr bbb))
(setf bbb 
      (let ((count 0))
        (list
          #'(lambda () (incf count))
          #'(lambda () (decf count))
          #'(lambda () count))))


(defvar x 0)

(defun foo ()
  (format t "x value: ~a~%" x))

(defun bar ()
  (foo)
  (let ((x 20)) (foo))
  (foo))

(setf x 10)
(defun foo (x) (setf x 10))

;;setq setfの違い
(setq lst '(a b c))
(setf (car lst) 480)
lst


