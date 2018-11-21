# 第9章 実践；ユニットテストフレームワーク

- この章ではLisp用の簡単なユニットテストフレームワークを作成する
- マクロ、ダイナミック変数といった機能を活用する方法を学ぶ

- 実際に使えるフレームワークを目指す
- 全てのテストケースはブール式に要約される
  - 各テストケースは真か偽を評価結果にもつ式になる
  - 副作用を持つ関数のテストは少し工夫が必要

## 9.1 最初の試みを 2件

- とりあえず動く簡単なテスト
```
(defun test-+ ()
  (and
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ -1 -3) -4)))

CL-USER> (test-+)
T
```

- 問題点
  - エラーになったときにどこで問題が起きたかわからない
  - 改善版
    - 最初の引数が偽だと FAIL、そうでなければ PASS を表示させている 
```
(defun test-+ ()
  (format t "~:[FAIL~;pass~] ... ~a~%" (= (+ 1 2) 3) `(= (+ 1 2) 3))
  (format t "~:[FAIL~;pass~] ... ~a~%" (= (+ 1 2 3) 6) `(= (+ 1 2 3) 6))
  (format t "~:[FAIL~;pass~] ... ~a~%" (= (+ -1 -3) -4) `(= (+ -1 -3) -4)))


? (test-+)
pass ... (= (+ 1 2) 3)
pass ... (= (+ 1 2 3) 6)
pass ... (= (+ -1 -3) -4)
NIL
```
- レポート自体は求めているものに近い
- コードは冗長で良いとは言えない
  - 繰り返し同じようなfomat関数を呼び出している
  - テスト式が繰り返されている
  - 全てのテストをパスしたという表示がない

## 9.2 リファクタリング

- 最初のバージョンと同じくらい記述を簡潔にしたい
- リファクタリングしていく
  - 繰り返しが多く無駄が多い
  - 繰り返しを取り払っていく

- 毎回 format を書くのが冗長
```
(defun report-result (result form)
    (format t "~:[FAIL~;pass~] ... ~a~%" result form))

(defun test-+ ()
  (report-result (= (+ 1 2) 3) `(= (+ 1 2) 3))
  (report-result (= (+ 1 2 3) 6) `(= (+ 1 2 3) 6))
  (report-result (= (+ -1 -3) -4) `(= (+ -1 -3) -4)))  
```

- 結果のラベルと式を別々に指定するのは冗長
  - マクロ化する
```
(defun report-result (result form)
    (format t "~:[FAIL~;pass~] ... ~a~%" result form))

(defmacro check (form)
    `(report-result ,form ',form))

(defun test-+ ()
  (check (= (+ 1 2) 3))
  (check (= (+ 1 2 3) 6))
  (check (= (+ -1 -3) -4)))
```

- check マクロwの繰り返しが冗長
  - PROGN で包むテクニック
```
(defun report-result (result form)
    (format t "~:[FAIL~;pass~] ... ~a~%" result form))

(defmacro check (&body forms)
    `(progn
        ,@(loop for f in forms collect `(report-result ,f ',f))))

(defun test-+ ()
    (check
        (= (+ 1 2) 3)
        (= (+ 1 2 3) 6)
        (= (+ -1 -3) -4)))
```
## 9.3 戻り値を手直しする

- 全テストケースをパスしたかどうかを戻り値で示したい
- report-result をテストの結果を返せるように変更する
```
(defun report-result (result form)
    (format t "~:[FAIL~;pass~] ... ~a~%" result form)
    result)
```
- PROGN を AND にすればよいか？
  - 良くない
  - AND は途中でNIL が起きると評価をやめてしまう
- このようなマクロがあればいい
```
(combine-results
  (foo)
  (bar)
  (baz))

;; このように展開されてほしい
(let ((result t))
  (unless (foo) (setf result nil))
  (unless (bar) (setf result nil))
  (unless (baz) (setf result nil))
  result)
```
- 注意
  - 変数 result をマクロで展開すると抽象化の漏れが起きる可能性がある
  - 8章で作った with-gensyms を使用する

```
(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
      ,@body))

(defmacro combine-results (&body forms)
    (with-gensyms (result)
        `(let ((,result t))
            ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
            ,result)))
```

- PROGN の代わりに combine-result を使用する
```
(defmacro check (&body forms)
    `(combine-results
        ,@(loop for f in forms collect `(report-result ,f ',f))))
```

```
(defun test-+ ()
    (check
        (= (+ 1 2) 3)
        (= (+ 1 2 3) 6)
        (= (+ -1 -3) -4)))

? (test-+)
pass ... (= (+ 1 2) 3)
pass ... (= (+ 1 2 3) 6)
pass ... (= (+ -1 -3) -4)
T
```

## 9.4 より良い結果レポートのために

- 今までの成果物で複数のテストを同時実行できる

```
(defun test-+ ()
    (check
        (= (+ 1 2) 3)
        (= (+ 1 2 3) 6)
        (= (+ -1 -3) -4)))

(defun test-* ()
    (check
        (= (* 2 2) 4)
        (= (* 3 5) 15)))

(defun test-arithmetic ()
    (combine-results
        (test-+)
        (test-*)))

? (test-arithmetic)
pass ... (= (+ 1 2) 3)
pass ... (= (+ 1 2 3) 6)
pass ... (= (+ -1 -3) -4)
pass ... (= (* 2 2) 4)
pass ... (= (* 3 5) 15)
T
```
- 全てのテストが通らなかった場合、問題の追跡が難しい
  - どのテストで失敗したのかわからない
  - テストが増えると追跡が難しくなる

- 結果出力を最適化する
  - 結果に どのテストから呼ばれたのか出力できるようにすれば良い
    - テスト名を維持するためダイナミック変数を使用する
```
(defvar *test-name* nil)
```
    - repotr-result でダイナミック変数を出力するよう改良を加える
```
(format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
```
    - test名を出力するためテスト関数を手直しする
```
(defun test-+ ()
  (let ((*test-name* 'test-+))
      (check
        (= (+ 1 2) 3)
        (= (+ 1 2 3) 6)
        (= (+ -1 -3) -4))))

(defun test-* ()
  (let ((*test-name* 'test-*))
    (check
      (= (* 2 2) 4)
        (= (* 3 5) 15))))
```
- 結果、テスト名がテストごとに出力されるようになる
```
? (test-arithmetic)
pass ... TEST-+: (= (+ 1 2) 3)
pass ... TEST-+: (= (+ 1 2 3) 6)
pass ... TEST-+: (= (+ -1 -3) -4)
pass ... TEST-*: (= (* 2 2) 4)
pass ... TEST-*: (= (* 3 5) 15)
T
```
