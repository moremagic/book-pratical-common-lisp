# 7章 マクロ：標準的な制御構文の構築

- マクロ；CommonLispが他の言語の追随を許していない機能
- 他の言語のマクロとは違う
- 言語を拡張できる方法；クラス、関数、マクロ
- この章ではマクロの定義方法について説明する

## 7.1 WHEN と UNLESS

- 特殊オペレータ`IF`の構文
```
(if condition then-form [else-form])

(if (> 2 3) "Yup" "Nope")  -> "Nope"
(if (> 2 3) "Yup")         -> NIL
(if (> 3 2) "Yup" "Nope")  -> "Yupe"
```
  - else-form に複数処理をそのままで書くことはできない
  - `PROGN` オペレータが必要
  - `PROGN`オペレータを`IF`を書くときに毎回書かなくてはならないのか？
```
;; 以下の処理は想定通り動かない
(if (spam-p current-message)
  (file-in-spam-folder current-message)
  (update-spam-database curren-message))

;;以下のようにすると動く
(if (spam-p current-message)
  (progn
    (file-in-spam-folder current-message)
    (update-spam-database curren-message)))
```

- マクロを使った制御構文の拡張
  - `WHEN` マクロ
```
(defmacro when (condition &rest body)
  `(if ,condition (progn ,@body)))
```
  - `UNLESS` マクロ
```
(defmacro when (condition &rest body)
  `(if (not ,condition) (progn ,@body)))
```
- 注意
  - `WHEN` `UNLESS` は CommonLispパッケージ内のマクロなので再定義してはいけない
  - 実験するときは`MY-WHEN` 等名前を変換するようにする

## 7.2 COND

- 複数の条件分岐のツラミを解決するマクロ
```
;;わかりにくいし冗長
(if a
  (do-x)
  (if b
    (do-y)
    (do-z)))
```

- `COND` マクロ
```
(cond
  (test-1 form)
  (test-2 form)
      :
      :
  (test-N form))

;; 冗長な例を書き直す
(cond
  (a (do-x))
  (b (do-y))
  (t (do-z)))
```

## 7.3 AND と OR と NOT

- 論理演算マクロ
  - 正確には `NOT` は関数

```
(not nil)              -> T
(not (= 1 1))          -> NIL
(and (= 1 2) (= 3 3))  -> NIL
(or (= 1 2) (= 3 3))   -> T
```

## 7.4 繰り返し

- Lisp の ループ構造は強力
  - 全部で25種類ある制御オペレータ内に直接ループ構文をサポートするものはない
  - Lispにおけるループ構造は原始的な2つの特殊オペレータ(goto機能)で実現されている
    - `TAGBODY` `GO`

- ループ構文
  - `DO`
    - いちばん高機能。ベースになるマクロ
  - `DOLIST`
    - `DO` ループに展開されるマクロ
  - `DOTIMES`
    - `DO` ループに展開されるマクロ
  - 'LOOP' マクロ
    - 高機能ループ。好き嫌いが別れる

## 7.5 DOLIST と DOTIMES

- `DOLIST`
  - perl foreach, Python for と似ている
  - 基本構文
```
(dolist (var list-form)
  body-form*)
```

```
(dolist (x '(1 2 3)) (print x))

1 
2 
3 
NIL
```

  - `DOLIST` を中断したい場合`RETURN` が使える
```
* (dolist (x '(1 2 3)) (print x) (if (evenp x) (return)))

1 
2 
NIL
* 
```

- `DOTIMES` は数を数えるループ
```
* (dotimes (i 4) (print i))

0 
1 
2 
3 
NIL
```
  - `DOLIST` と同じように`RETURN` で途中中断できる

## 7.6 DO

- 柔軟性のある繰り返し構文 `DO`
```
(do (variable-definition*)
  (end-test-form result-form*)
  statement*)
```

  - variable-definition の構文
```
(var init-form step-form)
```
    - var が init-form で初期化される
    - var がループごとに step-form で評価された値に変化する
    - init-form を省略すると var は NIL になる
  
  - end-test-form
    - end-test-form の評価値が 真 のとき result-form が評価される
    - 偽 のとき statement が実行される
    - result-form の評価値が `DO` フォームの値となる

- `DO` フォームの例
  - 11番目のフィボナッチ数を求める
```
(do ((n 0 (1+ n))
      (cur 0 next)
      (next 1 (+ cur next)))
  ((= 10 n) cur))
```

