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

