# 5章 関数

- Lisp三大要素；関数、変数、マクロ
- この章では関数について解説する
  - Lispそのものが大部分は関数で構成されている
    - 言語仕様の 3/4 以上が関数の名前
    - 組み込みデータ型にしても「その型を扱う関数が何か」という形で定義
      - どういういみ?
  - マクロも最終的には関数を生成する
    - マクロそのものも関数
      - プログラムの作用を決める関数ではなく
      - コードそのものを生成する関数

## 5.1 新しい関数の定義

- 通常、関数は DEFUNマクロを使って定義される
  - DEFUNってマクロなのか！
  - LAMBDAを作り出すマクロなんだろうな
- ex: 関数の基本的な骨組み
  - `*`がついてるやつはなんじゃろ？
```
(defun name (parameter*)
  "省略可能なドキュメンテーション"
  body-form*)
```

- 関数の名前(name)
  - どのようなシンボルでもる帰る
  - 通常関数の名前はアルファベットとハイフンを使う
    - Lispではケバブケース(kebab-case)を使うのが良い
    - キャメルケース（CamelCase）, スネークケース(snake_case)はあまり使わない
  - ほかの命名規約もある
    - ある種類の値を別な種類に変更する関数の名前には`->` を使うこともある
    - ex) `string->widget`
- 関数のパラメータ(parameter)
  - 関数を呼ぶときに渡す引数を定義する
  - ラムダリストとも呼ばれる
    - Lamda関数の歴史的な関係から
  - 関数が引数を受け取らないときは `()`
  - 引数の種類
    - 必須パラメータ
    - オプショナルパラメータ
    - マルチプルパラメータ
    - キーワードパラメータ
  - 本文(body-form*)
    - 任意の個数のLisp式
    - 順番に評価され最後の式の値が関数の値として返される
    - 最後の式を戻り値として使用したくない場合 RETURN-FORM という特殊オペレータを使う

## 5.2  関数のパラメータリスト

- 通常のパラメータリスト；必須パラメータ
  - 引数が足りない、もしくは多い場合Lispはエラーを通知する

## 5.3 オプショナルパラメータ

- 必須ではないオプショナルな引数を定義するために使う
- `&optional` というシンボルを使用する
- ex)
```
(defun foo  (a b &optional c d) (list a b c d))

(foo 1 2) -> (1 2 nil nil)
(foo 1 2 3) -> (1 2 3 nil)
(foo 1 2 3 4) -> (1 2 3 4)
```

- オプショナルパラメータに初期値を与えることもできる
- ex)
```
(defun foo (a &optional (b 10)) (list a b))

(foo 1) -> (1 10)
(foo 1 2) -> (1 2)
```

- オプショナルパラメータが呼び出し元で指定されたのかデフォルト値なのかを見分ける方法もある
- 規約；オプショナルパラメータに `-supplied-p` をつける
- ex)
```
(defun foo (a b &optional (c 3 c-supplied-p))
  (list a b c c-supplied-p))

(foo 1 2) -> (1 2 3 nil)
(foo 1 2 3) -> (1 2 3 T)
(foo 1 2 4) -> (1 2 4 T)
```

## 5.4 レストパラメータ

- 引数の数ごとの関数を作るのは大変
  - 引数の最大値は処理系依存
  - CALL-ARGUMENT-LIMIT関数；実装ごとの特有な値を調べる関数
  - 最近の実装なら引数の最大値は 4096〜536870911個にもなる
- 可変長引数を渡す手段としてレスとパラメータが準備されている
  - 可変長引数が必要な関数
  - ex
```
(format t "hello world")
(format t "hello ~a" name)
(format t "x: ~d y: ~d" x y)

(+)
(+ 1)
(+ 1 3)
(+ 1 2 3)
```
- レストパラメータの使いかた
- 必須パラメータのあとに `&rest` をつける
- ex
```
(defun format (stream string &rest values)
  ...)

(defun + (&rest numbers)
  ...)
```

## 5.5 キーワードパラメータ

- 引数の順番を指定できる方法
- 引数の後半だけひつよな場合等に使用する
- 必須パラーメータ、`&optional`、`&rest` のあとに `&key` をつける
- ex
```
(defun foo (&key a b c)
  (list a b c))

(foo) -> (NIL NIL NIL)
(foo :a 1) -> (1 NIL NIL)
(foo :b 1) -> (NIL 1 NIL)
(foo :c 1) -> (NIL NIL 1)
(foo :a 1 :c 3) -> (1 NIL 3)
(foo :a 1 :b 2 :c 3) -> (1 2 3)
(foo :a 1 :c 3 :b 2) -> (1 2 3)
```

- キーワードパラメータではデフォルト値のフォーム、`supplied-p`変数 も使える
  - `supplied-p` : その値が引数として渡されていれば T
- ex
```
(defun foo (&key (a 0) (b 0 supplied-p) (c (+ a b)))
  (list a b c b-supplied-p))

(foo :a 1) -> (1 0 1 NIL)
(foo :b 1) -> (0 1 1 T)
(foo :b 1 c: 4) -> (0 1 4 T)
(foo :a 2 :b 1 :c 4) -> (2 1 4 T)
```

- 呼び出し元がパラメータを指定するときに使うキーワードを替えたい場合
- ex
```
(defun foo (&key ((:apple a)) ((:box b) 0) ((:chalie c) 0 c-supplied-p))
  (list a b c c-supplied-p))

(foo :apple 10 :box 20 :chrlie 30) -> (10 0 20 T)
```

## 5.6 異なる種類のパラメータ

- 4種類のパラメータを一つの関数で全部使うこともできる
- 使う場合は以下の順でないとならない
  - 必須パラメータ
  - オプショナルパラメータ `&optional`
  - レストパラメータ `&rest`
  - キーワードパラメータ `&key`
- 数種類のパラメータ
  - よくある組み合わせ；`&optional` + `&rest`
  - おすすめできない；[`&optional` | `&rest`] + `&rest`

- 一般的に `&optional` + `&key` をしたい場合は 全部`&key`であるべき
- 例外もある
  - `&optional` + `&key` な標準関数
    - READ-FROM-STRING
    - PARSE-NAMESTRING
    - WRITE-LINE
    - WRITE-STRING
  - 後方互換を維持するため残された
- おすすめできない組み合わせの挙動
  - `&option` + `&key`

```
(defun foo (x &optional y &key z) (list x y z))

(foo 1 2 z: 3) -> (1 2 3)
(foo 1) -> (1 NIL NIL)

(foo 1 :z 3) -> ERROR
```

  - ERRORになる理由
    - `:z` がy に食われてしまった
    - `&key` がすでにないので ERROR

  - `&rest` + `&key`

```
(defun foo (&rest rest &key a b c) (list rest a b c))

(foo :a 1 :b 2 :c 3) -> ((:a 1 :b 2 :c 3) 1 2 3)
```

  - `&rest` と `&key` は両方同時に集められる
  - 結果二回出力される

## 5.7 関数の戻り値

- 関数や制御公文から抜けるときに使う `RETURN-FORM`
- (RETURN-FORM `抜けたいブロックの名前` `評価値`)
  - 抜けたいブロックの名前に関数名を使用することで関数から抜けることができる
  - 抜けたいブロックの名前は評価されないため `QUORT` する必要がない
- 欠点
  - 関数の名前を指定しなければならない
  - 関数名が変わったら変更の必要がある
  - そんなに使わない関数なので、それほど大きな問題ではない（！）

## 5.8 データとしての関数または高階関数

- 関数をデータ型として使えると便利
  - ソートアルゴリズムでの比較関数
  - コールバックやフック
- Lispでは関数もオブジェクト
  - DEFUN では 関数オブジェクトの生成と 名前つけ の2つの作業をしている
- 関数オブジェクトの取得方法
  - `FUNCTION` 特殊オペレータ
```
(defun foo (x) (* 2 x))


(function foo) -> #<Intterupted Function FOO>
```
  - `#'` も FUNCTION 特殊オペレータの構文糖衣
```
#'foo -> #<Interrupted Function FOO>
```

- 関数オブジェクトの実行
  - `FUNCALL` `APPLY` 関数
  - 引数の渡しかたの差で使い分けるみたい

- FUNCALL
```
(foo 1 2 3) == (funcall #'foo 1 2 3)
```

- APPLY
```
(foo 1 2 3) == (apply #'foo '(1 2 3))
(foo 1 2 3) == (apply #'foo 1 '(2 3))
(foo 1 2 3) == (apply #'foo 1 2 '(3))
```
  - 最後の引数だけリストであればばらばらに渡せる

