# 第8章 マクロ：自分で定義しよう

- 自分でマクロを書く方法を説明する
- マクロは言語の一部分
  - マクロによってコア言語と標準ライブラリの上に抽象化を作る
- マクロを理解する最大の障壁
  - うまく言語に統合されてしまっていること
  - 関数とマクロの区別がつかない
- マクロは関数とは大きく違う

## 8.1 マック初めて物語

<省略>

## 8.2 マクロ展開時 vs. 実行時

- マクロ展開と実行は明確に区別される
  - マクロが動作する時間；マクロ展開時（macro expantion time）
  - 通常のコードが実行される時間（runtime）
- マクロ展開時と実行時 は時間的に絡み合っており境界がはっきりしない
  - 言語仕様でマクロ展開のタイミングは規定されていない
    - 全てのマクロを展開し終わったあとに実行するのか
    - 順次マクロ展開し、実行するのか
- マクロ展開時にはソースコード内に存在している情報のみ使用できる
- マクロは未評価のLispオブジェクトを受取、プログラムを作り出すのが役割

## 8.3 DEFMACRO

- マクロは`DEFMACRO`フォームで定義される
- `DEFUN` にとても良く似ている
```
(defmacro name (parameter*)
  "省略可能なドキュメンテーション文字列"
  body-form)
```

- マクロを書くステップ
  - どのようなコードを作り出したいかを理解していなくてはならない
    - マクロ呼び出しの例
    - 展開後のコード
  - 展開するコードを書く
    - 単純なマクロ
  　  - 展開後のコードに適切なパラメータを挿入
      - バッククォートしたテンプレートを書く
    - 複雑なマクロ
      - 補助関数とデータ構造を完備した独立したプログラム
  - 漏れを塞ぐ
    - 抽象化の漏れを塞ぐ
    - 詳細は8.7章で解説

## 8.4 試しにマクロを書いてみる

- 実際に `do-primes` というマクロを書いていく
- あくまでもデモンストレーションなので実用的ではない
- 補助関数が2つ
  - 与えられた数が素数かどうかを判定する関数
  - 与えられた引数と同じかそれより大きい次の素数を返す
```
(defun primep (number)
  (when (> number 1)
    (loop for fac from 2 to (isqrt number) never (zerop (mod number fac)))))

(defun next-prime (number)
  (loop for n from number when (primep n) return n))
```
- 目指すマクロ
  - 0 〜 19 までの間の素数をループしてその時の素数を p に保持する
```
(do-primes (p 0 19)
  (format t "~d " p))
```
  - 展開型
```
(do ((p (next-prime 0) (next-prime (1+ p))))
  ((> p 19))
  (format t "~d " p))
```

## 8.5 マクロのパラメータ

- マクロ展開後のテンプレートに値を埋め込んでいく
  - 展開時に必要な全ての値を取得する必要がある
  - マクロに渡される引数を展開する必要がある
```
(defmacro do-primes (var-and-range &rest body)
  (let ((var (first var-and-range))
        (start (second var-and-range))
        (end (third var-and-range)))
  `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
    ((> ,var ,end))
    ,@body)))
```
- マクロ定義の特殊機能
  - 分配（destructuring）パラメータリスト
    - macro内で `var-and-range` の展開を全て行う必要はない
    - `(var start end)` というリストが指定できて、マクロ内で自動的に分解される
    - SLIME エラーチェック時にパラメータリスト担っているとエラー情報が詳細になる
  - `&body` パラメータ
    - マクロ定義で使える
    - `&rest`  と同義
    - エディタ側で `&body` パラメータを見つけた場合適切に整形する 

- 最終的なマクロ定義
```
(defmacro do-primes ((var start end) &body body)
  `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
    ((> ,var ,end))
    ,@body))
```

## 8.6 展開形生成

- バッククオート式
  - バッククオートをつけることで式を未評価にする
  - 一部評価したいときはカンマをつける
  - カンマ、アットマーク でリストを外側リスト内に展開する

- バッククオート式はすごく便利
  - quote 式とは違う
  - 一部分をカンマでアンクオート（unquote）できる
  - マクロを書くときはとても便利
```
`(a (+ 1 2) c)        -> (a (+ 1 2) c)
`(a ,(+ 1 2) c)       -> (a 3 c)
`(a (list 1 2) c)     -> (a (list 1 2) c)
`(a ,(list 1 2) c)    -> (a (1 2) c)
`(a ,@(list 1 2) c)   -> (a 1 2 c)
```

- バッククオート式が正しく展開されたか確認してみる
  - `MACROEXPAND-1` 関数でマクロ展開結果を確認することができる
```
(macroexpand-1 '(do-primes (p 0 19) (format t "~d " p)))

(DO ((P (NEXT-PRIME 0) (NEXT-PRIME (1+ P)))) ((> P 19)) (FORMAT T "~d " P))
```

## 8.7 漏れを塞ぐ

− マクロは抽象化を作り出す
− JoelSpolsky；漏れのある抽象化の法則
  − 抽象化されているはずの細部が「漏れる」ことを漏れのある抽象化と表現した

− マクロで内部動作が漏れる可能性は3つ
  − 引数の多重評価
  - ?
  - ?

- do-primes で抽象化の漏れ問題
  − 部分フォーム `end` が多重評価される
  − ループ終了条件で毎回評価されてしまう

```
(do-primes (p 0 (random 100))
  (format t "~d " p))

(macroexpand-1 `(do-primes (p 0 (random 100)) (format t "~d " p)))

-> (DO ((P (NEXT-PRIME 0) (NEXT-PRIME (1+ P)))) ((> P (RANDOM 100))) (FORMAT T "~d " P))
```

- `end` を評価して変数に詰める版
```
(defmacro do-primes ((var start end) &body body)
  `(do ((ending-value ,end)
        (,var (next-prime ,start) (next-prime (1+ ,var))))
       ((> ,var ending-value))
    ,@body))
```
  − 問題が2つ
    − 評価順に関わる問題
      − `DO` フォームは変数の出現順に評価される問題
      − `end` の評価が `start` の前に行われる
      − `start` `end` に副作用のあるフォームを指定されたときにおかしくなる
    − 変数名の問題
      − `ending-value` という変数名をフォーム内で束縛している
      - そのため 同名の変数を使用してマクロ呼び出しすると結果がおかしくなる

- 評価順に関わる問題
  - 評価順を変える
```
(defmacro do-primes ((var start end) &body body)
  `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
        (ending-value ,end)
       ((> ,var ending-value))
    ,@body))
```

- 変数名の問題
  - `GENSYM` 関数を使用する
    - ユニークな変数名を生成する関数
    - 必ず一意になる
```
(defmacro do-primes ((var start end) &body body)
  (let ((ending-value (gensym)))
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
          (,ending-value ,end)
         ((> ,var ,ending-value))
      ,@body)))

? (macroexpand-1 '(do-primes (p 0 (random 100)) (format t "~d " p)))
(DO ((P (NEXT-PRIME 0) (NEXT-PRIME (1+ P)))) (#:G466 (RANDOM 100)) ((> P #:G466)) (FORMAT T "~d " P))
```

- マクロの抽象化の漏れをなくすには以下のルールに従う
  - 特別な理由がない限り、部分フォームは出現順に評価する
  - 特別な理由がない限り、部分フォームは一度だけ評価されるようにする
  - 展開系の中で使う変数は、`GENSYM`関数をつかって作ること

## 8.8 マクロを書くマクロ

- 関数を書くとき以外もマクロは使える
- マクロを書くマクロを紹介する

- 多くのマクロはマクロ無い変数を使用するとき `gensym` を使う
- これを抽象化する`with-gensyms` マクロを作ってみる
- 以下のように使用する
```
(defmacro do-primes ((var start end) &body body)
  (with-gensyms (end-value-name)
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
          (,ending-value-name ,end))
         ((> ,var ,ending-value-name))
        ,@body)))
```

```
(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
      ,@body))
```
- 以下のように動作確認できる
```
? (loop for n in `(a b c) collect `(,n (gensym)))
((A (GENSYM)) (B (GENSYM)) (C (GENSYM)))
```



