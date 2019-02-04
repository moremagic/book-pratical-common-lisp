# 11章 コレクション

- コレクション
    - 複数の値を一つのオブジェクトにまとめた標準的なデータ型
- 基本的なコレクション型
    - 整数をインデックスとする配列
        - 配列、リスト、タプル
    - 任意のキーと値を対応させるテーブル型
        - ハッシュテーブル、連想配列、マップ、辞書
- この章ではベクタ、ハッシュテーブルについて取り上げる
    - シーケンスという抽象化のサブタイプ

## 11.1 ベクタ

- ベクタとは整数でインデックスされた基本的なコレクション
    - 配列に似ている
        - ベクタは 配列 に似ているが ではない
        - ベクタを 一次元の配列 と呼ぶのは正しい
    - 固定サイズの ベクタ
        - 連続したメモリの塊
    - 可変サイズの ベクタ
        - java.util.ArrayList に似ている
- ベクタの作成
    - リテラルでの表記も可能
    - `#()` は REPL の リテラル表記
        ```
        ? #(1 2 3 4 5)
        #(1 2 3 4 5)
        ```
    - 固定サイズのベクタは `vector` 関数で作成できる
        ```
        ? (vector)
        #()
        ? #(1 2 3)
        #(1 2 3)
        ```
- `make-array` 関数
    - 固定サイズ、可変サイズのベクタが作れる
    - 固定サイズベクタの作り方
        ```
        ? (make-array 2)
        #(0 0)
        ? (make-array 5)
        #(0 0 0 0 0)
        ```
        - 初期化するときは 引数`initial-element` を指定する
            ```
            ? (make-array 5 :initial-element nil)
            #(NIL NIL NIL NIL NIL)
            ```
        - 要素保持サイズを指定する
            ```
            ? (make-array 3 :fill-pointer 0)
            #()
            ```
            - ベクタサイズ 3、現在の要素サイズ 0
                - 3個以上の値を挿入できない
    - 可変サイズベクタ
        - `:adjustable t` で可変になる
    - ベクタ操作
        - `vector-push`, `vector-pop`
        ```
        ? (setf a (make-array 3 :fill-pointer 0))
        #()
        ? (vector-push 1 a)
        0
        ? a
        #(1)
        ? (vector-pop a)
        1
        ? a
        #()
        ```
        - ベクタサイズをかえたいときは `vector-push-extend` を使う
            - ベクタサイズを超えて `vector-push` は効かない
        ```
        ? (defparameter *x* (make-array 0 :fill-pointer 0 :adjustable t))
        *X*
        ? *x*
        #()
        ? (vector-push 1 *x*)
        NIL
        ? *x*
        #()
        ? (vector-push-extend 1 *x*)
        0
        ? *x*
        #(1)
        ```
## 11.2 特殊ベクタ

- 特殊ベクタ；要素内の型を制限したベクタ
```
? (setf a (make-array 5 :fill-pointer 0 :adjustable t :element-type 'character))
""
? a
""
? (vector-push #\a a)
2
? a
"a"
? (vector-push #\b a)
3
? a
"ab"
? (vector-pop a)
#\b
? a
"a"
```
## 11.3 シーケンスとしてのベクタ

- ベクタとリストはシーケンスのサブタイプ
- 最も基本的なシーケンス関数
    - `LENGTH` 要素の数を返す
    - `ELT` インデックスアクセス

```
? (defparameter *x* (vector 1 2 3))
*X*
? (length *x*)
3
? (elt *x* 0)
1
? (elt *x* 1)
2
? (elt *x* 2)
3
? (elt *x* 3)
> Error: 3 is not a valid sequence index for #<SIMPLE-VECTOR 3>
> While executing: ELT, in process listener(1).
> Type :POP to abort, :R for a list of available restarts.
> Type :? for other options.
1 > q
? (setf (elt *x* 0) 100)
100
? *x*
#(100 2 3)
```