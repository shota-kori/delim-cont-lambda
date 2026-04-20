# delim-cont-lambda

([English ver.](./README.md))

**delim-cont-lambda**はlet多相と限定継続演算子を備えたラムダ計算のインタプリタです。
Haskellで実装されており、対話型REPLとして実行できます。

## 概要

このプロジェクトは、ラムダ計算を基にした関数型言語の実装です。
参考文献の型規則を基にしたHindley-Milner型システムを持ち、限定継続によって制御フローを記述することができます。

## 実行例

### 基本的な型と演算
`Int`型と`Bool`型、整数の演算(`+`, `-`, `*`)と`if`式を記述できます。

```
> 1
 : Int => 1
> + 2 3
 : Int => 5
> True
 : Bool => True
> if False then 1 else 0
 : Int => 0
```

### 関数型
この言語では限定継続を扱うために、関数型は4つの型引数を取ります。
`\`はλを表し、`\x.`でラムダ抽象を開始します。(`x`は任意の変数名)

```
> \x. x
 : (t1 / t2 -> t1 / t2) => (\x. x)
> \x. + x 2
 : (Int / t10 -> Int / t10) => (\x. + x 2)
```

### let多相
`let`式は多相な関数を変数に束縛でき、異なる型の値に適用できます。

```
> let id = \x. x in if (id True) then (id 1) else (id 0)
 : Int => 1
```

### 限定継続
`shift`演算子で継続を補足し、`reset`演算子で継続の範囲を限定します。

```
> reset (+ 1 (shift k. k (k 10)))
 : Int => 12
> reset (- 100 (shift k. False))
 : Bool => False
```

## Web REPL
ブラウザ上で直接試すことができます: [Web REPL](https://shota-kori.github.io/delim-cont-lambda/html/)

ダイアログ上に前回の結果とプロンプト(`>`)、入力欄が表示されるので、評価したい式を入力してOKを押してください。
Cancelを押すとプログラムを終了し、今までの入出力全体を見ることができます。

## 参考文献
- [shift/reset プログラミング入門](http://pllab.is.ocha.ac.jp/~asai/cw2011tutorial/main-j.pdf)

