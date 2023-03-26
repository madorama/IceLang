# Syntax

## コメント

`//` から行末までは `行コメント`、 `/*` `*/` で囲むとブロックコメントになります。

コメントは、コンパイル後のAiScriptには残りません。

```
let x = 42 // line comment
/*
  block comment
*/
```

ブロックコメントはネスト可能です。

```
/*
  /*
    nested block comment
  */
*/
```

## リテラル

```
true
false

42
3.14

"string"
'string'

`template {"string"}`

[1, 2, 3]

{ field: 42 }

null
```

## 変数宣言

```
let x = 42
var y = 42
```

`let` で宣言された変数はイミュータブルになります。

## 代入

```
var x = 42
x = 0
```

代入演算子は以下が使用可能です。

| 演算子 |
| --- |
| =     |
| +=    |
| -=    |

`let` で宣言された変数に代入しようとすると、エラーになります。

## 関数定義

```
@add(x, y) {
  x + y
}
```

## for

```
for let i, 10 {
  print(i)
}
```

```
for let i = 0, 10 {
  print(i)
}
```

```
var i = 0
for 10 {
  print(i)
  i += 2
}
```

## each

```
let xs = [1, 2, 3]
each let x, xs {
  print(x)
}
```

## loop

```
var i = 0
loop {
  if i % 5 == 0 continue
  if i >= 10 break
  print(i)
  i += 1
}
```

## 演算子

以下の演算子が存在します。

|  演算子  | 優先度 | 結合性  |
| :------: | :----: | :-----: |
| !        | 7      | 前置    |
| *        | 6      | 左結合  |
| /        | 6      | 左結合  |
| %        | 6      | 左結合  |
| ^        | 6      | 左結合  |
| +        | 5      | 左結合  |
| -        | 5      | 左結合  |
| <=       | 4      | なし    |
| >=       | 4      | なし    |
| ==       | 4      | なし    |
| !=       | 4      | なし    |
| <        | 4      | なし    |
| >        | 4      | なし    |
| &&       | 3      | 左結合  |
| \|\|     | 2      | 左結合  |
| ??       | 1      | 左結合  |
| \|>      | 0      | 左結合  |

### null合体演算子

左辺が `null` の時、右辺の値が評価されます。

```
x ?? 42

// 以下と等価

if x != null { x } else { 42 }
```

### パイプライン演算子

左辺の値を右辺の関数の最後の引数に適用します。

```
let map = @(f, xs) {
  xs.map(f)
}

[1, 2, 3]
  |> map(@(x) { x * 2 })

// 以下と等価

map(@(x) { x * 2 }, [1, 2, 3])
```

## オブジェクトアクセサ

```
obj.value
```

右辺は `識別子` のみを受け付けます。

```
obj1.obj2.value

// 以下と等価

(obj1.obj2).value
```

## オプショナルチェーン

```
obj?.value
```

左辺が `null` の場合、右辺を評価せず式全体が `null` になります。

```
obj1?.obj2?.value

// 以下と等価

if obj1 != null {
  if obj1.obj2 != null {
    obj1.obj2.value
  } else {
    null
  }
} else {
  null
}
```

右辺は `識別子` もしくは `関数適用式 (param1, param2 ...)` のみを受け付けます。

```
obj?.value
function?.(x, y)
```

## 名前空間アクセサ

```
Core:type
```

## 配列アクセサ

```
xs[0]
```

## ラムダ式

```
let identity = @(x) { x }
```

## 関数適用

```
identity(42)
```

## if式

```
if true {
  42
}
```

```
if x == 42 {
  print("x is 42")
} else {
  print("x is other")
}
```

```
if x == 42 {
  print("x is 42")
} elif x == 0 {
  print("x is 0")
} else {
  print("x is other")
}
```

```
if x == 42 {
  print("x is 42")
} else if x == 0 {
  print("x is 0")
} else {
  print("x is other")
}
```

```
let result =
  if x == 42 { "x is 42" }
  else { "x is other" }
```

## match式

```
match x {
  1 => 1
  * => 42
}
```

## ブロック式

```
let x = eval {
  42
}
```