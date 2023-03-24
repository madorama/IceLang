# IceLang

[AiScript](https://github.com/syuilo/aiscript)にトランスパイルするプログラミング言語です。

構文は、ほぼAiScriptと同じです。

## 使用方法

### .exeファイルをダウンロードして使用する

[Release](https://github.com/madorama/IceLang/releases/latest)から `IceLang.exe` をダウンロードしてください。

以下のコマンドで、ファイルをコンパイルできます。

```
IceLang.exe FILENAME -o OUTPUT_FILENAME
```

### ビルドして使用する

[Stack](https://docs.haskellstack.org/en/stable/)が必要です。

インストール後、以下のコマンドでファイルをコンパイルできます。

```
stack run -- FILENAME -o OUTPUT_FILENAME
```

## 機能

### 簡易的な静的解析

簡易的な静的解析が備わっています。

#### letへの再代入

`let` で宣言された変数に対して再代入を行おうとすると、エラーになります。

#### 定義されていない識別子

定義されていない識別子を使用しようとすると、エラーになります。

#### 定義済みの識別子

定義済みの識別子を、同じスコープ内で再定義しようとすると、エラーになります。

親スコープで定義された識別子は再定義可能です。

#### 予約済みの識別子

`__` から始まる識別子を定義しようとすると、コンパイラが生成する変数に使用される為、エラーになります。

### 使用できない構文

#### <:

`print` の糖衣構文である `<:` は使用できません。

### 追加された構文

#### ?.

`x?.y` のとき、`x` が `null` なら式は `null` になります。`x` が `null` でない場合、式は `y` になります。

`?.` の右辺は、`識別子` `関数呼び出し (p1 ...pn)` のみが有効です。

### FAQ

#### `FILENAME: hGetContents: invalid argument (invalid byte sequence)` というエラーが出ます

`chcp 65001` して、もう一度試してみてください。