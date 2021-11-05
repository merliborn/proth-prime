# More Equal Pseudorandom Number Generator

Latest update: 2021-08-22
By: Lat.S (@merliborn)

## 目的

- いくつかの分割数Nについて、乱数をN分割した各確率が正確に等分になるよう、疑似乱数生成器の周期を求める。

### 全体仕様

1. 乱数器はCMWC (Complementary Multiply with Carry, [CL97]) を用いる。これにより可能な周期の種類が多くなる。

## 構成プログラムおよび仕様

### proth-prime&mdash;Proth数の素数判定テスト

- N = a\*2^b + 1 (aは正の奇数、a&lt;2^b) の形の数に対して、Prothの定理を用いて素数判定を行う。
- アルゴリズム：Sze [Sze18]。決定的アルゴリズム。計算量の期待値および最悪計算量は、N = a\*2^b + 1 に対して &Otilde;((a\*log a+log N)\*log N) および &Otilde;((a\*log a+log N)\*(log N)^2) となる。
- [MITライセンス](https://opensource.org/licenses/MIT)で提供

アルゴリズムのための補助的機能

- 与えられた N = a\*2^b + 1 に対して、x^2≡-1 (mod N) となるxを見つける。このアルゴリズムはNが素数ならば必ず成功する。
- 与えられた N = a\*2^b + 1 と 1&lt;x&lt;N-1 を満たす x, そして既知の d^2≡-1 (mod N) を満たす d に対して、z^2≡x (mod N) となるzを見つける。このアルゴリズムはNが素数ならば必ず成功する。

将来的に実装したい機能

- NをInt32,64,Word32,64型の範囲に制限したもの
- モンゴメリ乗算の利用による高速化

以下の機能はproth-primeから分離させるかもしれない：

1. 与えられた数 a と上限 L に対して、Proth素数 p = a\*2^b+1 を探索する。ただし p&lt;=L で見つけられなかった場合はFailとする。
1. 与えられた数 a と指数の下界 m および上限 L に対して、Proth素数 p = a\*2^b+1 (b&gt;m) を探索する。ただし p&lt;=L で見つけられなかった場合はFailとする。

### (ファイル名未定)&mdash;乱数生成器のパラメータ決定

### (ファイル名未定)&mdash;乱数生成器本体あるいはジェネレータ

- CMWC法によって動作する乱数生成器の本体、あるいは必要なパラメータから乱数生成器を生成するジェネレータ (仕様未確定)。

- 不都合が無ければ既存ライブラリの仕様も考慮。

## ライセンス

- Haskellモジュールの全体はMIT Licenseで提供

### このテキストのライセンス

このテキストそのものは、[CC BY 4.0 International](https://creativecommons.org/licenses/by/4.0/)の下で利用可能とします。

&copy; Merliborn (@merliborn) 2021. This text is licensed under the [Creative Commons Attribution 4.0 International](https://creativecommons.org/licenses/by/4.0/deed.en).

## 参考文献

- [CL97] Raymond Couture, and Pierre L'Ecuyer. (1997) Distribution Properties of Multiply-with-Carry Random Number Generators, *Mathematics of Computation* 66(218), pp. 591&ndash;607. ISSN:1088-6842, doi:[10.1090/S0025-5718-97-00827-2](https://doi.org/10.1090/S0025-5718-97-00827-2), JSTOR:[2153884](https://www.jstor.org/stable/2153884).

- [Sze98] Sze, Tsz-Wo (施子和). (2018) *Deterministic Primality Proving on Proth Numbers*. Unpublished, arXiv:[0812.2596](https://arxiv.org/abs/0812.2596)
