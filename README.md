# Occupational injury rates by establishment size in Japan, 2009-2025

論文の表と本文の数値を公開統計から再現するコードとデータ。

## 実行

R と tidyverse で、`V2_HOME` にこのフォルダを指定して番号順に実行する。

```
V2_HOME=. Rscript 02_evidence.R
```

取得済みのデータは `cache/` に同梱しているので、e-Stat の appId がなくても `02_evidence.R` から実行できる。データを取り直すときだけ `01_fetch.R` を使い、appId を `~/.estat_appid` か環境変数 `ESTAT_APPID` に置く。

| ファイル | 内容 |
|---|---|
| `00_setup.R` | 共通の定義（規模区分、業種群、e-Stat の統計表 ID） |
| `01_fetch.R` | 確定値と e-Stat API の応答を `cache/` に取得する |
| `02_evidence.R` | 取得したデータを公表値と照合し、目録 `evidence/manifest.csv` を作る |
| `03_build.R` | 解析パネル `output/panel_all` と `output/panel_industry` を作る |
| `04_verify.R` | 表を出力し、本文と表の数値を再計算して照合する |
| `05_trace.R` | パネルのセルを生データのセルまでたどって印字する |
| `06_extra.R` | 本文に載せていない追加解析 |
| `07_census_pdf.R` | 経済センサスの値を e-Stat の表と概要 PDF の総数に並べて印字する |
| `08_manual_estat.R` | ブラウザで取得した e-Stat の表（`manual/`）を API の値と照合する。手順は `manual_estat.md` |

## データ

| フォルダ | 内容 |
|---|---|
| `cache/` | 確定値のワークブック、e-Stat API の応答、分類辞書 |
| `evidence/` | 分類名を付けた CSV、経済センサスの結果の概要 PDF、取得元 URL・取得日・MD5 の目録 |
| `output/` | 解析パネル（年×規模 102行、業種×規模×年 918行）と、JSIC 中分類・小分類から業種群への対応表 `jsic_groups.csv` |
| `manual/` | ブラウザで取得した e-Stat の表と照合の記録 |

分子は厚生労働省の労働災害統計（確定値）、分母は総務省・経済産業省の経済センサス。

業種群への割り当ては、安全衛生統計の業種分類と日本標準産業分類との対比表（業種区分一覧表、日本標準産業分類 第13回改定対応）に従う。表は厚生労働省の公開資料としては見当たらず、日本法令『月刊ビジネスガイド』2015年9月号の付録資料（https://www.horei.co.jp/linkbg/2015_9bg.pdf）として公開されているものを用いた。小分類が細分類で別の群に分かれる場合は、2016年経済センサス（e-Stat 0003218580）で常用雇用者の多い側に寄せた。どの小分類をどの群へ移したかと、その根拠は `jsic_groups.csv` の note 列にある。

## ライセンス

コードは MIT。同梱した統計表は各官庁の利用条件に従う。
