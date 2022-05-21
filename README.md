
<!-- README.md is generated from README.Rmd. Please edit that file -->

# exampler

<!-- badges: start -->
<!-- badges: end -->

このパッケージは
[Qiita](https://qiita.com/wakuteka/items/54f81f32baf50e243461)
で紹介されていた事例を再現することを目的とする。

## Installation

You can install the development version of exampler from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("MTRBEKTCBT/exampler")
```

## Example

ある期間の 土日・祝日を除いた営業日日数をカウントする。

``` r
library(exampler)
count_business_days(2022, 1, 12)
#> [1] 245
```
