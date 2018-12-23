<!-- README.md is generated from README.Rmd. Please edit that file -->
Korean Votes
============

The goal of `krvotes` is to provide the Korean votes information.

Installation
------------

You can install the devloping version of `krvotes` from
[github](https://github.com/statkclee/krvotes) with:

``` r
# install.packages('remotes')
remotes::install_github("statkclee/krvotes")
```

Example - Presidential Election in 2017
---------------------------------------

``` r
# load package
library(krvotes)
suppressMessages(library(tidyverse))
# read presidential votes 2018 and assign it to president_df
president_df <- president
# check the structure of the object
str(object = president_df)
#> Classes 'tbl_df', 'tbl' and 'data.frame':    18455 obs. of  11 variables:
#>  $ 시도명  : chr  "서울특별시" "서울특별시" "서울특별시" "서울특별시" ...
#>  $ 구시군명: chr  "종로구" "종로구" "종로구" "종로구" ...
#>  $ 읍면동명: chr  "거소·선상투표" "관외사전투표" "재외투표" "청운효자동" ...
#>  $ 투표구명: chr  "거소·선상투표" "관외사전투표" "재외투표" "관내사전투표" ...
#>  $ 선거인수: num  218 12803 2490 1784 2493 ...
#>  $ 투표수  : num  206 12803 1813 1784 1682 ...
#>  $ 문재인  : num  64 5842 987 819 664 ...
#>  $ 홍준표  : num  42 2025 215 331 451 ...
#>  $ 안철수  : num  65 2509 304 352 342 ...
#>  $ 유승민  : num  8 1156 75 120 107 ...
#>  $ 심상정  : num  15 1145 214 149 96 ...
```

Example - local Election in 2018
--------------------------------

``` r
local_2018_df <- local_2018

jeju_df <- local_2018_df %>%
  filter(str_detect(precinct, "제주")) %>%
  pull(data_clean) %>%
  .[[1]]

jeju_df %>%
  summarize(`문대림` = sum(democracy))
#> # A tibble: 1 x 1
#>   문대림
#>    <dbl>
#> 1 137901
```
