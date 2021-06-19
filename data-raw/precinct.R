# 2016년 국회의원 선거구 지역구

# 0. 팩키지 -------------
library(tidyverse)
library(readxl)
library(testthat)

# 1. 데이터 -------------

precinct_2016 <- read_csv("data-raw/precinct.csv")

usethis::use_data(precinct_2016, overwrite = TRUE)

