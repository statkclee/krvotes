# 0. 팩키지 불러오기 -------------
library(tidyverse)
library(readxl)
library(testthat)
library(here)

# 제19대 대통령 -------------
## 1. 원본데이터 가져오기 -------------
## 출처: https://bit.ly/2QRqyGQ

file_src_2017 <- glue::glue("{here::here()}/data-raw/@@20170510-19대선-투표구별개표자료(공개용).xlsx")

file_src_2017 <- iconv(file_src_2017 ,"UTF-8", "CP949")

presid_2017_dat <- read_excel(file_src_2017, sheet="19대선", skip=1) %>%
    janitor::clean_names(ascii = FALSE)


## 2. 데이터 정제작업 -------------
var_names <- c("x1", "x2", "x3", "x4", "x5", "x6", "더불어민주당_문재인",
               "자유한국당_홍준표", "국민의당_안철수", "바른정당_유승민",
               "정의당_심상정", "새누리당_조원진", "경제애국당_오영국",
               "국민대통합당_장성민", "늘푸른한국당_이재오", "민중연합당_김선동",
               "한국국민당_이경희", "홍익당_윤홍식", "무소속_김민찬",
               "계", "x7", "x8")

president_2017 <- presid_2017_dat %>%
    rename(시도명=`x1`, 구시군명=`x2`, 읍면동명=`x3`, 투표구명=`x4`, 선거인수=`x5`, 투표수=`x6`,
              문재인=`더불어민주당_문재인`,
              홍준표=`자유한국당_홍준표`,
              안철수=`국민의당_안철수`,
              유승민=`바른정당_유승민`,
              심상정=`정의당_심상정`,
              조원진=`새누리당_조원진`,
              오영국=`경제애국당_오영국`,
              장성민=`국민대통합당_장성민`,
              이재오=`늘푸른한국당_이재오`,
              김선동=`민중연합당_김선동`,
              이경희=`한국국민당_이경희`,
              윤홍식=`홍익당_윤홍식`,
              김민찬=`무소속_김민찬`,
              무표투표수=`x21`,
              기권수=`x22`) %>%
    filter(`시도명` != "전국") %>%
    filter(`구시군명` != "합계") %>%
    filter(`읍면동명` != "합계") %>%
    mutate(`투표구명` = ifelse(is.na(`투표구명`), `읍면동명`, `투표구명`)) %>% # because of NA
    filter(`투표구명` != "합계")

# names(president_2017) <- iconv(names(president_2017), "UTF-8", "CP949")

## 3. 단위테스트 검증 -------------

test_that("대선 2018 후보득표검증", {

    presid_2017_check_df <- president_2017 %>%
        summarise(`문재인` = sum(`문재인`),
                  `홍준표` = sum(`홍준표`),
                  `안철수` = sum(`안철수`),
                  `유승민` = sum(`유승민`),
                  `심상정` = sum(`심상정`))

    expect_that( presid_2017_check_df$문재인, equals(13423800))
    expect_that( presid_2017_check_df$홍준표, equals(7852849))
    expect_that( presid_2017_check_df$안철수, equals(6998342))
    expect_that( presid_2017_check_df$유승민, equals(2208771))
    expect_that( presid_2017_check_df$심상정, equals(2017458))
})

usethis::use_data(president_2017, overwrite = TRUE)
