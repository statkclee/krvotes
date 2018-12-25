# 0. 팩키지 불러오기 -------------
library(tidyverse)
library(readxl)
library(testthat)

# 1. 원본데이터 가져오기 -------------
## 출처: https://bit.ly/2QRqyGQ
presid_2018_dat <- read_excel("data-raw/@@20170510-19대선-투표구별개표자료(공개용).xlsx", sheet="19대선", skip=1)

# 2. 데이터 정제작업 -------------
var_names <- c("X__1", "X__2", "X__3", "X__4", "X__5", "X__6", "더불어민주당\r\n문재인",
               "자유한국당\r\n홍준표", "국민의당\r\n안철수", "바른정당\r\n유승민",
               "정의당\r\n심상정", "새누리당\r\n조원진", "경제애국당\r\n오영국",
               "국민대통합당\r\n장성민", "늘푸른한국당\r\n이재오", "민중연합당\r\n김선동",
               "한국국민당\r\n이경희", "홍익당\r\n윤홍식", "무소속\r\n김민찬",
               "계", "X__7", "X__8")

president <- presid_2018_dat %>%
    rename(시도명=`X__1`, 구시군명=`X__2`, 읍면동명=`X__3`, 투표구명=`X__4`, 선거인수=`X__5`, 투표수=`X__6`,
              문재인=`더불어민주당\r\n문재인`,
              홍준표=`자유한국당\r\n홍준표`,
              안철수=`국민의당\r\n안철수`,
              유승민=`바른정당\r\n유승민`,
              심상정=`정의당\r\n심상정`,
              조원진=`새누리당\r\n조원진`,
              오영국=`경제애국당\r\n오영국`,
              장성민=`국민대통합당\r\n장성민`,
              이재오=`늘푸른한국당\r\n이재오`,
              김선동=`민중연합당\r\n김선동`,
              이경희=`한국국민당\r\n이경희`,
              윤홍식=`홍익당\r\n윤홍식`,
              김민찬=`무소속\r\n김민찬`,
              무표투표수=`X__7`,
              기권수=`X__8`) %>%
    filter(`시도명` != "전국") %>%
    filter(`구시군명` != "합계") %>%
    filter(`읍면동명` != "합계") %>%
    mutate(`투표구명` = ifelse(is.na(`투표구명`), `읍면동명`, `투표구명`)) %>% # because of NA
    filter(`투표구명` != "합계")

names(president) <- enc2native(names(president))

# 3. 단위테스트 검증 -------------

test_that("대선 2018 후보득표검증", {

    presid_2018_check_df <- president %>%
        summarise(`문재인` = sum(`문재인`),
                  `홍준표` = sum(`홍준표`),
                  `안철수` = sum(`안철수`),
                  `유승민` = sum(`유승민`),
                  `심상정` = sum(`심상정`))

    expect_that( presid_2018_check_df$문재인, equals(13423800))
    expect_that( presid_2018_check_df$홍준표, equals(7852849))
    expect_that( presid_2018_check_df$안철수, equals(6998342))
    expect_that( presid_2018_check_df$유승민, equals(2208771))
    expect_that( presid_2018_check_df$심상정, equals(2017458))
})

usethis::use_data(president, overwrite = TRUE)
