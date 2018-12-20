# packages -------------
library(tidyverse)
library(readxl)
library(testthat)

# read rawd data -------------

presid_2018_dat <- read_excel("data-raw/@@20170510-19대선-투표구별개표자료(공개용).xlsx", sheet="19대선", skip=1)


# data wrangling -------------

president <- presid_2018_dat %>%
    rename(시도명=`X__1`, 구시군명=`X__2`, 읍면동명=`X__3`, 투표구명=`X__4`, 선거인수=`X__5`, 투표수=`X__6`,
              문재인=`더불어민주당\r\n문재인`,
              홍준표=`자유한국당\r\n홍준표`,
              안철수=`국민의당\r\n안철수`,
              유승민=`바른정당\r\n유승민`,
              심상정=`정의당\r\n심상정`) %>%
    select(`시도명`, `구시군명`, `읍면동명`, `투표구명`, `선거인수`, `투표수`,
                  `문재인`, `홍준표`, `안철수`,
                  `유승민`, `심상정`) %>%
    filter(`시도명` != "전국") %>%
    filter(`구시군명` != "합계") %>%
    filter(`읍면동명` != "합계") %>%
    mutate(`투표구명` = ifelse(is.na(`투표구명`), `읍면동명`, `투표구명`)) %>% # because of NA
    filter(`투표구명` != "합계")

# unit test -------------


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
