# 2020년 국회의원 개표결과 --------------------------------

# 0. 팩키지 -------------
library(tidyverse)
library(readxl)
library(testthat)

# 1. 데이터 -------------
# 출처: https://www.nec.go.kr/site/nec/ex/bbs/View.do?cbIdx=1129&bcIdx=15052
## 1.1. 지역구 한곳 -----
### 후보정당과 후보 변수명 처리 자동화
var_names <- c("읍면동명","투표구명","선거인수","투표수",
               paste0("party_", seq(1:50)), "계", "무표투표수", "기권수")

one_dat <- read_excel("data-raw/제21대 국회의원선거(재보궐 포함) 투표구별 개표결과/지역구/9경기/개표상황(투표구별)_성남시분당구을.xlsx", sheet="2020년 제21대 국회의원선거", skip=4)

candidate_name <- one_dat %>%
    select(grep("[ㄱ-힗]", names(one_dat), value = TRUE)) %>%
    names %>% setdiff(., "계") %>%
    str_replace_all(., "\r\n", " ")

column_names <- c("읍면동명","투표구명","선거인수","투표수", candidate_name,
                  paste0("party_", seq(1:(50-length(candidate_name)))), "계", "무표투표수", "기권수")

### 데이터 정리

one_dat <- read_excel("data-raw/제21대 국회의원선거(재보궐 포함) 투표구별 개표결과/지역구/9경기/개표상황(투표구별)_성남시분당구을.xlsx", sheet="2020년 제21대 국회의원선거", skip=4)

names(one_dat) <- enc2native(names(one_dat))

one_df <- one_dat %>%
    set_names(column_names) %>%
    select(-contains("party")) %>%
    filter(row_number() != 1) %>%
    mutate(읍면동명 = zoo::na.locf(읍면동명)) %>% # 동별 NA 값 채워넣기
    filter(읍면동명 !="합계") %>%
    mutate(`투표구명` = ifelse(is.na(`투표구명`), `읍면동명`, `투표구명`)) %>%
    filter(`투표구명` !="소계")

### 데이터 정합성 확인

test_that("국회선거 성남시 분당구을  2020 후보득표검증", {

    one_df_check <- one_df %>%
        summarise(`더불어민주당 김병욱` = sum(`더불어민주당 김병욱`),
                  `미래통합당 김민수`     = sum(`미래통합당 김민수`),
                  `정의당 양호영`     = sum(`정의당 양호영`),
                  `무소속 이나영`       = sum(`무소속 이나영`))

    expect_that( one_df_check$`더불어민주당 김병욱`, equals(68387))
    expect_that( one_df_check$`미래통합당 김민수`,   equals(64342))
    expect_that( one_df_check$`정의당 양호영`,       equals(3021))
    expect_that( one_df_check$`무소속 이나영`,       equals(5662))
})

## 1.2. 지역구: 경기도 -----
### 디렉토리 파일명 생성
congress_dir_names <- list.dirs("data-raw/제21대 국회의원선거(재보궐 포함) 투표구별 개표결과/지역구/9경기/")

congress_file_names <- list.files(congress_dir_names) %>%
    str_extract(., "^(?!\\~).*")  # 임시 엑셀파일 제거
congress_file_names <- congress_file_names[!is.na(congress_file_names)]

congress_dir_file_names <- paste0(congress_dir_names, congress_file_names)

#### 한글경로명 이슈 : https://github.com/tidyverse/readxl/pull/477
congress_dir_file_names <- enc2native(congress_dir_file_names)

### 경기도 투표데이터 가져오기
#### 경기도 선거구 데이터 프레임작성
gg_dat <- tibble(
    sido = congress_dir_names,
    precinct = congress_file_names,
    input_file_name = congress_dir_file_names
)

gg_dat <- gg_dat %>%
    mutate(sido = str_extract(sido, "/([0-9])[ㄱ-흫].+/$") %>% str_remove_all(., "/|([0-9])")) %>%
    mutate(precinct = str_extract(precinct, "_[ㄱ-흫].+\\.") %>% str_remove_all(., "_|\\."))

#### 경기도 선거구별 개표결과 데이터 생성
gg_list <- list()

for(i in 1:nrow(gg_dat)) {
    gg_list[[i]] <- read_excel(gg_dat$input_file_name[i], sheet="2020년 제21대 국회의원선거", skip=4)
    names(gg_list[[i]]) <- enc2native(names(gg_list[[i]]))
}

gg_vote_df <- gg_list %>% enframe %>%
    rename(data = value)

#### 경기도 선거구와 개표결과 데이터 결합
gg_df <- bind_cols(gg_dat, gg_vote_df) %>%
    select(sido, precinct, data)

### 경기도 선거구와 개표결과 데이터 정제
#### 각 선거구별 칼럼명 정의
gg_candidate_name <- list()

for(i in 1:nrow(gg_vote_df)) {
    tmp_df <- gg_vote_df$data[i] %>% .[[1]]

    candidate_name <- tmp_df %>%
        select(grep("[ㄱ-힗]", names(tmp_df), value = TRUE)) %>%
        names %>% setdiff(., "계") %>%
        str_replace_all(., "\r\n", " ")

    column_names <- c("읍면동명","투표구명","선거인수","투표수", candidate_name,
                      paste0("party_", seq(1:(50-length(candidate_name)))), "계", "무표투표수", "기권수")

    gg_candidate_name[[i]] <- enc2native(column_names)
}

#### 각 선거구별 데이터 정제작업

gg_data_clean_list <- list()

for(i in 1:nrow(gg_vote_df)) {

    tmp_df <- gg_vote_df$data[i] %>% .[[1]]

    gg_data_clean_list[[i]] <- tmp_df %>%
        set_names(gg_candidate_name[[i]]) %>%
        select(-contains("party")) %>%
        filter(row_number() != 1) %>%
        mutate(읍면동명 = zoo::na.locf(읍면동명)) %>% # 동별 NA 값 채워넣기
        filter(읍면동명 !="합계") %>%
        mutate(`투표구명` = ifelse(is.na(`투표구명`), `읍면동명`, `투표구명`)) %>%
        filter(`투표구명` !="소계")
}

gg_data_clean_df <- gg_data_clean_list %>% enframe %>%
    rename(data_clean = value)

gg_df <- bind_cols(gg_df, gg_data_clean_df)  %>%
    select(-name)


### 데이터 정합성 확인

test_that("국회선거 경기도 2020 성남시분당구을 후보득표검증", {

    gg_check_df <- gg_df %>%
        filter(sido == "경기" & precinct == "성남시분당구을") %>%
        pull(data_clean) %>% .[[1]] %>%
        summarise(`더불어민주당 김병욱` = sum(`더불어민주당 김병욱`),
                  `미래통합당 김민수`     = sum(`미래통합당 김민수`),
                  `정의당 양호영`     = sum(`정의당 양호영`),
                  `무소속 이나영`       = sum(`무소속 이나영`))

    expect_that( gg_check_df$`더불어민주당 김병욱`, equals(68387))
    expect_that( gg_check_df$`미래통합당 김민수`,   equals(64342))
    expect_that( gg_check_df$`정의당 양호영`,       equals(3021))
    expect_that( gg_check_df$`무소속 이나영`,       equals(5662))
})

test_that("국회선거 경기도 2020 안산시상록구을 후보득표검증", {

    gg_check_tbl <- gg_df %>%
        filter(sido == "경기" & precinct == "안산시상록구을") %>%
        pull(data_clean) %>% .[[1]] %>%
        summarise(`더불어민주당 김철민` = sum(`더불어민주당 김철민`),
                  `미래통합당 홍장표`     = sum(`미래통합당 홍장표`))

    expect_that( gg_check_tbl$`더불어민주당 김철민`, equals(43599))
    expect_that( gg_check_tbl$`미래통합당 홍장표`,   equals(30747))
})


## 1.3. 지역구: 전국 -----
### 디렉토리 파일명 생성
congress_dir_file_names <- list()

congress_dir_names <- list.dirs("data-raw/제21대 국회의원선거(재보궐 포함) 투표구별 개표결과/지역구/")

congress_dir_names <- congress_dir_names[-1] # 자기자신 디렉토리 제거

for(i in 1:length(congress_dir_names)) {
    cat(i, ":", congress_dir_names[i], "\n")
    congress_file_names <- list.files(congress_dir_names[[i]]) %>%
        str_extract(., "^(?!\\~).*")  # 임시 엑셀파일 제거
    congress_file_names <- congress_file_names[!is.na(congress_file_names)]

    congress_dir_file_names[[i]] <- enc2native(paste0(congress_dir_names[[i]],"/", congress_file_names))
    cat(i, ":", congress_dir_file_names[[i]], "\n")
}

### 전국 투표데이터 가져오기
#### 전국 선거구 데이터 프레임작성
congress_dat <- tibble(
    input_file_name = congress_dir_file_names %>% unlist
)

congress_dat <- congress_dat %>%
    separate(input_file_name, into=c("sido", "precinct"), "\\/개표상황\\(투표구별\\)_", remove = FALSE) %>%
    mutate(sido = str_extract(sido, "/\\d+[ㄱ-힗].+$") %>% str_remove_all(., "/|([0-9])")) %>%
    mutate(precinct = str_remove(precinct, "\\.xlsx")) %>%
    select(sido, precinct, input_file_name)

#### 전국 선거구별 개표결과 데이터 생성
congress_list <- list()

for(i in 1:nrow(congress_dat)) {
    congress_list[[i]] <- read_excel(congress_dat$input_file_name[i], sheet="2020년 제21대 국회의원선거", skip=4)
    names(congress_list[[i]]) <- enc2native(names(congress_list[[i]]))
}

congress_vote_df <- congress_list %>% enframe %>%
    rename(data = value)

#### 경기도 선거구와 개표결과 데이터 결합
congress_df <- bind_cols(congress_dat, congress_vote_df) %>%
    select(sido, precinct, data)

### 전국 선거구와 개표결과 데이터 정제
#### 각 선거구별 칼럼명 정의
congress_candidate_name <- list()

for(i in 1:nrow(congress_vote_df)) {
    tmp_df <- congress_vote_df$data[i] %>% .[[1]]
    candidate_name <- tmp_df %>%
        select(grep("[ㄱ-힗]", names(tmp_df), value = TRUE)) %>%
        names %>% setdiff(., "계") %>%
        str_replace_all(., "\r\n", " ")

    column_names <- c("읍면동명","투표구명","선거인수","투표수", candidate_name,
                      paste0("party_", seq(1:(50-length(candidate_name)))), "계", "무표투표수", "기권수")

    congress_candidate_name[[i]] <- enc2native(column_names)
}


#### 각 선거구별 데이터 정제작업

congress_data_clean_list <- list()

for(i in 1:nrow(congress_vote_df)) {

    tmp_df <- congress_vote_df$data[i] %>% .[[1]]

    congress_data_clean_list[[i]] <- tmp_df %>%
        set_names(congress_candidate_name[[i]]) %>%
        select(-contains("party")) %>%
        filter(row_number() != 1) %>%
        mutate(읍면동명 = zoo::na.locf(읍면동명)) %>% # 동별 NA 값 채워넣기
        filter(읍면동명 !="합계") %>%
        mutate(`투표구명` = ifelse(is.na(`투표구명`), `읍면동명`, `투표구명`)) %>%
        filter(`투표구명` !="소계")
}

congress_data_clean_df <- congress_data_clean_list %>% enframe %>%
    rename(data_clean = value)

congress_2020 <- bind_cols(congress_df, congress_data_clean_df)  %>%
    select(-name)

### 데이터 정합성 확인
test_that("국회선거 2020 성남시분당구을 후보득표검증", {

    congress_check_df <- congress_2020 %>%
        filter(sido == "경기" & precinct == "성남시분당구을") %>%
        pull(data_clean) %>% .[[1]] %>%
        summarise(`더불어민주당 김병욱` = sum(`더불어민주당 김병욱`),
                  `미래통합당 김민수`     = sum(`미래통합당 김민수`),
                  `정의당 양호영`     = sum(`정의당 양호영`),
                  `무소속 이나영`       = sum(`무소속 이나영`))

    expect_that( congress_check_df$`더불어민주당 김병욱`, equals(68387))
    expect_that( congress_check_df$`미래통합당 김민수`,   equals(64342))
    expect_that( congress_check_df$`정의당 양호영`,       equals(3021))
    expect_that( congress_check_df$`무소속 이나영`,       equals(5662))
})

usethis::use_data(congress_2020, overwrite = TRUE)

