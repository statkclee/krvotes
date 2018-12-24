# 0. 팩키지 -------------
library(tidyverse)
library(readxl)
library(testthat)

# 1. 데이터 -------------
## 1.1. 지역구 한곳 -----
### 후보정당과 후보 변수명 처리 자동화
var_names <- c("emd","type","electorate","vote",
               paste0("party_", seq(1:21)), "total", "invalid", "abstention")

one_dat <- read_excel("data-raw/국회의원선거 개표결과(제20대)/지역구/9경기/개표상황(투표구별)_성남시분당구을.xlsx", sheet="sheet1", skip=4)

candidate_name <- one_dat %>%
    select(grep("[ㄱ-힗]", names(one_dat), value = TRUE)) %>%
    names %>% setdiff(., "계") %>%
    str_replace_all(., "\r\n", " ")

column_names <- c("emd","type","electorate","vote", candidate_name,
                  paste0("party_", seq(1:(21-length(candidate_name)))), "total", "invalid", "abstention")

column_names

### 데이터 정리

one_dat <- read_excel("data-raw/국회의원선거 개표결과(제20대)/지역구/9경기/개표상황(투표구별)_성남시분당구을.xlsx", sheet="sheet1", skip=4)

one_df <- one_dat %>%
    set_names(column_names) %>%
    select(-contains("party")) %>%
    filter(row_number() != 1) %>%
    mutate(emd = zoo::na.locf(emd)) %>% # 동별 NA 값 채워넣기
    filter(emd !="합계") %>%
    mutate(type = ifelse(is.na(type), emd, type)) %>%
    filter(type !="소계")

### 데이터 정합성 확인

test_that("국회선거  2018 후보득표검증", {

    local_sigungu_2018_check <- local_sigungu_2018 %>%
        filter(sido == "서울특별시" & precinct == "종로구") %>%
        pull(data_clean) %>% .[[1]] %>%
        mutate_at(vars(contains("당")), funs(as.numeric) ) %>%
        summarise(`더불어민주당 김영종` = sum(`더불어민주당 김영종`),
                  `자유한국당 이숙연`   = sum(`자유한국당 이숙연`),
                  `바른미래당 김복동`   = sum(`바른미래당 김복동`))

    expect_that( local_sigungu_2018_check$`더불어민주당 김영종`, equals(51305))
    expect_that( local_sigungu_2018_check$`자유한국당 이숙연`,   equals(19628))
    expect_that( local_sigungu_2018_check$`자유한국당 이숙연`,   equals(19628))
    expect_that( local_sigungu_2018_check$`바른미래당 김복동`,   equals(8765))
})

## 1.2. 지역구: 경기도 -----
### 디렉토리 파일명 생성
congress_dir_names <- list.dirs("data-raw/국회의원선거 개표결과(제20대)/지역구/9경기/")

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
    gg_list[[i]] <- read_excel(gg_dat$input_file_name[i], sheet="sheet1", skip=4)
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

    column_names <- c("emd","type","electorate","vote", candidate_name,
                      paste0("party_", seq(1:(21-length(candidate_name)))), "total", "invalid", "abstention")

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
        mutate(emd = zoo::na.locf(emd)) %>% # 동별 NA 값 채워넣기
        filter(emd !="합계") %>%
        mutate(type = ifelse(is.na(type), emd, type)) %>%
        filter(type !="소계")
}

gg_data_clean_df <- gg_data_clean_list %>% enframe %>%
    rename(data_clean = value)

gg_df <- bind_cols(gg_df, gg_data_clean_df)  %>%
    select(-name)


### 데이터 정합성 확인

test_that("국회선거 경기도 2018 후보득표검증", {

    gg_check_df <- gg_df %>%
        filter(sido == "경기" & precinct == "성남시분당구을") %>%
        pull(data_clean) %>% .[[1]] %>%
        summarise(`더불어민주당 김병욱` = sum(`더불어민주당 김병욱`),
                  `새누리당 전하진`     = sum(`새누리당 전하진`),
                  `국민의당 윤은숙`     = sum(`국민의당 윤은숙`),
                  `무소속 임태희`       = sum(`무소속 임태희`))

    expect_that( gg_check_df$`더불어민주당 김병욱`, equals(50661))
    expect_that( gg_check_df$`새누리당 전하진`,     equals(39367))
    expect_that( gg_check_df$`국민의당 윤은숙`,     equals(11936))
    expect_that( gg_check_df$`무소속 임태희`,       equals(23921))
})

## 1.3. 지역구: 전국 -----
### 디렉토리 파일명 생성
congress_dir_file_names <- list()

congress_dir_names <- list.dirs("data-raw/국회의원선거 개표결과(제20대)/지역구/")

congress_dir_names <- congress_dir_names[-1] # 자기자신 디렉토리 제거

for(i in 1:length(congress_dir_names)) {
    cat(i, ":", congress_dir_names[i], "\n")
    congress_file_names <- list.files(congress_dir_names[[i]]) %>%
        str_extract(., "^(?!\\~).*")  # 임시 엑셀파일 제거
    congress_file_names <- congress_file_names[!is.na(congress_file_names)]

    congress_dir_file_names[[i]] <- enc2native(paste0(congress_dir_names[[i]],"/", congress_file_names))
    cat(i, ":", congress_dir_file_names[[i]], "\n")
}

# listviewer::jsonedit(congress_dir_file_names)

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
    congress_list[[i]] <- read_excel(congress_dat$input_file_name[i], sheet="sheet1", skip=4)
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

    column_names <- c("emd","type","electorate","vote", candidate_name,
                      paste0("party_", seq(1:(21-length(candidate_name)))), "total", "invalid", "abstention")

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
        mutate(emd = zoo::na.locf(emd)) %>% # 동별 NA 값 채워넣기
        filter(emd !="합계") %>%
        mutate(type = ifelse(is.na(type), emd, type)) %>%
        filter(type !="소계")
}

congress_data_clean_df <- congress_data_clean_list %>% enframe %>%
    rename(data_clean = value)

congress_2018 <- bind_cols(congress_df, congress_data_clean_df)  %>%
    select(-name)

### 데이터 정합성 확인
test_that("국회선거 전국 2018 후보득표검증", {

    congress_check_df <- congress_2018 %>%
        filter(sido == "경기" & precinct == "성남시분당구을") %>%
        pull(data_clean) %>% .[[1]] %>%
        summarise(`더불어민주당 김병욱` = sum(`더불어민주당 김병욱`),
                  `새누리당 전하진`     = sum(`새누리당 전하진`),
                  `국민의당 윤은숙`     = sum(`국민의당 윤은숙`),
                  `무소속 임태희`       = sum(`무소속 임태희`))

    expect_that( congress_check_df$`더불어민주당 김병욱`, equals(50661))
    expect_that( congress_check_df$`새누리당 전하진`,     equals(39367))
    expect_that( congress_check_df$`국민의당 윤은숙`,     equals(11936))
    expect_that( congress_check_df$`무소속 임태희`,       equals(23921))
})

usethis::use_data(congress_2018, overwrite = TRUE)

