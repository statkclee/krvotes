#' 제21대 2020년 국회의원 선거
#'
#' 2020년 4월 15일 치뤄진 국회의원 선거 득표 데이터
#'
#' @name congress_2020
#' @source \url{https://www.nec.go.kr/site/nec/ex/bbs/View.do?cbIdx=1129&bcIdx=15052}
#' @format list-column을 갖는 티블
#' @examples
#'   속초시 <- congress_2020 %>% filter(precinct == "속초시") %>% pull(data_clean) %>% .[[1]]
#'   bundang_eul <- congress_2020 %>% filter(precinct == "성남시분당구을") %>% pull(data_clean) %>% .[[1]]
NULL
