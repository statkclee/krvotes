#' 제20대 2016년 국회의원 선거
#'
#' @source http://www.nec.go.kr/portal/bbs/view/B0000338/37352.do?menuNo=200185&searchYear=&searchMonth=&searchWrd=%EA%B0%9C%ED%91%9C%EA%B2%B0%EA%B3%BC&searchCnd=1&viewType=&pageIndex=1&section=&searchOption1=
#' @format list-column을 갖는 티블
#' @examples
#'   속초시 <- congress_2016 %>% filter(precinct == "속초시") %>% pull(data_clean) %>% .[[1]]
'congress_2016'
