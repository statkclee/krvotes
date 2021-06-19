#' 제20대 2016년 국회의원 선거
#'
#' 2016년 4월 13일 치뤄진 국회의원 선거
#'
#' @name congress_2016
#' @source \url{https://www.nec.go.kr/site/nec/ex/bbs/View.do?cbIdx=1129&bcIdx=14879}
#' @format list-column을 갖는 티블
#' @examples
#'   속초시 <- congress_2016 %>% filter(precinct == "속초시") %>% pull(data_clean) %>% .[[1]]
NULL
