library(readxl)
library(tidyverse)
## install.packages('clipr')
library(clipr)


df_수탁 <- read_excel('D:/Work/2021/데이터맵/수탁데이터 조사표(최종).xlsx', skip = 2, na = '-', sheet = '조사표(작성양식)',  col_names = F)

View(df_수탁)

colnames(df_수탁) <- c('연번', '기관명', '시스템명', '데이터명', '관리부서', '데이터목적', '데이터설명', '세부영역', '주업무목적', '주업무목적_기타', '대상학교급', '대상학교급_기타', '데이터저장단위', '데이터저장단위_기타', '공개범위', '공개범위_기타', '데이터입력범위', '데이터입력범위_기타', '데이터갱신주기', '데이터갱신주기_기타', '시작년도', '시작년도_기타', '데이터구축방법', '데이터구축방법_기타', '데이터형태', '데이터형태_기타', '데이터저장방법', '데이터저장방법_기타', '공개대상', '공개대상_기타', '고유식별정보보유', '고유식별정보보유_기타', '문의처')


df_수탁 <- df_수탁 |> 
  filter(!is.na(주업무목적))

distinct(df_수탁, 대상학교급)


df_수탁$대상학교급 <- fct_relevel(df_수탁$대상학교급, '교육전반', '유초중등교육', '고등교육', '평생교육')

df_수탁$세부영역 <- fct_relevel(df_수탁$세부영역, '기관영역(학교)', '교원(강사)정보영역', '학급(학과)정보영역', '학생정보영역', '학부모정보영역', '교육과정(강좌)운영영역', '학업성취영역', '시설기자재영역', '예결산영역', '교육지원영역', '학생역량영역', '교원역량영역', '기타')

                          
## 기관별 데이터명
df_수탁 |> 
  group_by(기관명) |>
  count(기관명, 데이터명) |>
  write_clip()


## 조사 분야별 조사수
df_수탁 |> 
  group_by(세부영역, 주업무목적) |>
  count() |>
  ungroup()|>
  spread(주업무목적, n) |>
  write_clip()

## 조사 분야별 조사수
df_수탁 |> 
  group_by(세부영역, 대상학교급) |>
  count() |>
  ungroup()|>
  spread(대상학교급, n) |>
  write_clip()

## 조사 분야별 조사수
df_수탁 |> 
  group_by(세부영역, 데이터저장단위) |>
  count() |>
  ungroup()|>
  spread(데이터저장단위, n) |>
  write_clip()

## 조사 분야별 조사수
df_수탁 |> 
  group_by(세부영역, 공개범위) |>
  count() |>
  ungroup()|>
  spread(공개범위, n) |>
  write_clip()



## 조사 분야별 조사수
df_수탁 |> 
  group_by(세부영역, 데이터입력범위) |>
  count() |>
  ungroup()|>
  spread(데이터입력범위, n) |>
  write_clip()


## 조사 분야별 조사수
df_수탁 |> 
  group_by(세부영역, 데이터갱신주기) |>
  count() |>
  ungroup()|>
  spread(데이터갱신주기, n) |>
  write_clip()


## 조사 분야별 조사수
df_수탁 |> 
  group_by(세부영역, 데이터구축방법) |>
  count() |>
  ungroup()|>
  spread(데이터구축방법, n) |>
  write_clip()


## 조사 분야별 조사수
df_수탁 |> 
  group_by(세부영역, 데이터형태) |>
  count() |>
  ungroup()|>
  spread(데이터형태, n) |>
  write_clip()


## 조사 분야별 조사수
df_수탁 |> 
  group_by(세부영역, 데이터저장방법) |>
  count() |>
  ungroup()|>
  spread(데이터저장방법, n) |>
  write_clip()


distinct(df_수탁, 공개대상)

df_수탁 |> 
  filter(stringr::str_detect(공개대상, '3단계')) |>
  mutate(공개대상 = '3단계(1, 2 단계 및 대국민)') |>
  View()
  
df_수탁$공개대상 <- ifelse(stringr::str_detect(df_수탁$공개대상, '3단계'), '3단계(1, 2 단계 및 대국민)', df_수탁$공개대상)  
  
  

## 조사 분야별 조사수
df_수탁 |> 
  group_by(세부영역, 공개대상) |>
  count() |>
  ungroup()|>
  spread(공개대상, n) |>
  write_clip()


df_수탁 |> 
  group_by(세부영역, 고유식별정보보유) |>
  count() |>
  ungroup()|>
  spread(고유식별정보보유, n) |>
  write_clip()
