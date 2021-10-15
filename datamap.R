library(readxl)
library(tidyverse)
install.packages('clipr')
library(clipr)

df <- read_excel('D:/Work/2021/데이터맵/연구데이터 조사표(최종).xlsx', skip = 2, na = '-', sheet = '통합', col_types = c(rep('text',26)), col_names = F)


colnames(df) <- c('기관명', '과제명', '연구책임자', '조사명', '조사설명', '분석분야', '분석분야_기타', '분석대상', '분석대상_기타', '조사방법', '조사방법_기타', '설문대상', '설문대상_기타', '설문규모', '설문규모_기타', '설문지공개', '설문지공개_기타', '조사회수', '조사회수_기타', '응답자기본정보항목수', '응답자기본정보항목수_기타', '원본데이터포함여부', '원본데이터포함여부_기타', '다년조사', '다년조사_기타', '승인통계여부', '승인통계여부_기타')

distinct(df, 분석분야)



df$기관명 <- as.factor(df$기관명)
df$과제명 <- as.factor(df$과제명)
df$분석분야 <- as.factor(df$분석분야)
df$분석대상 <- as.factor(df$분석대상)
df$조사방법 <- as.factor(df$조사방법)
df$설문대상 <- as.factor(df$설문대상)
df$설문규모 <- as.factor(df$설문규모)
df$설문지공개 <- as.factor(df$설문지공개)
df$조사회수 <- as.factor(df$조사회수)
df$응답자기본정보항목수 <- as.factor(df$응답자기본정보항목수)
df$원본데이터포함여부 <- as.factor(df$원본데이터포함여부)
df$다년조사 <- as.factor(df$다년조사)
df$승인통계여부 <- as.factor(df$승인통계여부)

df$분석분야 <- fct_relevel(df$분석분야, '교육전반', '유아분야', '초중등교육', '고등교육', '평생교육', '기타')

##  기관별 과제명
df |> 
  group_by(기관명) |>
  count(기관명, 과제명) |>
  write_clip()

## 기관별 과제수
df |> 
  group_by(기관명) |>
  count(기관명, 과제명) |>
  group_by(기관명) |>
  ungroup() |>
  count(기관명) |>
  write_clip()


## 기관별 조사수
df |> 
  group_by(기관명)|>
  count(기관명) |>
  write_clip()


## 조사 분야별 조사수
df |> 
  group_by(기관명, 분석분야) |>
  count() |>
  ungroup()|>
  spread(분석분야, n) |>
  write_clip()


## 조사 대상별 조사수
df |> 
  group_by(기관명, 분석대상) |>
  count() |>
  ungroup()|>
  spread(분석대상, n) |>
  write_clip()

## 조사 대상별 조사방법수
df |> 
  group_by(기관명, 조사방법) |>
  count() |>
  ungroup()|>
  spread(조사방법, n) |>
  write_clip()


## 조사 대상별 설문대상수
df |> 
  group_by(기관명, 설문대상) |>
  count() |>
  ungroup()|>
  spread(설문대상, n) |>
  write_clip()


## 조사 대상별 설문규모수
df |> 
  group_by(기관명, 설문규모) |>
  count() |>
  ungroup()|>
  spread(설문규모, n) |>
  write_clip()


## 조사 대상별 설문지공개수
df |> 
  group_by(기관명, 설문지공개) |>
  count() |>
  ungroup()|>
  spread(설문지공개, n) |>
  write_clip()


## 조사 대상별 조사회수
df |> 
  group_by(기관명, 조사회수) |>
  count() |>
  ungroup()|>
  spread(조사회수, n) |>
  write_clip()


## 조사 대상별 응답자기본정보항목수
df |> 
  group_by(기관명, 응답자기본정보항목수) |>
  count() |>
  ungroup()|>
  spread(응답자기본정보항목수, n) |>
  write_clip()


## 조사 대상별 원본데이터포함여부
df |> 
  group_by(기관명, 원본데이터포함여부) |>
  count() |>
  ungroup()|>
  spread(원본데이터포함여부, n) |>
  write_clip()


## 조사 대상별 다년조사
df |> 
  group_by(기관명, 다년조사) |>
  count() |>
  ungroup()|>
  spread(다년조사, n) |>
  write_clip()



## 
df |> 
  group_by(기관명, 분석분야, 조사방법) |>
  count()|>
  spread(기관명, n) |>
  write_clip()

df |> 
  group_by(기관명, 분석분야, 조사방법) |>
  count() |>
  ungroup()|>
  ggplot(aes(x = 분석분야, y = 조사방법)) +
  geom_text(aes(label = n)) + 
  facet_wrap(~기관명)

## 
df |> 
  group_by(기관명, 분석분야, 분석대상) |>
  count() |>
  ungroup()|>
  ggplot(aes(x = 분석분야, y = 분석대상)) +
  geom_text(aes(label = n)) + 
  facet_wrap(~기관명)


## 
df |> 
  group_by(기관명, 분석분야, 설문대상) |>
  count() |>
  ungroup()|>
  ggplot(aes(x = 분석분야, y = 설문대상)) +
  geom_text(aes(label = n)) + 
  facet_wrap(~기관명)


## 
df |> 
  group_by(기관명, 조사방법, 설문대상) |>
  count() |>
  ungroup()|>
  ggplot(aes(x = 조사방법, y = 설문대상)) +
  geom_text(aes(label = n)) + 
  facet_wrap(~기관명)


## 
df |> 
  group_by(기관명, 조사방법, 설문규모) |>
  count() |>
  ungroup()|>
  ggplot(aes(x = 조사방법, y = 설문규모)) +
  geom_text(aes(label = n)) + 
  facet_wrap(~기관명)


## 
df |> 
  group_by(기관명, 설문대상, 설문규모) |>
  count() |>
  ungroup()|>
  ggplot(aes(x = 설문대상, y = 설문규모)) +
  geom_text(aes(label = n)) + 
  facet_wrap(~기관명)


## 
df |> 
  group_by(기관명, 분석분야, 설문규모) |>
  count() |>
  ungroup()|>
  ggplot(aes(x = 분석분야, y = 설문규모)) +
  geom_text(aes(label = n)) + 
  facet_wrap(~기관명)
