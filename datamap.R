library(readxl)
library(tidyverse)
install.packages('clipr')
library(clipr)

df <- read_excel('D:/Work/2021/데이터맵/데이터 분류 조사표(최종).xlsx', skip = 2, na = '-', sheet = '통합', col_types = c(rep('text',26)), col_names = F)

View(df)

colnames(df) <- c('기관명', '과제명', '연구책임자', '조사명', '조사설명', '분석분야', '분석분야_기타', '분석대상', '분석대상_기타', '조사방법', '조사방법_기타', '설문대상', '설문대상_기타', '설문규모', '설문규모_기타', '설문지공개', '설문지공개_기타', '조사회수', '조사회수_기타', '응답자기본정보항목수', '응답자기본정보항목수_기타', '원본데이터포함여부', '원본데이터포함여부_기타', '다년조사', '다년조사_기타', '승인통계여부', '승인통계여부_기타')

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

glimpse(df)

df |> 
  group_by(기관명)|>
  count(기관명)

df |> 
  group_by(기관명, 과제명)|>
  count(기관명, 과제명) |>
  write_clip()


df |> 
  group_by(기관명) |>
  count(기관명, 과제명) |>
  group_by(기관명) |>
  count(기관명)
  