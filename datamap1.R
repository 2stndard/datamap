library(readxl)
library(tidyverse)
## install.packages('clipr')
library(clipr)
install.packages('ggthemes')
library(ggthemes)

df_수탁 <- read_excel('D:/Work/2021/데이터맵/수탁데이터 조사표(최종).xlsx', skip = 2, na = '-', sheet = '조사표(작성양식)',  col_names = F)

df_수탁 <- read_excel('./수탁데이터 조사표(최종).xlsx', skip = 2, na = '-', sheet = '조사표(작성양식)',  col_names = F)


View(df_수탁)

colnames(df_수탁) <- c('연번', '기관명', '시스템명', '데이터명', '관리부서', '데이터목적', '데이터설명', '세부영역', '주업무목적', '주업무목적_기타', '대상학교급', '대상학교급_기타', '데이터저장단위', '데이터저장단위_기타', '공개범위', '공개범위_기타', '데이터입력범위', '데이터입력범위_기타', '데이터갱신주기', '데이터갱신주기_기타', '시작년도', '시작년도_기타', '데이터구축방법', '데이터구축방법_기타', '데이터형태', '데이터형태_기타', '데이터저장방법', '데이터저장방법_기타', '공개대상', '공개대상_기타', '고유식별정보보유', '고유식별정보보유_기타', '문의처')


df_수탁 <- df_수탁 |> 
  filter(!is.na(주업무목적))

distinct(df_수탁, 대상학교급)


df_수탁$대상학교급 <- fct_relevel(df_수탁$대상학교급, '교육전반', '유초중등교육', '고등교육', '평생교육')

df_수탁$세부영역 <- fct_relevel(df_수탁$세부영역, '기관영역(학교)', '교원(강사)정보영역', '학급(학과)정보영역', '학생정보영역', '학부모정보영역', '교육과정(강좌)운영영역', '학업성취영역', '시설기자재영역', '예결산영역', '교육지원영역', '학생역량영역', '교원역량영역', '기타')

df_수탁$공개대상 <- ifelse(stringr::str_detect(df_수탁$공개대상, '3단계'), '3단계(1, 2 단계 및 대국민)', df_수탁$공개대상)  

df_수탁$데이터저장단위 <- fct_relevel(df_수탁$데이터저장단위, '개별 단위(학교별, 교원별, 학생별 등)', '집계 통계 단위(학교수, 학생수 등)', '기타')

df_수탁$데이터갱신주기 <- fct_relevel(df_수탁$데이터갱신주기, '매년', '매학기', '매분기', '매월', '매주', '매일', '비정기', '기타')

df_수탁$공개범위 <- fct_relevel(df_수탁$공개범위, '전체공개', '일부공개', '가공(가명화, 통계처리 등) 후 공개', '공개불가', '기타')


## 기관별 데이터명
df_수탁 |> 
  group_by(기관명, 시스템명) |>
  ungroup() |>
  distinct(기관명, 시스템명) |>
  write_clip()

df_수탁 |> 
  group_by(기관명, 시스템명, 데이터명) |>
  ungroup() |>
  count(기관명, 시스템명, 데이터명) |>
  write_clip()


df_수탁 |> 
  group_by(기관명) |>
  count(기관명, 시스템명) |>
  write_clip()


## 조사 분야별 조사수
df_수탁 |> 
  group_by(세부영역, 주업무목적) |>
  count() |>
  ungroup()|>
  spread(주업무목적, n) |>
  write_clip()

df_수탁 |> 
  group_by(세부영역, 주업무목적) |>
  count() |>
  ggplot(aes(x = 세부영역, y = n, fill = 주업무목적)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = -90, hjust = 0), 
        legend.position = 'bottom') + 
  labs(y = '사례수')

ggsave("4-18.pdf", width = 13.5, height = 17.5, units = "cm")



## 조사 분야별 조사수
df_수탁 |> 
  group_by(세부영역, 대상학교급) |>
  count() |>
  ungroup()|>
  spread(대상학교급, n) |>
  write_clip()

df_수탁 |> 
  group_by(세부영역, 대상학교급) |>
  count() |>
  ggplot(aes(x = 세부영역, y = n, fill = 대상학교급)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = -90, hjust = 0), 
        legend.position = 'bottom') + 
  labs(y = '사례수')

ggsave("4-19.pdf", width = 13.5, height = 17.5, units = "cm")


## 조사 분야별 조사수
df_수탁 |> 
  group_by(세부영역, 데이터저장단위) |>
  count() |>
  ungroup()|>
  spread(데이터저장단위, n) |>
  write_clip()

df_수탁 |> 
  group_by(세부영역, 데이터저장단위) |>
  count() |>
  ggplot(aes(x = 세부영역, y = n, fill = 데이터저장단위)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = -90, hjust = 0), 
        legend.position = 'bottom') + 
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(y = '사례수')

ggsave("4-20.pdf", width = 13.5, height = 17.5, units = "cm")



## 조사 분야별 조사수
df_수탁 |> 
  group_by(세부영역, 공개범위) |>
  count() |>
  ungroup()|>
  spread(공개범위, n) |>
  write_clip()

df_수탁 |> 
  group_by(세부영역, 공개범위) |>
  count() |>
  ggplot(aes(x = 세부영역, y = n, fill = 공개범위)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = -90, hjust = 0), 
        legend.position = 'bottom') + 
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(y = '사례수')

ggsave("4-21.pdf", width = 13.5, height = 17.5, units = "cm")


## 조사 분야별 조사수
df_수탁 |> 
  group_by(세부영역, 데이터입력범위) |>
  count() |>
  ungroup()|>
  spread(데이터입력범위, n) |>
  write_clip()

df_수탁 |> 
  group_by(세부영역, 데이터입력범위) |>
  count() |>
  ggplot(aes(x = 세부영역, y = n, fill = 데이터입력범위)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = -90, hjust = 0), 
        legend.position = 'bottom') + 
  labs(y = '사례수')

ggsave("4-21.pdf", width = 13.5, height = 17.5, units = "cm")



## 조사 분야별 조사수
df_수탁 |> 
  group_by(세부영역, 데이터갱신주기) |>
  count() |>
  ungroup()|>
  spread(데이터갱신주기, n) |>
  write_clip()

df_수탁 |> 
  group_by(세부영역, 데이터갱신주기) |>
  count() |>
  ggplot(aes(x = 세부영역, y = n, fill = 데이터갱신주기)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = -90, hjust = 0), 
        legend.position = 'bottom') + 
  labs(y = '사례수')

ggsave("4-22.pdf", width = 13.5, height = 17.5, units = "cm")

## 조사 분야별 조사수
df_수탁 |> 
  group_by(세부영역, 데이터구축방법) |>
  count() |>
  ungroup()|>
  spread(데이터구축방법, n) |>
  write_clip()

df_수탁 |> 
  group_by(세부영역, 데이터구축방법) |>
  count() |>
  ggplot(aes(x = 세부영역, y = n, fill = 데이터구축방법)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = -90, hjust = 0), 
        legend.position = 'bottom') + 
  labs(y = '사례수')

ggsave("4-23.pdf", width = 13.5, height = 17.5, units = "cm")

## 조사 분야별 조사수
df_수탁 |> 
  group_by(세부영역, 데이터형태) |>
  count() |>
  ungroup()|>
  spread(데이터형태, n) |>
  write_clip()

df_수탁 |> 
  group_by(세부영역, 데이터형태) |>
  count() |>
  ggplot(aes(x = 세부영역, y = n, fill = 데이터형태)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = -90, hjust = 0), 
        legend.position = 'bottom') + 
  labs(y = '사례수')

ggsave("4-24.pdf", width = 13.5, height = 17.5, units = "cm")

## 조사 분야별 조사수
df_수탁 |> 
  group_by(세부영역, 데이터저장방법) |>
  count() |>
  ungroup()|>
  spread(데이터저장방법, n) |>
  write_clip()

df_수탁 |> 
  group_by(세부영역, 데이터저장방법) |>
  count() |>
  ggplot(aes(x = 세부영역, y = n, fill = 데이터저장방법)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = -90, hjust = 0), 
        legend.position = 'bottom') + 
  guides(fill = guide_legend(nrow = 3, byrow = TRUE)) +
  labs(y = '사례수')

ggsave("4-25.pdf", width = 13.5, height = 17.5, units = "cm")

distinct(df_수탁, 공개대상)

df_수탁 |> 
  filter(stringr::str_detect(공개대상, '3단계')) |>
  mutate(공개대상 = '3단계(1, 2 단계 및 대국민)') |>
  View()





## 조사 분야별 조사수
df_수탁 |> 
  group_by(세부영역, 공개대상) |>
  count() |>
  ungroup()|>
  spread(공개대상, n) |>
  write_clip()

df_수탁 |> 
  group_by(세부영역, 공개대상) |>
  count() |>
  ggplot(aes(x = 세부영역, y = n, fill = 공개대상)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = -90, hjust = 0), 
        legend.position = 'bottom') + 
  guides(fill = guide_legend(nrow = 3, byrow = TRUE)) +
  labs(y = '사례수') 

ggsave("4-26.pdf", width = 13.5, height = 17.5, units = "cm")



df_수탁 |> 
  group_by(세부영역, 고유식별정보보유) |>
  count() |>
  ungroup()|>
  spread(고유식별정보보유, n) |>
  write_clip()

df_수탁 |> 
  group_by(세부영역, 고유식별정보보유) |>
  count() |>
  ggplot(aes(x = 세부영역, y = n, fill = 고유식별정보보유)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = -90, hjust = 0), 
        legend.position = 'bottom') + 
  labs(y = '사례수')

ggsave("4-27.pdf", width = 13.5, height = 17.5, units = "cm")



#################################

df_수탁 |> 
  group_by(대상학교급, 세부영역, 주업무목적) |>
  count() |>
  group_by(세부영역) |>
  mutate(nn = sum(n)) |> 
  ggplot(aes(x = 대상학교급, y = 주업무목적)) +
  geom_rect(aes(xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=Inf, fill = nn)) +
  geom_text(aes(label = n), size = 3, color = 'white') + 
  facet_wrap(~세부영역, ncol = 2) +
  theme(axis.text.x = element_text(angle = 0),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "black")) +
  labs(fill = '전체사례수')  +
  scale_x_discrete(labels = c("교육\n전반", "유초중등\n교육", "고등\n교육", '평생\n교육', '기타'))




ggsave("4-28.pdf", width = 13.5, height = 17.5, units = "cm")




#####################################

distinct(df_수탁, 데이터저장단위)


df_수탁 |> 
  group_by(대상학교급, 세부영역, 데이터저장단위) |>
  count() |>
  group_by(세부영역) |>
  mutate(nn = sum(n)) |> 
  ggplot(aes(x = 대상학교급, y = 데이터저장단위)) +
  geom_rect(aes(xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=Inf, fill = nn)) +
  geom_text(aes(label = n), size = 3, color = 'white') + 
  facet_wrap(~세부영역, ncol = 2) +
  theme(axis.text.x = element_text(angle = 0),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "black")) +
  labs(fill = '전체사례수') + 
  scale_y_discrete(labels = c("기타", "집계 통계 단위", "개별 단위"), 
                   limits=rev) +
  scale_x_discrete(labels = c("교육\n전반", "유초중등\n교육", "고등\n교육", '평생\n교육', '기타'))


ggsave("4-29.pdf", width = 13.5, height = 17.5, units = "cm")



#####################################

distinct(df_수탁, 공개범위)



df_수탁 |> 
  group_by(대상학교급, 세부영역, 공개범위) |>
  count() |>
  group_by(세부영역) |>
  mutate(nn = sum(n)) |> 
  ggplot(aes(x = 대상학교급, y = 공개범위)) +
  geom_rect(aes(xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=Inf, fill = nn)) +
  geom_text(aes(label = n), size = 3, color = 'white') + 
  facet_wrap(~세부영역, ncol = 2) +
  theme(axis.text.x = element_text(angle = 0),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "black")) +
  labs(fill = '전체사례수') +
  scale_x_discrete(labels = c("교육\n전반", "유초중등\n교육", "고등\n교육", '평생\n교육', '기타')) + 
    scale_y_discrete(limits=rev)


ggsave("4-30.pdf", width = 13.5, height = 17.5, units = "cm")


#####################################


distinct(df_수탁, 데이터저장단위)
distinct(df_수탁, 데이터갱신주기)



df_수탁 |> 
  group_by(데이터저장단위, 세부영역, 공개범위) |>
  count() |>
  group_by(세부영역) |>
  mutate(nn = sum(n)) |> 
  ggplot(aes(x = 공개범위, y = 데이터저장단위)) +
  geom_rect(aes(xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=Inf, fill = nn)) +
  geom_text(aes(label = n), size = 3, color = 'white') + 
  facet_wrap(~세부영역, ncol = 2) +
  theme(axis.text.x = element_text(angle = 0),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "black")) +
  labs(fill = '전체사례수') + 
  scale_x_discrete(labels = c("전체\n공개", "일부\n공개", "가공후\n공개", '공개\n불가', '기타')) + 
  scale_y_discrete(limits=rev)

ggsave("4-31.pdf", width = 13.5, height = 17.5, units = "cm")
