df_수탁 |> 
  group_by(세부영역, 주업무목적) |>
  count() |>
  ggplot(aes(x = 세부영역, y = n, fill = 주업무목적)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = -90, hjust = 0), 
        legend.position = 'bottom') + 
  labs(y = '사례수') + 
  theme(text = element_text(size=8))

ggsave("4-18.pdf", width = 13.5, height = 17.5, units = "cm")



## 조사 분야별 조사수

df_수탁 |> 
  group_by(세부영역, 대상학교급) |>
  count() |>
  ggplot(aes(x = 세부영역, y = n, fill = 대상학교급)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = -90, hjust = 0), 
        legend.position = 'bottom') + 
  labs(y = '사례수') + 
  theme(text = element_text(size=8))

ggsave("4-19.pdf", width = 13.5, height = 17.5, units = "cm")


## 조사 분야별 조사수

df_수탁 |> 
  group_by(세부영역, 데이터저장단위) |>
  count() |>
  ggplot(aes(x = 세부영역, y = n, fill = 데이터저장단위)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = -90, hjust = 0), 
        legend.position = 'bottom') + 
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(y = '사례수') + 
  theme(text = element_text(size=8))

ggsave("4-20.pdf", width = 13.5, height = 17.5, units = "cm")



## 조사 분야별 조사수
df_수탁 |> 
  group_by(세부영역, 공개범위) |>
  count() |>
  ggplot(aes(x = 세부영역, y = n, fill = 공개범위)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = -90, hjust = 0), 
        legend.position = 'bottom') + 
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(y = '사례수') + 
  theme(text = element_text(size=8))

ggsave("4-21.pdf", width = 13.5, height = 17.5, units = "cm")


## 조사 분야별 조사수
df_수탁 |> 
  group_by(세부영역, 데이터입력범위) |>
  count() |>
  ggplot(aes(x = 세부영역, y = n, fill = 데이터입력범위)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = -90, hjust = 0), 
        legend.position = 'bottom') + 
  labs(y = '사례수') + 
  theme(text = element_text(size=8))

ggsave("4-22.pdf", width = 13.5, height = 17.5, units = "cm")



## 조사 분야별 조사수
df_수탁 |> 
  group_by(세부영역, 데이터갱신주기) |>
  count() |>
  ggplot(aes(x = 세부영역, y = n, fill = 데이터갱신주기)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = -90, hjust = 0), 
        legend.position = 'bottom') + 
  labs(y = '사례수') + 
  theme(text = element_text(size=8))

ggsave("4-23.pdf", width = 13.5, height = 17.5, units = "cm")

## 조사 분야별 조사수
df_수탁 |> 
  group_by(세부영역, 데이터구축방법) |>
  count() |>
  ggplot(aes(x = 세부영역, y = n, fill = 데이터구축방법)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = -90, hjust = 0), 
        legend.position = 'bottom') + 
  labs(y = '사례수') + 
  theme(text = element_text(size=8))

ggsave("4-24.pdf", width = 13.5, height = 17.5, units = "cm")

## 조사 분야별 조사수
df_수탁 |> 
  group_by(세부영역, 데이터형태) |>
  count() |>
  ggplot(aes(x = 세부영역, y = n, fill = 데이터형태)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = -90, hjust = 0), 
        legend.position = 'bottom') + 
  labs(y = '사례수') + 
  theme(text = element_text(size=8))

ggsave("4-25.pdf", width = 13.5, height = 17.5, units = "cm")

## 조사 분야별 조사수
df_수탁 |> 
  group_by(세부영역, 데이터저장방법) |>
  count() |>
  ggplot(aes(x = 세부영역, y = n, fill = 데이터저장방법)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = -90, hjust = 0), 
        legend.position = 'bottom') + 
  guides(fill = guide_legend(nrow = 3, byrow = TRUE)) +
  labs(y = '사례수') + 
  theme(text = element_text(size=8))

ggsave("4-26.pdf", width = 13.5, height = 17.5, units = "cm")


## 조사 분야별 조사수
df_수탁 |> 
  group_by(세부영역, 공개대상) |>
  count() |>
  ggplot(aes(x = 세부영역, y = n, fill = 공개대상)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = -90, hjust = 0), 
        legend.position = 'bottom') + 
  guides(fill = guide_legend(nrow = 3, byrow = TRUE)) +
  labs(y = '사례수')  + 
  theme(text = element_text(size=8))

ggsave("4-27.pdf", width = 13.5, height = 17.5, units = "cm")



df_수탁 |> 
  group_by(세부영역, 고유식별정보보유) |>
  count() |>
  ggplot(aes(x = 세부영역, y = n, fill = 고유식별정보보유)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = -90, hjust = 0), 
        legend.position = 'bottom') + 
  labs(y = '사례수') + 
  theme(text = element_text(size=8))

ggsave("4-28.pdf", width = 13.5, height = 17.5, units = "cm")



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
  scale_x_discrete(labels = c("교육\n전반", "유초중등\n교육", "고등\n교육", '평생\n교육', '기타')) + 
  theme(text = element_text(size=8))




ggsave("4-29.pdf", width = 13.5, height = 17.5, units = "cm")




#####################################



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
  scale_x_discrete(labels = c("교육\n전반", "유초중등\n교육", "고등\n교육", '평생\n교육', '기타')) + 
  theme(text = element_text(size=8))


ggsave("4-30.pdf", width = 13.5, height = 17.5, units = "cm")



#####################################




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
  scale_y_discrete(limits=rev) + 
  theme(text = element_text(size=8))


ggsave("4-31.pdf", width = 13.5, height = 17.5, units = "cm")


#####################################





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
  scale_y_discrete(limits=rev) + 
  theme(text = element_text(size=8))

ggsave("4-32.pdf", width = 13.5, height = 17.5, units = "cm")
