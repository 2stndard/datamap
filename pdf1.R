df |> 
  group_by(기관명, 분석분야) |>
  count() |>
  ggplot(aes(x = 기관명, y = n, fill = 분석분야)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = -0, hjust = 0.5), 
        legend.position = 'bottom') + 
  labs(y = '사례수') + 
  theme(text = element_text(size=8))

ggsave("4-1.pdf", width = 13.5, height = 10, units = "cm")

##################################

df |> 
  group_by(기관명, 조사방법) |>
  count() |>
  ggplot(aes(x = 기관명, y = n, fill = 조사방법)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = -0, hjust = 0.5), 
        legend.position = 'bottom') + 
  labs(y = '사례수') + 
  guides(fill = guide_legend(nrow = 2)) + 
  theme(text = element_text(size=8))

ggsave("4-3.pdf", width = 13.5, height = 10, units = "cm")



##################################

df |> 
  group_by(기관명, 분석대상) |>
  count() |>
  ggplot(aes(x = 기관명, y = n, fill = 분석대상)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = -0, hjust = 0.5), 
        legend.position = 'bottom') + 
  labs(y = '사례수') + 
  guides(fill = guide_legend(nrow = 1)) + 
  theme(text = element_text(size=8))

ggsave("4-2.pdf", width = 13.5, height = 10, units = "cm")


##################################

df |> 
  group_by(기관명, 설문대상) |>
  count() |>
  ggplot(aes(x = 기관명, y = n, fill = 설문대상)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = -0, hjust = 0.5), 
        legend.position = 'bottom') + 
  labs(y = '사례수') + 
  guides(fill = guide_legend(nrow = 3)) + 
  theme(text = element_text(size=8))

ggsave("4-4.pdf", width = 13.5, height = 10, units = "cm")


df |> 
           group_by(기관명, 설문규모) |>
           count() |>
           ggplot(aes(x = 기관명, y = n, fill = 설문규모)) +
           geom_col() +
           theme(axis.text.x = element_text(angle = -0, hjust = 0.5), 
                 legend.position = 'bottom') + 
           labs(y = '사례수') + 
           guides(fill = guide_legend(nrow = 1)) + 
  theme(text = element_text(size=8))

ggsave("4-5.pdf", width = 13.5, height = 10, units = "cm")


df |> 
  group_by(기관명, 분석분야, 조사방법) |>
  count() |>
  group_by(기관명) |>
  mutate(nn = sum(n)) |>   ggplot(aes(x = 분석분야, y = 조사방법)) +
  geom_rect(aes(xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=Inf, fill = nn)) +
  geom_text(aes(label = n), color = 'white') + 
  facet_wrap(~기관명, ncol = 1) +
  scale_y_discrete(labels = c("만족도조사","면담 및 FGI","설문조사", '패널조사', '협의및토론\n델파이')) +
  labs(fill = '전체사례수') + 
  theme(text = element_text(size=8))


ggsave("4-6.pdf", width = 13.5, height = 17.5, units = "cm")

## 
df |> 
  group_by(기관명, 분석분야, 분석대상) |>
  count() |>
  group_by(기관명) |>
  mutate(nn = sum(n)) |>   ggplot(aes(x = 분석분야, y = 분석대상)) +
  geom_rect(aes(xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=Inf, fill = nn)) +
  geom_text(aes(label = n), color = 'white') + 
  facet_wrap(~기관명, ncol = 1) +
  labs(fill = '전체사례수') + 
  theme(text = element_text(size=8))


ggsave("4-7.pdf", width = 13.5, height = 17.5, units = "cm")

## 
df |> 
  group_by(기관명, 분석분야, 설문대상) |>
  count() |>
  group_by(기관명) |>
  mutate(nn = sum(n)) |>   ggplot(aes(x = 분석분야, y = 설문대상)) +
  geom_rect(aes(xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=Inf, fill = nn)) +
  geom_text(aes(label = n), size = 2.5, color = 'white') + 
  facet_wrap(~기관명, ncol = 1) +
  labs(fill = '전체사례수') + 
  theme(text = element_text(size=8))




ggsave("4-8.pdf", width = 13.5, height = 17.5, units = "cm")

## 
df |> 
  group_by(기관명, 조사방법, 설문대상) |>
  count() |>
  group_by(기관명) |>
  mutate(nn = sum(n)) |>   
  ggplot(aes(x = 조사방법, y = 설문대상)) +
  geom_rect(aes(xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=Inf, fill = nn)) +
  geom_text(aes(label = n), size = 2.5, color = 'white') + 
  facet_wrap(~기관명, ncol = 1) +
  labs(fill = '전체사례수') +
  scale_x_discrete(labels = c("만족도조사","면담 및 FGI","설문조사", '패널조사', '협의및토론\n델파이')) + 
  theme(text = element_text(size=8))

ggsave("4-9.pdf", width = 13.5, height = 17.5, units = "cm")




## 
df |> 
  group_by(기관명, 조사방법, 설문규모) |>
  count() |>
  group_by(기관명) |>
  mutate(nn = sum(n)) |>   
  ggplot(aes(x = 조사방법, y = 설문규모)) +
  geom_rect(aes(xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=Inf, fill = nn)) +
  geom_text(aes(label = n), size = 2.5, color = 'white') + 
  facet_wrap(~기관명, ncol = 1) +
  labs(fill = '전체사례수') +
  scale_x_discrete(labels = c("만족도조사","면담 및 FGI","설문조사", '패널조사', '협의및토론\n델파이')) + 
  theme(text = element_text(size=8))

ggsave("4-10.pdf", width = 13.5, height = 17.5, units = "cm")




## 
df |> 
  group_by(기관명, 설문대상, 설문규모) |>
  count() |>
  group_by(기관명) |>
  mutate(nn = sum(n)) |>   
  ggplot(aes(x = 설문대상, y = 설문규모)) +
  geom_rect(aes(xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=Inf, fill = nn)) +
  geom_text(aes(label = n), size = 2.5, color = 'white') + 
  facet_wrap(~기관명, ncol = 1) +
  labs(fill = '전체사례수') +
  scale_x_discrete(labels = c("대국민","정부부처\n교원","연구자\n전문가", '학교행정가', '교원', '학생', '학부모', '전문가\n행정가\n교원', '기타')) + 
  theme(text = element_text(size=8))

ggsave("4-11.pdf", width = 13.5, height = 17.5, units = "cm")




## 
df |> 
  group_by(설문대상, 분석분야, 설문규모) |>
  count() |>
  group_by(설문대상) |>
  mutate(nn = sum(n)) |>   
  ggplot(aes(x = 분석분야, y = 설문규모)) +
  geom_rect(aes(xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=Inf, fill = nn)) +
  geom_text(aes(label = n), size = 3, color = 'white') + 
  facet_wrap(~설문대상, ncol = 2) +
  labs(fill = '전체사례수') +
  scale_x_discrete(labels = c("교육\n전반","유아\n분야","초중등\n교육", '고등\n교육', '평생\n교육', '기타')) + 
  theme(text = element_text(size=8))

ggsave("4-12.pdf", width = 13.5, height = 17.5, units = "cm")



## 
df |> 
  group_by(설문대상, 분석분야, 조사방법) |>
  count() |>
  group_by(설문대상) |>
  mutate(nn = sum(n)) |>   
  ggplot(aes(x = 분석분야, y = 조사방법)) +
  geom_rect(aes(xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=Inf, fill = nn)) +
  geom_text(aes(label = n), size = 3, color = 'white') + 
  facet_wrap(~설문대상, ncol = 2) +
  labs(fill = '전체사례수') +
  scale_x_discrete(labels = c("교육\n전반","유아\n분야","초중등\n교육", '고등\n교육', '평생\n교육', '기타')) +
  scale_y_discrete(labels = c("만족도조사","면담 및 FGI","설문조사", '패널조사', '협의및토론\n델파이')) + 
  theme(text = element_text(size=8))

ggsave("4-13.pdf", width = 13.5, height = 17.5, units = "cm")



## 
df |> 
  group_by(설문대상, 조사방법, 설문규모) |>
  count() |>
  group_by(설문대상) |>
  mutate(nn = sum(n)) |>   
  ggplot(aes(x = 조사방법, y = 설문규모)) +
  geom_rect(aes(xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=Inf, fill = nn)) +
  geom_text(aes(label = n), size = 3, color = 'white') + 
  facet_wrap(~설문대상, ncol = 2) +
  labs(fill = '전체사례수') +
  scale_x_discrete(labels = c("만족도조사","면담\nFGI","설문조사", '패널조사', '협의및토론\n델파이')) + 
  theme(text = element_text(size=8))

ggsave("4-14.pdf", width = 13.5, height = 17.5, units = "cm")
