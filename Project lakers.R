# 01.00 소스 불러오기 ----
source("C:\Users\hyung\OneDrive\바탕 화면\hr kaggle data/source_test.R")
library(installr)
check.for.updates.R()
install.R()
version

# libraries that I've used, those are in the Source_test.R file.
library(dplyr)
library(echarts4r)
library(data.table)
library(lubridate)
library(stringr)
library(readxl)
library(writexl)
library(clipr)
library(dlookr)
library_list <- library()
library_list$results %>% tbl_df %>%
  select(Package, Title) %>%
  print(n = 300)




# 01.10  ----
data()
lakers <- lubridate::lakers %>%
  as_tibble()


lakers <- lubridate::lakers %>% 
  as_tibble()


# 01.20 Check the datasets---
lakers %>% 
  colnames %>% 
  as_tibble() %>% 
  write_clip()

lakers %>%
  head(10) %>%
  t

lakers %>% 
  tail(10) %>% 
  t


# 02.00 preprocess ----

# 02.10 check NA or null values ----


lakers %>% 
  count(date) %>% 
  print(n=100)

lakers %>% 
  count(opponent) %>% 
  print(n=100)

lakers %>% 
  count(game_type) %>% 
  print(n=100)

lakers %>% 
  count(time, period) %>% 
  data.table %>% 
  dcast.data.table(time ~ period, value.var = "n", sum) %>% 
  as_tibble() %>% 
  print(n=1000)

lakers %>% 
  count(time) %>% 
  print(n=10000)

lakers %>% 
  count(period) %>% 
  print(n=100)

lakers %>% 
  count(etype) %>% 
  print(n=10000)

lakers %>% 
  count(team) %>% 
  print(n=10000)


lakers %>% 
  filter(date == '20081028') %>% 
  print(n=10000)

# 02.11 Null values : player ----
lakers %>% 
  count(player) %>% 
  print(n=10000)

# 02.12 Null values : result ----
lakers %>% 
  count(result)

lakers %>% 
  filter(player %>% nchar() <1)

lakers %>% 
  count(points)

# 02.13 Null values : type ----
lakers %>% 
  count(type) %>% 
  print(n=100)

# 02.14 Null values : x,y ----
lakers %>% 
  count(x,y) %>% 
  tail


# 02.20 Preprocess NA or null values ----
lakers <- lakers %>% 
  mutate(player = ifelse(player %>% nchar() < 1, NA, player),
         result = ifelse(result %>% nchar() < 1, NA, result),
         type = ifelse(type %>% nchar() < 1, NA, type))

# 02.30 preprocess.. ----
lakers <- lakers %>% 
  mutate(date = date %>% ymd)

# 02.31 Divide some actions into negative or positive. ----
lakers <- lakers %>% 
  mutate(etype =   ifelse(etype == "free throw", 'free_throw', etype),
         etype_2 = ifelse(etype %in% c("ejection","foul","timeout","turnover","violation"),"neg",
                   ifelse(etype %in% c("jump ball","sub"),"neu",'pos')),
         etype_3 = ifelse(etype == 'ejection',-3,
                   ifelse(etype %in% c('violation','foul'),-2,
                   ifelse(etype %in% c('turnover','timeout'),-1,
                   ifelse(etype %in% c('sub','jump ball'),0,
                   ifelse(etype == 'rebound',1,
                   ifelse(etype == 'shot',2,3)))))))



# 03.32  find some insights (avg_ability) ----
key_player_spec <- lakers %>% 
  filter(!player %>% is.na) %>% 
  group_by(team, date, player, etype) %>% 
  summarise(ability = sum(etype_3)) %>% 
  group_by(team, player, etype) %>% 
  summarise(avg_ability = mean(ability)) %>% 
  data.table() %>% 
  dcast(team + player ~ etype, value.var = "avg_ability",  sum) %>% 
  as_tibble() 

key_player_sum <- lakers %>% 
  filter(!player %>% is.na) %>% 
  group_by(team, date, player, etype) %>% 
  summarise(ability = sum(etype_3)) %>% 
  group_by(team, player, etype) %>% 
  summarise(avg_ability = mean(ability)) %>% 
  group_by(player) %>% 
  summarise(sum_ability = sum(avg_ability))

key_player <- left_join(key_player_spec,key_player_sum)

key_player %>% 
  filter(team == "LAL") %>% 
  arrange(sum_ability %>% desc) %>% 
  write_clip()


# 03.13 team scores & ability by each teams
team_score <- key_player %>% 
  group_by(team) %>% 
  summarise(avg_ability = mean(sum_ability),
            min_ability = min(sum_ability),
            max_ability = max(sum_ability),
            sd_ability = sd(sum_ability)) %>% 
  arrange(avg_ability %>% desc)


key_player %>% 
  arrange(team, sum_ability %>% desc) %>% 
  # filter(team %in% c('ATL', "LAL", 'BOS')) %>% 
  distinct(team,.keep_all = T) %>% 
  arrange(sum_ability %>% desc) %>% 
  distinct(player, .keep_all = T) %>% 
  print(n= 100)


# 03.20 How are the players different from usual when the game is poor?

# 03.21 What is the win or loss of a particular match?
lakers %>% 
  filter(date == '2008-10-28') %>% 
  group_by(team) %>% 
  summarise(points = sum(points)) %>% 
  filter(team != "OFF")

# 03.22 What is the win/loss ratio for each quarter in a particular game?
lakers %>% 
  filter(date == '2008-10-28') %>% 
  group_by(team, period) %>% 
  summarise(points = sum(points)) %>% 
  filter(team != "OFF") %>% 
  data.table() %>% 
  dcast.data.table(period ~ team, value.var = "points") %>% 
  as_tibble()

# 03.23 What are the division scores within each quarter in a particular game?
# > 분기별로 보기엔 너무 범위가 촘촘하다. 그러므로 쿼터별로 기준을 고려한다.
lakers %>% 
  mutate(min = time %>% str_sub(1,2)) %>% 
  filter(date == '2008-10-28') %>% 
  group_by(team, period, min) %>% 
  summarise(points = sum(points)) %>% 
  filter(team != "OFF") %>% 
  data.table() %>% 
  dcast.data.table(period + min ~ team, value.var = "points") %>% 
  as_tibble()

# 03.24 Let's see if which team is better each quarter in a particular game.
lakers %>%
  filter(date == '2008-10-28') %>% 
  group_by(date, team, period) %>% 
  summarise(points = sum(points)) %>% 
  filter(team != "OFF") %>% 
  mutate(tag_order = ifelse(team == "LAL",1,2)) %>% 
  arrange(date, period, tag_order) %>% 
  group_by(period) %>% 
  mutate(opp_points = lead(points)) %>% 
  filter(tag_order == 1) %>% 
  group_by(date) %>% 
  summarise(cum_points = cumsum(points),
            cum_opp_points = cumsum(opp_points)) %>% 
  mutate(points_gap = cum_points - cum_opp_points,
         points_yn = ifelse(points_gap > 0, '1',
                            ifelse(points_gap == 0,'0',
                                   ifelse(points_gap < 0, '-1','error'))),
         points_yn = points_yn %>% as.numeric,
         period = paste0("p_",row_number()))

# 03.25 Distribute it over the entire game.
play_result <- lakers %>% 
  group_by(date, team, period) %>% 
  summarise(points = sum(points)) %>% 
  filter(team != "OFF") %>% 
  mutate(tag_order = ifelse(team == "LAL",1,2)) %>% 
  arrange(date, period, tag_order) %>% 
  group_by(period) %>% 
  mutate(opp_points = lead(points)) %>% 
  filter(tag_order == 1) %>% 
  group_by(date) %>% 
  summarise(cum_points = cumsum(points),
            cum_opp_points = cumsum(opp_points)) %>% 
  mutate(points_gap = cum_points - cum_opp_points,
         points_yn = ifelse(points_gap > 0, '1',
                     ifelse(points_gap == 0,'0',
                     ifelse(points_gap < 0, '-1','error'))),
         points_yn = points_yn %>% as.numeric,
         period = paste0("p_",row_number()))%>% 
  data.table() %>% 
  dcast.data.table(date ~ period, value.var = "points_yn", fill = 0) %>% 
  as_tibble() %>% 
  mutate(play_result = p_1 + p_2 + p_3 + p_4 + p_5)




# 03.26 Under normal circumstances, what do Lakers players usually do? ----
play_status_type_2 <-  lakers %>% 
  filter(team == "LAL") %>% 
  group_by(date, etype_2) %>% 
  summarise(etype_3 = sum(etype_3)) %>% 
  data.table() %>% 
  dcast.data.table(date ~ etype_2, value.var = "etype_3", sum) %>% 
  as_tibble()


play_status_type_2_result <- left_join(play_status_type_2, play_result) %>% 
  mutate(result_grp = ifelse(play_result > 0, 'result_pos',
                      ifelse(play_result == 0,'result_neu',
                      ifelse(play_result < 0, 'result_neg','error')))) %>% 
  arrange(play_result) %>% 
  group_by(result_grp) %>% 
  summarise(avg_neg = mean(neg),
            avg_pos = mean(pos),
            avg_neu = mean(neu))




play_status_type_1 <-  lakers %>% 
  filter(team == "LAL") %>% 
  group_by(date, etype_2, etype) %>% 
  summarise(etype_3 = sum(etype_3))

play_status_type_1_result <- left_join(play_status_type_1, play_result) %>% 
  mutate(result_grp = ifelse(play_result > 0, 'result_pos',
                      ifelse(play_result == 0,'result_neu',
                      ifelse(play_result < 0, 'result_neg','error')))) %>% 
  arrange(play_result) %>% 
  group_by(result_grp, etype, etype_2) %>% 
  summarise(etype_3 = mean(etype_3)) %>% 
  data.table() %>% 
  dcast.data.table(etype_2 + etype ~ result_grp, value.var = "etype_3", sum) %>% 
  as_tibble()



play_status_overview_result <- left_join(play_status_type_1, play_result) %>% 
  mutate(result_grp = ifelse(play_result > 0, 'result_pos',
                      ifelse(play_result == 0,'result_neu',
                      ifelse(play_result < 0, 'result_neg','error')))) %>% 
  arrange(play_result) %>% 
  group_by(etype_2, etype) %>% 
  summarise(result_overview = mean(etype_3))


result_table <- left_join(play_status_type_1_result, play_status_overview_result)




# 03.30 What is the trend of behavior type by game?
left_join(play_status_type_2,play_result) %>% 
  mutate(result_yn = ifelse(play_result > 0, "Win",
                     ifelse(play_result == 0, "Draw",
                     ifelse(play_result < 0, "Lose",99)))) %>% 
  # filter(!result_yn == 'Draw') %>% 
  mutate(etype_result = neg + pos + neu) %>% 
  group_by(result_yn) %>% 
  e_chart(date) %>% 
  e_line(etype_result, smooth = T) %>% 
  e_color( color = c("skyblue","Red")) %>% 
  e_theme("dark-fresh-cut") %>% 
  e_tooltip(trigger = "axis")



# 03.31 What is the monthly performance of each top key playerS?
lakers %>% 
  filter(team == "LAL") %>% 
  mutate(month = date %>% substr(1,7)) %>% 
  group_by(month, player) %>% 
  summarise(etype_3 = sum(etype_3)) %>% 
  inner_join(key_player %>% 
               filter(team == "LAL") %>% 
               arrange(sum_ability %>% desc) %>% 
               head(5) %>% 
               select(player)) %>% 
  group_by(player) %>% 
  e_chart(month) %>% 
  e_line(etype_3, smooth = T) %>% 
  e_tooltip(trigger = "axis")

