library(tidyverse)

#保存したデータを読み込む
raw <- readRDS("data/statcast_2025_06_w1.rds")

#列名と型をざっと見る
glimpse(raw)

#type 列の中身を数える
raw |> count(type)

#events列の中身を多い順に数える
raw |> count(events, sort = TRUE)

#気になる列だけを取り出して最初の１０行を見る
raw |>
  select(game_date, player_name, type,events,launch_speed,launch_angle)|>
  head(10)

library(tidyverse)

raw <- readRDS("data/statcast_2025_06_w1.rds")

bip <- raw|>
  filter(type== "X") |>
  select(game_date, player_name, events,bb_type,
         launch_speed,launch_angle) |>
  mutate(
    is_hit = if_else(
      events %in% c("single","double","triple","home_run"),
      1,0

    )
  )

nrow(bip)

bip |> count(is_hit)

summary(bip$launch_speed)
summary(bip$launch_angle)

#仮説チェック　ヒットとアウトで打球速度の平均は違うのか
bip |>
  group_by(is_hit) |>
  summarise(
  n         = n(),
  平均速度 = mean(launch_speed, na.rm = TRUE),
  平均角度 = mean(launch_angle, na.rm =TRUE)
  )

saveRDS(bip, "data/bip.rds")

library(tidyverse)

bip <- readRDS("data/bip.rds")

bip <- bip|>
  filter(!is.na(launch_speed),!is.na(launch_angle)) |>
  mutate(result = if_else(is_hit==1, "ヒット","アウト"))

theme_set(theme_gray(base_family = "HiraKakuProN-W3"))

ggplot(bip,aes(x = launch_angle, fill = result)) +
  geom_histogram(binwidth = 5, alpha = 0.6, position = "identity") +
  labs(
    title = "打球角度の分布",
    x      = "打球角度（度）",
    y      ="打球数",
    fill   ="結果"
  )

#角度と打球速度の散布図
ggplot(bip,      , y= launch_angle, color = result)) +
  geom_point(alpha = 0.4, size =1) +
  labs(
    title ="打球速度と打球角度",
    x     ="打球速度(mph)",
    y     ="打球角度（度）",
    color ="結果"

  )

