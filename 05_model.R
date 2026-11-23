#モデル：打球速度、角度からヒット確率を予測する
library(tidyverse)

bip <- readRDS("data/bip.rds") |>
                 filter(!is.na(launch_speed), !is.na(launch_angle))

lm_model <- lm(is_hit ~ launch_speed, data = bip)
coef(lm_model)

new_speed <- tibble(launch_speed = c(30,60,80,100,120))
predict(lm_model,newdata = new_speed)

logit1 <- glm(is_hit ~ launch_speed, data = bip, family = binomial)
summary(logit1)

predict(logit1, newdata = new_speed, type = "response")

#角度をそのまま入れる
logit2 <- glm(is_hit ~ launch_speed+ launch_angle,
              data = bip, family = binomial)

#角度の２乗も入れる
logit3 <- glm(is_hit ~ launch_speed +launch_angle + I(launch_angle^2),
              data = bip, family = binomial)

summary(logit3)

AIC(logit1,logit2,logit3)

theme_set(theme_gray(base_family = "HiraKakuProN-W3"))

grid <- expand_grid(
  launch_speed = c(70,90,110),
  launch_angle = seq(-40,70,by=1)
)

grid
nrow(grid)

#それぞれの組み合わせでヒット確率を予測
grid <- grid |>
  mutate(prob = predict(logit3,newdata = grid, type = "response"))

ggplot(grid, aes(x = launch_angle, y = prob, color = factor(launch_speed))) +
  geom_line(linewidth =1) +
  labs(
    title = "モデルが予測するヒット確率",
    x     ="打球角度（度）",
    y     ="ヒット確率",
    color ="打球速度（mph）"
  )

actual70 <- bip|>
  filter(launch_speed >= 70, launch_speed < 75) |>
  mutate(angle_bin = floor(launch_angle / 5) * 5) |>
  group_by(angle_bin) |>
  summarise( n= n(), 実際= mean(is_hit), .groups = "drop") |>
  filter( n >= 10)

pred70 <- tibble(
  launch_speed = 72.5,
  launch_angle = actual70$angle_bin + 2.5
)

actual70 |>
  mutate(
    モデル3  = predict(logit3, newdata = pred70, type = "response"),
    モデル4  = predict(logit4, newdata = pred70, type = "response")
  ) |>
  print(n = Inf)

#交互作用あり　速度と角度の掛け算を入れる
logit4 <-  glm(
  is_hit ~ launch_speed*(launch_angle + I(launch_angle^2)),
  data = bip,
  family = binomial
)

summary(logit4)

#AICを調べる
AIC(logit3,logit4)

grid4 <- expand_grid(
  launch_speed = c(70, 90, 110),
  launch_angle = seq(-40, 70, by = 1)
)


grid4 <-  grid4 |>
  mutate(prob = predict(logit4, newdata = grid4, type = "response"))

ggplot(grid4, aes( x = launch_angle, y = prob, color = factor(launch_speed))) +
  geom_line(linewidth =1) +
  labs(
    title ="交互作用ありのモデルのヒット確率",
    x     ="打球角度",
    y     ="ヒット確率",
    color ="打球速度（mph）"
  )

actual70 |>
  mutate(
    モデル3 = predict(logit3, newdata = pred70, type = "response"),
    モデル4 = predict(logit4, newdata = pred70, type = "response")
  ) |>
  print( n = Inf)


install.packages("mgcv") #初回だけ。終わったらこの行は消してOK
library(mgcv)

gam1 <- gam(
  is_hit ~ s(launch_speed, launch_angle, k = 100),
  data = bip,
  family = binomial,
  method = "REML"
)

summary(gam1)

#AICで調べる
AIC(logit4, gam1)

#70~75mph ゾーンで比べる
actual70 |>
  mutate(
    モデル４ = predict(logit4, newdata = pred70, type = "response"),
    GAM      = predict(gam1,   newdata = pred70, type = "response")
  ) |>
  print( n = Inf)

#実際のヒット率vs GAMの予測をヒートマップで比較
#実際のヒット率
hit_zone <- bip |>
  mutate(
    speed_bin = floor(launch_speed / 5)*5,
    angle_bin = floor(launch_angle / 5)*5
  )|>
group_by(speed_bin, angle_bin)|>
  summarise( n=n(), hit_rate = mean(is_hit), .groups = "drop")|>
  filter( n >= 10 )


gam_zone <- hit_zone |>
  mutate(
    hit_rate = as.numeric(predict(
      gam1,
      newdata = tibble(
        launch_speed = speed_bin + 2.5,
        launch_angle = angle_bin + 2.5
      ),
      type = "response"
    ))
  )


compare <-  bind_rows(実際 = hit_zone, GAM = gam_zone, .id = "種類")

ggplot(compare, aes(x = speed_bin, y = angle_bin, fill = hit_rate)) +
  geom_tile()+
  scale_fill_gradient(low = "white", high = "red",limits = c(0,  1)) +
  facet_wrap(~種類)　+
  labs(
    title = "ヒット率:実績vs GAMの予測",
    x     = "打球速度（mph）",
    y     = "打球角度（度）",
    fill  ="ヒット率"
  )

ggsave("output/actual_vs_gam.png", width = 10, height = 5, dpi = 300)
