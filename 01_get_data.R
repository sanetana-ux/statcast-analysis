#statcastdataの取得
library(baseballr)
library(tidyverse)

#20250601~7日の全然打席のデータ
#取りたい日付を並べる
dates <- seq(as.Date("2025-06-01"),as.Date("2025-06-07"),by ="day")
dates

#一日ずつデータをとる（７回￥ダウンロードするので時間かかる）
raw_list <- map(dates, function(d){
  statcast_search(
    start_date = as.character(d),
    end_date   = as.character(d),
    player_type = "batter"
  )
})

#各日の行数を確認
map_int(raw_list, nrow)

#七日分を縦にくっ付ける
raw <- bind_rows(raw_list)
dim(raw)
