#statcastdataの取得
library(baseballr)
library(tidyverse)

#20250601~7日の全然打席のデータ
#取りたい日付を並べる
dates <-　seq(as.Date("2025-06-01"),as.Date("2025-06-30"), by = "day")


#1日分をとる関数を自分で作る
get_one_day <- function(d){
  Sys.sleep(1)
  message("取得中:", d)
  statcast_search(
    start_date = as.character(d),
    end_date = as.character(d),
    player_type = "batter"
  )

}
#失敗してもエラーで止まらない版にする
safe_get <- possibly(get_one_day, otherwise = NULL)

#30日分をとる
raw_list <- map(dates, safe_get)

#失敗した日があるかどうか確認
failed <- dates[map_lgl(raw_list, is.null)]
failed

#成功した日の行数
raw_list |>compact() |> map_int(nrow)

raw <- bind_rows(raw_list)
dim(raw)

dir.create("data", showWarnings = FALSE)
saveRDS(raw, "data/statcast_2025_06.rds")
