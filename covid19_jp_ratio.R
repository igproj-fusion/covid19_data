library(ggplot2)
library(ggthemes)
library(dplyr)
library(slider)
library(sf)

URL <- "https://www3.nhk.or.jp/n-data/opendata/coronavirus/nhk_news_covid19_prefectures_daily_data.csv"

read.csv(URL, header = TRUE) %>% 
  select(date = "日付", pref = "都道府県名",
         case = "各地の感染者数_1日ごとの発表数") %>% 
  mutate(date = as.Date(date)) %>%
  filter(date > max(date) - 14) %>%
  group_by(pref) %>%
  mutate(ms7 = slide_vec(case, .f = sum, .before = 6)) %>%
  filter(date %in% c(max(date), max(date) - 7)) %>%
  mutate(ratio = ms7[2] / ms7[1]) %>%
  filter(date == max(date)) -> df

jp <- read_sf("https://github.com/igproj-fusion/R-gis/raw/main/japan.geojson")
jp$ratio <- df$ratio

breaks <- c(min(jp$ratio), 1.0, max(jp$ratio))
border <- data.frame(xx <- c(128, 131.5, 137.5, 137.5),
                     yy <- c( 41, 41, 45, 46))
ggplot() + 
  geom_sf(data = jp, aes(fill = ratio), size = .1) +
  geom_path(data = border, aes(xx, yy), size = .1) +
  scale_fill_gradient2(low = "darkgreen", mid = "white", high = "red", 
                       midpoint = 1, breaks = breaks, 
                       labels = format(breaks, digits = 3)) +
  ylim(30, 46) +
  theme_map() +
  labs(title = paste0("新規感染者数 前週比（", max(df$date),"現在）"),
       caption = paste0("Source: ", URL)) +
  theme(plot.margin　= unit(c(2, 2, 2, 2), "lines"),
        plot.title = element_text(size = rel(2)),
        plot.caption = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1.0)),
        legend.position = c(0.85, 0.05),
        legend.title = element_blank())

