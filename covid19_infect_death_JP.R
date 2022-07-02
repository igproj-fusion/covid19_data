
library(ggplot2)
library(ggthemes)
library(scales)
library(dplyr)
library(ggrepel)
library(ggpmisc)


URL <- "https://www3.nhk.or.jp/n-data/opendata/coronavirus/nhk_news_covid19_prefectures_daily_data.csv"

pop47 <- read.csv("https://raw.githubusercontent.com/igproj-fusion/covid19_data/main/pop47.csv", header = T)

St <- as.Date("2021-12-15")
read.csv(URL) %>%
  select(date = "日付", pref = "都道府県名", cump = "各地の感染者数_累計", cumd = "各地の死者数_累計") %>%
  mutate(date = as.Date(date)) %>%
  filter(date >= St - 1) %>%
  group_by(pref) %>%
  mutate(cump = cump - min(cump)) %>%
  mutate(cumd = cumd - min(cumd)) %>%
  filter(date == max(date)) %>%
  left_join(pop47, by=c("pref" = "PREF")) %>%
  mutate(perP = cump / POP *100000) %>%
  mutate(perD = cumd / POP *100000) -> df

g <- ggplot(df, aes(x = perP, y = perD))
g <- g + geom_smooth(method = "lm", color = "lightblue", alpha = 0.3)
g <- g + geom_point(color="blue")
g <- g + geom_text_repel(aes(x = perP, y = perD, label = pref),
                         max.overlaps=20, segment.size = 0.2)
g <- g + scale_y_continuous(breaks = c(0,  5.0, 10, 15.0, 20.0, 25.0),
                            limits = c(min = 0.0, max = 25.0),
                            labels = scales::comma)
g <- g + stat_poly_eq(formula = y ~ x,
                      aes(label = paste(stat(eq.label),
                                        stat(rr.label),
                                        stat(adj.rr.label),
                                        sep = "*\", \"*")),
                      eq.with.lhs = "italic(hat(y))~`=`~",
                      eq.x.rhs = "~italic(z)",
                      parse = TRUE, size = 5)
g <- g + theme_light()
g <- g + theme(plot.margin= unit(c(2, 2, 2, 2), "lines"))
g <- g + labs(title = "第6波以降の累積感染者数と累積死亡者数（人口10万人あたり）",
              subtitle = paste0("2021-12-15〜", df$date[nrow(df)]),
              x = "Positives per 100,000 people", 
              y = "Deaths per 100,000 people",
              caption = paste("Source: ", URL))
g <- g + theme(plot.title = element_text(size = rel(1.6)),
               plot.subtitle = element_text(size = rel(1.4)),
               plot.caption = element_text(size = rel(1.0)),
               axis.title = element_text(size = rel(1.4)),
               axis.text = element_text(size = rel(1.0)))
print(g)
