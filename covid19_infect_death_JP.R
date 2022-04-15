library(ggplot2)
library(ggthemes)
library(scales)
library(dplyr)
library(ggrepel)
library(ggpmisc)


URL <- "https://www3.nhk.or.jp/n-data/opendata/coronavirus/nhk_news_covid19_prefectures_daily_data.csv"
St <- as.Date("2021-12-15")

pop47 <- read.csv("https://raw.githubusercontent.com/igproj-fusion/covid19_data/main/pop47.csv")

read.csv(URL) %>%
  select(date = "日付", 
         pref = "都道府県名", 
         cum_c = "各地の感染者数_累計", 
         cum_d = "各地の死者数_累計") %>%
  mutate(date = as.Date(date)) %>%
  filter(date >= St - 1) %>%
  group_by(pref) %>%
  mutate(cum_c = cum_c - min(cum_c)) %>%
  mutate(cum_d = cum_d - min(cum_d)) %>%
  filter(date == max(date)) %>%
  left_join(pop47, by = c("pref" = "PREF")) %>%
  mutate(C_perP = cum_c / POP * 100000) %>%
  mutate(D_perP = cum_d / POP * 100000) %>%
  select(date, pref, C_perP, D_perP) -> df

g <- ggplot(df, aes(x = C_perP, y = D_perP))
g <- g + geom_smooth(method = "lm", color = "lightblue", alpha = 0.3)
g <- g + geom_point(color = "blue")
g <- g + geom_text_repel(aes(x = C_perP, y = D_perP, label = pref),
                         max.overlaps = 20, segment.size = 0.2)
g <- g + scale_y_continuous(limits = c(min = 0.0, max = max(df$D_perP)),
                            labels = scales::comma)
g <- g + stat_poly_eq(formula = y ~ x,
                      aes(label = paste(stat(eq.label),
                                        stat(rr.label),
                                        stat(adj.rr.label),
                                        sep = "*\", \"*")),
                      eq.with.lhs = "italic(hat(y))~` = `~",
                      eq.x.rhs = "~italic(z)",
                      parse = TRUE, size = 5)
g <- g + theme_light()
g <- g + theme(plot.margin= unit(c(2, 2, 2, 2), "lines"))
g <- g + labs(title = "第6波の累積感染者数と累積死亡者数（人口10万人あたり）",
              subtitle = paste0(St, "〜", df$date[1]),
              x = "Positives per 100,000 people", 
              y = "Deaths per 100,000 people",
              caption = paste("Source: ", URL))
g <- g + theme(plot.title = element_text(size = rel(1.6)),
               plot.subtitle = element_text(size = rel(1.4)),
               plot.caption = element_text(size = rel(1.0)),
               axis.title = element_text(size = rel(1.4)),
               axis.text = element_text(size = rel(1.0)))
print(g)
