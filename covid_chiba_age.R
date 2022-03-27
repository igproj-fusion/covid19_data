library(ggplot2)
library(ggthemes)
library(scales)
library(openxlsx)
library(tidyverse)
library(rvest)
library(RColorBrewer)


html <- read_html("https://www.pref.chiba.lg.jp/shippei/press/2019/ncov-index.html")
URL <- html %>% 
  html_nodes("a") %>% 
  html_attr("href") %>%
  str_subset("kansensya2.xlsx") %>%
  paste0("https://www.pref.chiba.lg.jp/", .)

read.xlsx(URL, sheet = 1) %>%
  select(age = X3, date = X8) %>%
  slice(-1:-5) %>%
  mutate(attr = 1) -> ss
read.xlsx(URL, sheet = 2) %>%
  select(age = X3, date = X7) %>%
  slice(-1) %>%
  mutate(attr = 0) -> sa

cat <- c("10歳未満", "10代", "20代", "30代", "40代",    
         "50代", "60代", "70代", "80代", "90代以上")

rbind(ss, sa) %>%
  filter(age %in% cat) %>%
  mutate(age = gsub(age, pattern = "10歳未満",
                    replacement = "0代", ignore.case = TRUE)) %>%
  mutate(date = as.Date(as.numeric(date), origin = "1899-12-30")) %>%
  filter(!is.na(date)) %>%
  count(age, date) %>%
  group_by(age) %>%
  mutate(cum = cumsum(n)) -> dft

FirstD <- min(dft$date)
LastD <- max(dft$date)

dft0 <- dft[dft$age == "0代", ]

datebreaks <- c(seq(as.Date("2022-01-01"), by = "week", length.out = 26)) 
mycolors = c(brewer.pal(name = "RdYlBu", n = 10))
color <- "grey70"

g <- ggplot(dft)
g <- g + scale_color_manual(values = mycolors, labels = cat)
g <- g + geom_line(aes(x = date, y = cum, group = age, color = age), size = 1)
g <- g + geom_line(data = dft0, aes(x = date, y = cum), 
                   color = mycolors[1], size = 1.2)
g <- g + theme(plot.margin = unit(c(2, 2, 2, 2), "lines"))
g <- g + scale_x_date(breaks = datebreaks, labels = date_format("%m/%d"),
                      limits = c(min=FirstD, max = LastD) )
g <- g + scale_y_continuous(limits = c(0, max(dft$cum)), 
                            breaks = seq(0, 40000, by = 10000))
g <- g + labs(title = "千葉県：各年代の検査確定日別新規陽性者数（累積）",
              subtitle = paste0(FirstD, "〜",　LastD),
              x = "", 
              y = "Positives",
              caption = paste("Source: ", URL) )
g <- g + theme(panel.grid.minor = element_blank(),
               plot.title = element_text(size = rel(1.6)),
               plot.subtitle = element_text(size = rel(1.3)),
               legend.title = element_text(size = rel(1.2)),
               legend.text = element_text(size = rel(1.0)),
               plot.caption = element_text(size = rel(1.0)),
               axis.title = element_text(size = rel(1.3)),
               axis.text = element_text(size = rel(1.0)),
               axis.ticks = element_blank())
g <- g + guides(color = guide_legend(title = ""))  
g <- g + theme(rect = element_rect(colour = color, fill = color),
               text = element_text(color = "gray25"),
               plot.background = element_rect(colour = color, fill = color),
               panel.background = element_rect(colour = color, fill = color),
               panel.border = element_rect(fill = NA, colour = "grey85", size = 1),
               panel.grid.major = element_line(colour = "grey80"),
               legend.key = element_rect(colour = color, fill = color),
               axis.text = element_text(colour = "gray25"))
print(g)
