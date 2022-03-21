
library(ggplot2)
library(ggthemes)
library(scales)
library(openxlsx)
library(tidyverse)
library(rvest)

html <- read_html("https://www.pref.chiba.lg.jp/shippei/press/2019/ncov-index.html")
URL <- html %>% 
  html_nodes("a") %>% 
  html_attr("href") %>%
  str_subset("kansensya2.xlsx") %>%
  paste0("https://www.pref.chiba.lg.jp/", .)

s1 <- read.xlsx(URL, sheet = 1)
s2 <- s1[-c(1:5), -c(1,2,5,6,7)] 
s2$X3[s2$X3 == "10歳未満"] <- "0代"
s3 <- s2[s2$X3 != "-", ]


a1 <- read.xlsx(URL, sheet = 2)
a2 <- a1[-1, -c(1,2,5,6)] 
a2$X3[a2$X3 == "10歳未満"] <- "0代"
a3 <- a2[a2$X3 != "-", ]

colnames(s3) <- c("AGE", "SEX", "DATE", "DATE.R")
colnames(a3) <- c("AGE", "SEX", "DATE", "DATE.R")

s3$ATT <- 1
a3$ATT <- 0

df <- rbind(s3, a3)

df$DATE <- as.Date(as.numeric(df$DATE), origin="1899-12-30")
df$DATE.R <- as.Date(as.numeric(df$DATE.R), origin="1899-12-30")

dft <- table(df$DATE, df$AGE)
colnames(dft) <- c("A00", "A10", "A20", "A30", "A40", "A50", "A60", "A70", "A80", "A90")

dftdf <- as.data.frame(dft)

dftdf %>%
  rename(date = Var1, age = Var2) %>%
  group_by(age) %>%
  mutate(cumFreq = cumsum(Freq)) -> dftdfs

dftdfs$date <- as.Date(dftdfs$date)
dftdfs$age <- as.vector(as.character(dftdfs$age))

datebreaks <- c(seq(as.Date("2022-01-01"), by = "week", length.out = 26)) 

library(RColorBrewer)
mycolors = c(brewer.pal(name="RdYlBu", n = 10), brewer.pal(name="Set1", n = 6))
color <- "grey60"

FirstD <- min(dftdfs$date)
LastD <- max(dftdfs$date)

g <- ggplot(dftdfs)
#g <- g + geom_hline(yintercept= 1.0, col= "gray20")
g <- g + scale_color_manual(values = mycolors, 
                            labels=c("10歳未満","10代","20代","30代","40代",
                                     "50代","60代","70代","80代","90代以上"))
g <- g + geom_line(aes(x = date, y = cumFreq, group=rev(age), color = age), 
                   size = 1)
g <- g + theme(plot.margin= unit(c(2, 2, 2, 2), "lines"))
g <- g + scale_x_date(breaks = datebreaks, labels = date_format("%m/%d"),
                      limits = c(min=FirstD, max =LastD) )
g <- g + scale_y_continuous(
  limits = c(0, 40000), breaks = seq(0, 40000, by = 10000))
g <- g + labs(title = "千葉県：各年代の検査確定日別新規陽性者数（累積）",
              subtitle = paste0(FirstD, "〜",　LastD),
              x = "", 
              y = "Positives",
              caption = paste("Data Source: ", URL) )
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
               text = element_text(color = "white"),
               plot.background = element_rect(colour = color, fill = color),
               panel.background = element_rect(colour = color, fill = color),
               panel.border = element_rect(fill = NA, colour = "grey85", size = 1),
               panel.grid.major = element_line(colour = "grey80"),
               #panel.grid.minor = element_line(colour = ""),
               legend.key = element_rect(colour = color, fill = color),
               axis.text = element_text(colour = "white"),
)

print(g)
