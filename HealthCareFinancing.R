## Code book at
## http://ec.europa.eu/eurostat/cache/metadata/en/hlth_sha_esms.htm
## EU-27 (excluding Greece, Ireland, Italy, Malta and the United Kingdom),
## Norway, Iceland, Switzerland, Japan, USA, Australia and Korea.

library(SmarterPoland)
library(dplyr)
library(ggplot2)

dataOrig <- getEurostatRCV(kod="hlth_sha_hf")
sha <- read.delim("SystemOfHealthAccountsCodes.txt", header = F,
                  col.names = c("Code", "Description"))
CountryCodes <- read.delim("CountryCodes.txt")
data <- filter(dataOrig, unit == "PC_GDP" | unit == "PC_CHE",
               icha_hf == "HF1" | icha_hf == "HF2" | icha_hf == "HF1-3")
data <- merge(CountryCodes, data, by.x = "code", by.y = "geo")
data <- merge(data, sha, by.x = "icha_hf", by.y = "Code")
data <- droplevels(data)
data$time <- as.numeric(as.character(data$time))

CHE <- filter(data, unit == "PC_CHE", icha_hf != "HF1-3")
GDP <- filter(data, unit == "PC_GDP", icha_hf == "HF1-3")

mytheme <- theme_bw() +
        theme(strip.background = element_rect(fill = "azure2", size = NA),
              legend.position = "top",
              legend.background = element_blank(),
              legend.key = element_blank(),
              panel.grid.major = element_line(colour = "gray84"),
              panel.grid.minor = element_line(colour = "gray84"),
              panel.border = element_rect(size = NA),
              plot.title = element_text(face = "bold", size = 16),
              axis.title.y = element_text(face = "bold", vjust = 1.8)
        )

ggplot(CHE, aes(x = time, y = value, colour = Description, group = Description)) +
        geom_point(size = 2.5) +
        geom_line(size = 1) + xlab("") +
        ylab("Percentual share of total current health expenditure") +
        ggtitle("Health care expenditure by financing agent") +
        facet_wrap(~ country, ncol = 5, scales = "free_x") +
        scale_color_manual(values = c("steelblue4", "darkorange3"),
                           guide = guide_legend(title = NULL, keywidth = 2)) +
        scale_x_continuous(breaks = c(seq(2004, 2012, 2))) +
        scale_y_continuous(breaks = c(seq(0, 100, 20))) +
        mytheme +
        theme(axis.text = element_text(size = rel(0.75)))

ggplot(GDP, aes(x = time, y = value)) +
        geom_point(size = 2.5, colour = "royalblue3") +
        geom_line(size = 1, colour = "royalblue3") + xlab("") +
        ylab("Percentage of GDP") +
        ggtitle("Health care expenditure") +
        facet_wrap(~ country, ncol = 5, scales = "free_x") +
        scale_x_continuous(breaks = c(seq(2004, 2012, 2))) +
        scale_y_continuous(breaks = c(seq(0, 20, 5)), limits = c(2, 20)) +        
        mytheme +
        theme(axis.text = element_text(size = rel(0.75)),
              plot.title = element_text(face = "bold", vjust = 2.5)
        )

GDP <- arrange(GDP, desc(time))
year <- numeric()
country <- character()
value <- numeric()

for (i in 1:nrow(GDP)) {
        if (!is.na(GDP[i, 6]) & (GDP[i,3] %in% country) == F) {
                country <- c(country, as.character(GDP[i, 3]))
                year <- c(year, GDP[i, 5])
                value <- c(value, GDP[i, 6])
        }
}
GDP2 <- data.frame(country = country, year = year, value = value)

ggplot(GDP2, aes(x = reorder(country, value), y = value)) +
        geom_bar(stat = "identity", width = 0.8, fill = "mediumseagreen") +
        geom_text( y = value-1, label = paste(value, "%"), colour = "white", size = rel(4)) +
        xlab("") + ylab("") + guides(fill = F) +
        ggtitle("Health expenditure\n Sum of all financing agents\n Percentage of GDP") +
        coord_flip() +
        theme_bw() +
        theme(panel.grid.major = element_line(colour = NA),
              panel.border = element_rect(size = NA),
              axis.text.y = element_text(face = "bold"),
              plot.title = element_text(face = "bold", vjust = 2.5)                     
        )
