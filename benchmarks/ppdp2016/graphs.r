library(ggplot2)
library(plyr)
library(reshape2)

df <- read.csv2("data.csv")
str(df)

ggplot(df) +
geom_line(aes(factor(N), meanms, group=prov, color=prov)) +
facet_wrap(~query, scales="free")
