library(ggplot2)
library(ggthemes)
library(plyr)
library(reshape2)
library(tikzDevice)

## http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

df <- read.csv2("data.csv")
str(df)


tikz("~/Documents/language-integrated-provenance/ppdp2016/graph.tex", width=3.3, height=3)
ggplot(df, aes(factor(N), medianms, group=prov, color=prov)) +
    geom_line() +
    geom_point(size = 1, aes(shape=prov)) +
    scale_y_log10("Time (ms), median over 5 runs.") +
    scale_x_discrete("Number of departments.") +
    facet_wrap(~query, scales="free") +
    theme_tufte() +
    theme(plot.margin=margin(t=-5, r=0, b=5, l=0), panel.margin=margin(1)) +
    ## theme(plot.background= element_rect()) +
    theme(legend.position="bottom") +
    theme(legend.margin=margin(t=-8, r=0, b=0, l=0)) +
    ## theme(legend.background = element_rect()) +
    theme(axis.title = element_text(size=7)) +
    theme(axis.text.x = element_text(angle=90, vjust=0.5, size=5)) +
    theme(axis.text.y = element_text(size=5)) +
    scale_color_manual(values=cbbPalette)
dev.off()

