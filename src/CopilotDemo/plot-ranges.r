library(ggplot2)
rs <- read.csv("ranges.csv")
xs <- seq(0, nrow(rs) - 1)
lo <- loess(unlist(rs['ranges']) ~ xs)
m <- ggplot(rs, aes(y=ranges, x=xs)) + xlab("time") + ylab("range") +
     stat_smooth(method='loess', span=0.1, size=3, color='dodgerblue4') +
     geom_point(size=2, color='sky blue')
xl <- seq(min(xs), max(xs), (max(xs) - min(xs))/1000)
print(m)
