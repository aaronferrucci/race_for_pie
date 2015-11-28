library(ggplot2)
library(gridExtra)
source("clean.R")

data10k <- clean("run_for_pie_2015_10k_raw.txt")
# Drop some outliers
data10k <- subset(data10k, elapsed <= 6e06)
friends10k <- data10k[grep("Ferrucci", data10k$name),]
elapsed_ticks10k <- seq(0, max(data10k$elapsed), 900000)

p10k <- ggplot(data10k, aes(x=elapsed, y=age, group=sex, color=sex))
p10k <- p10k + ggtitle("10k Results")
p10k <- p10k + geom_point(position=position_jitter(width=0, height=0.2), size=3)
p10k <- p10k  + geom_point(
  data=friends10k,
  aes(x=elapsed, y=age, shape=name),
  size=3,
  color="black"
)
p10k <- p10k + scale_x_continuous(
  breaks = elapsed_ticks10k,
  labels = timestr(elapsed_ticks10k),
  name = "elapsed time (hh:mm:ss)"
)
p10k <- p10k + expand_limits(x = .5 * 3600 * 1000)
# p10k <- p10k + stat_smooth(method = "gam", formula = y ~ s(x, bs="cs"))
p10k <- p10k + stat_smooth(method = "lm")

data5k <- clean("run_for_pie_2015_5k_raw.txt")
friends5k <- data5k[grep("Matsumoto", data5k$name),]
elapsed_ticks5k <- seq(0, max(data5k$elapsed), 900000)
p5k <- ggplot(data5k, aes(x=elapsed, y=age, group=sex, color=sex))
p5k <- p5k + ggtitle("5k Results")
p5k <- p5k + geom_point(position=position_jitter(width=0, height=0.2), size=3)
p5k <- p5k  + geom_point(
  data=friends5k,
  aes(x=elapsed, y=age, shape=name),
  size=3,
  color="black"
)
p5k <- p5k + scale_x_continuous(
  breaks = elapsed_ticks5k,
  labels = timestr(elapsed_ticks5k),
  name = "elapsed time (hh:mm:ss)"
)
p5k <- p5k + expand_limits(x = .5 * 3600 * 1000)
# p5k <- p5k + stat_smooth(method = "gam", formula = y ~ s(x, bs="cs"))
p5k <- p5k + stat_smooth(method = "lm")

grid.arrange(p5k, p10k, ncol=2, main="2015 Race For Pie")
