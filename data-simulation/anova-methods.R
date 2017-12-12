library(tidyverse)
library(ggplot2)

# y_{ijk} = \tau{i} + m_{j} + (\tau m)_{ij} + \epsilon_{ijk}
# i = small,large
# j = month
# k = replicants

data <- read.table("data-simulation/lizards.txt",header=TRUE)
x <- data$X
month <- factor(data$MONTH)
size <- factor(data$SIZE)
fit <- lm(x ~ size + month + size:month)
anova(fit)


ggplot(data) +
	geom_line(data = data %>% filter(size == "small"), aes(x = month, y = x))

ggplot(data) +
	geom_point(aes(x = month, y = x)) +
	facet_wrap(~ size)

# The p-values are calculated assuming normality

# Permutation test are distribution-free. So we can try using them.
