library(tidyverse)

before<- c(120,124,130,118,140,128,140,135,126,130,126,127)
after <- c(128,131,131,127,132,125,141,137,118,132,129,135)
diff <- before-after

nr <- 500 #number of rearrangements to be examined
st <- numeric(nr)
sttrue <- mean(diff)
n <- length(diff)
stat <- numeric(n)
cnt <- 0 #zero the counter
for (i in 1:nr) {
	for (j in 1:n) {
		stat[j] <- ifelse(runif(1) < 0.5, diff[j], -diff[j])
	}
	st[i] <- mean(stat)
	if (st[i] < sttrue){
		cnt <- cnt + 1
	}
}
p.val <- cnt/nr # p-value

p.val

# Visualisation of the p-value in a histogram
ggplot() +
	geom_histogram(aes(st), binwidth = 1) +
	# geom_vline(aes(xintercept = sttrue), colour = "blue") +
	geom_vline(aes(xintercept = quantile(st, prob = 1-p.val)), colour = "blue") +
	geom_vline(aes(xintercept = quantile(st, prob = 0.95)), colour = "red") +
	# annotate("text", x = -12, y = 700, label = paste("p-value = ", p.val)) +
	labs(title = paste("Permutation test;", "p-value =", p.val, "    HYPOTHESIS:", ifelse(p.val < 1-0.95 ,"YEA BOI!", "FAIL!")))
