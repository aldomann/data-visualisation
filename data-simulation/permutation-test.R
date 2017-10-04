library(ggplot2)

# Permutation test
nr <- 10000 #number of rearrangements to be examined
st <- numeric(nr)
conv <- c(65, 79, 90, 75, 61, 85, 98, 80, 97, 75)
new <- c (90, 98, 73, 79, 84, 81, 98, 90, 83, 88, 99, 99, 99)
n1 <- length(new)
n2 <- length(conv)
total <- n1+n2

sttrue= mean(new)-mean(conv)
cnt= 0 #zero the counter
#Put both sets of observations in a single vector
vect = c(new, conv)
for (i in 1:nr){
	d= sample (vect,n1+n2)
	ne <- d[1:n1]
	a <- n1+1
	co <- d[a:total]
	st[i] <- mean(ne) - mean(co)
	if (st[i] > sttrue){
		cnt <- cnt+1
	}
}

# p-value
p.val <- cnt/nr

# Visualisation of the p-value in a histogram
ggplot() +
	geom_histogram(aes(st), binwidth = 1) +
	geom_vline(aes(xintercept = sttrue), colour = "blue") +
	# geom_vline(aes(xintercept = quantile(st, prob = 1-p.val)), colour = "green") +
	geom_vline(aes(xintercept = quantile(st, prob = 0.95)), colour = "red") +
	# annotate("text", x = -12, y = 700, label = paste("p-value = ", p.val)) +
	labs(title = paste("Permutation test;", "p-value =", p.val, "    HYPOTHESIS:", ifelse(p.val < 1-0.95 ,"YEA BOI!", "FAIL!")))
