# Convert parameters of the beta distribution from (t, s) to (t_n, s_n), (alpha, beta), (alpha_n, beta_n)

# Values of the (t, s) to convert
tt <- 0.3
ss <- 4

nn <- 10  # number of trials in the binomial distribution
yy <- 6   # number of successes in the n trials

# Transformation between (tt, ss) to (tt_n, ss_n)
tt_n <- (ss * tt + yy)/(ss + nn)
ss_n <- ss + nn

# Converting the values (s, t) to (alpha, beta):
alpha <- ss * tt
beta  <- ss * (1 - tt)

# Converting (alpha, beta) to (alpha_n, beta_n)
alpha_n <- alpha + yy
beta_n  <- beta + nn - yy

# Print on the screen a simple summary of the results
cat(paste("# tt = ", tt, ", ss = ", ss,
	", nn = ", nn, ", yy = ", yy,
	", tt_n = ", round(tt_n, 4), ", ss_n = ", round(ss_n, 4),
	", alpha = ", round(alpha, 4), ", beta = ", beta,
	", alpha_n = ", round(alpha_n, 4), ", beta_n = ", round(beta_n, 4),
	sep=""))
