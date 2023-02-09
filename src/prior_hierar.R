# PDF AND CDF PLOTS OF PRIOR DISTRIBUTION IN THE HIERARCHICAL MODEL

# PDF of hierarchical prior: s^(-5/2) * Beta(t,s)

# ########################################################60

# 		Plot PDF of hierarchical prior: s^(-5/2) * Beta(t,s)
#		for given values of (t, s)

set.seed(1234)

library(scales) # to plot with alpha transparency

# Grid size
grid_size <- 1e4

# Define the grid of values for theta.
grid_theta <- seq(from=1e-4, to= (1-1e-4), length.out=grid_size)

tt <- 0.9333333
ss_init <- 3.333333

# Converting the values of (s, t) to (alpha, beta):
alpha <- ss_init * tt
beta  <- ss_init * (1 - tt)

# Define the (unnormalized) prior distro.
prior_pdf_unnorm <- ((alpha + beta)^(-5/2)) * dbeta(grid_theta, alpha, beta)

# Normalize the prior distro.
prior_pdf <- (prior_pdf_unnorm * grid_size) / sum(prior_pdf_unnorm)

# Plot PDF
plot(grid_theta, prior_pdf, xlab=expression(theta), ylab="PDF", type = "l", lwd=2, col = "blue"
	)

# Save the plot
dev.copy(jpeg, paste("pdf_prior_t_", round(tt, 3), "_s_", round(ss_init, 3),
	"_alpha_", round(alpha, 3), "_beta_", round(beta, 3),
	"_.jpeg", sep="") ); dev.off()

# -------------------

#		Compute the CDF

sample_size = 1e4

# Generate a random sample of theta from the prior PDF
sample_pdf_theta <- sample(grid_theta, prob= prior_pdf, size = sample_size, replace=TRUE)

# Compute the empirical CDF from the random sample of theta
sample_ecdf <- ecdf(sample_pdf_theta)

# Plot the CDF (line):
plot( knots(sample_ecdf) , sample_ecdf(knots(sample_ecdf)) ,
    xlim=c(0.037,0.963), ylim=c(0.037,0.963),
    xlab=expression(theta), ylab="CDF", type='l', lwd=2, col = "blue" )

# Save the plot
dev.copy(jpeg, paste("cdf_prior_t_", round(tt, 3), "_s_", round(ss_init, 3),
	"_alpha_", round(alpha, 3), "_beta_", round(beta, 3),
	"_.jpeg", sep="") ); dev.off()
