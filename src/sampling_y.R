# Generate a random "y" from the posterior distribution

setwd("/Users/aavelino/Downloads/tests") #_

nn <- 100

# Fixed values
tt <- 0.3
ss <- 4
theta <- tt

# --------------------------------------------------------60
# 		Determine the PDF of "y" as a function of the other parameters

# Create an ordered sequence of "y" values.
y_grid <- seq(from=0, to = nn)

# Converting the values (s, t) to (alpha, beta):
alpha <- ss * tt
beta  <- ss * (1 - tt)

# Converting (alpha, beta) to (alpha_n, beta_n)
alpha_n <- alpha + y_grid
beta_n  <- beta + nn - y_grid

# Compute the PDF at every value of "y".
# Given that (alpha_n, beta_n) are changing at each value of "y, then
# the following PDF is not normalized.
pdf_post_grid <- dbeta(theta, alpha_n, beta_n)

# Normalization
pdf_post_grid_norm <- pdf_post_grid / sum(pdf_post_grid)

plot(y_grid, pdf_post_grid_norm, xlab="y", ylab= "PDF(y)")

dev.copy( jpeg, paste("posterior_pdf_y_sample_n_", nn, "_theta_", theta, "_t_",
	tt, "_ss_", ss, "_.jpeg", sep="" ) ); dev.off()

# --------------------------------------------------------60

# Generate a random value of "y" from the PDF posterior.

sample_size <- 1

sample_y_post <- sample(y_grid, prob=pdf_post_grid_norm, size = sample_size, replace = TRUE)

# Show the generated random value of "y"
sample_y_post

# ########################################################60

