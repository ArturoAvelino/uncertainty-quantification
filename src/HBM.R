# R code to compute the CDFs of theta using values of (t,s) with the
# highest posterior probabiliy.

post_sampling_t_s.df <- data.frame(sample.tt, sample.ss)

freq.df <- count(post_sampling_t_s.df, vars=c("sample.tt", "sample.ss"))

# Checking that the frequencies corresponds to the sample_size
sum(freq.df$freq)

# Largest frequency of (t,s) values
max(freq.df$freq)

# Select the rows with the largest probability
highest_freq.df <- freq.df[freq.df$freq > (max(freq.df$freq) - 271), ]

sum(highest_freq.df$freq)

sum(highest_freq.df$freq) / sum(freq.df$freq)

# --------------------------------------------------------60

#       CDF

# Boundary values.
tt <- highest_freq.df[1,1]
ss <- highest_freq.df[1,2]

# Converting the values (s, t) to (alpha, beta):
alpha_int <- ss * tt
beta_int  <- ss * (1 - tt)


# Grid size
grid_size <- 1e2

# Define the grid of values for theta.
grid_theta <- seq(from=1e-2, to= (1-1e-2), length.out=grid_size)

# Compute the unnormalized PDF of the posterior in HBM
posterior_pdf_unnorm <- ((alpha_int + beta_int)^(-5/2)) * dbeta(grid_theta,
    alpha_int, beta_int) * dbinom(yy, size = nn, prob = grid_theta)

# Normalize the posterior
posterior_pdf <- (posterior_pdf_unnorm * grid_size) / sum(posterior_pdf_unnorm)

# Plot the normalize posterior
plot(grid_theta, posterior_pdf)
