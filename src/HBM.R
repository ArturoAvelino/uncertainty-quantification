# R code to compute the CDFs of theta using values of (t,s) with the
# highest posterior probabiliy.

#           CDF band around the marginal CDF of theta

# Dataframe with (t,s) values only.
post_sampling_t_s.df <- data.frame(sample.tt, sample.ss)

# Dataframe with the unique values of (t,s), and its frequency in the "post_sampling_t_s.df" dataframe.
freq.df <- count(post_sampling_t_s.df, vars=c("sample.tt", "sample.ss"))

# Checking that the frequencies corresponds to the sample_size #no
sum(freq.df$freq) #no

# Largest frequency of (t,s) values #no
max(freq.df$freq) #no

# Select the rows with the largest probability
highest_freq.df <- freq.df[freq.df$freq > (max(freq.df$freq) - 2606), ]

sum(highest_freq.df$freq) #no

# Quantile of the frequency accounted by the pairs (t,s) with the largest probability.
probab_region <- sum(highest_freq.df$freq) / sum(freq.df$freq)

# --------------------------------------------------------60

#           Loop: Compute and plot the CDF

# Grid size
grid_size <- 1e2

# Define the grid of values for theta.
grid_theta <- seq(from=1e-2, to= (1-1e-2), length.out=grid_size)

for (i1 in 1:nrow(highest_freq.df) ) {

    tt_int <- highest_freq.df[i1,1]
    ss_int <- highest_freq.df[i1,2]

    # Converting the values (s, t) to (alpha, beta):
    alpha_int <- ss_int * tt_int
    beta_int  <- ss_int * (1 - tt_int)

    # Compute the unnormalized PDF of the theta posterior in HBM
    posterior_pdf_unnorm <- ((alpha_int + beta_int)^(-5/2)) * dbeta(grid_theta,
        alpha_int, beta_int) * dbinom(yy, size = nn, prob = grid_theta)

    # Normalize the posterior of theta
    posterior_pdf <- (posterior_pdf_unnorm * grid_size) / sum(posterior_pdf_unnorm)

    # Plot the normalize posterior of theta
    # plot(grid_theta, posterior_pdf)

    # Generate a random sample of theta, following its the posterior distribution
    sample_pdf_theta <- sample(grid_theta, prob= posterior_pdf, size = sample_size, replace=TRUE)

    # hist(sample_pdf_theta, breaks = 9, freq = FALSE)

    # Compute the empirical CDF from the random sample of theta
    sample_ecdf <- ecdf(sample_pdf_theta)

    # Plot the CDF (line):
    points( knots(sample_ecdf) , sample_ecdf(knots(sample_ecdf)) ,
        xlim=c(0.037,0.963), ylim=c(0.037,0.963),
        xlab="theta", ylab="CDF", type='l', lwd=2
        # , col = "blue"
        , col = "grey"
        )
}

dev.copy(jpeg, "cdf_theta_band_.jpeg"); dev.off()
