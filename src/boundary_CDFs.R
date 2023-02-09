#       CDF BOUNDARIES FOR THE BETA-BINOMIAL MODEL

# ########################################################60

#       USER DEFINITIONS

nn <- 10 # the "n" number of trials
yy <- 6 # the "y" number of successes in the "n" trials

mu <- 0.6       # mean value

# -------------------------

# Model 1: Walley's {t fix, s varying}:

t_m1       <- 0.2   # prior "t" for both CDFs

s_m1_left  <- 30   # prior "s" for left CDF
s_m1_right <- 0.01  # prior "s" for right CDF

color_m1 <- "lightgrey"  # color to plot the CDFs

# -------------------------

# Model 2: Walley's {t varying, s fix}:

t_m2_left  <- 1e-1         # prior "t" for left CDFs
t_m2_right <- 1 - t_m2_left  # prior "t" for right CDFs

s_m2  <- 15           # prior "s" for both CDF

color_m2 <- "green"  # color to plot the CDFs

# -------------------------

# Model 3: Walley's general model {t varying, s varying}:

t_m3_left  <- 1e-6     # prior "t" for left CDFs
t_m3_right <- 1 - t_m3_left  # prior "t" for right CDFs

s_m3_left  <- 30      # prior "s" for left CDF
s_m3_right <- 30       # prior "s" for right CDF

color_m3 <- "black"  # color to plot the CDFs

# -------------------------

# Model 4: HBM
# There is not "left" and "right" CDF, and {t, s} are marginanlized.

color_m4 <- "red"  # color to plot the CDFs

# -------------------------

# Model 5.1: PBA, for the case [a,b] known.

aa <- 1e-6      # minimum known value of theta
bb <- 1 - aa  # maximum known value of theta

color_m5 <- "blue"

# -------------------------

# Model 5.2: PBA, for the case [a,b], and mean "mu" known.

aa <- 1e-6      # minimum known value of theta
bb <- 1 - aa  # maximum known value of theta

color_m5 <- "blue"

# -------------------------

# The "true" CDF. Fixed values of the parameters used to simulate samples

t_true <- 0.3
s_true <- 4

color_true <- "black"

# ########################################################60
#
#       THE CODE: NO NEED FOR USER INTERACTION

# Random seed
set.seed(1234)

# sample size
sample_size <- 1e4

# --------------------------------------------------------60
# The "true" CDF

# Grid size
grid_size <- 1e4

# Define the grid of values for theta.
grid_theta <- seq(from=1e-8, to=(1 - 1e-8), length.out=grid_size)

# Converting the values (s, t) to (alpha, beta):
alpha_true <- s_true * t_true
beta_true  <- s_true * (1 - t_true)

# Converting (alpha, beta) to (alpha_n, beta_n)
alpha_n_true <- alpha_true + yy
beta_n_true  <- beta_true + nn - yy

# Transformation between (tt, ss) to (tt_n, ss_n)
tt_n_true <- (s_true * t_true + yy)/(s_true + nn)
ss_n_true <- s_true + nn

# Create a sample of random data from a posterior (beta) distribution
sample_true <- rbeta(sample_size, shape1 = alpha_n_true, shape2 = beta_n_true)

# Compute the CDF
cdf_true <- ecdf(sample_true)

# --------------------------------------------------------60

# Model 1: Walley's {t fix, s varying}

#       LEFT CDF

# Converting the values (s, t) to (alpha, beta):
alpha_m1_left <- s_m1_left * t_m1
beta_m1_left  <- s_m1_left * (1 - t_m1)

# Converting (alpha, beta) to (alpha_n, beta_n)
alpha_n_m1_left <- alpha_m1_left + yy
beta_n_m1_left  <- beta_m1_left + nn - yy

# Transformation between (tt, ss) to (tt_n, ss_n)
tt_n_m1_left <- (s_m1_left * t_m1 + yy)/(s_m1_left + nn)
ss_n_m1_left <- s_m1_left + nn

# Create a sample of random data from a posterior (beta) distribution
sample_m1_left <- rbeta(sample_size, shape1 = alpha_n_m1_left, shape2 = beta_n_m1_left)

# Compute the CDF
cdf_m1_left <- ecdf(sample_m1_left)

# --------------------------------------------------------60

#       RIGHT CDF

# Converting the values (s, t) to (alpha, beta):
alpha_m1_right <- s_m1_right * t_m1
beta_m1_right  <- s_m1_right * (1 - t_m1)

# Converting (alpha, beta) to (alpha_n, beta_n)
alpha_n_m1_right <- alpha_m1_right + yy
beta_n_m1_right  <- beta_m1_right + nn - yy

# Transformation between (tt, ss) to (tt_n, ss_n)
tt_n_m1_right <- (s_m1_right * t_m1 + yy)/(s_m1_right + nn)
ss_n_m1_right <- s_m1_right + nn

# Create a sample of random data from a posterior (beta) distribution
sample_m1_right <- rbeta(sample_size, shape1 = alpha_n_m1_right, shape2 = beta_n_m1_right)

# Compute the CDF
cdf_m1_right <- ecdf(sample_m1_right)

# ########################################################60

# Model 2: Walley's {t varying, s fix}:

#       LEFT

# Converting the values (s_m2, t) to (alpha, beta):
alpha_m2_left <- s_m2 * t_m2_left
beta_m2_left  <- s_m2 * (1 - t_m2_left)

# Converting (alpha, beta) to (alpha_n, beta_n)
alpha_n_m2_left <- alpha_m2_left + yy
beta_n_m2_left  <- beta_m2_left + nn - yy

# Transformation between (tt, ss) to (tt_n, ss_n)
tt_n_m2_left <- (s_m2 * t_m2_left + yy)/(s_m2 + nn)
ss_n_m2_left <- s_m2 + nn

# Create a sample of random data from a posterior (beta) distribution
sample_m2_left <- rbeta(sample_size, shape1 = alpha_n_m2_left, shape2 = beta_n_m2_left)

# Compute the CDF
cdf_m2_left <- ecdf(sample_m2_left)

# Plot the CDF
# plot(cdf_m2_left, xlab="theta", ylab="CDF", xlim=c(0,1))
# dev.copy(jpeg, "left_cdf_theta_B_.jpeg"); dev.off()
#
# # A plot with more flexibility to manipulate:
# plot(sample_m2_left, cdf_m2_left(sample_m2_left) ,
#     xlab="theta", ylab="CDF", xlim=c(0,1))
# dev.copy(jpeg, "left_cdf_theta_A_.jpeg"); dev.off()

# --------------------------------------------------------60

#       RIGHT

# Converting the values (s_m2, t) to (alpha, beta):
alpha_m2_right <- s_m2 * t_m2_right
beta_m2_right  <- s_m2 * (1 - t_m2_right)

# Converting (alpha, beta) to (alpha_n, beta_n)
alpha_n_m2_right <- alpha_m2_right + yy
beta_n_m2_right  <- beta_m2_right + nn - yy

# Transformation between (tt, ss) to (tt_n, ss_n)
tt_n_m2_right <- (s_m2 * t_m2_right + yy)/(s_m2 + nn)
ss_n_m2_right <- s_m2 + nn

# Create a sample of random data from a posterior (beta) distribution
sample_m2_right <- rbeta(sample_size, shape1 = alpha_n_m2_right, shape2 = beta_n_m2_right)

# Visualize the data
# hist(sample_m2_right, breaks = 15, freq = FALSE)
# dev.copy(jpeg, "right_histo_theta_.jpeg"); dev.off()
#
# sample_dens_right <- density(sample_m2_right, adjust=2)
# # sample_dens_right <- density(sample_m2_right, adjust=2)
# plot(sample_dens_right, xlab="theta", xlim=c(0,1))
# dev.copy(jpeg, "right_pdf_density_.jpeg"); dev.off()

# Compute the CDF
cdf_m2_right <- ecdf(sample_m2_right)

# Plot the CDF
# plot(cdf_m2_right, xlab="theta", ylab="CDF", xlim=c(0,1))
# dev.copy(jpeg, "right_cdf_theta_B_.jpeg"); dev.off()
#
# # A plot with more flexibility to manipulate:
# plot(sample_m2_right, cdf_m2_right(sample_m2_right) ,
#     xlab="theta", ylab="CDF", xlim=c(0,1))
# dev.copy(jpeg, "right_cdf_theta_A_.jpeg"); dev.off()

# ########################################################60

# Model 4: HBM

#       Parameters: theta, t, s

ite_theta <- 30
ite_tt  <- 30
ite_ss  <- 30

tt_max <- 1
ss_max <- 5

# Define a dataframe where i will write the values.
posterior.df <- data.frame(
    theta    = rep(NA,  (ite_theta+1) * (ite_ss) * (ite_tt-1)) ,
    tt       = rep(NA,  (ite_theta+1) * (ite_ss) * (ite_tt-1)) ,
    tt_n     = rep(NA,  (ite_theta+1) * (ite_ss) * (ite_tt-1)) ,
    ss       = rep(NA,  (ite_theta+1) * (ite_ss) * (ite_tt-1)) ,
    ss_n     = rep(NA,  (ite_theta+1) * (ite_ss) * (ite_tt-1)) ,
    alpha    = rep(NA,  (ite_theta+1) * (ite_ss) * (ite_tt-1)) ,
    beta     = rep(NA,  (ite_theta+1) * (ite_ss) * (ite_tt-1)) ,
    alpha_n  = rep(NA,  (ite_theta+1) * (ite_ss) * (ite_tt-1)) ,
    beta_n   = rep(NA,  (ite_theta+1) * (ite_ss) * (ite_tt-1)) ,
    posterior= rep(NA,  (ite_theta+1) * (ite_ss) * (ite_tt-1))
    )

# Initialize the "row" variable
row <- 1

for (i1 in 1:ite_ss){ # loop for s

    ss_int <- i1 * (ss_max / ite_ss)

    for (i2 in 1:(ite_tt-1)) { # loop for t

        tt_int <- i2 * (tt_max / ite_tt)

        # Convert the values of (s, t) to (alpha, beta):
        alpha_int <- ss_int * tt_int
        beta_int  <- ss_int * (1 - tt_int)

        # Convert the values from (alpha, beta) to (alpha_n, beta_n):
        alpha_n_int <- alpha_int + yy
        beta_n_int  <- beta_int + nn - yy

        # Transformation between (tt, ss) to (tt_n, ss_n)
        tt_n_int <- (ss_int * tt_int + yy)/(ss_int + nn)
        ss_n_int <- ss_int + nn

        for (i3 in 0:ite_theta) { # loop for theta

            theta_int <- i3/ite_theta

            # Posterior distribution with the non-informative hyperprior
            post_int <- ((alpha_int + beta_int)^(-5/2)) * dbeta(theta_int,
                alpha_int, beta_int) * dbinom(yy, size = nn, prob = theta_int)

            # Write in the data frame the variable values and the posterior
            posterior.df$theta[row]   <- theta_int
            posterior.df$tt[row]      <- tt_int
            posterior.df$tt_n[row]    <- tt_n_int
            posterior.df$ss[row]      <- ss_int
            posterior.df$ss_n[row]    <- ss_n_int
            posterior.df$alpha[row]   <- alpha_int
            posterior.df$beta[row]    <- beta_int
            posterior.df$alpha_n[row] <- alpha_n_int
            posterior.df$beta_n[row]  <- beta_n_int
            posterior.df$posterior[row]<-post_int

            row = row + 1
        }
    }
}

# --------------------------------------------------------60

#       MARGINAL distributions by sampling from the posterior, and central CDF

# Remove all rows with NaN value for the probability in the dataframe
posterior.df2 <-na.omit(posterior.df)

# Remove the rows with +/-Inf values.
posterior.df3 <- posterior.df2[is.finite(rowSums(posterior.df2)),]

# Normalize the values of the posterior column in the dataframe
posterior.df3$post_norm = posterior.df3$posterior * (nrow(posterior.df3) / sum(posterior.df3$posterior) )

# Randomize the rows.
sample.rows <- sample( 1:nrow(posterior.df3), size=1e5, replace=TRUE,
      prob=posterior.df3$post_norm)

sample.theta <- posterior.df3$theta[ sample.rows ]
sample.tt <- posterior.df3$tt[ sample.rows ]
sample.ss  <- posterior.df3$ss[  sample.rows ]

# --------------------------------------------------------60

#       CENTRAL CDF

# Compute the CDF
cdf_m4_theta <- ecdf(sample.theta)

# plot( knots(cdf_m4_theta) , cdf_m4_theta(knots(cdf_m4_theta)) ,
#     xlim=c(0.037,0.963), ylim=c(0.037,0.963),
#     xlab="theta", ylab="CDF", type='l', lwd=2, col = "red")
#
# dev.copy(jpeg, "cdf_theta_line_all_.jpeg"); dev.off()

# ########################################################60

# Model 3: Walley's general model {t varying, s varying}:

#       LEFT

# Converting the values (s, t) to (alpha, beta):
alpha_m3_left <- s_m3_left * t_m3_left
beta_m3_left  <- s_m3_left * (1 - t_m3_left)

# Converting (alpha, beta) to (alpha_n, beta_n)
alpha_n_m3_left <- alpha_m3_left + yy
beta_n_m3_left  <- beta_m3_left + nn - yy

# Transformation between (tt, ss) to (tt_n, ss_n)
tt_n_m3_left <- (s_m3_left * t_m3_left + yy)/(s_m3_left + nn)
ss_n_m3_left <- s_m3_left + nn

# Create a sample of random data from a posterior (beta) distribution
sample_m3_left <- rbeta(sample_size, shape1 = alpha_n_m3_left, shape2 = beta_n_m3_left)

# Visualize the data
# hist(sample_m3_left, breaks = 15, freq = FALSE)
# dev.copy(jpeg, paste("left_histo_theta_tt", t_m3_left, "ss", s_m3_left, "_.jpeg")); dev.off()
#
# sample_dens_m3_left <- density(sample_m3_left, adjust=1)
# plot(sample_dens_m3_left, xlab="theta", xlim=c(0,1))
# dev.copy(jpeg, paste("left_pdf_density_tt", t_m3_left, "ss", s_m3_left, "_.jpeg")); dev.off()

# Compute the CDF
cdf_m3_left <- ecdf(sample_m3_left)

# Plot the CDF
# plot(cdf_m3_left, xlab="theta", ylab="CDF", xlim=c(0,1))
# dev.copy(jpeg, paste("left_cdf_theta_B_tt", t_m3_left, "ss", s_m3_left, "_.jpeg")); dev.off()
#
# # A plot with more flexibility to manipulate:
# plot(sample_m3_left, cdf_m3_left(sample_m3_left) ,
#     xlab="theta", ylab="CDF", xlim=c(0,1))
# dev.copy(jpeg, paste("left_cdf_theta_A_tt", t_m3_left, "ss", s_m3_left, "_.jpeg")); dev.off()

# --------------------------------------------------------60

#       RIGHT

# Converting the values (s, t) to (alpha, beta):
alpha_m3_right <- s_m3_right * t_m3_right
beta_m3_right  <- s_m3_right * (1 - t_m3_right)

# Converting (alpha, beta) to (alpha_n, beta_n)
alpha_n_m3_right <- alpha_m3_right + yy
beta_n_m3_right  <- beta_m3_right + nn - yy

# Transformation between (tt, ss) to (tt_n, ss_n)
tt_n_m3_right <- (s_m3_right * t_m3_right + yy)/(s_m3_right + nn)
ss_n_m3_right <- s_m3_right + nn

# Create a sample of random data from a posterior (beta) distribution
sample_m3_right <- rbeta(sample_size, shape1 = alpha_n_m3_right, shape2 = beta_n_m3_right)

# Visualize the data
# hist(sample_m3_right, breaks = 15, freq = FALSE)
# dev.copy(jpeg, paste("right_histo_theta_tt", t_m3_right, "ss", s_m3_right, "_.jpeg")); dev.off()
#
# sample_dens_m3_right <- density(sample_m3_right, adjust=1)
# plot(sample_dens_m3_right, xlab="theta", xlim=c(0,1))
# dev.copy(jpeg, paste("right_pdf_density_tt", t_m3_right, "ss", s_m3_right, "_.jpeg")); dev.off()

# Compute the CDF
cdf_m3_right <- ecdf(sample_m3_right)

# Plot the CDF
# plot(cdf_m3_right, xlab="theta", ylab="CDF", xlim=c(0,1))
# dev.copy(jpeg, paste("right_cdf_theta_B_tt", t_m3_right, "ss", s_m3_right, "_.jpeg")); dev.off()
#
# # A plot with more flexibility to manipulate:
# plot(sample_m3_right, cdf_m3_right(sample_m3_right) ,
#     xlab="theta", ylab="CDF", xlim=c(0,1))
# dev.copy(jpeg, paste("right_cdf_theta_A_tt", t_m3_right, "ss", s_m3_right, "_.jpeg")); dev.off()

# ########################################################60

# Model 5.1: PBA, for the case [a,b] known.

#       "Minimum data" = [a,b]

# Define the Lower Boundary Function (LBF). Eq. (1), Iskandar 2020.
LBF_a_b_fun <- function(theta, bb){
    if (theta < bb) {
        0
    } else { 1 }
}

# Define the Upper Boundary Function (UBF). Eq. (2), Iskandar 2020.
UBF_a_b_fun <- function(theta, aa){
    if (theta < aa) {
        0
    } else { 1 }
}

# Initialize the vector with -1 non-sense values.
LBF_a_b_sample <- rep(-1, grid_size)
UBF_a_b_sample <- rep(-1, grid_size)

# Compute the LBF and UBF values
for(i1 in 1:length(grid_theta)) {

    LBF_a_b_sample[i1] <- LBF_a_b_fun(grid_theta[i1], bb)
    UBF_a_b_sample[i1] <- UBF_a_b_fun(grid_theta[i1], aa)
}

# plot(grid_theta, LBF_a_b_sample, cex=0.2, xlim=c(0,1))
# dev.copy(jpeg, "LBF_a_b_.jpeg"); dev.off()
#
# plot(grid_theta, UBF_a_b_sample, cex=0.2, xlim=c(0,1))
# dev.copy(jpeg, "UBF_a_b_.jpeg"); dev.off()
#
# # Plot together
# plot(grid_theta, LBF_a_b_sample, type="l", lwd=2, col = "blue",
#     xlab=expression(theta), ylab="CDF", xlim=c(0.05,0.968), ylim=c(0.03,0.97))
# points(grid_theta, UBF_a_b_sample, type="l", lwd=2, col = "red")
# dev.copy(jpeg, paste("cdf_LBF_UBF_a_", round(aa, 4), "_b_", round(bb, 4),
#     "_.jpeg", sep="")); dev.off()

# ########################################################60

# Model 5.2: PBA, for the case [a,b] and the mean "mu" are known.

# Define the Lower Boundary Function (LBF). Eq. (5), Iskandar 2020.
LBF_mean_fun <- function(theta, aa, bb, mu){
    if (theta < mu) {
        0
    } else if (mu < theta & theta < bb ) {
        (theta - mu)/ (theta - aa)
    } else { 1 }
}

# Define the Upper Boundary Function (UBF). Eq. (6), Iskandar 2020.
UBF_mean_fun <- function(theta, aa, bb, mu){
    if (theta < aa) {
        0
    } else if (aa < theta & theta < mu ) {
        (bb - mu)/ (bb - theta)
    } else { 1 }
}

# Initialize the vector with -1 non-sense values.
LBF_mean_sample <- rep(-1, grid_size)
UBF_mean_sample <- rep(-1, grid_size)

# Compute the LBF and UBF values
for(i1 in 1:length(grid_theta)) {

    LBF_mean_sample[i1] <- LBF_mean_fun(grid_theta[i1], aa, bb, mu)
    UBF_mean_sample[i1] <- UBF_mean_fun(grid_theta[i1], aa, bb, mu)
}

# ########################################################60

#       Overlap plots

# -----------------------
#       Walley's general model {t varying, s varying}:

plot(knots(cdf_m3_left), cdf_m3_left(knots(cdf_m3_left)),
    col = color_m3, type = "l", lwd=3, xlab=expression(theta), ylab="CDF",
    xlim=c(0.03,0.975), ylim=c(0.036,0.964) )

points(knots(cdf_m3_right),  cdf_m3_right( knots(cdf_m3_right)),
    col = color_m3, type = "l", lwd=3)

# -----------------------
#       Walley's {t varying, s fix}:

points(knots(cdf_m2_left), cdf_m2_left(knots(cdf_m2_left)),
    col = color_m2, type = "l", lwd=2 )

points(knots(cdf_m2_right), cdf_m2_right(knots(cdf_m2_right)),
    col = color_m2, type = "l", lwd=2)

# -----------------------
#       Walley's {t fix, s varying}:

# points(knots(cdf_m1_left), cdf_m1_left(knots(cdf_m1_left)),
#     col = color_m1, type = "l", lwd=2)
#     # , lty="dotted")
#
# points(knots(cdf_m1_right), cdf_m1_right(knots(cdf_m1_right)),
#     col = color_m1, type = "l", lwd=2)
#     # , lty="dotted")

# -----------------------
#       HBM

# points(knots(cdf_m4_theta), cdf_m4_theta(knots(cdf_m4_theta)),
#     col= color_m4, type = "l", lwd=2)

# -----------------------
#       PBA, [a,b] known

# points(grid_theta, LBF_a_b_sample, type="l", lwd=3, col = color_m5)
#
# points(grid_theta, UBF_a_b_sample, type="l", lwd=3, col = color_m5)

# -----------------------
#       PBA, [a,b], mean, known

points(grid_theta, LBF_mean_sample, type="l", lwd=3, col = color_m5)

points(grid_theta, UBF_mean_sample, type="l", lwd=3, col = color_m5)

# -----------------------
#       The "true" CDF

points(knots(cdf_true),  cdf_true( knots(cdf_true)),
    col = color_true, type = "l", lwd=2, lty="dotted")

# -----------------------

dev.copy(jpeg, "cdf_boundaries.jpeg"); dev.off()

# ########################################################60

# Determine the horizontal distance between the left and right CDFs for the different approaches.

distance_m2 <- tt_n_m2_right - tt_n_m2_left
distance_m3 <- tt_n_m3_right - tt_n_m3_left
distance_m5 <- bb - aa

cat(paste("CDF distance, Walley (t vary, s fix)  = ", round(distance_m2,4), sep=""))
cat(paste("CDF distance, Walley (t vary, s vary) = ", round(distance_m3,4), sep=""))
cat(paste("CDF distance, PBA (a, b, mean) known  = ", round(distance_m5,4), sep=""))

# ########################################################60
