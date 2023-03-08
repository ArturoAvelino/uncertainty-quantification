#		USER DEFINITIONS

nn <- 100 # the "n" number of trials
yy <- 33 # the "y" number of successes in the "n" trials

mu <- 0.33       # mean value

# -------------------------

# Model 1: Walley's {t fix, s varying}:

t_m1       <- 0.2   # prior "t" for both CDFs

s_m1_left  <- 30   # prior "s" for left CDF
s_m1_right <- 0.01	# prior "s" for right CDF

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
s_m3_right <- 30	   # prior "s" for right CDF

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
