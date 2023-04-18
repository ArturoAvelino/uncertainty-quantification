# Plot "n" versus the difference in the mean between the likelihood PDF and
# the posterior PDF

nn_delta.df <- data.frame(
    nn = c (0, 10, 100, 1000),
    delta_hbm    = c(0.4292195, 0.0841455, 0.0113137, 0.001163),
    delta_walley = c(0.5, 0.4329411, 0.0794418, 0.009276) )

# ------------------------

plot(nn_delta.df$nn, nn_delta.df$delta_hbm, log="y", type="b", lwd = 2, col="red",
    xlab=expression(italic("n")),
    ylab=expression(paste(Delta, mu, " (log scale)", sep=""))
    )

points(nn_delta.df$nn, nn_delta.df$delta_walley, type="b", lwd = 2, col="black")

dev.copy(jpeg, "plot_n_vs_delta_mean_log_y_.jpeg"); dev.off()
