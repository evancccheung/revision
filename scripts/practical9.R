# Practical 9

# 1
n_sim <- 100000
n_sample <- 568
n_prior_base <- c(6.6, 60)

sim_theta <- rbeta(n_sim, n_prior_base[1], n_prior_base[2])
plot(density(sim_theta), col="blue", lwd=2, xlim=c(0, 0.3), ylim=c(0, 40), las=1)

# 2
sim_y <- rbinom(n_sim, n_sample, sim_theta)

# 3
plot(sim_theta, sim_y, cex=0.5)
abline(h=60, col="red", lty=2)

# 4
plot(density(sim_theta[sim_y==60]), col="red", lwd=2, xlim=c(0, 0.3), ylim=c(0, 40), las=1)

mean(sim_theta[sim_y==60]); quantile(sim_theta[sim_y==60], c(0.025, 0.975))
# Posterior mean = 0.10 (95% CI: 0.08, 0.13)