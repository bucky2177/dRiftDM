rm(list = ls())
# Hallo Vali! :)

one_dm = dmc_dm()
print(one_dm)

# simulate data
data = simulate_data(one_dm, 10000, seed = 1)

# set the data
one_dm = set_data(drift_dm_obj = one_dm, obs_data = data, eval_model = T)
summary(one_dm)

# plot the model
par(mfrow = c(1,1))
#plot_model(one_dm, add_x = FALSE, x_lim = c(0, 0.5))
plot_cafs(one_dm)

# define upper and lower boundary values
        # muc, b, non,   sd,   tau,   A, alpha
lower = c(1, 0.2, 0.1, 0.005, 0.01, 0.01, 2)
upper = c(7, 0.8, 0.6,   0.1, 0.12,  0.4, 8)
one_dm = estimate_model(one_dm, lower = lower, upper = upper,
                        de_control = list(trace = T, reltol = .1),
                        polish = F, verbose = T,seed = 1)

