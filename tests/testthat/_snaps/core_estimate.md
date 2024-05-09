# snapshot of the model running through nmkb

    Code
      estimate_model(a_model, lower = c(1, 0.2, 0.1), upper = c(7, 0.8, 0.6),
      use_de_optim = F, use_nmkb = T, verbose = 2)
    Output
      INFO: Running bounded Nelder-Mead
      [33mINFO: Parameters muc=2, b=0.5, non_dec=0.2 
      	gave -log_like_val of -274.8004 [0m
      [33mINFO: Parameters muc=4.582, b=0.574, non_dec=0.246 
      	gave -log_like_val of 390.913 [0m
      [33mINFO: Parameters muc=2.489, b=0.729, non_dec=0.246 
      	gave -log_like_val of -1941.545 [0m
      [33mINFO: Parameters muc=2.489, b=0.574, non_dec=0.425 
      	gave -log_like_val of 20448.46 [0m
      [33mINFO: Parameters muc=3.349, b=0.658, non_dec=0.131 
      	gave -log_like_val of 1057.441 [0m
      [33mINFO: Parameters muc=3.115, b=0.639, non_dec=0.166 
      	gave -log_like_val of 44.61002 [0m
      [33mINFO: Parameters muc=1.411, b=0.691, non_dec=0.166 
      	gave -log_like_val of -731.6069 [0m
      [33mINFO: Parameters muc=1.3, b=0.675, non_dec=0.246 
      	gave -log_like_val of -1262.912 [0m
      [33mINFO: Parameters muc=1.333, b=0.777, non_dec=0.229 
      	gave -log_like_val of -1003.169 [0m
      [33mINFO: Parameters muc=1.728, b=0.768, non_dec=0.351 
      	gave -log_like_val of 4007.269 [0m
      [33mINFO: Parameters muc=1.475, b=0.718, non_dec=0.198 
      	gave -log_like_val of -1067.521 [0m
      [33mINFO: Parameters muc=2.091, b=0.534, non_dec=0.228 
      	gave -log_like_val of -1043.739 [0m
      [33mINFO: Parameters muc=1.825, b=0.636, non_dec=0.228 
      	gave -log_like_val of -1450.529 [0m
      [33mINFO: Parameters muc=2.119, b=0.642, non_dec=0.291 
      	gave -log_like_val of -1991.782 [0m
      [33mINFO: Parameters muc=2.633, b=0.591, non_dec=0.349 
      	gave -log_like_val of 868.8253 [0m
      [33mINFO: Parameters muc=3.998, b=0.676, non_dec=0.262 
      	gave -log_like_val of -1586.379 [0m
      [33mINFO: Parameters muc=4.171, b=0.725, non_dec=0.309 
      	gave -log_like_val of -1948.259 [0m
      [33mINFO: Parameters muc=1.972, b=0.727, non_dec=0.301 
      	gave -log_like_val of -1300.3 [0m
      [33mINFO: Parameters muc=3.391, b=0.69, non_dec=0.272 
      	gave -log_like_val of -2119.497 [0m
      [33mINFO: Parameters muc=3.892, b=0.636, non_dec=0.339 
      	gave -log_like_val of -484.0995 [0m
      [33mINFO: Parameters muc=2.797, b=0.711, non_dec=0.267 
      	gave -log_like_val of -2129.426 [0m
      [33mINFO: Parameters muc=1.755, b=0.628, non_dec=0.247 
      	gave -log_like_val of -1619.114 [0m
      [33mINFO: Parameters muc=3.41, b=0.706, non_dec=0.293 
      	gave -log_like_val of -2210.055 [0m
      [33mINFO: Parameters muc=4.544, b=0.743, non_dec=0.263 
      	gave -log_like_val of -1396.617 [0m
      [33mINFO: Parameters muc=2.598, b=0.675, non_dec=0.284 
      	gave -log_like_val of -2154.497 [0m
      [33mINFO: Parameters muc=2.503, b=0.706, non_dec=0.291 
      	gave -log_like_val of -1992.717 [0m
      [33mINFO: Parameters muc=3.15, b=0.694, non_dec=0.276 
      	gave -log_like_val of -2199.899 [0m
      [33mINFO: Parameters muc=3.293, b=0.671, non_dec=0.302 
      	gave -log_like_val of -2201.654 [0m
      [33mINFO: Parameters muc=4.058, b=0.706, non_dec=0.296 
      	gave -log_like_val of -2076.143 [0m
      [33mINFO: Parameters muc=2.924, b=0.683, non_dec=0.287 
      	gave -log_like_val of -2215.975 [0m
      [33mINFO: Parameters muc=3.26, b=0.68, non_dec=0.312 
      	gave -log_like_val of -2004.248 [0m
      [33mINFO: Parameters muc=3.177, b=0.691, non_dec=0.285 
      	gave -log_like_val of -2221.526 [0m
      [33mINFO: Parameters muc=3.043, b=0.713, non_dec=0.275 
      	gave -log_like_val of -2176.594 [0m
      [33mINFO: Parameters muc=3.229, b=0.683, non_dec=0.295 
      	gave -log_like_val of -2235.343 [0m
      [33mINFO: Parameters muc=2.825, b=0.663, non_dec=0.286 
      	gave -log_like_val of -2221.408 [0m
      [33mINFO: Parameters muc=3.227, b=0.675, non_dec=0.29 
      	gave -log_like_val of -2236.804 [0m
      [33mINFO: Parameters muc=3.386, b=0.671, non_dec=0.291 
      	gave -log_like_val of -2234.662 [0m
      [33mINFO: Parameters muc=3.627, b=0.701, non_dec=0.294 
      	gave -log_like_val of -2192.403 [0m
      [33mINFO: Parameters muc=3.014, b=0.673, non_dec=0.288 
      	gave -log_like_val of -2235.342 [0m
      [33mINFO: Parameters muc=3.134, b=0.662, non_dec=0.297 
      	gave -log_like_val of -2240.741 [0m
      [33mINFO: Parameters muc=3.113, b=0.645, non_dec=0.303 
      	gave -log_like_val of -2235.768 [0m
      [33mINFO: Parameters muc=3.387, b=0.674, non_dec=0.3 
      	gave -log_like_val of -2198.694 [0m
      [33mINFO: Parameters muc=3.104, b=0.673, non_dec=0.291 
      	gave -log_like_val of -2239.922 [0m
      [33mINFO: Parameters muc=3.082, b=0.656, non_dec=0.29 
      	gave -log_like_val of -2241.712 [0m
      [33mINFO: Parameters muc=3.01, b=0.642, non_dec=0.287 
      	gave -log_like_val of -2239.442 [0m
      [33mINFO: Parameters muc=2.989, b=0.652, non_dec=0.295 
      	gave -log_like_val of -2240.64 [0m
      [33mINFO: Parameters muc=3.032, b=0.639, non_dec=0.297 
      	gave -log_like_val of -2258.559 [0m
      [33mINFO: Parameters muc=2.997, b=0.62, non_dec=0.3 
      	gave -log_like_val of -2259.661 [0m
      [33mINFO: Parameters muc=3.154, b=0.641, non_dec=0.296 
      	gave -log_like_val of -2259.866 [0m
      [33mINFO: Parameters muc=3.238, b=0.635, non_dec=0.297 
      	gave -log_like_val of -2258.057 [0m
      [33mINFO: Parameters muc=3.02, b=0.615, non_dec=0.294 
      	gave -log_like_val of -2250.393 [0m
      [33mINFO: Parameters muc=3.031, b=0.591, non_dec=0.304 
      	gave -log_like_val of -2270.877 [0m
      [33mINFO: Parameters muc=3.006, b=0.554, non_dec=0.311 
      	gave -log_like_val of -2247.314 [0m
      [33mINFO: Parameters muc=3.1, b=0.621, non_dec=0.306 
      	gave -log_like_val of -2233.076 [0m
      [33mINFO: Parameters muc=3.04, b=0.616, non_dec=0.297 
      	gave -log_like_val of -2267.572 [0m
      [33mINFO: Parameters muc=3.154, b=0.614, non_dec=0.298 
      	gave -log_like_val of -2261.322 [0m
      [33mINFO: Parameters muc=2.997, b=0.57, non_dec=0.303 
      	gave -log_like_val of -2259.996 [0m
      [33mINFO: Parameters muc=3.036, b=0.589, non_dec=0.301 
      	gave -log_like_val of -2270.161 [0m
      [33mINFO: Parameters muc=2.921, b=0.584, non_dec=0.304 
      	gave -log_like_val of -2270.808 [0m
      [33mINFO: Parameters muc=2.951, b=0.557, non_dec=0.309 
      	gave -log_like_val of -2262.288 [0m
      [33mINFO: Parameters muc=3.018, b=0.602, non_dec=0.3 
      	gave -log_like_val of -2264.755 [0m
      [33mINFO: Parameters muc=3.087, b=0.595, non_dec=0.304 
      	gave -log_like_val of -2269.157 [0m
      [33mINFO: Parameters muc=3.029, b=0.592, non_dec=0.305 
      	gave -log_like_val of -2261.257 [0m
      [33mINFO: Parameters muc=3.026, b=0.578, non_dec=0.307 
      	gave -log_like_val of -2265.852 [0m
      [33mINFO: Parameters muc=3.068, b=0.584, non_dec=0.305 
      	gave -log_like_val of -2265.675 [0m
      [33mINFO: Parameters muc=3.058, b=0.586, non_dec=0.305 
      	gave -log_like_val of -2267.569 [0m
      [33mINFO: Parameters muc=3.091, b=0.603, non_dec=0.301 
      	gave -log_like_val of -2269.709 [0m
      [33mINFO: Parameters muc=3.082, b=0.606, non_dec=0.301 
      	gave -log_like_val of -2269.46 [0m
      [33mINFO: Parameters muc=3.048, b=0.606, non_dec=0.3 
      	gave -log_like_val of -2270.063 [0m
      [33mINFO: Parameters muc=3.032, b=0.593, non_dec=0.302 
      	gave -log_like_val of -2271.259 [0m
      [33mINFO: Parameters muc=3.008, b=0.586, non_dec=0.303 
      	gave -log_like_val of -2270.402 [0m
      [33mINFO: Parameters muc=2.984, b=0.59, non_dec=0.303 
      	gave -log_like_val of -2271.826 [0m
      [33mINFO: Parameters muc=2.931, b=0.584, non_dec=0.304 
      	gave -log_like_val of -2270.915 [0m
      [33mINFO: Parameters muc=2.983, b=0.577, non_dec=0.306 
      	gave -log_like_val of -2266.529 [0m
      [33mINFO: Parameters muc=3.032, b=0.599, non_dec=0.302 
      	gave -log_like_val of -2271.462 [0m
      [33mINFO: Parameters muc=3.001, b=0.597, non_dec=0.301 
      	gave -log_like_val of -2271.813 [0m
      [33mINFO: Parameters muc=2.979, b=0.598, non_dec=0.301 
      	gave -log_like_val of -2271.63 [0m
      [33mINFO: Parameters muc=2.945, b=0.591, non_dec=0.302 
      	gave -log_like_val of -2271.778 [0m
      [33mINFO: Parameters muc=2.974, b=0.588, non_dec=0.303 
      	gave -log_like_val of -2271.638 [0m
      [33mINFO: Parameters muc=2.975, b=0.591, non_dec=0.302 
      	gave -log_like_val of -2271.925 [0m
      [33mINFO: Parameters muc=3.029, b=0.594, non_dec=0.302 
      	gave -log_like_val of -2271.443 [0m
      [33mINFO: Parameters muc=2.966, b=0.592, non_dec=0.302 
      	gave -log_like_val of -2271.98 [0m
      [33mINFO: Parameters muc=2.949, b=0.585, non_dec=0.304 
      	gave -log_like_val of -2271.088 [0m
      [33mINFO: Parameters muc=2.988, b=0.594, non_dec=0.302 
      	gave -log_like_val of -2272.003 [0m
      [33mINFO: Parameters muc=2.968, b=0.594, non_dec=0.301 
      	gave -log_like_val of -2271.923 [0m
      [33mINFO: Parameters muc=2.972, b=0.593, non_dec=0.301 
      	gave -log_like_val of -2271.999 [0m
      [33mINFO: Parameters muc=2.975, b=0.596, non_dec=0.301 
      	gave -log_like_val of -2271.863 [0m
      [33mINFO: Parameters muc=2.975, b=0.592, non_dec=0.302 
      	gave -log_like_val of -2271.998 [0m
      [33mINFO: Parameters muc=2.991, b=0.594, non_dec=0.301 
      	gave -log_like_val of -2271.994 [0m
      [33mINFO: Parameters muc=2.985, b=0.594, non_dec=0.301 
      	gave -log_like_val of -2272.013 [0m
      [33mINFO: Parameters muc=2.988, b=0.595, non_dec=0.301 
      	gave -log_like_val of -2271.951 [0m
      [33mINFO: Parameters muc=2.978, b=0.593, non_dec=0.302 
      	gave -log_like_val of -2272.016 [0m
      [33mINFO: Parameters muc=2.995, b=0.594, non_dec=0.302 
      	gave -log_like_val of -2271.97 [0m
      [33mINFO: Parameters muc=2.978, b=0.593, non_dec=0.301 
      	gave -log_like_val of -2272.015 [0m
      [33mINFO: Parameters muc=2.973, b=0.592, non_dec=0.301 
      	gave -log_like_val of -2272.008 [0m
      [33mINFO: Parameters muc=2.977, b=0.593, non_dec=0.302 
      	gave -log_like_val of -2272.016 [0m
      [33mINFO: Parameters muc=2.971, b=0.592, non_dec=0.302 
      	gave -log_like_val of -2272.002 [0m
      [33mINFO: Parameters muc=2.981, b=0.593, non_dec=0.302 
      	gave -log_like_val of -2272.018 [0m
      [33mINFO: Parameters muc=2.98, b=0.593, non_dec=0.302 
      	gave -log_like_val of -2272.012 [0m
      [33mINFO: Parameters muc=2.978, b=0.593, non_dec=0.302 
      	gave -log_like_val of -2272.017 [0m
      [33mINFO: Parameters muc=2.982, b=0.593, non_dec=0.302 
      	gave -log_like_val of -2272.017 [0m
      [33mINFO: Parameters muc=2.983, b=0.594, non_dec=0.301 
      	gave -log_like_val of -2272.013 [0m
      [33mINFO: Parameters muc=2.979, b=0.593, non_dec=0.302 
      	gave -log_like_val of -2272.018 [0m
      [33mINFO: Parameters muc=2.977, b=0.593, non_dec=0.301 
      	gave -log_like_val of -2272.017 [0m
      [33mINFO: Parameters muc=2.981, b=0.593, non_dec=0.302 
      	gave -log_like_val of -2272.018 [0m
      [33mINFO: Parameters muc=2.983, b=0.593, non_dec=0.302 
      	gave -log_like_val of -2272.016 [0m
      [33mINFO: Parameters muc=2.979, b=0.593, non_dec=0.302 
      	gave -log_like_val of -2272.018 [0m
      [33mINFO: Parameters muc=2.979, b=0.593, non_dec=0.302 
      	gave -log_like_val of -2272.018 [0m
      [33mINFO: Parameters muc=2.979, b=0.593, non_dec=0.302 
      	gave -log_like_val of -2272.018 [0m
      [33mINFO: Parameters muc=2.978, b=0.593, non_dec=0.302 
      	gave -log_like_val of -2272.017 [0m
      [33mINFO: Parameters muc=2.98, b=0.593, non_dec=0.302 
      	gave -log_like_val of -2272.018 [0m
      [33mINFO: Parameters muc=2.98, b=0.593, non_dec=0.302 
      	gave -log_like_val of -2272.018 [0m
      [33mINFO: Parameters muc=2.98, b=0.593, non_dec=0.302 
      	gave -log_like_val of -2272.018 [0m
      [33mINFO: Parameters muc=2.98, b=0.593, non_dec=0.302 
      	gave -log_like_val of -2272.018 [0m
      [33mINFO: Parameters muc=2.98, b=0.593, non_dec=0.302 
      	gave -log_like_val of -2272.018 [0m
      [33mINFO: Parameters muc=2.98, b=0.593, non_dec=0.302 
      	gave -log_like_val of -2272.018 [0m
      [33mINFO: Parameters muc=2.98, b=0.593, non_dec=0.302 
      	gave -log_like_val of -2272.018 [0m
      [33mINFO: Parameters muc=2.98, b=0.593, non_dec=0.302 
      	gave -log_like_val of -2272.018 [0m
      [33mINFO: Parameters muc=2.981, b=0.593, non_dec=0.302 
      	gave -log_like_val of -2272.018 [0m
      [33mINFO: Parameters muc=2.98, b=0.593, non_dec=0.302 
      	gave -log_like_val of -2272.018 [0m
      [33mINFO: Parameters muc=2.98, b=0.593, non_dec=0.302 
      	gave -log_like_val of -2272.018 [0m
      [33mINFO: Parameters muc=2.98, b=0.593, non_dec=0.302 
      	gave -log_like_val of -2272.018 [0m
      [33mINFO: Parameters muc=2.979, b=0.593, non_dec=0.302 
      	gave -log_like_val of -2272.018 [0m
      [33mINFO: Parameters muc=2.98, b=0.593, non_dec=0.302 
      	gave -log_like_val of -2272.018 [0m
      [33mINFO: Parameters muc=2.98, b=0.593, non_dec=0.302 
      	gave -log_like_val of -2272.018 [0m
      [33mINFO: Parameters muc=2.98, b=0.593, non_dec=0.302 
      	gave -log_like_val of -2272.018 [0m
      [33mINFO: Parameters muc=2.98, b=0.593, non_dec=0.302 
      	gave -log_like_val of -2272.018 [0m
      [33mINFO: Parameters muc=2.98, b=0.593, non_dec=0.302 
      	gave -log_like_val of -2272.018 [0m
      [33mINFO: Parameters muc=2.98, b=0.593, non_dec=0.302 
      	gave -log_like_val of -2272.018 [0m
      [33mINFO: Parameters muc=2.98, b=0.593, non_dec=0.302 
      	gave -log_like_val of -2272.018 [0m
      [33mINFO: Parameters muc=2.98, b=0.593, non_dec=0.302 
      	gave -log_like_val of -2272.018 [0m
      [33mINFO: Parameters muc=2.98, b=0.593, non_dec=0.302 
      	gave -log_like_val of -2272.018 [0m
      [33mINFO: Parameters muc=2.98, b=0.593, non_dec=0.302 
      	gave -log_like_val of -2272.018 [0m
      [33mINFO: Estimation gave muc=2.98, b=0.593, non_dec=0.302 
      	with -log_like_val of -2272.018 [0m
      Class(es): ratcliff_dm, drift_dm
      
      Model Parameters:
        values: muc=2.98, b=0.593, non_dec=0.302
        free: muc, b, non_dec
      
      Conditions: null
      
      Deriving PDFs:
        solver: kfe
        values: sigma=1, t_max=3, dt=0.005, dx=0.05, nt=600, nx=40
      
      Observed Data: 3000 trials

# behavior of DE and nmkb toggles

    Code
      estimate_model(drift_dm_obj = a_model, lower = c(1, 0.1), upper = c(5, 0.5),
      seed = 1, verbose = 1, use_de_optim = TRUE, de_control = list(reltol = 1e-09,
        steptol = 50, itermax = 200, trace = TRUE))
    Output
      INFO: Running differential evolution
      Iteration: 1 bestvalit: -577.757131 bestmemit:    3.748091    0.297417
      Iteration: 2 bestvalit: -577.757131 bestmemit:    3.748091    0.297417
      Iteration: 3 bestvalit: -577.757131 bestmemit:    3.748091    0.297417
      Iteration: 4 bestvalit: -579.705425 bestmemit:    3.955018    0.310652
      Iteration: 5 bestvalit: -579.705425 bestmemit:    3.955018    0.310652
      Iteration: 6 bestvalit: -583.687943 bestmemit:    3.976858    0.305039
      Iteration: 7 bestvalit: -583.687943 bestmemit:    3.976858    0.305039
      Iteration: 8 bestvalit: -583.953586 bestmemit:    4.193860    0.309204
      Iteration: 9 bestvalit: -583.953586 bestmemit:    4.193860    0.309204
      Iteration: 10 bestvalit: -584.824471 bestmemit:    4.124334    0.305837
      Iteration: 11 bestvalit: -584.824471 bestmemit:    4.124334    0.305837
      Iteration: 12 bestvalit: -584.824471 bestmemit:    4.124334    0.305837
      Iteration: 13 bestvalit: -584.945391 bestmemit:    4.189786    0.305424
      Iteration: 14 bestvalit: -584.945391 bestmemit:    4.189786    0.305424
      Iteration: 15 bestvalit: -584.945391 bestmemit:    4.189786    0.305424
      Iteration: 16 bestvalit: -584.946686 bestmemit:    4.203591    0.306190
      Iteration: 17 bestvalit: -584.953494 bestmemit:    4.202107    0.305834
      Iteration: 18 bestvalit: -584.953494 bestmemit:    4.202107    0.305834
      Iteration: 19 bestvalit: -584.957250 bestmemit:    4.183445    0.305671
      Iteration: 20 bestvalit: -584.957250 bestmemit:    4.183445    0.305671
      Iteration: 21 bestvalit: -584.957845 bestmemit:    4.193842    0.305792
      Iteration: 22 bestvalit: -584.957845 bestmemit:    4.193842    0.305792
      Iteration: 23 bestvalit: -584.958657 bestmemit:    4.186873    0.305826
      Iteration: 24 bestvalit: -584.958657 bestmemit:    4.186873    0.305826
      Iteration: 25 bestvalit: -584.958778 bestmemit:    4.190052    0.305821
      Iteration: 26 bestvalit: -584.958778 bestmemit:    4.190052    0.305821
      Iteration: 27 bestvalit: -584.958778 bestmemit:    4.190052    0.305821
      Iteration: 28 bestvalit: -584.958778 bestmemit:    4.190052    0.305821
      Iteration: 29 bestvalit: -584.958807 bestmemit:    4.187914    0.305809
      Iteration: 30 bestvalit: -584.958807 bestmemit:    4.187914    0.305809
      Iteration: 31 bestvalit: -584.958811 bestmemit:    4.189094    0.305824
      Iteration: 32 bestvalit: -584.958811 bestmemit:    4.189094    0.305824
      Iteration: 33 bestvalit: -584.958814 bestmemit:    4.188990    0.305822
      Iteration: 34 bestvalit: -584.958814 bestmemit:    4.188990    0.305822
      Iteration: 35 bestvalit: -584.958817 bestmemit:    4.189166    0.305820
      Iteration: 36 bestvalit: -584.958823 bestmemit:    4.188966    0.305815
      Iteration: 37 bestvalit: -584.958823 bestmemit:    4.188966    0.305815
      Iteration: 38 bestvalit: -584.958825 bestmemit:    4.188577    0.305806
      Iteration: 39 bestvalit: -584.958826 bestmemit:    4.188721    0.305809
      Iteration: 40 bestvalit: -584.958826 bestmemit:    4.188721    0.305809
      Iteration: 41 bestvalit: -584.958826 bestmemit:    4.188721    0.305809
      Iteration: 42 bestvalit: -584.958826 bestmemit:    4.188721    0.305809
      Iteration: 43 bestvalit: -584.958826 bestmemit:    4.188714    0.305809
      Iteration: 44 bestvalit: -584.958826 bestmemit:    4.188714    0.305809
      Iteration: 45 bestvalit: -584.958826 bestmemit:    4.188714    0.305809
      Iteration: 46 bestvalit: -584.958826 bestmemit:    4.188714    0.305809
      Iteration: 47 bestvalit: -584.958826 bestmemit:    4.188714    0.305809
      Iteration: 48 bestvalit: -584.958826 bestmemit:    4.188714    0.305809
      Iteration: 49 bestvalit: -584.958826 bestmemit:    4.188714    0.305809
      Iteration: 50 bestvalit: -584.958826 bestmemit:    4.188714    0.305809
      Iteration: 51 bestvalit: -584.958826 bestmemit:    4.188714    0.305809
      Iteration: 52 bestvalit: -584.958826 bestmemit:    4.188710    0.305809
      Iteration: 53 bestvalit: -584.958826 bestmemit:    4.188710    0.305809
      Iteration: 54 bestvalit: -584.958826 bestmemit:    4.188710    0.305809
      Iteration: 55 bestvalit: -584.958826 bestmemit:    4.188710    0.305809
      Iteration: 56 bestvalit: -584.958826 bestmemit:    4.188710    0.305809
      Iteration: 57 bestvalit: -584.958826 bestmemit:    4.188710    0.305809
      Iteration: 58 bestvalit: -584.958826 bestmemit:    4.188708    0.305809
      Iteration: 59 bestvalit: -584.958826 bestmemit:    4.188708    0.305809
      Iteration: 60 bestvalit: -584.958826 bestmemit:    4.188708    0.305809
      Iteration: 61 bestvalit: -584.958826 bestmemit:    4.188709    0.305809
      Iteration: 62 bestvalit: -584.958826 bestmemit:    4.188709    0.305809
      Iteration: 63 bestvalit: -584.958826 bestmemit:    4.188709    0.305809
      Iteration: 64 bestvalit: -584.958826 bestmemit:    4.188709    0.305809
      Iteration: 65 bestvalit: -584.958826 bestmemit:    4.188709    0.305809
      Iteration: 66 bestvalit: -584.958826 bestmemit:    4.188709    0.305809
      Iteration: 67 bestvalit: -584.958826 bestmemit:    4.188709    0.305809
      Iteration: 68 bestvalit: -584.958826 bestmemit:    4.188709    0.305809
      Iteration: 69 bestvalit: -584.958826 bestmemit:    4.188709    0.305809
      Iteration: 70 bestvalit: -584.958826 bestmemit:    4.188709    0.305809
      Iteration: 71 bestvalit: -584.958826 bestmemit:    4.188709    0.305809
      Iteration: 72 bestvalit: -584.958826 bestmemit:    4.188709    0.305809
      Iteration: 73 bestvalit: -584.958826 bestmemit:    4.188709    0.305809
      Iteration: 74 bestvalit: -584.958826 bestmemit:    4.188709    0.305809
      Iteration: 75 bestvalit: -584.958826 bestmemit:    4.188709    0.305809
      Iteration: 76 bestvalit: -584.958826 bestmemit:    4.188709    0.305809
      Iteration: 77 bestvalit: -584.958826 bestmemit:    4.188709    0.305809
      Iteration: 78 bestvalit: -584.958826 bestmemit:    4.188709    0.305809
      Iteration: 79 bestvalit: -584.958826 bestmemit:    4.188709    0.305809
      Iteration: 80 bestvalit: -584.958826 bestmemit:    4.188709    0.305809
      Iteration: 81 bestvalit: -584.958826 bestmemit:    4.188709    0.305809
      Iteration: 82 bestvalit: -584.958826 bestmemit:    4.188709    0.305809
      Iteration: 83 bestvalit: -584.958826 bestmemit:    4.188709    0.305809
      Iteration: 84 bestvalit: -584.958826 bestmemit:    4.188709    0.305809
      Iteration: 85 bestvalit: -584.958826 bestmemit:    4.188709    0.305809
      Iteration: 86 bestvalit: -584.958826 bestmemit:    4.188709    0.305809
      Iteration: 87 bestvalit: -584.958826 bestmemit:    4.188709    0.305809
      Iteration: 88 bestvalit: -584.958826 bestmemit:    4.188709    0.305809
      Iteration: 89 bestvalit: -584.958826 bestmemit:    4.188709    0.305809
      Iteration: 90 bestvalit: -584.958826 bestmemit:    4.188709    0.305809
      [33mINFO: Estimation gave muc=4.189, b=0.6, non_dec=0.306, sd_non_dec=0.02, tau=0.04, a=2, A=0.1, alpha=4 
      	with -log_like_val of -584.9588 [0m
      Class(es): dmc_dm, drift_dm
      
      Model Parameters:
        values: muc=4.189, b=0.6, non_dec=0.306, sd_non_dec=0.02, tau=0.04, a=2, A=0.1, alpha=4
        free: muc, non_dec
      
      Conditions: comp, incomp
      
      Deriving PDFs:
        solver: kfe
        values: sigma=1, t_max=1, dt=0.01, dx=0.1, nt=100, nx=20
      
      Observed Data: 600 trials

---

    Code
      estimate_model(drift_dm_obj = a_model, lower = c(1, 0.1), upper = c(5, 0.5),
      seed = 1, verbose = 1, use_de_optim = TRUE, de_control = list(reltol = 1e-09,
        steptol = 50, itermax = 200, trace = TRUE), de_n_cores = 2)
    Output
      INFO: Running differential evolution
      Iteration: 1 bestvalit: -577.757131 bestmemit:    3.748091    0.297417
      Iteration: 2 bestvalit: -577.757131 bestmemit:    3.748091    0.297417
      Iteration: 3 bestvalit: -577.757131 bestmemit:    3.748091    0.297417
      Iteration: 4 bestvalit: -579.705425 bestmemit:    3.955018    0.310652
      Iteration: 5 bestvalit: -579.705425 bestmemit:    3.955018    0.310652
      Iteration: 6 bestvalit: -583.687943 bestmemit:    3.976858    0.305039
      Iteration: 7 bestvalit: -583.687943 bestmemit:    3.976858    0.305039
      Iteration: 8 bestvalit: -583.953586 bestmemit:    4.193860    0.309204
      Iteration: 9 bestvalit: -583.953586 bestmemit:    4.193860    0.309204
      Iteration: 10 bestvalit: -584.824471 bestmemit:    4.124334    0.305837
      Iteration: 11 bestvalit: -584.824471 bestmemit:    4.124334    0.305837
      Iteration: 12 bestvalit: -584.824471 bestmemit:    4.124334    0.305837
      Iteration: 13 bestvalit: -584.945391 bestmemit:    4.189786    0.305424
      Iteration: 14 bestvalit: -584.945391 bestmemit:    4.189786    0.305424
      Iteration: 15 bestvalit: -584.945391 bestmemit:    4.189786    0.305424
      Iteration: 16 bestvalit: -584.946686 bestmemit:    4.203591    0.306190
      Iteration: 17 bestvalit: -584.953494 bestmemit:    4.202107    0.305834
      Iteration: 18 bestvalit: -584.953494 bestmemit:    4.202107    0.305834
      Iteration: 19 bestvalit: -584.957250 bestmemit:    4.183445    0.305671
      Iteration: 20 bestvalit: -584.957250 bestmemit:    4.183445    0.305671
      Iteration: 21 bestvalit: -584.957845 bestmemit:    4.193842    0.305792
      Iteration: 22 bestvalit: -584.957845 bestmemit:    4.193842    0.305792
      Iteration: 23 bestvalit: -584.958657 bestmemit:    4.186873    0.305826
      Iteration: 24 bestvalit: -584.958657 bestmemit:    4.186873    0.305826
      Iteration: 25 bestvalit: -584.958778 bestmemit:    4.190052    0.305821
      Iteration: 26 bestvalit: -584.958778 bestmemit:    4.190052    0.305821
      Iteration: 27 bestvalit: -584.958778 bestmemit:    4.190052    0.305821
      Iteration: 28 bestvalit: -584.958778 bestmemit:    4.190052    0.305821
      Iteration: 29 bestvalit: -584.958807 bestmemit:    4.187914    0.305809
      Iteration: 30 bestvalit: -584.958807 bestmemit:    4.187914    0.305809
      Iteration: 31 bestvalit: -584.958811 bestmemit:    4.189094    0.305824
      Iteration: 32 bestvalit: -584.958811 bestmemit:    4.189094    0.305824
      Iteration: 33 bestvalit: -584.958814 bestmemit:    4.188990    0.305822
      Iteration: 34 bestvalit: -584.958814 bestmemit:    4.188990    0.305822
      Iteration: 35 bestvalit: -584.958817 bestmemit:    4.189166    0.305820
      Iteration: 36 bestvalit: -584.958823 bestmemit:    4.188966    0.305815
      Iteration: 37 bestvalit: -584.958823 bestmemit:    4.188966    0.305815
      Iteration: 38 bestvalit: -584.958825 bestmemit:    4.188577    0.305806
      Iteration: 39 bestvalit: -584.958826 bestmemit:    4.188721    0.305809
      Iteration: 40 bestvalit: -584.958826 bestmemit:    4.188721    0.305809
      Iteration: 41 bestvalit: -584.958826 bestmemit:    4.188721    0.305809
      Iteration: 42 bestvalit: -584.958826 bestmemit:    4.188721    0.305809
      Iteration: 43 bestvalit: -584.958826 bestmemit:    4.188714    0.305809
      Iteration: 44 bestvalit: -584.958826 bestmemit:    4.188714    0.305809
      Iteration: 45 bestvalit: -584.958826 bestmemit:    4.188714    0.305809
      Iteration: 46 bestvalit: -584.958826 bestmemit:    4.188714    0.305809
      Iteration: 47 bestvalit: -584.958826 bestmemit:    4.188714    0.305809
      Iteration: 48 bestvalit: -584.958826 bestmemit:    4.188714    0.305809
      Iteration: 49 bestvalit: -584.958826 bestmemit:    4.188714    0.305809
      Iteration: 50 bestvalit: -584.958826 bestmemit:    4.188714    0.305809
      Iteration: 51 bestvalit: -584.958826 bestmemit:    4.188714    0.305809
      Iteration: 52 bestvalit: -584.958826 bestmemit:    4.188710    0.305809
      Iteration: 53 bestvalit: -584.958826 bestmemit:    4.188710    0.305809
      Iteration: 54 bestvalit: -584.958826 bestmemit:    4.188710    0.305809
      Iteration: 55 bestvalit: -584.958826 bestmemit:    4.188710    0.305809
      Iteration: 56 bestvalit: -584.958826 bestmemit:    4.188710    0.305809
      Iteration: 57 bestvalit: -584.958826 bestmemit:    4.188710    0.305809
      Iteration: 58 bestvalit: -584.958826 bestmemit:    4.188708    0.305809
      Iteration: 59 bestvalit: -584.958826 bestmemit:    4.188708    0.305809
      Iteration: 60 bestvalit: -584.958826 bestmemit:    4.188708    0.305809
      Iteration: 61 bestvalit: -584.958826 bestmemit:    4.188709    0.305809
      Iteration: 62 bestvalit: -584.958826 bestmemit:    4.188709    0.305809
      Iteration: 63 bestvalit: -584.958826 bestmemit:    4.188709    0.305809
      Iteration: 64 bestvalit: -584.958826 bestmemit:    4.188709    0.305809
      Iteration: 65 bestvalit: -584.958826 bestmemit:    4.188709    0.305809
      Iteration: 66 bestvalit: -584.958826 bestmemit:    4.188709    0.305809
      Iteration: 67 bestvalit: -584.958826 bestmemit:    4.188709    0.305809
      Iteration: 68 bestvalit: -584.958826 bestmemit:    4.188709    0.305809
      Iteration: 69 bestvalit: -584.958826 bestmemit:    4.188709    0.305809
      Iteration: 70 bestvalit: -584.958826 bestmemit:    4.188709    0.305809
      Iteration: 71 bestvalit: -584.958826 bestmemit:    4.188709    0.305809
      Iteration: 72 bestvalit: -584.958826 bestmemit:    4.188709    0.305809
      Iteration: 73 bestvalit: -584.958826 bestmemit:    4.188709    0.305809
      Iteration: 74 bestvalit: -584.958826 bestmemit:    4.188709    0.305809
      Iteration: 75 bestvalit: -584.958826 bestmemit:    4.188709    0.305809
      Iteration: 76 bestvalit: -584.958826 bestmemit:    4.188709    0.305809
      Iteration: 77 bestvalit: -584.958826 bestmemit:    4.188709    0.305809
      Iteration: 78 bestvalit: -584.958826 bestmemit:    4.188709    0.305809
      Iteration: 79 bestvalit: -584.958826 bestmemit:    4.188709    0.305809
      Iteration: 80 bestvalit: -584.958826 bestmemit:    4.188709    0.305809
      Iteration: 81 bestvalit: -584.958826 bestmemit:    4.188709    0.305809
      Iteration: 82 bestvalit: -584.958826 bestmemit:    4.188709    0.305809
      Iteration: 83 bestvalit: -584.958826 bestmemit:    4.188709    0.305809
      Iteration: 84 bestvalit: -584.958826 bestmemit:    4.188709    0.305809
      Iteration: 85 bestvalit: -584.958826 bestmemit:    4.188709    0.305809
      Iteration: 86 bestvalit: -584.958826 bestmemit:    4.188709    0.305809
      Iteration: 87 bestvalit: -584.958826 bestmemit:    4.188709    0.305809
      Iteration: 88 bestvalit: -584.958826 bestmemit:    4.188709    0.305809
      Iteration: 89 bestvalit: -584.958826 bestmemit:    4.188709    0.305809
      Iteration: 90 bestvalit: -584.958826 bestmemit:    4.188709    0.305809
      [33mINFO: Estimation gave muc=4.189, b=0.6, non_dec=0.306, sd_non_dec=0.02, tau=0.04, a=2, A=0.1, alpha=4 
      	with -log_like_val of -584.9588 [0m
      Class(es): dmc_dm, drift_dm
      
      Model Parameters:
        values: muc=4.189, b=0.6, non_dec=0.306, sd_non_dec=0.02, tau=0.04, a=2, A=0.1, alpha=4
        free: muc, non_dec
      
      Conditions: comp, incomp
      
      Deriving PDFs:
        solver: kfe
        values: sigma=1, t_max=1, dt=0.01, dx=0.1, nt=100, nx=20
      
      Observed Data: 600 trials

