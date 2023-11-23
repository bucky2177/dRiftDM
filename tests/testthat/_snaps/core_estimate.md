# input checks estimate_model

    Code
      estimate_model(a_model, lower = c(1, 0.2, 0.1), upper = c(7, 0.8, 0.6),
      use_de_optim = F, use_nmkb = T, verbose = T)
    Output
      
      INFO: Running bounded Nelder-Mead
      [33m INFO: Parameters muc=3, b=0.6, non_dec=0.3 
      	gave -log_like_val of -2271.369 [0m
      
      [33m INFO: Parameters muc=5.603, b=0.657, non_dec=0.358 
      	gave -log_like_val of 4273.136 [0m
      
      [33m INFO: Parameters muc=3.669, b=0.758, non_dec=0.358 
      	gave -log_like_val of 4894.595 [0m
      
      [33m INFO: Parameters muc=3.669, b=0.657, non_dec=0.507 
      	gave -log_like_val of 45992.03 [0m
      
      [33m INFO: Parameters muc=4.602, b=0.714, non_dec=0.18 
      	gave -log_like_val of 1664.201 [0m
      
      [33m INFO: Parameters muc=5.173, b=0.478, non_dec=0.199 
      	gave -log_like_val of 5735.409 [0m
      
      [33m INFO: Parameters muc=4.06, b=0.722, non_dec=0.313 
      	gave -log_like_val of -1876.79 [0m
      
      [33m INFO: Parameters muc=2.229, b=0.712, non_dec=0.181 
      	gave -log_like_val of -1172.522 [0m
      
      [33m INFO: Parameters muc=1.891, b=0.651, non_dec=0.362 
      	gave -log_like_val of 4340.961 [0m
      
      [33m INFO: Parameters muc=3.803, b=0.701, non_dec=0.214 
      	gave -log_like_val of -692.7221 [0m
      
      [33m INFO: Parameters muc=2.38, b=0.669, non_dec=0.307 
      	gave -log_like_val of -1841.884 [0m
      
      [33m INFO: Parameters muc=4.174, b=0.617, non_dec=0.46 
      	gave -log_like_val of 37635.55 [0m
      
      [33m INFO: Parameters muc=2.627, b=0.693, non_dec=0.235 
      	gave -log_like_val of -1753.39 [0m
      
      [33m INFO: Parameters muc=3.624, b=0.645, non_dec=0.387 
      	gave -log_like_val of 14084.32 [0m
      
      [33m INFO: Parameters muc=2.854, b=0.682, non_dec=0.269 
      	gave -log_like_val of -2124.877 [0m
      
      [33m INFO: Parameters muc=4.353, b=0.68, non_dec=0.28 
      	gave -log_like_val of -1633.374 [0m
      
      [33m INFO: Parameters muc=2.8, b=0.672, non_dec=0.3 
      	gave -log_like_val of -2110.599 [0m
      
      [33m INFO: Parameters muc=2.005, b=0.555, non_dec=0.267 
      	gave -log_like_val of -1804.216 [0m
      
      [33m INFO: Parameters muc=3.45, b=0.692, non_dec=0.301 
      	gave -log_like_val of -2164.249 [0m
      
      [33m INFO: Parameters muc=3.41, b=0.651, non_dec=0.279 
      	gave -log_like_val of -2095.024 [0m
      
      [33m INFO: Parameters muc=2.945, b=0.667, non_dec=0.295 
      	gave -log_like_val of -2209.636 [0m
      
      [33m INFO: Parameters muc=3.416, b=0.627, non_dec=0.33 
      	gave -log_like_val of -1130.892 [0m
      
      [33m INFO: Parameters muc=2.988, b=0.67, non_dec=0.283 
      	gave -log_like_val of -2221.524 [0m
      
      [33m INFO: Parameters muc=2.556, b=0.592, non_dec=0.285 
      	gave -log_like_val of -2144.428 [0m
      
      [33m INFO: Parameters muc=3.209, b=0.671, non_dec=0.297 
      	gave -log_like_val of -2232.185 [0m
      
      [33m INFO: Parameters muc=3.188, b=0.63, non_dec=0.292 
      	gave -log_like_val of -2246.868 [0m
      
      [33m INFO: Parameters muc=3.278, b=0.596, non_dec=0.309 
      	gave -log_like_val of -2247.467 [0m
      
      [33m INFO: Parameters muc=3.1, b=0.533, non_dec=0.304 
      	gave -log_like_val of -2172.041 [0m
      
      [33m INFO: Parameters muc=3.181, b=0.642, non_dec=0.299 
      	gave -log_like_val of -2258.892 [0m
      
      [33m INFO: Parameters muc=3.116, b=0.596, non_dec=0.314 
      	gave -log_like_val of -2218.414 [0m
      
      [33m INFO: Parameters muc=3.17, b=0.622, non_dec=0.297 
      	gave -log_like_val of -2262.479 [0m
      
      [33m INFO: Parameters muc=2.959, b=0.645, non_dec=0.288 
      	gave -log_like_val of -2239.973 [0m
      
      [33m INFO: Parameters muc=3.197, b=0.609, non_dec=0.304 
      	gave -log_like_val of -2263.498 [0m
      
      [33m INFO: Parameters muc=3.062, b=0.576, non_dec=0.302 
      	gave -log_like_val of -2259.841 [0m
      
      [33m INFO: Parameters muc=3.092, b=0.593, non_dec=0.301 
      	gave -log_like_val of -2268.548 [0m
      
      [33m INFO: Parameters muc=3.022, b=0.579, non_dec=0.306 
      	gave -log_like_val of -2265.867 [0m
      
      [33m INFO: Parameters muc=2.885, b=0.572, non_dec=0.301 
      	gave -log_like_val of -2265.921 [0m
      
      [33m INFO: Parameters muc=2.96, b=0.598, non_dec=0.295 
      	gave -log_like_val of -2264.283 [0m
      
      [33m INFO: Parameters muc=3.007, b=0.584, non_dec=0.304 
      	gave -log_like_val of -2269.343 [0m
      
      [33m INFO: Parameters muc=3.186, b=0.612, non_dec=0.302 
      	gave -log_like_val of -2264.028 [0m
      
      [33m INFO: Parameters muc=2.958, b=0.582, non_dec=0.301 
      	gave -log_like_val of -2270.162 [0m
      
      [33m INFO: Parameters muc=2.887, b=0.584, non_dec=0.302 
      	gave -log_like_val of -2270.31 [0m
      
      [33m INFO: Parameters muc=2.89, b=0.594, non_dec=0.299 
      	gave -log_like_val of -2262.698 [0m
      
      [33m INFO: Parameters muc=2.977, b=0.586, non_dec=0.302 
      	gave -log_like_val of -2271.132 [0m
      
      [33m INFO: Parameters muc=2.951, b=0.598, non_dec=0.302 
      	gave -log_like_val of -2270.928 [0m
      
      [33m INFO: Parameters muc=3.067, b=0.605, non_dec=0.301 
      	gave -log_like_val of -2269.968 [0m
      
      [33m INFO: Parameters muc=2.931, b=0.59, non_dec=0.302 
      	gave -log_like_val of -2271.586 [0m
      
      [33m INFO: Parameters muc=2.988, b=0.586, non_dec=0.301 
      	gave -log_like_val of -2270.77 [0m
      
      [33m INFO: Parameters muc=2.96, b=0.595, non_dec=0.301 
      	gave -log_like_val of -2271.74 [0m
      
      [33m INFO: Parameters muc=2.95, b=0.603, non_dec=0.3 
      	gave -log_like_val of -2266.327 [0m
      
      [33m INFO: Parameters muc=2.971, b=0.591, non_dec=0.302 
      	gave -log_like_val of -2271.931 [0m
      
      [33m INFO: Parameters muc=2.909, b=0.583, non_dec=0.303 
      	gave -log_like_val of -2270.602 [0m
      
      [33m INFO: Parameters muc=2.977, b=0.596, non_dec=0.301 
      	gave -log_like_val of -2271.847 [0m
      
      [33m INFO: Parameters muc=3.007, b=0.598, non_dec=0.301 
      	gave -log_like_val of -2271.7 [0m
      
      [33m INFO: Parameters muc=2.988, b=0.596, non_dec=0.301 
      	gave -log_like_val of -2271.906 [0m
      
      [33m INFO: Parameters muc=2.997, b=0.593, non_dec=0.301 
      	gave -log_like_val of -2271.943 [0m
      
      [33m INFO: Parameters muc=3.015, b=0.592, non_dec=0.301 
      	gave -log_like_val of -2271.584 [0m
      
      [33m INFO: Parameters muc=2.994, b=0.591, non_dec=0.302 
      	gave -log_like_val of -2271.755 [0m
      
      [33m INFO: Parameters muc=2.981, b=0.595, non_dec=0.301 
      	gave -log_like_val of -2271.982 [0m
      
      [33m INFO: Parameters muc=2.977, b=0.59, non_dec=0.301 
      	gave -log_like_val of -2271.799 [0m
      
      [33m INFO: Parameters muc=2.986, b=0.594, non_dec=0.301 
      	gave -log_like_val of -2271.997 [0m
      
      [33m INFO: Parameters muc=3.005, b=0.598, non_dec=0.3 
      	gave -log_like_val of -2271.764 [0m
      
      [33m INFO: Parameters muc=2.979, b=0.592, non_dec=0.301 
      	gave -log_like_val of -2272.007 [0m
      
      [33m INFO: Parameters muc=2.967, b=0.594, non_dec=0.301 
      	gave -log_like_val of -2271.907 [0m
      
      [33m INFO: Parameters muc=2.989, b=0.594, non_dec=0.301 
      	gave -log_like_val of -2272 [0m
      
      [33m INFO: Parameters muc=2.988, b=0.592, non_dec=0.301 
      	gave -log_like_val of -2271.962 [0m
      
      [33m INFO: Parameters muc=2.983, b=0.594, non_dec=0.301 
      	gave -log_like_val of -2272.008 [0m
      
      [33m INFO: Parameters muc=2.982, b=0.592, non_dec=0.301 
      	gave -log_like_val of -2271.993 [0m
      
      [33m INFO: Parameters muc=2.985, b=0.594, non_dec=0.301 
      	gave -log_like_val of -2272.011 [0m
      
      [33m INFO: Parameters muc=2.975, b=0.593, non_dec=0.301 
      	gave -log_like_val of -2272.01 [0m
      
      [33m INFO: Parameters muc=2.983, b=0.595, non_dec=0.301 
      	gave -log_like_val of -2271.958 [0m
      
      [33m INFO: Parameters muc=2.98, b=0.593, non_dec=0.301 
      	gave -log_like_val of -2272.018 [0m
      
      [33m INFO: Parameters muc=2.977, b=0.593, non_dec=0.301 
      	gave -log_like_val of -2272.016 [0m
      
      [33m INFO: Parameters muc=2.986, b=0.593, non_dec=0.301 
      	gave -log_like_val of -2272.007 [0m
      
      [33m INFO: Parameters muc=2.978, b=0.593, non_dec=0.301 
      	gave -log_like_val of -2272.016 [0m
      
      [33m INFO: Parameters muc=2.972, b=0.592, non_dec=0.302 
      	gave -log_like_val of -2272.003 [0m
      
      [33m INFO: Parameters muc=2.982, b=0.593, non_dec=0.301 
      	gave -log_like_val of -2272.017 [0m
      
      [33m INFO: Parameters muc=2.983, b=0.594, non_dec=0.301 
      	gave -log_like_val of -2272.014 [0m
      
      [33m INFO: Parameters muc=2.978, b=0.593, non_dec=0.301 
      	gave -log_like_val of -2272.018 [0m
      
      [33m INFO: Parameters muc=2.982, b=0.593, non_dec=0.301 
      	gave -log_like_val of -2272.016 [0m
      
      [33m INFO: Parameters muc=2.979, b=0.593, non_dec=0.301 
      	gave -log_like_val of -2272.018 [0m
      
      [33m INFO: Parameters muc=2.977, b=0.593, non_dec=0.301 
      	gave -log_like_val of -2272.015 [0m
      
      [33m INFO: Parameters muc=2.98, b=0.593, non_dec=0.301 
      	gave -log_like_val of -2272.018 [0m
      
      [33m INFO: Parameters muc=2.98, b=0.593, non_dec=0.301 
      	gave -log_like_val of -2272.017 [0m
      
      [33m INFO: Parameters muc=2.979, b=0.593, non_dec=0.301 
      	gave -log_like_val of -2272.018 [0m
      
      [33m INFO: Parameters muc=2.981, b=0.593, non_dec=0.301 
      	gave -log_like_val of -2272.018 [0m
      
      [33m INFO: Parameters muc=2.979, b=0.593, non_dec=0.301 
      	gave -log_like_val of -2272.018 [0m
      
      [33m INFO: Parameters muc=2.979, b=0.593, non_dec=0.301 
      	gave -log_like_val of -2272.017 [0m
      
      [33m INFO: Parameters muc=2.98, b=0.593, non_dec=0.301 
      	gave -log_like_val of -2272.018 [0m
      
      [33m INFO: Parameters muc=2.979, b=0.593, non_dec=0.301 
      	gave -log_like_val of -2272.017 [0m
      
      [33m INFO: Parameters muc=2.98, b=0.593, non_dec=0.301 
      	gave -log_like_val of -2272.018 [0m
      
      [33m INFO: Parameters muc=2.98, b=0.593, non_dec=0.301 
      	gave -log_like_val of -2272.018 [0m
      
      [33m INFO: Parameters muc=2.981, b=0.593, non_dec=0.301 
      	gave -log_like_val of -2272.018 [0m
      
      [33m INFO: Parameters muc=2.98, b=0.593, non_dec=0.301 
      	gave -log_like_val of -2272.018 [0m
      
      [33m INFO: Parameters muc=2.98, b=0.593, non_dec=0.301 
      	gave -log_like_val of -2272.018 [0m
      
      [33m INFO: Parameters muc=2.98, b=0.593, non_dec=0.301 
      	gave -log_like_val of -2272.018 [0m
      
      [33m INFO: Parameters muc=2.98, b=0.593, non_dec=0.301 
      	gave -log_like_val of -2272.018 [0m
      
      [33m INFO: Parameters muc=2.98, b=0.593, non_dec=0.301 
      	gave -log_like_val of -2272.018 [0m
      
      [33m INFO: Parameters muc=2.98, b=0.593, non_dec=0.301 
      	gave -log_like_val of -2272.018 [0m
      
      [33m INFO: Parameters muc=2.98, b=0.593, non_dec=0.301 
      	gave -log_like_val of -2272.018 [0m
      
      [33m INFO: Parameters muc=2.979, b=0.593, non_dec=0.301 
      	gave -log_like_val of -2272.018 [0m
      
      [33m INFO: Parameters muc=2.98, b=0.593, non_dec=0.301 
      	gave -log_like_val of -2272.018 [0m
      
      [33m INFO: Parameters muc=2.98, b=0.593, non_dec=0.301 
      	gave -log_like_val of -2272.018 [0m
      
      [33m INFO: Parameters muc=2.98, b=0.593, non_dec=0.301 
      	gave -log_like_val of -2272.018 [0m
      
      [33m INFO: Parameters muc=2.98, b=0.593, non_dec=0.301 
      	gave -log_like_val of -2272.018 [0m
      
      [33m INFO: Parameters muc=2.98, b=0.593, non_dec=0.301 
      	gave -log_like_val of -2272.018 [0m
      
      [33m INFO: Parameters muc=2.98, b=0.593, non_dec=0.301 
      	gave -log_like_val of -2272.018 [0m
      
      [33m INFO: Parameters muc=2.98, b=0.593, non_dec=0.301 
      	gave -log_like_val of -2272.018 [0m
      
      [33m INFO: Parameters muc=2.98, b=0.593, non_dec=0.301 
      	gave -log_like_val of -2272.018 [0m
      
      [33m INFO: Parameters muc=2.98, b=0.593, non_dec=0.301 
      	gave -log_like_val of -2272.018 [0m
      
      [33m INFO: Parameters muc=2.98, b=0.593, non_dec=0.301 
      	gave -log_like_val of -2272.018 [0m
      
      [33m INFO: Parameters muc=2.98, b=0.593, non_dec=0.301 
      	gave -log_like_val of -2272.018 [0m
      
      [33m INFO: Parameters muc=2.98, b=0.593, non_dec=0.301 
      	gave -log_like_val of -2272.018 [0m
      
      [33m INFO: Parameters muc=2.98, b=0.593, non_dec=0.301 
      	gave -log_like_val of -2272.018 [0m
      
      [33m INFO: Parameters muc=2.98, b=0.593, non_dec=0.301 
      	gave -log_like_val of -2272.018 [0m
      
      [33m INFO: Parameters muc=2.98, b=0.593, non_dec=0.301 
      	gave -log_like_val of -2272.018 [0m
      
      [33m INFO: Estimation gave muc=2.98, b=0.593, non_dec=0.301 
      	gave -log_like_val of -2272.018 [0m
      Class(es): ratcliff_dm_simple, drift_dm
      
      Model Parameters:
        values: muc=2.98, b=0.593, non_dec=0.301
        free: muc, b, non_dec
      
      Conditions: null
      
      Discretization:
        solver: kfe
        values: sigma=1, t_max=3, dt=0.005, dx=0.05, nt=600, nx=40

