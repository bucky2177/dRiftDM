# snapshot of the model running through nmkb

    Code
      estimate_model(a_model, lower = c(1, 0.2, 0.1), upper = c(7, 0.8, 0.6),
      use_de_optim = F, use_nmkb = T, verbose = 2)
    Output
      INFO: Running bounded Nelder-Mead
      [33mINFO: Parameters muc=2, b=0.5, non_dec=0.2 
      	gave -log_like_val of -267.7863 [0m
      [33mINFO: Parameters muc=4.582, b=0.574, non_dec=0.246 
      	gave -log_like_val of 412.1934 [0m
      [33mINFO: Parameters muc=2.489, b=0.729, non_dec=0.246 
      	gave -log_like_val of -1952.079 [0m
      [33mINFO: Parameters muc=2.489, b=0.574, non_dec=0.425 
      	gave -log_like_val of 20666.45 [0m
      [33mINFO: Parameters muc=3.349, b=0.658, non_dec=0.131 
      	gave -log_like_val of 1061.246 [0m
      [33mINFO: Parameters muc=3.115, b=0.639, non_dec=0.166 
      	gave -log_like_val of 46.3558 [0m
      [33mINFO: Parameters muc=1.411, b=0.691, non_dec=0.166 
      	gave -log_like_val of -734.37 [0m
      [33mINFO: Parameters muc=1.3, b=0.675, non_dec=0.246 
      	gave -log_like_val of -1269.42 [0m
      [33mINFO: Parameters muc=1.333, b=0.777, non_dec=0.229 
      	gave -log_like_val of -1007.352 [0m
      [33mINFO: Parameters muc=1.728, b=0.768, non_dec=0.351 
      	gave -log_like_val of 4605.005 [0m
      [33mINFO: Parameters muc=1.475, b=0.718, non_dec=0.198 
      	gave -log_like_val of -1071.046 [0m
      [33mINFO: Parameters muc=2.091, b=0.534, non_dec=0.228 
      	gave -log_like_val of -1040.535 [0m
      [33mINFO: Parameters muc=1.825, b=0.636, non_dec=0.228 
      	gave -log_like_val of -1454.325 [0m
      [33mINFO: Parameters muc=2.119, b=0.642, non_dec=0.291 
      	gave -log_like_val of -2016.361 [0m
      [33mINFO: Parameters muc=2.633, b=0.591, non_dec=0.349 
      	gave -log_like_val of 860.1302 [0m
      [33mINFO: Parameters muc=3.998, b=0.676, non_dec=0.262 
      	gave -log_like_val of -1595.611 [0m
      [33mINFO: Parameters muc=4.171, b=0.725, non_dec=0.309 
      	gave -log_like_val of -1949.925 [0m
      [33mINFO: Parameters muc=1.972, b=0.727, non_dec=0.301 
      	gave -log_like_val of -1308.073 [0m
      [33mINFO: Parameters muc=3.391, b=0.69, non_dec=0.272 
      	gave -log_like_val of -2137.438 [0m
      [33mINFO: Parameters muc=1.649, b=0.648, non_dec=0.233 
      	gave -log_like_val of -1442.916 [0m
      [33mINFO: Parameters muc=3.348, b=0.709, non_dec=0.289 
      	gave -log_like_val of -2242.657 [0m
      [33mINFO: Parameters muc=3.346, b=0.618, non_dec=0.325 
      	gave -log_like_val of -1575.16 [0m
      [33mINFO: Parameters muc=2.683, b=0.708, non_dec=0.264 
      	gave -log_like_val of -2096.765 [0m
      [33mINFO: Parameters muc=4.411, b=0.743, non_dec=0.259 
      	gave -log_like_val of -1422.325 [0m
      [33mINFO: Parameters muc=2.571, b=0.675, non_dec=0.283 
      	gave -log_like_val of -2168.145 [0m
      [33mINFO: Parameters muc=3.527, b=0.675, non_dec=0.298 
      	gave -log_like_val of -2239.925 [0m
      [33mINFO: Parameters muc=2.879, b=0.684, non_dec=0.309 
      	gave -log_like_val of -2014.884 [0m
      [33mINFO: Parameters muc=3.258, b=0.689, non_dec=0.281 
      	gave -log_like_val of -2240.242 [0m
      [33mINFO: Parameters muc=4.288, b=0.706, non_dec=0.295 
      	gave -log_like_val of -2003.141 [0m
      [33mINFO: Parameters muc=2.953, b=0.684, non_dec=0.286 
      	gave -log_like_val of -2244.389 [0m
      [33mINFO: Parameters muc=2.861, b=0.712, non_dec=0.272 
      	gave -log_like_val of -2179.291 [0m
      [33mINFO: Parameters muc=3.353, b=0.685, non_dec=0.292 
      	gave -log_like_val of -2257.537 [0m
      [33mINFO: Parameters muc=3.171, b=0.697, non_dec=0.297 
      	gave -log_like_val of -2204.999 [0m
      [33mINFO: Parameters muc=3.236, b=0.691, non_dec=0.285 
      	gave -log_like_val of -2242.103 [0m
      [33mINFO: Parameters muc=3.193, b=0.695, non_dec=0.293 
      	gave -log_like_val of -2244.559 [0m
      [33mINFO: Parameters muc=2.986, b=0.663, non_dec=0.292 
      	gave -log_like_val of -2266.267 [0m
      [33mINFO: Parameters muc=2.815, b=0.635, non_dec=0.293 
      	gave -log_like_val of -2269.153 [0m
      [33mINFO: Parameters muc=3.282, b=0.662, non_dec=0.299 
      	gave -log_like_val of -2268.478 [0m
      [33mINFO: Parameters muc=3.096, b=0.621, non_dec=0.297 
      	gave -log_like_val of -2292.868 [0m
      [33mINFO: Parameters muc=3.048, b=0.575, non_dec=0.299 
      	gave -log_like_val of -2257.311 [0m
      [33mINFO: Parameters muc=2.786, b=0.586, non_dec=0.301 
      	gave -log_like_val of -2291.262 [0m
      [33mINFO: Parameters muc=2.548, b=0.558, non_dec=0.295 
      	gave -log_like_val of -2201.935 [0m
      [33mINFO: Parameters muc=3.085, b=0.64, non_dec=0.298 
      	gave -log_like_val of -2286.697 [0m
      [33mINFO: Parameters muc=3.164, b=0.597, non_dec=0.304 
      	gave -log_like_val of -2290.723 [0m
      [33mINFO: Parameters muc=2.94, b=0.559, non_dec=0.303 
      	gave -log_like_val of -2277.553 [0m
      [33mINFO: Parameters muc=3.048, b=0.621, non_dec=0.299 
      	gave -log_like_val of -2293.754 [0m
      [33mINFO: Parameters muc=2.793, b=0.622, non_dec=0.294 
      	gave -log_like_val of -2270.896 [0m
      [33mINFO: Parameters muc=3.068, b=0.603, non_dec=0.302 
      	gave -log_like_val of -2297.03 [0m
      [33mINFO: Parameters muc=3.375, b=0.643, non_dec=0.297 
      	gave -log_like_val of -2269.973 [0m
      [33mINFO: Parameters muc=2.925, b=0.601, non_dec=0.3 
      	gave -log_like_val of -2295.705 [0m
      [33mINFO: Parameters muc=2.933, b=0.595, non_dec=0.304 
      	gave -log_like_val of -2297.895 [0m
      [33mINFO: Parameters muc=2.854, b=0.581, non_dec=0.308 
      	gave -log_like_val of -2286.917 [0m
      [33mINFO: Parameters muc=2.903, b=0.577, non_dec=0.305 
      	gave -log_like_val of -2295.286 [0m
      [33mINFO: Parameters muc=2.939, b=0.589, non_dec=0.303 
      	gave -log_like_val of -2298.482 [0m
      [33mINFO: Parameters muc=3.034, b=0.591, non_dec=0.306 
      	gave -log_like_val of -2288.492 [0m
      [33mINFO: Parameters muc=2.952, b=0.598, non_dec=0.302 
      	gave -log_like_val of -2297.725 [0m
      [33mINFO: Parameters muc=2.819, b=0.585, non_dec=0.304 
      	gave -log_like_val of -2293.958 [0m
      [33mINFO: Parameters muc=3.004, b=0.599, non_dec=0.302 
      	gave -log_like_val of -2298.421 [0m
      [33mINFO: Parameters muc=2.965, b=0.59, non_dec=0.305 
      	gave -log_like_val of -2298.718 [0m
      [33mINFO: Parameters muc=2.971, b=0.586, non_dec=0.307 
      	gave -log_like_val of -2290.244 [0m
      [33mINFO: Parameters muc=3.006, b=0.59, non_dec=0.303 
      	gave -log_like_val of -2298.142 [0m
      [33mINFO: Parameters muc=2.987, b=0.591, non_dec=0.303 
      	gave -log_like_val of -2298.689 [0m
      [33mINFO: Parameters muc=2.923, b=0.581, non_dec=0.305 
      	gave -log_like_val of -2291.324 [0m
      [33mINFO: Parameters muc=2.984, b=0.594, non_dec=0.303 
      	gave -log_like_val of -2298.823 [0m
      [33mINFO: Parameters muc=3.019, b=0.595, non_dec=0.304 
      	gave -log_like_val of -2298.527 [0m
      [33mINFO: Parameters muc=2.999, b=0.593, non_dec=0.304 
      	gave -log_like_val of -2298.746 [0m
      [33mINFO: Parameters muc=2.977, b=0.594, non_dec=0.305 
      	gave -log_like_val of -2298.818 [0m
      [33mINFO: Parameters muc=3.009, b=0.598, non_dec=0.303 
      	gave -log_like_val of -2298.528 [0m
      [33mINFO: Parameters muc=2.976, b=0.592, non_dec=0.304 
      	gave -log_like_val of -2298.826 [0m
      [33mINFO: Parameters muc=2.959, b=0.594, non_dec=0.304 
      	gave -log_like_val of -2298.711 [0m
      [33mINFO: Parameters muc=2.989, b=0.593, non_dec=0.304 
      	gave -log_like_val of -2298.821 [0m
      [33mINFO: Parameters muc=2.988, b=0.592, non_dec=0.303 
      	gave -log_like_val of -2298.789 [0m
      [33mINFO: Parameters muc=2.98, b=0.594, non_dec=0.304 
      	gave -log_like_val of -2298.838 [0m
      [33mINFO: Parameters muc=2.971, b=0.593, non_dec=0.304 
      	gave -log_like_val of -2298.824 [0m
      [33mINFO: Parameters muc=2.967, b=0.592, non_dec=0.305 
      	gave -log_like_val of -2286.051 [0m
      [33mINFO: Parameters muc=2.98, b=0.594, non_dec=0.304 
      	gave -log_like_val of -2298.838 [0m
      [33mINFO: Parameters muc=2.986, b=0.593, non_dec=0.304 
      	gave -log_like_val of -2298.826 [0m
      [33mINFO: Parameters muc=2.982, b=0.593, non_dec=0.304 
      	gave -log_like_val of -2298.839 [0m
      [33mINFO: Parameters muc=2.986, b=0.595, non_dec=0.304 
      	gave -log_like_val of -2298.797 [0m
      [33mINFO: Parameters muc=2.978, b=0.593, non_dec=0.304 
      	gave -log_like_val of -2298.841 [0m
      [33mINFO: Parameters muc=2.98, b=0.593, non_dec=0.304 
      	gave -log_like_val of -2298.836 [0m
      [33mINFO: Parameters muc=2.98, b=0.593, non_dec=0.304 
      	gave -log_like_val of -2298.842 [0m
      [33mINFO: Parameters muc=2.981, b=0.593, non_dec=0.305 
      	gave -log_like_val of -2298.832 [0m
      [33mINFO: Parameters muc=2.98, b=0.593, non_dec=0.304 
      	gave -log_like_val of -2298.842 [0m
      [33mINFO: Parameters muc=2.977, b=0.593, non_dec=0.304 
      	gave -log_like_val of -2298.84 [0m
      [33mINFO: Parameters muc=2.978, b=0.593, non_dec=0.304 
      	gave -log_like_val of -2298.843 [0m
      [33mINFO: Parameters muc=2.981, b=0.594, non_dec=0.304 
      	gave -log_like_val of -2298.832 [0m
      [33mINFO: Parameters muc=2.979, b=0.593, non_dec=0.304 
      	gave -log_like_val of -2298.843 [0m
      [33mINFO: Parameters muc=2.978, b=0.593, non_dec=0.304 
      	gave -log_like_val of -2298.843 [0m
      [33mINFO: Parameters muc=2.976, b=0.593, non_dec=0.304 
      	gave -log_like_val of -2298.842 [0m
      [33mINFO: Parameters muc=2.979, b=0.593, non_dec=0.304 
      	gave -log_like_val of -2298.843 [0m
      [33mINFO: Parameters muc=2.979, b=0.593, non_dec=0.304 
      	gave -log_like_val of -2298.843 [0m
      [33mINFO: Parameters muc=2.979, b=0.593, non_dec=0.304 
      	gave -log_like_val of -2298.843 [0m
      [33mINFO: Parameters muc=2.978, b=0.593, non_dec=0.304 
      	gave -log_like_val of -2298.842 [0m
      [33mINFO: Parameters muc=2.979, b=0.593, non_dec=0.304 
      	gave -log_like_val of -2298.843 [0m
      [33mINFO: Parameters muc=2.978, b=0.593, non_dec=0.304 
      	gave -log_like_val of -2298.843 [0m
      [33mINFO: Parameters muc=2.979, b=0.593, non_dec=0.304 
      	gave -log_like_val of -2298.843 [0m
      [33mINFO: Parameters muc=2.978, b=0.593, non_dec=0.304 
      	gave -log_like_val of -2298.843 [0m
      [33mINFO: Parameters muc=2.979, b=0.593, non_dec=0.304 
      	gave -log_like_val of -2298.843 [0m
      [33mINFO: Parameters muc=2.978, b=0.593, non_dec=0.304 
      	gave -log_like_val of -2298.843 [0m
      [33mINFO: Parameters muc=2.978, b=0.593, non_dec=0.304 
      	gave -log_like_val of -2298.843 [0m
      [33mINFO: Parameters muc=2.979, b=0.593, non_dec=0.304 
      	gave -log_like_val of -2298.843 [0m
      [33mINFO: Parameters muc=2.979, b=0.593, non_dec=0.304 
      	gave -log_like_val of -2298.843 [0m
      [33mINFO: Parameters muc=2.978, b=0.593, non_dec=0.304 
      	gave -log_like_val of -2298.843 [0m
      [33mINFO: Parameters muc=2.978, b=0.593, non_dec=0.304 
      	gave -log_like_val of -2298.843 [0m
      [33mINFO: Parameters muc=2.979, b=0.593, non_dec=0.304 
      	gave -log_like_val of -2298.843 [0m
      [33mINFO: Parameters muc=2.979, b=0.593, non_dec=0.304 
      	gave -log_like_val of -2298.843 [0m
      [33mINFO: Parameters muc=2.978, b=0.593, non_dec=0.304 
      	gave -log_like_val of -2298.843 [0m
      [33mINFO: Parameters muc=2.978, b=0.593, non_dec=0.304 
      	gave -log_like_val of -2298.843 [0m
      [33mINFO: Parameters muc=2.979, b=0.593, non_dec=0.304 
      	gave -log_like_val of -2298.843 [0m
      [33mINFO: Parameters muc=2.979, b=0.593, non_dec=0.304 
      	gave -log_like_val of -2298.843 [0m
      [33mINFO: Parameters muc=2.979, b=0.593, non_dec=0.304 
      	gave -log_like_val of -2298.843 [0m
      [33mINFO: Parameters muc=2.979, b=0.593, non_dec=0.304 
      	gave -log_like_val of -2298.843 [0m
      [33mINFO: Parameters muc=2.978, b=0.593, non_dec=0.304 
      	gave -log_like_val of -2298.843 [0m
      [33mINFO: Parameters muc=2.979, b=0.593, non_dec=0.304 
      	gave -log_like_val of -2298.843 [0m
      [33mINFO: Parameters muc=2.979, b=0.593, non_dec=0.304 
      	gave -log_like_val of -2298.843 [0m
      [33mINFO: Parameters muc=2.979, b=0.593, non_dec=0.304 
      	gave -log_like_val of -2298.843 [0m
      [33mINFO: Parameters muc=2.979, b=0.593, non_dec=0.304 
      	gave -log_like_val of -2298.843 [0m
      [33mINFO: Parameters muc=2.979, b=0.593, non_dec=0.304 
      	gave -log_like_val of -2298.843 [0m
      [33mINFO: Estimation gave muc=2.979, b=0.593, non_dec=0.304 
      	with -log_like_val of -2298.843 [0m
      Class(es): ratcliff_dm, drift_dm
      
      Model Parameters:
        values: muc=2.979, b=0.593, non_dec=0.304
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
      Iteration: 1 bestvalit: -577.124234 bestmemit:    4.147303    0.314631
      Iteration: 2 bestvalit: -577.124234 bestmemit:    4.147303    0.314631
      Iteration: 3 bestvalit: -577.124234 bestmemit:    4.147303    0.314631
      Iteration: 4 bestvalit: -577.124234 bestmemit:    4.147303    0.314631
      Iteration: 5 bestvalit: -581.936340 bestmemit:    4.011332    0.309021
      Iteration: 6 bestvalit: -583.670601 bestmemit:    4.283513    0.308962
      Iteration: 7 bestvalit: -583.670601 bestmemit:    4.283513    0.308962
      Iteration: 8 bestvalit: -583.778577 bestmemit:    4.331571    0.307216
      Iteration: 9 bestvalit: -583.778577 bestmemit:    4.331571    0.307216
      Iteration: 10 bestvalit: -583.778577 bestmemit:    4.331571    0.307216
      Iteration: 11 bestvalit: -584.275269 bestmemit:    4.147303    0.306440
      Iteration: 12 bestvalit: -584.275269 bestmemit:    4.147303    0.306440
      Iteration: 13 bestvalit: -584.275269 bestmemit:    4.147303    0.306440
      Iteration: 14 bestvalit: -584.335860 bestmemit:    4.194618    0.305609
      Iteration: 15 bestvalit: -584.339587 bestmemit:    4.207138    0.305942
      Iteration: 16 bestvalit: -584.339587 bestmemit:    4.207138    0.305942
      Iteration: 17 bestvalit: -584.339587 bestmemit:    4.207138    0.305942
      Iteration: 18 bestvalit: -584.339587 bestmemit:    4.207138    0.305942
      Iteration: 19 bestvalit: -584.344794 bestmemit:    4.205634    0.306037
      Iteration: 20 bestvalit: -584.355217 bestmemit:    4.176338    0.306033
      Iteration: 21 bestvalit: -584.356014 bestmemit:    4.190461    0.306168
      Iteration: 22 bestvalit: -584.356014 bestmemit:    4.190461    0.306168
      Iteration: 23 bestvalit: -584.356014 bestmemit:    4.190461    0.306168
      Iteration: 24 bestvalit: -584.356101 bestmemit:    4.185410    0.306161
      Iteration: 25 bestvalit: -584.357806 bestmemit:    4.184155    0.306037
      Iteration: 26 bestvalit: -584.357806 bestmemit:    4.184155    0.306037
      Iteration: 27 bestvalit: -584.357806 bestmemit:    4.184155    0.306037
      Iteration: 28 bestvalit: -584.357806 bestmemit:    4.184155    0.306037
      Iteration: 29 bestvalit: -584.357882 bestmemit:    4.185241    0.306031
      Iteration: 30 bestvalit: -584.357882 bestmemit:    4.185241    0.306031
      Iteration: 31 bestvalit: -584.357882 bestmemit:    4.185241    0.306031
      Iteration: 32 bestvalit: -584.357882 bestmemit:    4.185241    0.306031
      Iteration: 33 bestvalit: -584.357883 bestmemit:    4.185241    0.305999
      Iteration: 34 bestvalit: -584.357900 bestmemit:    4.184557    0.306013
      Iteration: 35 bestvalit: -584.357900 bestmemit:    4.184557    0.306013
      Iteration: 36 bestvalit: -584.357900 bestmemit:    4.185280    0.306008
      Iteration: 37 bestvalit: -584.357900 bestmemit:    4.185280    0.306008
      Iteration: 38 bestvalit: -584.357904 bestmemit:    4.184696    0.306012
      Iteration: 39 bestvalit: -584.357906 bestmemit:    4.185064    0.306014
      Iteration: 40 bestvalit: -584.357906 bestmemit:    4.185064    0.306014
      Iteration: 41 bestvalit: -584.357906 bestmemit:    4.185064    0.306014
      Iteration: 42 bestvalit: -584.357906 bestmemit:    4.184963    0.306012
      Iteration: 43 bestvalit: -584.357906 bestmemit:    4.184963    0.306012
      Iteration: 44 bestvalit: -584.357906 bestmemit:    4.184942    0.306012
      Iteration: 45 bestvalit: -584.357906 bestmemit:    4.184977    0.306012
      Iteration: 46 bestvalit: -584.357906 bestmemit:    4.184982    0.306013
      Iteration: 47 bestvalit: -584.357906 bestmemit:    4.184982    0.306013
      Iteration: 48 bestvalit: -584.357906 bestmemit:    4.184982    0.306013
      Iteration: 49 bestvalit: -584.357906 bestmemit:    4.184982    0.306013
      Iteration: 50 bestvalit: -584.357906 bestmemit:    4.184999    0.306013
      Iteration: 51 bestvalit: -584.357906 bestmemit:    4.184999    0.306013
      Iteration: 52 bestvalit: -584.357906 bestmemit:    4.184999    0.306013
      Iteration: 53 bestvalit: -584.357906 bestmemit:    4.184999    0.306013
      Iteration: 54 bestvalit: -584.357906 bestmemit:    4.185002    0.306013
      Iteration: 55 bestvalit: -584.357906 bestmemit:    4.185002    0.306013
      Iteration: 56 bestvalit: -584.357906 bestmemit:    4.185002    0.306013
      Iteration: 57 bestvalit: -584.357906 bestmemit:    4.185002    0.306013
      Iteration: 58 bestvalit: -584.357906 bestmemit:    4.185002    0.306013
      Iteration: 59 bestvalit: -584.357906 bestmemit:    4.185002    0.306013
      Iteration: 60 bestvalit: -584.357906 bestmemit:    4.185000    0.306013
      Iteration: 61 bestvalit: -584.357906 bestmemit:    4.185000    0.306013
      Iteration: 62 bestvalit: -584.357906 bestmemit:    4.184999    0.306013
      Iteration: 63 bestvalit: -584.357906 bestmemit:    4.185001    0.306013
      Iteration: 64 bestvalit: -584.357906 bestmemit:    4.185001    0.306013
      Iteration: 65 bestvalit: -584.357906 bestmemit:    4.185001    0.306013
      Iteration: 66 bestvalit: -584.357906 bestmemit:    4.185000    0.306013
      Iteration: 67 bestvalit: -584.357906 bestmemit:    4.185000    0.306013
      Iteration: 68 bestvalit: -584.357906 bestmemit:    4.185000    0.306013
      Iteration: 69 bestvalit: -584.357906 bestmemit:    4.185000    0.306013
      Iteration: 70 bestvalit: -584.357906 bestmemit:    4.185000    0.306013
      Iteration: 71 bestvalit: -584.357906 bestmemit:    4.185000    0.306013
      Iteration: 72 bestvalit: -584.357906 bestmemit:    4.185000    0.306013
      Iteration: 73 bestvalit: -584.357906 bestmemit:    4.185000    0.306013
      Iteration: 74 bestvalit: -584.357906 bestmemit:    4.185000    0.306013
      Iteration: 75 bestvalit: -584.357906 bestmemit:    4.185000    0.306013
      Iteration: 76 bestvalit: -584.357906 bestmemit:    4.185000    0.306013
      Iteration: 77 bestvalit: -584.357906 bestmemit:    4.185000    0.306013
      Iteration: 78 bestvalit: -584.357906 bestmemit:    4.185000    0.306013
      Iteration: 79 bestvalit: -584.357906 bestmemit:    4.185000    0.306013
      Iteration: 80 bestvalit: -584.357906 bestmemit:    4.185000    0.306013
      Iteration: 81 bestvalit: -584.357906 bestmemit:    4.185000    0.306013
      Iteration: 82 bestvalit: -584.357906 bestmemit:    4.185000    0.306013
      Iteration: 83 bestvalit: -584.357906 bestmemit:    4.185000    0.306013
      Iteration: 84 bestvalit: -584.357906 bestmemit:    4.185000    0.306013
      Iteration: 85 bestvalit: -584.357906 bestmemit:    4.185000    0.306013
      Iteration: 86 bestvalit: -584.357906 bestmemit:    4.185000    0.306013
      Iteration: 87 bestvalit: -584.357906 bestmemit:    4.185000    0.306013
      Iteration: 88 bestvalit: -584.357906 bestmemit:    4.185000    0.306013
      Iteration: 89 bestvalit: -584.357906 bestmemit:    4.185000    0.306013
      Iteration: 90 bestvalit: -584.357906 bestmemit:    4.185000    0.306013
      [33mINFO: Estimation gave muc=4.185, b=0.6, non_dec=0.306, sd_non_dec=0.02, tau=0.04, a=2, A=0.1, alpha=4 
      	with -log_like_val of -584.3579 [0m
      Class(es): dmc_dm, drift_dm
      
      Model Parameters:
        values: muc=4.185, b=0.6, non_dec=0.306, sd_non_dec=0.02, tau=0.04, a=2, A=0.1, alpha=4
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
      Iteration: 1 bestvalit: -577.124234 bestmemit:    4.147303    0.314631
      Iteration: 2 bestvalit: -577.124234 bestmemit:    4.147303    0.314631
      Iteration: 3 bestvalit: -577.124234 bestmemit:    4.147303    0.314631
      Iteration: 4 bestvalit: -577.124234 bestmemit:    4.147303    0.314631
      Iteration: 5 bestvalit: -581.936340 bestmemit:    4.011332    0.309021
      Iteration: 6 bestvalit: -583.670601 bestmemit:    4.283513    0.308962
      Iteration: 7 bestvalit: -583.670601 bestmemit:    4.283513    0.308962
      Iteration: 8 bestvalit: -583.778577 bestmemit:    4.331571    0.307216
      Iteration: 9 bestvalit: -583.778577 bestmemit:    4.331571    0.307216
      Iteration: 10 bestvalit: -583.778577 bestmemit:    4.331571    0.307216
      Iteration: 11 bestvalit: -584.275269 bestmemit:    4.147303    0.306440
      Iteration: 12 bestvalit: -584.275269 bestmemit:    4.147303    0.306440
      Iteration: 13 bestvalit: -584.275269 bestmemit:    4.147303    0.306440
      Iteration: 14 bestvalit: -584.335860 bestmemit:    4.194618    0.305609
      Iteration: 15 bestvalit: -584.339587 bestmemit:    4.207138    0.305942
      Iteration: 16 bestvalit: -584.339587 bestmemit:    4.207138    0.305942
      Iteration: 17 bestvalit: -584.339587 bestmemit:    4.207138    0.305942
      Iteration: 18 bestvalit: -584.339587 bestmemit:    4.207138    0.305942
      Iteration: 19 bestvalit: -584.344794 bestmemit:    4.205634    0.306037
      Iteration: 20 bestvalit: -584.355217 bestmemit:    4.176338    0.306033
      Iteration: 21 bestvalit: -584.356014 bestmemit:    4.190461    0.306168
      Iteration: 22 bestvalit: -584.356014 bestmemit:    4.190461    0.306168
      Iteration: 23 bestvalit: -584.356014 bestmemit:    4.190461    0.306168
      Iteration: 24 bestvalit: -584.356101 bestmemit:    4.185410    0.306161
      Iteration: 25 bestvalit: -584.357806 bestmemit:    4.184155    0.306037
      Iteration: 26 bestvalit: -584.357806 bestmemit:    4.184155    0.306037
      Iteration: 27 bestvalit: -584.357806 bestmemit:    4.184155    0.306037
      Iteration: 28 bestvalit: -584.357806 bestmemit:    4.184155    0.306037
      Iteration: 29 bestvalit: -584.357882 bestmemit:    4.185241    0.306031
      Iteration: 30 bestvalit: -584.357882 bestmemit:    4.185241    0.306031
      Iteration: 31 bestvalit: -584.357882 bestmemit:    4.185241    0.306031
      Iteration: 32 bestvalit: -584.357882 bestmemit:    4.185241    0.306031
      Iteration: 33 bestvalit: -584.357883 bestmemit:    4.185241    0.305999
      Iteration: 34 bestvalit: -584.357900 bestmemit:    4.184557    0.306013
      Iteration: 35 bestvalit: -584.357900 bestmemit:    4.184557    0.306013
      Iteration: 36 bestvalit: -584.357900 bestmemit:    4.185280    0.306008
      Iteration: 37 bestvalit: -584.357900 bestmemit:    4.185280    0.306008
      Iteration: 38 bestvalit: -584.357904 bestmemit:    4.184696    0.306012
      Iteration: 39 bestvalit: -584.357906 bestmemit:    4.185064    0.306014
      Iteration: 40 bestvalit: -584.357906 bestmemit:    4.185064    0.306014
      Iteration: 41 bestvalit: -584.357906 bestmemit:    4.185064    0.306014
      Iteration: 42 bestvalit: -584.357906 bestmemit:    4.184963    0.306012
      Iteration: 43 bestvalit: -584.357906 bestmemit:    4.184963    0.306012
      Iteration: 44 bestvalit: -584.357906 bestmemit:    4.184942    0.306012
      Iteration: 45 bestvalit: -584.357906 bestmemit:    4.184977    0.306012
      Iteration: 46 bestvalit: -584.357906 bestmemit:    4.184982    0.306013
      Iteration: 47 bestvalit: -584.357906 bestmemit:    4.184982    0.306013
      Iteration: 48 bestvalit: -584.357906 bestmemit:    4.184982    0.306013
      Iteration: 49 bestvalit: -584.357906 bestmemit:    4.184982    0.306013
      Iteration: 50 bestvalit: -584.357906 bestmemit:    4.184999    0.306013
      Iteration: 51 bestvalit: -584.357906 bestmemit:    4.184999    0.306013
      Iteration: 52 bestvalit: -584.357906 bestmemit:    4.184999    0.306013
      Iteration: 53 bestvalit: -584.357906 bestmemit:    4.184999    0.306013
      Iteration: 54 bestvalit: -584.357906 bestmemit:    4.185002    0.306013
      Iteration: 55 bestvalit: -584.357906 bestmemit:    4.185002    0.306013
      Iteration: 56 bestvalit: -584.357906 bestmemit:    4.185002    0.306013
      Iteration: 57 bestvalit: -584.357906 bestmemit:    4.185002    0.306013
      Iteration: 58 bestvalit: -584.357906 bestmemit:    4.185002    0.306013
      Iteration: 59 bestvalit: -584.357906 bestmemit:    4.185002    0.306013
      Iteration: 60 bestvalit: -584.357906 bestmemit:    4.185000    0.306013
      Iteration: 61 bestvalit: -584.357906 bestmemit:    4.185000    0.306013
      Iteration: 62 bestvalit: -584.357906 bestmemit:    4.184999    0.306013
      Iteration: 63 bestvalit: -584.357906 bestmemit:    4.185001    0.306013
      Iteration: 64 bestvalit: -584.357906 bestmemit:    4.185001    0.306013
      Iteration: 65 bestvalit: -584.357906 bestmemit:    4.185001    0.306013
      Iteration: 66 bestvalit: -584.357906 bestmemit:    4.185000    0.306013
      Iteration: 67 bestvalit: -584.357906 bestmemit:    4.185000    0.306013
      Iteration: 68 bestvalit: -584.357906 bestmemit:    4.185000    0.306013
      Iteration: 69 bestvalit: -584.357906 bestmemit:    4.185000    0.306013
      Iteration: 70 bestvalit: -584.357906 bestmemit:    4.185000    0.306013
      Iteration: 71 bestvalit: -584.357906 bestmemit:    4.185000    0.306013
      Iteration: 72 bestvalit: -584.357906 bestmemit:    4.185000    0.306013
      Iteration: 73 bestvalit: -584.357906 bestmemit:    4.185000    0.306013
      Iteration: 74 bestvalit: -584.357906 bestmemit:    4.185000    0.306013
      Iteration: 75 bestvalit: -584.357906 bestmemit:    4.185000    0.306013
      Iteration: 76 bestvalit: -584.357906 bestmemit:    4.185000    0.306013
      Iteration: 77 bestvalit: -584.357906 bestmemit:    4.185000    0.306013
      Iteration: 78 bestvalit: -584.357906 bestmemit:    4.185000    0.306013
      Iteration: 79 bestvalit: -584.357906 bestmemit:    4.185000    0.306013
      Iteration: 80 bestvalit: -584.357906 bestmemit:    4.185000    0.306013
      Iteration: 81 bestvalit: -584.357906 bestmemit:    4.185000    0.306013
      Iteration: 82 bestvalit: -584.357906 bestmemit:    4.185000    0.306013
      Iteration: 83 bestvalit: -584.357906 bestmemit:    4.185000    0.306013
      Iteration: 84 bestvalit: -584.357906 bestmemit:    4.185000    0.306013
      Iteration: 85 bestvalit: -584.357906 bestmemit:    4.185000    0.306013
      Iteration: 86 bestvalit: -584.357906 bestmemit:    4.185000    0.306013
      Iteration: 87 bestvalit: -584.357906 bestmemit:    4.185000    0.306013
      Iteration: 88 bestvalit: -584.357906 bestmemit:    4.185000    0.306013
      Iteration: 89 bestvalit: -584.357906 bestmemit:    4.185000    0.306013
      Iteration: 90 bestvalit: -584.357906 bestmemit:    4.185000    0.306013
      [33mINFO: Estimation gave muc=4.185, b=0.6, non_dec=0.306, sd_non_dec=0.02, tau=0.04, a=2, A=0.1, alpha=4 
      	with -log_like_val of -584.3579 [0m
      Class(es): dmc_dm, drift_dm
      
      Model Parameters:
        values: muc=4.185, b=0.6, non_dec=0.306, sd_non_dec=0.02, tau=0.04, a=2, A=0.1, alpha=4
        free: muc, non_dec
      
      Conditions: comp, incomp
      
      Deriving PDFs:
        solver: kfe
        values: sigma=1, t_max=1, dt=0.01, dx=0.1, nt=100, nx=20
      
      Observed Data: 600 trials

