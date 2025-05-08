# snapshot of the model running through nmkb

    Code
      estimate_model(a_model, lower = c(1, 0.2, 0.1, 0.01), upper = c(7, 0.8, 0.6,
        0.1), use_de_optim = F, use_nmkb = T, verbose = 2)
    Output
      INFO: Running bounded Nelder-Mead
      [33mINFO: Parameters
      muc=>2
      b=>0.5
      non_dec=>0.2
      range_non_dec=>0.02
      ==> gave -log_like_val of -164.3852[0m
      [33mINFO: Parameters
      muc=>5.548
      b=>0.594
      non_dec=>0.262
      range_non_dec=>0.027
      ==> gave -log_like_val of 864.0064[0m
      [33mINFO: Parameters
      muc=>2.661
      b=>0.764
      non_dec=>0.262
      range_non_dec=>0.027
      ==> gave -log_like_val of -1314.838[0m
      [33mINFO: Parameters
      muc=>2.661
      b=>0.594
      non_dec=>0.498
      range_non_dec=>0.027
      ==> gave -log_like_val of 28424.39[0m
      [33mINFO: Parameters
      muc=>2.661
      b=>0.594
      non_dec=>0.262
      range_non_dec=>0.07
      ==> gave -log_like_val of -1242.827[0m
      [33mINFO: Parameters
      muc=>3.651
      b=>0.679
      non_dec=>0.12
      range_non_dec=>0.04
      ==> gave -log_like_val of 1098.657[0m
      [33mINFO: Parameters
      muc=>3.386
      b=>0.66
      non_dec=>0.158
      range_non_dec=>0.036
      ==> gave -log_like_val of 285.8494[0m
      [33mINFO: Parameters
      muc=>1.255
      b=>0.703
      non_dec=>0.177
      range_non_dec=>0.045
      ==> gave -log_like_val of -475.5279[0m
      [33mINFO: Parameters
      muc=>1.311
      b=>0.675
      non_dec=>0.319
      range_non_dec=>0.039
      ==> gave -log_like_val of -182.556[0m
      [33mINFO: Parameters
      muc=>1.517
      b=>0.776
      non_dec=>0.309
      range_non_dec=>0.078
      ==> gave -log_like_val of -27.252[0m
      [33mINFO: Parameters
      muc=>1.853
      b=>0.614
      non_dec=>0.223
      range_non_dec=>0.03
      ==> gave -log_like_val of -899.9223[0m
      [33mINFO: Parameters
      muc=>3.252
      b=>0.699
      non_dec=>0.165
      range_non_dec=>0.044
      ==> gave -log_like_val of -147.3469[0m
      [33mINFO: Parameters
      muc=>1.543
      b=>0.681
      non_dec=>0.27
      range_non_dec=>0.04
      ==> gave -log_like_val of -973.0755[0m
      [33mINFO: Parameters
      muc=>4.153
      b=>0.656
      non_dec=>0.36
      range_non_dec=>0.035
      ==> gave -log_like_val of 2052.588[0m
      [33mINFO: Parameters
      muc=>1.542
      b=>0.693
      non_dec=>0.21
      range_non_dec=>0.043
      ==> gave -log_like_val of -801.0696[0m
      [33mINFO: Parameters
      muc=>2.988
      b=>0.669
      non_dec=>0.304
      range_non_dec=>0.038
      ==> gave -log_like_val of -1302.029[0m
      [33mINFO: Parameters
      muc=>3.035
      b=>0.742
      non_dec=>0.334
      range_non_dec=>0.059
      ==> gave -log_like_val of -349.4586[0m
      [33mINFO: Parameters
      muc=>2.081
      b=>0.657
      non_dec=>0.247
      range_non_dec=>0.036
      ==> gave -log_like_val of -1163.615[0m
      [33mINFO: Parameters
      muc=>4.357
      b=>0.692
      non_dec=>0.267
      range_non_dec=>0.043
      ==> gave -log_like_val of -956.0691[0m
      [33mINFO: Parameters
      muc=>1.95
      b=>0.684
      non_dec=>0.269
      range_non_dec=>0.041
      ==> gave -log_like_val of -1177.108[0m
      [33mINFO: Parameters
      muc=>3.083
      b=>0.721
      non_dec=>0.303
      range_non_dec=>0.051
      ==> gave -log_like_val of -1242.76[0m
      [33mINFO: Parameters
      muc=>4.066
      b=>0.719
      non_dec=>0.296
      range_non_dec=>0.05
      ==> gave -log_like_val of -1289.357[0m
      [33mINFO: Parameters
      muc=>3.048
      b=>0.679
      non_dec=>0.259
      range_non_dec=>0.04
      ==> gave -log_like_val of -1311.729[0m
      [33mINFO: Parameters
      muc=>3.734
      b=>0.771
      non_dec=>0.299
      range_non_dec=>0.019
      ==> gave -log_like_val of -1204.084[0m
      [33mINFO: Parameters
      muc=>2.906
      b=>0.664
      non_dec=>0.271
      range_non_dec=>0.054
      ==> gave -log_like_val of -1380.069[0m
      [33mINFO: Parameters
      muc=>2.02
      b=>0.687
      non_dec=>0.253
      range_non_dec=>0.03
      ==> gave -log_like_val of -1182.164[0m
      [33mINFO: Parameters
      muc=>3.461
      b=>0.712
      non_dec=>0.285
      range_non_dec=>0.044
      ==> gave -log_like_val of -1400.339[0m
      [33mINFO: Parameters
      muc=>3.03
      b=>0.744
      non_dec=>0.237
      range_non_dec=>0.043
      ==> gave -log_like_val of -1242.149[0m
      [33mINFO: Parameters
      muc=>2.999
      b=>0.693
      non_dec=>0.286
      range_non_dec=>0.039
      ==> gave -log_like_val of -1403.094[0m
      [33mINFO: Parameters
      muc=>2.947
      b=>0.743
      non_dec=>0.293
      range_non_dec=>0.041
      ==> gave -log_like_val of -1265.217[0m
      [33mINFO: Parameters
      muc=>3.022
      b=>0.699
      non_dec=>0.268
      range_non_dec=>0.04
      ==> gave -log_like_val of -1388.468[0m
      [33mINFO: Parameters
      muc=>3.569
      b=>0.545
      non_dec=>0.293
      range_non_dec=>0.065
      ==> gave -log_like_val of -1240.068[0m
      [33mINFO: Parameters
      muc=>2.87
      b=>0.737
      non_dec=>0.269
      range_non_dec=>0.035
      ==> gave -log_like_val of -1375.531[0m
      [33mINFO: Parameters
      muc=>3.326
      b=>0.628
      non_dec=>0.285
      range_non_dec=>0.054
      ==> gave -log_like_val of -1388.547[0m
      [33mINFO: Parameters
      muc=>3.508
      b=>0.705
      non_dec=>0.291
      range_non_dec=>0.036
      ==> gave -log_like_val of -1391.162[0m
      [33mINFO: Parameters
      muc=>3.632
      b=>0.675
      non_dec=>0.307
      range_non_dec=>0.046
      ==> gave -log_like_val of -1321.55[0m
      [33mINFO: Parameters
      muc=>3.169
      b=>0.693
      non_dec=>0.277
      range_non_dec=>0.042
      ==> gave -log_like_val of -1414.502[0m
      [33mINFO: Parameters
      muc=>3.235
      b=>0.747
      non_dec=>0.284
      range_non_dec=>0.029
      ==> gave -log_like_val of -1362.061[0m
      [33mINFO: Parameters
      muc=>3.303
      b=>0.668
      non_dec=>0.285
      range_non_dec=>0.047
      ==> gave -log_like_val of -1418.219[0m
      [33mINFO: Parameters
      muc=>2.966
      b=>0.679
      non_dec=>0.275
      range_non_dec=>0.051
      ==> gave -log_like_val of -1411.927[0m
      [33mINFO: Parameters
      muc=>2.779
      b=>0.649
      non_dec=>0.277
      range_non_dec=>0.045
      ==> gave -log_like_val of -1398.179[0m
      [33mINFO: Parameters
      muc=>3.281
      b=>0.698
      non_dec=>0.283
      range_non_dec=>0.044
      ==> gave -log_like_val of -1414.257[0m
      [33mINFO: Parameters
      muc=>3.364
      b=>0.677
      non_dec=>0.274
      range_non_dec=>0.053
      ==> gave -log_like_val of -1374.478[0m
      [33mINFO: Parameters
      muc=>3.087
      b=>0.689
      non_dec=>0.283
      range_non_dec=>0.042
      ==> gave -log_like_val of -1420.282[0m
      [33mINFO: Parameters
      muc=>3.465
      b=>0.696
      non_dec=>0.289
      range_non_dec=>0.037
      ==> gave -log_like_val of -1404.116[0m
      [33mINFO: Parameters
      muc=>3.086
      b=>0.683
      non_dec=>0.279
      range_non_dec=>0.047
      ==> gave -log_like_val of -1419.249[0m
      [33mINFO: Parameters
      muc=>3.043
      b=>0.667
      non_dec=>0.279
      range_non_dec=>0.045
      ==> gave -log_like_val of -1420.293[0m
      [33mINFO: Parameters
      muc=>2.928
      b=>0.65
      non_dec=>0.277
      range_non_dec=>0.045
      ==> gave -log_like_val of -1399.415[0m
      [33mINFO: Parameters
      muc=>3.089
      b=>0.66
      non_dec=>0.286
      range_non_dec=>0.049
      ==> gave -log_like_val of -1429.269[0m
      [33mINFO: Parameters
      muc=>3.05
      b=>0.64
      non_dec=>0.291
      range_non_dec=>0.053
      ==> gave -log_like_val of -1432.866[0m
      [33mINFO: Parameters
      muc=>2.842
      b=>0.674
      non_dec=>0.281
      range_non_dec=>0.047
      ==> gave -log_like_val of -1411.692[0m
      [33mINFO: Parameters
      muc=>3.183
      b=>0.67
      non_dec=>0.284
      range_non_dec=>0.047
      ==> gave -log_like_val of -1425.433[0m
      [33mINFO: Parameters
      muc=>3.095
      b=>0.65
      non_dec=>0.29
      range_non_dec=>0.046
      ==> gave -log_like_val of -1432.986[0m
      [33mINFO: Parameters
      muc=>3.099
      b=>0.631
      non_dec=>0.295
      range_non_dec=>0.046
      ==> gave -log_like_val of -1435.385[0m
      [33mINFO: Parameters
      muc=>3.1
      b=>0.609
      non_dec=>0.291
      range_non_dec=>0.053
      ==> gave -log_like_val of -1421.935[0m
      [33mINFO: Parameters
      muc=>3.174
      b=>0.606
      non_dec=>0.302
      range_non_dec=>0.055
      ==> gave -log_like_val of -1433.41[0m
      [33mINFO: Parameters
      muc=>3.153
      b=>0.663
      non_dec=>0.294
      range_non_dec=>0.047
      ==> gave -log_like_val of -1416.937[0m
      [33mINFO: Parameters
      muc=>3.113
      b=>0.624
      non_dec=>0.292
      range_non_dec=>0.052
      ==> gave -log_like_val of -1434.715[0m
      [33mINFO: Parameters
      muc=>3.035
      b=>0.574
      non_dec=>0.306
      range_non_dec=>0.056
      ==> gave -log_like_val of -1434.456[0m
      [33mINFO: Parameters
      muc=>3.161
      b=>0.575
      non_dec=>0.307
      range_non_dec=>0.051
      ==> gave -log_like_val of -1422.799[0m
      [33mINFO: Parameters
      muc=>3.077
      b=>0.625
      non_dec=>0.295
      range_non_dec=>0.053
      ==> gave -log_like_val of -1436.48[0m
      [33mINFO: Parameters
      muc=>2.99
      b=>0.622
      non_dec=>0.292
      range_non_dec=>0.048
      ==> gave -log_like_val of -1437.463[0m
      [33mINFO: Parameters
      muc=>2.902
      b=>0.63
      non_dec=>0.288
      range_non_dec=>0.045
      ==> gave -log_like_val of -1434.684[0m
      [33mINFO: Parameters
      muc=>3.105
      b=>0.67
      non_dec=>0.282
      range_non_dec=>0.043
      ==> gave -log_like_val of -1419.21[0m
      [33mINFO: Parameters
      muc=>3.053
      b=>0.6
      non_dec=>0.3
      range_non_dec=>0.053
      ==> gave -log_like_val of -1438.146[0m
      [33mINFO: Parameters
      muc=>2.997
      b=>0.616
      non_dec=>0.299
      range_non_dec=>0.048
      ==> gave -log_like_val of -1431.383[0m
      [33mINFO: Parameters
      muc=>3.084
      b=>0.622
      non_dec=>0.294
      range_non_dec=>0.051
      ==> gave -log_like_val of -1435.647[0m
      [33mINFO: Parameters
      muc=>3.003
      b=>0.604
      non_dec=>0.295
      range_non_dec=>0.056
      ==> gave -log_like_val of -1436.962[0m
      [33mINFO: Parameters
      muc=>2.978
      b=>0.604
      non_dec=>0.297
      range_non_dec=>0.054
      ==> gave -log_like_val of -1439.585[0m
      [33mINFO: Parameters
      muc=>2.926
      b=>0.595
      non_dec=>0.299
      range_non_dec=>0.056
      ==> gave -log_like_val of -1439.007[0m
      [33mINFO: Parameters
      muc=>2.936
      b=>0.589
      non_dec=>0.298
      range_non_dec=>0.053
      ==> gave -log_like_val of -1438.602[0m
      [33mINFO: Parameters
      muc=>2.975
      b=>0.605
      non_dec=>0.299
      range_non_dec=>0.048
      ==> gave -log_like_val of -1439.529[0m
      [33mINFO: Parameters
      muc=>2.98
      b=>0.576
      non_dec=>0.304
      range_non_dec=>0.056
      ==> gave -log_like_val of -1435.948[0m
      [33mINFO: Parameters
      muc=>2.988
      b=>0.611
      non_dec=>0.295
      range_non_dec=>0.05
      ==> gave -log_like_val of -1438.642[0m
      [33mINFO: Parameters
      muc=>2.888
      b=>0.604
      non_dec=>0.294
      range_non_dec=>0.05
      ==> gave -log_like_val of -1435.723[0m
      [33mINFO: Parameters
      muc=>3.011
      b=>0.601
      non_dec=>0.298
      range_non_dec=>0.052
      ==> gave -log_like_val of -1439.256[0m
      [33mINFO: Parameters
      muc=>3.041
      b=>0.621
      non_dec=>0.297
      range_non_dec=>0.049
      ==> gave -log_like_val of -1436.308[0m
      [33mINFO: Parameters
      muc=>2.962
      b=>0.597
      non_dec=>0.297
      range_non_dec=>0.052
      ==> gave -log_like_val of -1439.603[0m
      [33mINFO: Parameters
      muc=>2.975
      b=>0.592
      non_dec=>0.3
      range_non_dec=>0.053
      ==> gave -log_like_val of -1439.308[0m
      [33mINFO: Parameters
      muc=>2.935
      b=>0.598
      non_dec=>0.298
      range_non_dec=>0.051
      ==> gave -log_like_val of -1439.669[0m
      [33mINFO: Parameters
      muc=>2.897
      b=>0.596
      non_dec=>0.298
      range_non_dec=>0.051
      ==> gave -log_like_val of -1439.314[0m
      [33mINFO: Parameters
      muc=>2.95
      b=>0.609
      non_dec=>0.295
      range_non_dec=>0.05
      ==> gave -log_like_val of -1438.512[0m
      [33mINFO: Parameters
      muc=>2.969
      b=>0.597
      non_dec=>0.299
      range_non_dec=>0.052
      ==> gave -log_like_val of -1439.167[0m
      [33mINFO: Parameters
      muc=>2.921
      b=>0.598
      non_dec=>0.299
      range_non_dec=>0.051
      ==> gave -log_like_val of -1439.577[0m
      [33mINFO: Parameters
      muc=>2.913
      b=>0.595
      non_dec=>0.299
      range_non_dec=>0.05
      ==> gave -log_like_val of -1439.46[0m
      [33mINFO: Parameters
      muc=>2.915
      b=>0.594
      non_dec=>0.298
      range_non_dec=>0.053
      ==> gave -log_like_val of -1439.446[0m
      [33mINFO: Parameters
      muc=>2.915
      b=>0.601
      non_dec=>0.297
      range_non_dec=>0.051
      ==> gave -log_like_val of -1439.356[0m
      [33mINFO: Parameters
      muc=>2.927
      b=>0.592
      non_dec=>0.3
      range_non_dec=>0.052
      ==> gave -log_like_val of -1439.246[0m
      [33mINFO: Parameters
      muc=>2.918
      b=>0.599
      non_dec=>0.298
      range_non_dec=>0.051
      ==> gave -log_like_val of -1439.533[0m
      [33mINFO: Parameters
      muc=>2.929
      b=>0.6
      non_dec=>0.299
      range_non_dec=>0.048
      ==> gave -log_like_val of -1439.574[0m
      [33mINFO: Parameters
      muc=>2.938
      b=>0.603
      non_dec=>0.298
      range_non_dec=>0.051
      ==> gave -log_like_val of -1439.501[0m
      [33mINFO: Parameters
      muc=>2.932
      b=>0.601
      non_dec=>0.298
      range_non_dec=>0.051
      ==> gave -log_like_val of -1439.584[0m
      [33mINFO: Parameters
      muc=>2.941
      b=>0.6
      non_dec=>0.299
      range_non_dec=>0.05
      ==> gave -log_like_val of -1439.68[0m
      [33mINFO: Parameters
      muc=>2.952
      b=>0.601
      non_dec=>0.3
      range_non_dec=>0.049
      ==> gave -log_like_val of -1436.973[0m
      [33mINFO: Parameters
      muc=>2.935
      b=>0.598
      non_dec=>0.298
      range_non_dec=>0.053
      ==> gave -log_like_val of -1439.674[0m
      [33mINFO: Parameters
      muc=>2.95
      b=>0.6
      non_dec=>0.298
      range_non_dec=>0.051
      ==> gave -log_like_val of -1439.714[0m
      [33mINFO: Parameters
      muc=>2.965
      b=>0.601
      non_dec=>0.298
      range_non_dec=>0.052
      ==> gave -log_like_val of -1439.706[0m
      [33mINFO: Parameters
      muc=>2.948
      b=>0.597
      non_dec=>0.299
      range_non_dec=>0.052
      ==> gave -log_like_val of -1439.669[0m
      [33mINFO: Parameters
      muc=>2.944
      b=>0.598
      non_dec=>0.299
      range_non_dec=>0.052
      ==> gave -log_like_val of -1439.699[0m
      [33mINFO: Parameters
      muc=>2.951
      b=>0.6
      non_dec=>0.299
      range_non_dec=>0.052
      ==> gave -log_like_val of -1439.712[0m
      [33mINFO: Parameters
      muc=>2.957
      b=>0.601
      non_dec=>0.299
      range_non_dec=>0.049
      ==> gave -log_like_val of -1439.703[0m
      [33mINFO: Parameters
      muc=>2.961
      b=>0.6
      non_dec=>0.298
      range_non_dec=>0.052
      ==> gave -log_like_val of -1439.716[0m
      [33mINFO: Parameters
      muc=>2.971
      b=>0.6
      non_dec=>0.298
      range_non_dec=>0.053
      ==> gave -log_like_val of -1439.677[0m
      [33mINFO: Parameters
      muc=>2.965
      b=>0.603
      non_dec=>0.299
      range_non_dec=>0.05
      ==> gave -log_like_val of -1439.66[0m
      [33mINFO: Parameters
      muc=>2.949
      b=>0.599
      non_dec=>0.299
      range_non_dec=>0.051
      ==> gave -log_like_val of -1439.718[0m
      [33mINFO: Parameters
      muc=>2.948
      b=>0.599
      non_dec=>0.298
      range_non_dec=>0.054
      ==> gave -log_like_val of -1438.727[0m
      [33mINFO: Parameters
      muc=>2.955
      b=>0.601
      non_dec=>0.299
      range_non_dec=>0.05
      ==> gave -log_like_val of -1439.715[0m
      [33mINFO: Parameters
      muc=>2.957
      b=>0.6
      non_dec=>0.298
      range_non_dec=>0.051
      ==> gave -log_like_val of -1439.72[0m
      [33mINFO: Parameters
      muc=>2.96
      b=>0.599
      non_dec=>0.298
      range_non_dec=>0.051
      ==> gave -log_like_val of -1439.708[0m
      [33mINFO: Parameters
      muc=>2.961
      b=>0.6
      non_dec=>0.299
      range_non_dec=>0.051
      ==> gave -log_like_val of -1439.712[0m
      [33mINFO: Parameters
      muc=>2.953
      b=>0.6
      non_dec=>0.298
      range_non_dec=>0.051
      ==> gave -log_like_val of -1439.72[0m
      [33mINFO: Parameters
      muc=>2.955
      b=>0.599
      non_dec=>0.298
      range_non_dec=>0.053
      ==> gave -log_like_val of -1439.71[0m
      [33mINFO: Parameters
      muc=>2.955
      b=>0.6
      non_dec=>0.299
      range_non_dec=>0.051
      ==> gave -log_like_val of -1439.72[0m
      [33mINFO: Parameters
      muc=>2.947
      b=>0.6
      non_dec=>0.298
      range_non_dec=>0.05
      ==> gave -log_like_val of -1439.712[0m
      [33mINFO: Parameters
      muc=>2.957
      b=>0.6
      non_dec=>0.298
      range_non_dec=>0.052
      ==> gave -log_like_val of -1439.721[0m
      [33mINFO: Parameters
      muc=>2.962
      b=>0.601
      non_dec=>0.298
      range_non_dec=>0.051
      ==> gave -log_like_val of -1439.715[0m
      [33mINFO: Parameters
      muc=>2.952
      b=>0.6
      non_dec=>0.299
      range_non_dec=>0.051
      ==> gave -log_like_val of -1439.721[0m
      [33mINFO: Parameters
      muc=>2.952
      b=>0.6
      non_dec=>0.299
      range_non_dec=>0.052
      ==> gave -log_like_val of -1439.717[0m
      [33mINFO: Parameters
      muc=>2.956
      b=>0.6
      non_dec=>0.298
      range_non_dec=>0.051
      ==> gave -log_like_val of -1439.721[0m
      [33mINFO: Parameters
      muc=>2.957
      b=>0.6
      non_dec=>0.299
      range_non_dec=>0.051
      ==> gave -log_like_val of -1439.72[0m
      [33mINFO: Parameters
      muc=>2.954
      b=>0.6
      non_dec=>0.298
      range_non_dec=>0.051
      ==> gave -log_like_val of -1439.721[0m
      [33mINFO: Parameters
      muc=>2.955
      b=>0.599
      non_dec=>0.298
      range_non_dec=>0.052
      ==> gave -log_like_val of -1439.72[0m
      [33mINFO: Parameters
      muc=>2.955
      b=>0.6
      non_dec=>0.298
      range_non_dec=>0.051
      ==> gave -log_like_val of -1439.721[0m
      [33mINFO: Parameters
      muc=>2.951
      b=>0.6
      non_dec=>0.298
      range_non_dec=>0.051
      ==> gave -log_like_val of -1439.72[0m
      [33mINFO: Parameters
      muc=>2.956
      b=>0.6
      non_dec=>0.298
      range_non_dec=>0.051
      ==> gave -log_like_val of -1439.721[0m
      [33mINFO: Parameters
      muc=>2.958
      b=>0.6
      non_dec=>0.298
      range_non_dec=>0.051
      ==> gave -log_like_val of -1439.72[0m
      [33mINFO: Parameters
      muc=>2.954
      b=>0.6
      non_dec=>0.298
      range_non_dec=>0.051
      ==> gave -log_like_val of -1439.721[0m
      [33mINFO: Parameters
      muc=>2.956
      b=>0.6
      non_dec=>0.298
      range_non_dec=>0.051
      ==> gave -log_like_val of -1439.721[0m
      [33mINFO: Parameters
      muc=>2.954
      b=>0.6
      non_dec=>0.298
      range_non_dec=>0.051
      ==> gave -log_like_val of -1439.722[0m
      [33mINFO: Parameters
      muc=>2.954
      b=>0.6
      non_dec=>0.299
      range_non_dec=>0.051
      ==> gave -log_like_val of -1439.721[0m
      [33mINFO: Parameters
      muc=>2.955
      b=>0.6
      non_dec=>0.298
      range_non_dec=>0.051
      ==> gave -log_like_val of -1439.722[0m
      [33mINFO: Parameters
      muc=>2.955
      b=>0.6
      non_dec=>0.298
      range_non_dec=>0.051
      ==> gave -log_like_val of -1439.721[0m
      [33mINFO: Parameters
      muc=>2.955
      b=>0.6
      non_dec=>0.298
      range_non_dec=>0.051
      ==> gave -log_like_val of -1439.722[0m
      [33mINFO: Parameters
      muc=>2.953
      b=>0.6
      non_dec=>0.298
      range_non_dec=>0.051
      ==> gave -log_like_val of -1439.721[0m
      [33mINFO: Parameters
      muc=>2.955
      b=>0.6
      non_dec=>0.298
      range_non_dec=>0.051
      ==> gave -log_like_val of -1439.722[0m
      [33mINFO: Parameters
      muc=>2.956
      b=>0.6
      non_dec=>0.298
      range_non_dec=>0.051
      ==> gave -log_like_val of -1439.721[0m
      [33mINFO: Parameters
      muc=>2.954
      b=>0.6
      non_dec=>0.298
      range_non_dec=>0.051
      ==> gave -log_like_val of -1439.722[0m
      [33mINFO: Parameters
      muc=>2.955
      b=>0.6
      non_dec=>0.298
      range_non_dec=>0.051
      ==> gave -log_like_val of -1439.721[0m
      [33mINFO: Parameters
      muc=>2.955
      b=>0.6
      non_dec=>0.298
      range_non_dec=>0.051
      ==> gave -log_like_val of -1439.722[0m
      [33mINFO: Parameters
      muc=>2.954
      b=>0.6
      non_dec=>0.298
      range_non_dec=>0.051
      ==> gave -log_like_val of -1439.721[0m
      [33mINFO: Parameters
      muc=>2.955
      b=>0.6
      non_dec=>0.298
      range_non_dec=>0.051
      ==> gave -log_like_val of -1439.722[0m
      [33mINFO: Parameters
      muc=>2.955
      b=>0.6
      non_dec=>0.298
      range_non_dec=>0.051
      ==> gave -log_like_val of -1439.722[0m
      [33mINFO: Parameters
      muc=>2.955
      b=>0.6
      non_dec=>0.298
      range_non_dec=>0.051
      ==> gave -log_like_val of -1439.722[0m
      [33mINFO: Parameters
      muc=>2.954
      b=>0.6
      non_dec=>0.298
      range_non_dec=>0.051
      ==> gave -log_like_val of -1439.722[0m
      [33mINFO: Parameters
      muc=>2.955
      b=>0.6
      non_dec=>0.298
      range_non_dec=>0.051
      ==> gave -log_like_val of -1439.722[0m
      [33mINFO: Parameters
      muc=>2.955
      b=>0.6
      non_dec=>0.298
      range_non_dec=>0.051
      ==> gave -log_like_val of -1439.722[0m
      [33mINFO: Parameters
      muc=>2.955
      b=>0.6
      non_dec=>0.298
      range_non_dec=>0.051
      ==> gave -log_like_val of -1439.722[0m
      [33mINFO: Parameters
      muc=>2.955
      b=>0.6
      non_dec=>0.298
      range_non_dec=>0.051
      ==> gave -log_like_val of -1439.722[0m
      [33mINFO: Parameters
      muc=>2.955
      b=>0.6
      non_dec=>0.298
      range_non_dec=>0.051
      ==> gave -log_like_val of -1439.722[0m
      [33mINFO: Parameters
      muc=>2.955
      b=>0.6
      non_dec=>0.298
      range_non_dec=>0.051
      ==> gave -log_like_val of -1439.722[0m
      [33mINFO: Parameters
      muc=>2.955
      b=>0.6
      non_dec=>0.298
      range_non_dec=>0.051
      ==> gave -log_like_val of -1439.722[0m
      [33mINFO: Parameters
      muc=>2.955
      b=>0.6
      non_dec=>0.298
      range_non_dec=>0.051
      ==> gave -log_like_val of -1439.722[0m
      [33mINFO: Parameters
      muc=>2.955
      b=>0.6
      non_dec=>0.298
      range_non_dec=>0.051
      ==> gave -log_like_val of -1439.722[0m
      [33mINFO: Parameters
      muc=>2.955
      b=>0.6
      non_dec=>0.298
      range_non_dec=>0.051
      ==> gave -log_like_val of -1439.722[0m
      [33mINFO: Parameters
      muc=>2.955
      b=>0.6
      non_dec=>0.298
      range_non_dec=>0.051
      ==> gave -log_like_val of -1439.722[0m
      [33mINFO: Parameters
      muc=>2.955
      b=>0.6
      non_dec=>0.298
      range_non_dec=>0.051
      ==> gave -log_like_val of -1439.722[0m
      [33mINFO: Parameters
      muc=>2.955
      b=>0.6
      non_dec=>0.298
      range_non_dec=>0.051
      ==> gave -log_like_val of -1439.722[0m
      [33mINFO: Parameters
      muc=>2.955
      b=>0.6
      non_dec=>0.298
      range_non_dec=>0.051
      ==> gave -log_like_val of -1439.722[0m
      [33mINFO: Parameters
      muc=>2.955
      b=>0.6
      non_dec=>0.298
      range_non_dec=>0.051
      ==> gave -log_like_val of -1439.722[0m
      [33mINFO: Parameters
      muc=>2.955
      b=>0.6
      non_dec=>0.298
      range_non_dec=>0.051
      ==> gave -log_like_val of -1439.722[0m
      [33mINFO: Parameters
      muc=>2.955
      b=>0.6
      non_dec=>0.298
      range_non_dec=>0.051
      ==> gave -log_like_val of -1439.722[0m
      [33mINFO: Parameters
      muc=>2.955
      b=>0.6
      non_dec=>0.298
      range_non_dec=>0.051
      ==> gave -log_like_val of -1439.722[0m
      [33mINFO: Parameters
      muc=>2.955
      b=>0.6
      non_dec=>0.298
      range_non_dec=>0.051
      ==> gave -log_like_val of -1439.722[0m
      Class(es): ratcliff_dm, drift_dm
      
      Current Parameter Matrix:
             muc   b non_dec range_non_dec
      null 2.955 0.6   0.298         0.051
      
      Unique Parameters:
           muc b non_dec range_non_dec
      null   1 2       3             4
      
      Deriving PDFs:
        solver: kfe
        values: sigma=1, t_max=3, dt=0.005, dx=0.05, nt=600, nx=40
      
      Observed Data: 2000 trials null

# behavior of DE and nmkb toggles

    Code
      estimate_model(drift_dm_obj = a_model, lower = c(1, 0.1), upper = c(5, 0.5),
      seed = 1, verbose = 1, use_de_optim = TRUE, de_control = list(reltol = 1e-09,
        steptol = 50, itermax = 200, trace = TRUE))
    Output
      INFO: Running differential evolution
      Iteration: 1 bestvalit: -576.924669 bestmemit:    4.147303    0.314631
      Iteration: 2 bestvalit: -576.924669 bestmemit:    4.147303    0.314631
      Iteration: 3 bestvalit: -576.924669 bestmemit:    4.147303    0.314631
      Iteration: 4 bestvalit: -576.924669 bestmemit:    4.147303    0.314631
      Iteration: 5 bestvalit: -582.485176 bestmemit:    4.011332    0.303037
      Iteration: 6 bestvalit: -582.485176 bestmemit:    4.011332    0.303037
      Iteration: 7 bestvalit: -582.485176 bestmemit:    4.011332    0.303037
      Iteration: 8 bestvalit: -583.101231 bestmemit:    4.053700    0.305854
      Iteration: 9 bestvalit: -583.576052 bestmemit:    4.159118    0.305854
      Iteration: 10 bestvalit: -583.576052 bestmemit:    4.159118    0.305854
      Iteration: 11 bestvalit: -583.576052 bestmemit:    4.159118    0.305854
      Iteration: 12 bestvalit: -583.576052 bestmemit:    4.159118    0.305854
      Iteration: 13 bestvalit: -583.576052 bestmemit:    4.159118    0.305854
      Iteration: 14 bestvalit: -583.576052 bestmemit:    4.159118    0.305854
      Iteration: 15 bestvalit: -583.576052 bestmemit:    4.159118    0.305854
      Iteration: 16 bestvalit: -583.596575 bestmemit:    4.181470    0.306098
      Iteration: 17 bestvalit: -583.596575 bestmemit:    4.181470    0.306098
      Iteration: 18 bestvalit: -583.596575 bestmemit:    4.181470    0.306098
      Iteration: 19 bestvalit: -583.596575 bestmemit:    4.181470    0.306098
      Iteration: 20 bestvalit: -583.596575 bestmemit:    4.181470    0.306098
      Iteration: 21 bestvalit: -583.599226 bestmemit:    4.187401    0.306379
      Iteration: 22 bestvalit: -583.599226 bestmemit:    4.187401    0.306379
      Iteration: 23 bestvalit: -583.599341 bestmemit:    4.187401    0.306366
      Iteration: 24 bestvalit: -583.599341 bestmemit:    4.187401    0.306366
      Iteration: 25 bestvalit: -583.599341 bestmemit:    4.187401    0.306366
      Iteration: 26 bestvalit: -583.599394 bestmemit:    4.187401    0.306287
      Iteration: 27 bestvalit: -583.599578 bestmemit:    4.184021    0.306286
      Iteration: 28 bestvalit: -583.599578 bestmemit:    4.184021    0.306286
      Iteration: 29 bestvalit: -583.599578 bestmemit:    4.184021    0.306286
      Iteration: 30 bestvalit: -583.599578 bestmemit:    4.184021    0.306286
      Iteration: 31 bestvalit: -583.599619 bestmemit:    4.184808    0.306307
      Iteration: 32 bestvalit: -583.599619 bestmemit:    4.184808    0.306307
      Iteration: 33 bestvalit: -583.599622 bestmemit:    4.185414    0.306301
      Iteration: 34 bestvalit: -583.599622 bestmemit:    4.185414    0.306301
      Iteration: 35 bestvalit: -583.599622 bestmemit:    4.185414    0.306301
      Iteration: 36 bestvalit: -583.599622 bestmemit:    4.185414    0.306301
      Iteration: 37 bestvalit: -583.599622 bestmemit:    4.185589    0.306307
      Iteration: 38 bestvalit: -583.599623 bestmemit:    4.185508    0.306304
      Iteration: 39 bestvalit: -583.599624 bestmemit:    4.185447    0.306307
      Iteration: 40 bestvalit: -583.599626 bestmemit:    4.185248    0.306305
      Iteration: 41 bestvalit: -583.599626 bestmemit:    4.185248    0.306305
      Iteration: 42 bestvalit: -583.599626 bestmemit:    4.185248    0.306305
      Iteration: 43 bestvalit: -583.599626 bestmemit:    4.185248    0.306305
      Iteration: 44 bestvalit: -583.599626 bestmemit:    4.185199    0.306305
      Iteration: 45 bestvalit: -583.599626 bestmemit:    4.185220    0.306306
      Iteration: 46 bestvalit: -583.599626 bestmemit:    4.185220    0.306306
      Iteration: 47 bestvalit: -583.599626 bestmemit:    4.185220    0.306306
      Iteration: 48 bestvalit: -583.599626 bestmemit:    4.185220    0.306306
      Iteration: 49 bestvalit: -583.599626 bestmemit:    4.185220    0.306306
      Iteration: 50 bestvalit: -583.599626 bestmemit:    4.185220    0.306306
      Iteration: 51 bestvalit: -583.599626 bestmemit:    4.185220    0.306306
      Iteration: 52 bestvalit: -583.599626 bestmemit:    4.185228    0.306305
      Iteration: 53 bestvalit: -583.599626 bestmemit:    4.185228    0.306305
      Iteration: 54 bestvalit: -583.599626 bestmemit:    4.185228    0.306306
      Iteration: 55 bestvalit: -583.599626 bestmemit:    4.185228    0.306306
      Iteration: 56 bestvalit: -583.599626 bestmemit:    4.185228    0.306306
      Iteration: 57 bestvalit: -583.599626 bestmemit:    4.185231    0.306306
      Iteration: 58 bestvalit: -583.599626 bestmemit:    4.185231    0.306306
      Iteration: 59 bestvalit: -583.599626 bestmemit:    4.185231    0.306306
      Iteration: 60 bestvalit: -583.599626 bestmemit:    4.185229    0.306306
      Iteration: 61 bestvalit: -583.599626 bestmemit:    4.185229    0.306306
      Iteration: 62 bestvalit: -583.599626 bestmemit:    4.185229    0.306306
      Iteration: 63 bestvalit: -583.599626 bestmemit:    4.185229    0.306306
      Iteration: 64 bestvalit: -583.599626 bestmemit:    4.185229    0.306306
      Iteration: 65 bestvalit: -583.599626 bestmemit:    4.185229    0.306306
      Iteration: 66 bestvalit: -583.599626 bestmemit:    4.185229    0.306306
      Iteration: 67 bestvalit: -583.599626 bestmemit:    4.185229    0.306306
      Iteration: 68 bestvalit: -583.599626 bestmemit:    4.185229    0.306306
      Iteration: 69 bestvalit: -583.599626 bestmemit:    4.185229    0.306306
      Iteration: 70 bestvalit: -583.599626 bestmemit:    4.185229    0.306306
      Iteration: 71 bestvalit: -583.599626 bestmemit:    4.185229    0.306306
      Iteration: 72 bestvalit: -583.599626 bestmemit:    4.185229    0.306306
      Iteration: 73 bestvalit: -583.599626 bestmemit:    4.185229    0.306306
      Iteration: 74 bestvalit: -583.599626 bestmemit:    4.185229    0.306306
      Iteration: 75 bestvalit: -583.599626 bestmemit:    4.185229    0.306306
      Iteration: 76 bestvalit: -583.599626 bestmemit:    4.185229    0.306306
      Iteration: 77 bestvalit: -583.599626 bestmemit:    4.185229    0.306306
      Iteration: 78 bestvalit: -583.599626 bestmemit:    4.185229    0.306306
      Iteration: 79 bestvalit: -583.599626 bestmemit:    4.185229    0.306306
      Iteration: 80 bestvalit: -583.599626 bestmemit:    4.185229    0.306306
      Iteration: 81 bestvalit: -583.599626 bestmemit:    4.185229    0.306306
      Iteration: 82 bestvalit: -583.599626 bestmemit:    4.185229    0.306306
      Iteration: 83 bestvalit: -583.599626 bestmemit:    4.185229    0.306306
      Iteration: 84 bestvalit: -583.599626 bestmemit:    4.185229    0.306306
      Iteration: 85 bestvalit: -583.599626 bestmemit:    4.185229    0.306306
      Iteration: 86 bestvalit: -583.599626 bestmemit:    4.185229    0.306306
      Iteration: 87 bestvalit: -583.599626 bestmemit:    4.185229    0.306306
      Iteration: 88 bestvalit: -583.599626 bestmemit:    4.185229    0.306306
      Iteration: 89 bestvalit: -583.599626 bestmemit:    4.185229    0.306306
      Iteration: 90 bestvalit: -583.599626 bestmemit:    4.185229    0.306306
      Iteration: 91 bestvalit: -583.599626 bestmemit:    4.185229    0.306306
      [33mINFO: Parameters
      muc=>4.185
      non_dec=>0.306
      ==> gave -log_like_val of -583.5996[0m
      Class(es): dmc_dm, drift_dm
      
      Current Parameter Matrix:
               muc   b non_dec sd_non_dec  tau a    A alpha
      comp   4.185 0.6   0.306       0.02 0.04 2  0.1     4
      incomp 4.185 0.6   0.306       0.02 0.04 2 -0.1     4
      
      Unique Parameters:
             muc b non_dec sd_non_dec tau a A alpha
      comp     1 0       2          0   0 0 0     0
      incomp   1 0       2          0   0 0 0     0
      
      Custom Parameters:
             peak_l
      comp     0.04
      incomp   0.04
      
      Deriving PDFs:
        solver: kfe
        values: sigma=1, t_max=1, dt=0.01, dx=0.1, nt=100, nx=20
      
      Observed Data: 300 trials comp; 300 trials incomp

---

    Code
      estimate_model(drift_dm_obj = a_model, lower = c(1, 0.1), upper = c(5, 0.5),
      seed = 1, verbose = 1, use_de_optim = TRUE, de_control = list(reltol = 1e-09,
        steptol = 50, itermax = 200, trace = TRUE), de_n_cores = 2)
    Output
      INFO: Running differential evolution
      Iteration: 1 bestvalit: -576.924669 bestmemit:    4.147303    0.314631
      Iteration: 2 bestvalit: -576.924669 bestmemit:    4.147303    0.314631
      Iteration: 3 bestvalit: -576.924669 bestmemit:    4.147303    0.314631
      Iteration: 4 bestvalit: -576.924669 bestmemit:    4.147303    0.314631
      Iteration: 5 bestvalit: -582.485176 bestmemit:    4.011332    0.303037
      Iteration: 6 bestvalit: -582.485176 bestmemit:    4.011332    0.303037
      Iteration: 7 bestvalit: -582.485176 bestmemit:    4.011332    0.303037
      Iteration: 8 bestvalit: -583.101231 bestmemit:    4.053700    0.305854
      Iteration: 9 bestvalit: -583.576052 bestmemit:    4.159118    0.305854
      Iteration: 10 bestvalit: -583.576052 bestmemit:    4.159118    0.305854
      Iteration: 11 bestvalit: -583.576052 bestmemit:    4.159118    0.305854
      Iteration: 12 bestvalit: -583.576052 bestmemit:    4.159118    0.305854
      Iteration: 13 bestvalit: -583.576052 bestmemit:    4.159118    0.305854
      Iteration: 14 bestvalit: -583.576052 bestmemit:    4.159118    0.305854
      Iteration: 15 bestvalit: -583.576052 bestmemit:    4.159118    0.305854
      Iteration: 16 bestvalit: -583.596575 bestmemit:    4.181470    0.306098
      Iteration: 17 bestvalit: -583.596575 bestmemit:    4.181470    0.306098
      Iteration: 18 bestvalit: -583.596575 bestmemit:    4.181470    0.306098
      Iteration: 19 bestvalit: -583.596575 bestmemit:    4.181470    0.306098
      Iteration: 20 bestvalit: -583.596575 bestmemit:    4.181470    0.306098
      Iteration: 21 bestvalit: -583.599226 bestmemit:    4.187401    0.306379
      Iteration: 22 bestvalit: -583.599226 bestmemit:    4.187401    0.306379
      Iteration: 23 bestvalit: -583.599341 bestmemit:    4.187401    0.306366
      Iteration: 24 bestvalit: -583.599341 bestmemit:    4.187401    0.306366
      Iteration: 25 bestvalit: -583.599341 bestmemit:    4.187401    0.306366
      Iteration: 26 bestvalit: -583.599394 bestmemit:    4.187401    0.306287
      Iteration: 27 bestvalit: -583.599578 bestmemit:    4.184021    0.306286
      Iteration: 28 bestvalit: -583.599578 bestmemit:    4.184021    0.306286
      Iteration: 29 bestvalit: -583.599578 bestmemit:    4.184021    0.306286
      Iteration: 30 bestvalit: -583.599578 bestmemit:    4.184021    0.306286
      Iteration: 31 bestvalit: -583.599619 bestmemit:    4.184808    0.306307
      Iteration: 32 bestvalit: -583.599619 bestmemit:    4.184808    0.306307
      Iteration: 33 bestvalit: -583.599622 bestmemit:    4.185414    0.306301
      Iteration: 34 bestvalit: -583.599622 bestmemit:    4.185414    0.306301
      Iteration: 35 bestvalit: -583.599622 bestmemit:    4.185414    0.306301
      Iteration: 36 bestvalit: -583.599622 bestmemit:    4.185414    0.306301
      Iteration: 37 bestvalit: -583.599622 bestmemit:    4.185589    0.306307
      Iteration: 38 bestvalit: -583.599623 bestmemit:    4.185508    0.306304
      Iteration: 39 bestvalit: -583.599624 bestmemit:    4.185447    0.306307
      Iteration: 40 bestvalit: -583.599626 bestmemit:    4.185248    0.306305
      Iteration: 41 bestvalit: -583.599626 bestmemit:    4.185248    0.306305
      Iteration: 42 bestvalit: -583.599626 bestmemit:    4.185248    0.306305
      Iteration: 43 bestvalit: -583.599626 bestmemit:    4.185248    0.306305
      Iteration: 44 bestvalit: -583.599626 bestmemit:    4.185199    0.306305
      Iteration: 45 bestvalit: -583.599626 bestmemit:    4.185220    0.306306
      Iteration: 46 bestvalit: -583.599626 bestmemit:    4.185220    0.306306
      Iteration: 47 bestvalit: -583.599626 bestmemit:    4.185220    0.306306
      Iteration: 48 bestvalit: -583.599626 bestmemit:    4.185220    0.306306
      Iteration: 49 bestvalit: -583.599626 bestmemit:    4.185220    0.306306
      Iteration: 50 bestvalit: -583.599626 bestmemit:    4.185220    0.306306
      Iteration: 51 bestvalit: -583.599626 bestmemit:    4.185220    0.306306
      Iteration: 52 bestvalit: -583.599626 bestmemit:    4.185228    0.306305
      Iteration: 53 bestvalit: -583.599626 bestmemit:    4.185228    0.306305
      Iteration: 54 bestvalit: -583.599626 bestmemit:    4.185228    0.306306
      Iteration: 55 bestvalit: -583.599626 bestmemit:    4.185228    0.306306
      Iteration: 56 bestvalit: -583.599626 bestmemit:    4.185228    0.306306
      Iteration: 57 bestvalit: -583.599626 bestmemit:    4.185231    0.306306
      Iteration: 58 bestvalit: -583.599626 bestmemit:    4.185231    0.306306
      Iteration: 59 bestvalit: -583.599626 bestmemit:    4.185231    0.306306
      Iteration: 60 bestvalit: -583.599626 bestmemit:    4.185229    0.306306
      Iteration: 61 bestvalit: -583.599626 bestmemit:    4.185229    0.306306
      Iteration: 62 bestvalit: -583.599626 bestmemit:    4.185229    0.306306
      Iteration: 63 bestvalit: -583.599626 bestmemit:    4.185229    0.306306
      Iteration: 64 bestvalit: -583.599626 bestmemit:    4.185229    0.306306
      Iteration: 65 bestvalit: -583.599626 bestmemit:    4.185229    0.306306
      Iteration: 66 bestvalit: -583.599626 bestmemit:    4.185229    0.306306
      Iteration: 67 bestvalit: -583.599626 bestmemit:    4.185229    0.306306
      Iteration: 68 bestvalit: -583.599626 bestmemit:    4.185229    0.306306
      Iteration: 69 bestvalit: -583.599626 bestmemit:    4.185229    0.306306
      Iteration: 70 bestvalit: -583.599626 bestmemit:    4.185229    0.306306
      Iteration: 71 bestvalit: -583.599626 bestmemit:    4.185229    0.306306
      Iteration: 72 bestvalit: -583.599626 bestmemit:    4.185229    0.306306
      Iteration: 73 bestvalit: -583.599626 bestmemit:    4.185229    0.306306
      Iteration: 74 bestvalit: -583.599626 bestmemit:    4.185229    0.306306
      Iteration: 75 bestvalit: -583.599626 bestmemit:    4.185229    0.306306
      Iteration: 76 bestvalit: -583.599626 bestmemit:    4.185229    0.306306
      Iteration: 77 bestvalit: -583.599626 bestmemit:    4.185229    0.306306
      Iteration: 78 bestvalit: -583.599626 bestmemit:    4.185229    0.306306
      Iteration: 79 bestvalit: -583.599626 bestmemit:    4.185229    0.306306
      Iteration: 80 bestvalit: -583.599626 bestmemit:    4.185229    0.306306
      Iteration: 81 bestvalit: -583.599626 bestmemit:    4.185229    0.306306
      Iteration: 82 bestvalit: -583.599626 bestmemit:    4.185229    0.306306
      Iteration: 83 bestvalit: -583.599626 bestmemit:    4.185229    0.306306
      Iteration: 84 bestvalit: -583.599626 bestmemit:    4.185229    0.306306
      Iteration: 85 bestvalit: -583.599626 bestmemit:    4.185229    0.306306
      Iteration: 86 bestvalit: -583.599626 bestmemit:    4.185229    0.306306
      Iteration: 87 bestvalit: -583.599626 bestmemit:    4.185229    0.306306
      Iteration: 88 bestvalit: -583.599626 bestmemit:    4.185229    0.306306
      Iteration: 89 bestvalit: -583.599626 bestmemit:    4.185229    0.306306
      Iteration: 90 bestvalit: -583.599626 bestmemit:    4.185229    0.306306
      Iteration: 91 bestvalit: -583.599626 bestmemit:    4.185229    0.306306
      [33mINFO: Parameters
      muc=>4.185
      non_dec=>0.306
      ==> gave -log_like_val of -583.5996[0m
      Class(es): dmc_dm, drift_dm
      
      Current Parameter Matrix:
               muc   b non_dec sd_non_dec  tau a    A alpha
      comp   4.185 0.6   0.306       0.02 0.04 2  0.1     4
      incomp 4.185 0.6   0.306       0.02 0.04 2 -0.1     4
      
      Unique Parameters:
             muc b non_dec sd_non_dec tau a A alpha
      comp     1 0       2          0   0 0 0     0
      incomp   1 0       2          0   0 0 0     0
      
      Custom Parameters:
             peak_l
      comp     0.04
      incomp   0.04
      
      Deriving PDFs:
        solver: kfe
        values: sigma=1, t_max=1, dt=0.01, dx=0.1, nt=100, nx=20
      
      Observed Data: 300 trials comp; 300 trials incomp

