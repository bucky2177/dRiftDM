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
      ==> gave -log_like_val of -163.8117[0m
      [33mINFO: Parameters
      muc=>5.548
      b=>0.594
      non_dec=>0.262
      range_non_dec=>0.027
      ==> gave -log_like_val of 864.8377[0m
      [33mINFO: Parameters
      muc=>2.661
      b=>0.764
      non_dec=>0.262
      range_non_dec=>0.027
      ==> gave -log_like_val of -1317.159[0m
      [33mINFO: Parameters
      muc=>2.661
      b=>0.594
      non_dec=>0.498
      range_non_dec=>0.027
      ==> gave -log_like_val of 28568.82[0m
      [33mINFO: Parameters
      muc=>2.661
      b=>0.594
      non_dec=>0.262
      range_non_dec=>0.07
      ==> gave -log_like_val of -1243.767[0m
      [33mINFO: Parameters
      muc=>3.651
      b=>0.679
      non_dec=>0.12
      range_non_dec=>0.04
      ==> gave -log_like_val of 1099.519[0m
      [33mINFO: Parameters
      muc=>3.386
      b=>0.66
      non_dec=>0.158
      range_non_dec=>0.036
      ==> gave -log_like_val of 286.5439[0m
      [33mINFO: Parameters
      muc=>1.255
      b=>0.703
      non_dec=>0.177
      range_non_dec=>0.045
      ==> gave -log_like_val of -475.5472[0m
      [33mINFO: Parameters
      muc=>1.311
      b=>0.675
      non_dec=>0.319
      range_non_dec=>0.039
      ==> gave -log_like_val of -187.3958[0m
      [33mINFO: Parameters
      muc=>1.517
      b=>0.776
      non_dec=>0.309
      range_non_dec=>0.078
      ==> gave -log_like_val of -31.56045[0m
      [33mINFO: Parameters
      muc=>1.853
      b=>0.614
      non_dec=>0.223
      range_non_dec=>0.03
      ==> gave -log_like_val of -899.9658[0m
      [33mINFO: Parameters
      muc=>3.252
      b=>0.699
      non_dec=>0.165
      range_non_dec=>0.044
      ==> gave -log_like_val of -146.8409[0m
      [33mINFO: Parameters
      muc=>1.543
      b=>0.681
      non_dec=>0.27
      range_non_dec=>0.04
      ==> gave -log_like_val of -975.1118[0m
      [33mINFO: Parameters
      muc=>4.153
      b=>0.656
      non_dec=>0.36
      range_non_dec=>0.035
      ==> gave -log_like_val of 2169.04[0m
      [33mINFO: Parameters
      muc=>1.542
      b=>0.693
      non_dec=>0.21
      range_non_dec=>0.043
      ==> gave -log_like_val of -801.3423[0m
      [33mINFO: Parameters
      muc=>2.988
      b=>0.669
      non_dec=>0.304
      range_non_dec=>0.038
      ==> gave -log_like_val of -1306.814[0m
      [33mINFO: Parameters
      muc=>3.035
      b=>0.742
      non_dec=>0.334
      range_non_dec=>0.059
      ==> gave -log_like_val of -352.7689[0m
      [33mINFO: Parameters
      muc=>2.081
      b=>0.657
      non_dec=>0.247
      range_non_dec=>0.036
      ==> gave -log_like_val of -1164.365[0m
      [33mINFO: Parameters
      muc=>4.357
      b=>0.692
      non_dec=>0.267
      range_non_dec=>0.043
      ==> gave -log_like_val of -957.5941[0m
      [33mINFO: Parameters
      muc=>1.95
      b=>0.684
      non_dec=>0.269
      range_non_dec=>0.041
      ==> gave -log_like_val of -1179.135[0m
      [33mINFO: Parameters
      muc=>3.083
      b=>0.721
      non_dec=>0.303
      range_non_dec=>0.051
      ==> gave -log_like_val of -1247.374[0m
      [33mINFO: Parameters
      muc=>4.066
      b=>0.719
      non_dec=>0.296
      range_non_dec=>0.05
      ==> gave -log_like_val of -1293.568[0m
      [33mINFO: Parameters
      muc=>3.753
      b=>0.777
      non_dec=>0.322
      range_non_dec=>0.021
      ==> gave -log_like_val of -297.8428[0m
      [33mINFO: Parameters
      muc=>2.91
      b=>0.671
      non_dec=>0.276
      range_non_dec=>0.055
      ==> gave -log_like_val of -1403.966[0m
      [33mINFO: Parameters
      muc=>3.184
      b=>0.707
      non_dec=>0.266
      range_non_dec=>0.034
      ==> gave -log_like_val of -1375.981[0m
      [33mINFO: Parameters
      muc=>2.064
      b=>0.704
      non_dec=>0.259
      range_non_dec=>0.028
      ==> gave -log_like_val of -1202.916[0m
      [33mINFO: Parameters
      muc=>3.479
      b=>0.715
      non_dec=>0.286
      range_non_dec=>0.044
      ==> gave -log_like_val of -1400.896[0m
      [33mINFO: Parameters
      muc=>3.108
      b=>0.755
      non_dec=>0.243
      range_non_dec=>0.04
      ==> gave -log_like_val of -1281.023[0m
      [33mINFO: Parameters
      muc=>3.018
      b=>0.698
      non_dec=>0.288
      range_non_dec=>0.038
      ==> gave -log_like_val of -1403.099[0m
      [33mINFO: Parameters
      muc=>3.679
      b=>0.565
      non_dec=>0.297
      range_non_dec=>0.061
      ==> gave -log_like_val of -1279.203[0m
      [33mINFO: Parameters
      muc=>2.894
      b=>0.739
      non_dec=>0.27
      range_non_dec=>0.034
      ==> gave -log_like_val of -1379.691[0m
      [33mINFO: Parameters
      muc=>2.958
      b=>0.71
      non_dec=>0.294
      range_non_dec=>0.053
      ==> gave -log_like_val of -1335.409[0m
      [33mINFO: Parameters
      muc=>3.127
      b=>0.708
      non_dec=>0.273
      range_non_dec=>0.038
      ==> gave -log_like_val of -1406.512[0m
      [33mINFO: Parameters
      muc=>3.376
      b=>0.641
      non_dec=>0.292
      range_non_dec=>0.054
      ==> gave -log_like_val of -1413.629[0m
      [33mINFO: Parameters
      muc=>3.633
      b=>0.566
      non_dec=>0.303
      range_non_dec=>0.065
      ==> gave -log_like_val of -1341.73[0m
      [33mINFO: Parameters
      muc=>2.758
      b=>0.638
      non_dec=>0.278
      range_non_dec=>0.049
      ==> gave -log_like_val of -1402.955[0m
      [33mINFO: Parameters
      muc=>2.927
      b=>0.661
      non_dec=>0.28
      range_non_dec=>0.047
      ==> gave -log_like_val of -1421.822[0m
      [33mINFO: Parameters
      muc=>3.146
      b=>0.642
      non_dec=>0.272
      range_non_dec=>0.059
      ==> gave -log_like_val of -1354.608[0m
      [33mINFO: Parameters
      muc=>3.05
      b=>0.685
      non_dec=>0.284
      range_non_dec=>0.043
      ==> gave -log_like_val of -1421.637[0m
      [33mINFO: Parameters
      muc=>3.334
      b=>0.68
      non_dec=>0.289
      range_non_dec=>0.037
      ==> gave -log_like_val of -1418.711[0m
      [33mINFO: Parameters
      muc=>3.21
      b=>0.616
      non_dec=>0.299
      range_non_dec=>0.053
      ==> gave -log_like_val of -1434.612[0m
      [33mINFO: Parameters
      muc=>3.252
      b=>0.556
      non_dec=>0.313
      range_non_dec=>0.061
      ==> gave -log_like_val of -1412.935[0m
      [33mINFO: Parameters
      muc=>2.891
      b=>0.682
      non_dec=>0.285
      range_non_dec=>0.036
      ==> gave -log_like_val of -1413.639[0m
      [33mINFO: Parameters
      muc=>3.008
      b=>0.672
      non_dec=>0.286
      range_non_dec=>0.04
      ==> gave -log_like_val of -1425.867[0m
      [33mINFO: Parameters
      muc=>2.779
      b=>0.638
      non_dec=>0.286
      range_non_dec=>0.056
      ==> gave -log_like_val of -1421.319[0m
      [33mINFO: Parameters
      muc=>2.911
      b=>0.65
      non_dec=>0.287
      range_non_dec=>0.051
      ==> gave -log_like_val of -1430.686[0m
      [33mINFO: Parameters
      muc=>2.976
      b=>0.61
      non_dec=>0.292
      range_non_dec=>0.053
      ==> gave -log_like_val of -1439.415[0m
      [33mINFO: Parameters
      muc=>2.939
      b=>0.563
      non_dec=>0.296
      range_non_dec=>0.057
      ==> gave -log_like_val of -1415.178[0m
      [33mINFO: Parameters
      muc=>3.124
      b=>0.613
      non_dec=>0.302
      range_non_dec=>0.051
      ==> gave -log_like_val of -1432.345[0m
      [33mINFO: Parameters
      muc=>3.1
      b=>0.564
      non_dec=>0.304
      range_non_dec=>0.064
      ==> gave -log_like_val of -1430.756[0m
      [33mINFO: Parameters
      muc=>3.301
      b=>0.546
      non_dec=>0.312
      range_non_dec=>0.059
      ==> gave -log_like_val of -1402.181[0m
      [33mINFO: Parameters
      muc=>3.005
      b=>0.626
      non_dec=>0.293
      range_non_dec=>0.053
      ==> gave -log_like_val of -1440.334[0m
      [33mINFO: Parameters
      muc=>3.056
      b=>0.662
      non_dec=>0.289
      range_non_dec=>0.041
      ==> gave -log_like_val of -1430.578[0m
      [33mINFO: Parameters
      muc=>3.089
      b=>0.591
      non_dec=>0.301
      range_non_dec=>0.058
      ==> gave -log_like_val of -1439.074[0m
      [33mINFO: Parameters
      muc=>3.014
      b=>0.609
      non_dec=>0.29
      range_non_dec=>0.057
      ==> gave -log_like_val of -1429.932[0m
      [33mINFO: Parameters
      muc=>3.097
      b=>0.612
      non_dec=>0.299
      range_non_dec=>0.053
      ==> gave -log_like_val of -1439.235[0m
      [33mINFO: Parameters
      muc=>2.879
      b=>0.604
      non_dec=>0.293
      range_non_dec=>0.055
      ==> gave -log_like_val of -1440.234[0m
      [33mINFO: Parameters
      muc=>2.89
      b=>0.634
      non_dec=>0.289
      range_non_dec=>0.049
      ==> gave -log_like_val of -1433.197[0m
      [33mINFO: Parameters
      muc=>3.038
      b=>0.602
      non_dec=>0.298
      range_non_dec=>0.056
      ==> gave -log_like_val of -1440.516[0m
      [33mINFO: Parameters
      muc=>2.855
      b=>0.609
      non_dec=>0.289
      range_non_dec=>0.056
      ==> gave -log_like_val of -1432.715[0m
      [33mINFO: Parameters
      muc=>3.035
      b=>0.611
      non_dec=>0.297
      range_non_dec=>0.053
      ==> gave -log_like_val of -1442.016[0m
      [33mINFO: Parameters
      muc=>3.002
      b=>0.612
      non_dec=>0.298
      range_non_dec=>0.056
      ==> gave -log_like_val of -1439.12[0m
      [33mINFO: Parameters
      muc=>2.982
      b=>0.61
      non_dec=>0.294
      range_non_dec=>0.053
      ==> gave -log_like_val of -1441.322[0m
      [33mINFO: Parameters
      muc=>3.156
      b=>0.622
      non_dec=>0.297
      range_non_dec=>0.053
      ==> gave -log_like_val of -1437.823[0m
      [33mINFO: Parameters
      muc=>2.946
      b=>0.608
      non_dec=>0.294
      range_non_dec=>0.054
      ==> gave -log_like_val of -1441.249[0m
      [33mINFO: Parameters
      muc=>2.995
      b=>0.589
      non_dec=>0.298
      range_non_dec=>0.055
      ==> gave -log_like_val of -1442.056[0m
      [33mINFO: Parameters
      muc=>2.99
      b=>0.569
      non_dec=>0.3
      range_non_dec=>0.057
      ==> gave -log_like_val of -1433.621[0m
      [33mINFO: Parameters
      muc=>2.941
      b=>0.607
      non_dec=>0.294
      range_non_dec=>0.053
      ==> gave -log_like_val of -1441.198[0m
      [33mINFO: Parameters
      muc=>2.965
      b=>0.606
      non_dec=>0.295
      range_non_dec=>0.053
      ==> gave -log_like_val of -1441.085[0m
      [33mINFO: Parameters
      muc=>2.976
      b=>0.577
      non_dec=>0.298
      range_non_dec=>0.057
      ==> gave -log_like_val of -1439.052[0m
      [33mINFO: Parameters
      muc=>3.002
      b=>0.577
      non_dec=>0.3
      range_non_dec=>0.056
      ==> gave -log_like_val of -1438.264[0m
      [33mINFO: Parameters
      muc=>3.02
      b=>0.579
      non_dec=>0.3
      range_non_dec=>0.056
      ==> gave -log_like_val of -1437.98[0m
      [33mINFO: Parameters
      muc=>2.974
      b=>0.582
      non_dec=>0.298
      range_non_dec=>0.055
      ==> gave -log_like_val of -1440.947[0m
      [33mINFO: Parameters
      muc=>2.954
      b=>0.584
      non_dec=>0.297
      range_non_dec=>0.056
      ==> gave -log_like_val of -1437.39[0m
      [33mINFO: Parameters
      muc=>3.003
      b=>0.58
      non_dec=>0.299
      range_non_dec=>0.056
      ==> gave -log_like_val of -1439.3[0m
      [33mINFO: Parameters
      muc=>2.972
      b=>0.586
      non_dec=>0.297
      range_non_dec=>0.055
      ==> gave -log_like_val of -1433.662[0m
      [33mINFO: Parameters
      muc=>2.994
      b=>0.58
      non_dec=>0.299
      range_non_dec=>0.056
      ==> gave -log_like_val of -1439.531[0m
      [33mINFO: Parameters
      muc=>3.008
      b=>0.588
      non_dec=>0.299
      range_non_dec=>0.055
      ==> gave -log_like_val of -1441.646[0m
      [33mINFO: Parameters
      muc=>2.982
      b=>0.589
      non_dec=>0.298
      range_non_dec=>0.055
      ==> gave -log_like_val of -1442.37[0m
      [33mINFO: Parameters
      muc=>2.972
      b=>0.594
      non_dec=>0.297
      range_non_dec=>0.054
      ==> gave -log_like_val of -1437.636[0m
      [33mINFO: Parameters
      muc=>2.985
      b=>0.594
      non_dec=>0.297
      range_non_dec=>0.054
      ==> gave -log_like_val of -1437.373[0m
      [33mINFO: Parameters
      muc=>2.992
      b=>0.583
      non_dec=>0.299
      range_non_dec=>0.056
      ==> gave -log_like_val of -1440.892[0m
      [33mINFO: Parameters
      muc=>2.987
      b=>0.591
      non_dec=>0.297
      range_non_dec=>0.055
      ==> gave -log_like_val of -1441.154[0m
      [33mINFO: Parameters
      muc=>3.013
      b=>0.596
      non_dec=>0.298
      range_non_dec=>0.055
      ==> gave -log_like_val of -1442.382[0m
      [33mINFO: Parameters
      muc=>3.032
      b=>0.604
      non_dec=>0.298
      range_non_dec=>0.054
      ==> gave -log_like_val of -1442.324[0m
      [33mINFO: Parameters
      muc=>3.011
      b=>0.591
      non_dec=>0.299
      range_non_dec=>0.055
      ==> gave -log_like_val of -1441.985[0m
      [33mINFO: Parameters
      muc=>2.993
      b=>0.594
      non_dec=>0.297
      range_non_dec=>0.055
      ==> gave -log_like_val of -1437.149[0m
      [33mINFO: Parameters
      muc=>3.004
      b=>0.59
      non_dec=>0.298
      range_non_dec=>0.055
      ==> gave -log_like_val of -1442.017[0m
      [33mINFO: Parameters
      muc=>2.986
      b=>0.591
      non_dec=>0.297
      range_non_dec=>0.055
      ==> gave -log_like_val of -1436.009[0m
      [33mINFO: Parameters
      muc=>3.005
      b=>0.591
      non_dec=>0.298
      range_non_dec=>0.055
      ==> gave -log_like_val of -1442.157[0m
      [33mINFO: Parameters
      muc=>2.994
      b=>0.593
      non_dec=>0.297
      range_non_dec=>0.055
      ==> gave -log_like_val of -1439.675[0m
      [33mINFO: Parameters
      muc=>3.001
      b=>0.591
      non_dec=>0.298
      range_non_dec=>0.055
      ==> gave -log_like_val of -1442.189[0m
      [33mINFO: Parameters
      muc=>3.006
      b=>0.595
      non_dec=>0.298
      range_non_dec=>0.054
      ==> gave -log_like_val of -1442.458[0m
      [33mINFO: Parameters
      muc=>3.011
      b=>0.598
      non_dec=>0.298
      range_non_dec=>0.054
      ==> gave -log_like_val of -1442.383[0m
      [33mINFO: Parameters
      muc=>2.996
      b=>0.595
      non_dec=>0.298
      range_non_dec=>0.054
      ==> gave -log_like_val of -1441.968[0m
      [33mINFO: Parameters
      muc=>3.003
      b=>0.592
      non_dec=>0.298
      range_non_dec=>0.055
      ==> gave -log_like_val of -1442.311[0m
      [33mINFO: Parameters
      muc=>3
      b=>0.596
      non_dec=>0.298
      range_non_dec=>0.055
      ==> gave -log_like_val of -1442.063[0m
      [33mINFO: Parameters
      muc=>3.001
      b=>0.592
      non_dec=>0.298
      range_non_dec=>0.055
      ==> gave -log_like_val of -1442.34[0m
      [33mINFO: Parameters
      muc=>2.998
      b=>0.594
      non_dec=>0.298
      range_non_dec=>0.055
      ==> gave -log_like_val of -1442.545[0m
      [33mINFO: Parameters
      muc=>2.996
      b=>0.596
      non_dec=>0.298
      range_non_dec=>0.054
      ==> gave -log_like_val of -1442.168[0m
      [33mINFO: Parameters
      muc=>2.998
      b=>0.596
      non_dec=>0.298
      range_non_dec=>0.054
      ==> gave -log_like_val of -1442.112[0m
      [33mINFO: Parameters
      muc=>3
      b=>0.593
      non_dec=>0.298
      range_non_dec=>0.055
      ==> gave -log_like_val of -1442.434[0m
      [33mINFO: Parameters
      muc=>3.026
      b=>0.6
      non_dec=>0.298
      range_non_dec=>0.054
      ==> gave -log_like_val of -1442.102[0m
      [33mINFO: Parameters
      muc=>2.993
      b=>0.592
      non_dec=>0.298
      range_non_dec=>0.055
      ==> gave -log_like_val of -1442.481[0m
      [33mINFO: Parameters
      muc=>2.986
      b=>0.59
      non_dec=>0.298
      range_non_dec=>0.055
      ==> gave -log_like_val of -1442.456[0m
      [33mINFO: Parameters
      muc=>2.991
      b=>0.593
      non_dec=>0.298
      range_non_dec=>0.054
      ==> gave -log_like_val of -1442.582[0m
      [33mINFO: Parameters
      muc=>2.986
      b=>0.593
      non_dec=>0.298
      range_non_dec=>0.054
      ==> gave -log_like_val of -1441.862[0m
      [33mINFO: Parameters
      muc=>3.008
      b=>0.597
      non_dec=>0.298
      range_non_dec=>0.054
      ==> gave -log_like_val of -1442.443[0m
      [33mINFO: Parameters
      muc=>2.991
      b=>0.592
      non_dec=>0.298
      range_non_dec=>0.055
      ==> gave -log_like_val of -1442.513[0m
      [33mINFO: Parameters
      muc=>2.981
      b=>0.591
      non_dec=>0.298
      range_non_dec=>0.055
      ==> gave -log_like_val of -1441.382[0m
      [33mINFO: Parameters
      muc=>2.999
      b=>0.594
      non_dec=>0.298
      range_non_dec=>0.055
      ==> gave -log_like_val of -1442.508[0m
      [33mINFO: Parameters
      muc=>2.997
      b=>0.595
      non_dec=>0.298
      range_non_dec=>0.054
      ==> gave -log_like_val of -1442.568[0m
      [33mINFO: Parameters
      muc=>2.989
      b=>0.593
      non_dec=>0.298
      range_non_dec=>0.054
      ==> gave -log_like_val of -1441.805[0m
      [33mINFO: Parameters
      muc=>2.997
      b=>0.594
      non_dec=>0.298
      range_non_dec=>0.054
      ==> gave -log_like_val of -1442.536[0m
      [33mINFO: Parameters
      muc=>3
      b=>0.596
      non_dec=>0.298
      range_non_dec=>0.054
      ==> gave -log_like_val of -1442.111[0m
      [33mINFO: Parameters
      muc=>2.994
      b=>0.593
      non_dec=>0.298
      range_non_dec=>0.055
      ==> gave -log_like_val of -1442.546[0m
      [33mINFO: Parameters
      muc=>2.993
      b=>0.594
      non_dec=>0.298
      range_non_dec=>0.054
      ==> gave -log_like_val of -1441.863[0m
      [33mINFO: Parameters
      muc=>2.996
      b=>0.594
      non_dec=>0.298
      range_non_dec=>0.054
      ==> gave -log_like_val of -1442.551[0m
      [33mINFO: Parameters
      muc=>2.991
      b=>0.593
      non_dec=>0.298
      range_non_dec=>0.054
      ==> gave -log_like_val of -1442.577[0m
      [33mINFO: Parameters
      muc=>2.994
      b=>0.594
      non_dec=>0.298
      range_non_dec=>0.054
      ==> gave -log_like_val of -1442.593[0m
      [33mINFO: Parameters
      muc=>2.994
      b=>0.595
      non_dec=>0.298
      range_non_dec=>0.054
      ==> gave -log_like_val of -1442.604[0m
      [33mINFO: Parameters
      muc=>2.99
      b=>0.594
      non_dec=>0.298
      range_non_dec=>0.054
      ==> gave -log_like_val of -1442.625[0m
      [33mINFO: Parameters
      muc=>2.987
      b=>0.594
      non_dec=>0.298
      range_non_dec=>0.054
      ==> gave -log_like_val of -1442.655[0m
      [33mINFO: Parameters
      muc=>2.984
      b=>0.593
      non_dec=>0.298
      range_non_dec=>0.054
      ==> gave -log_like_val of -1442.639[0m
      [33mINFO: Parameters
      muc=>2.987
      b=>0.594
      non_dec=>0.298
      range_non_dec=>0.054
      ==> gave -log_like_val of -1442.11[0m
      [33mINFO: Parameters
      muc=>2.99
      b=>0.593
      non_dec=>0.298
      range_non_dec=>0.054
      ==> gave -log_like_val of -1442.605[0m
      [33mINFO: Parameters
      muc=>2.986
      b=>0.594
      non_dec=>0.298
      range_non_dec=>0.054
      ==> gave -log_like_val of -1442.665[0m
      [33mINFO: Parameters
      muc=>2.984
      b=>0.595
      non_dec=>0.298
      range_non_dec=>0.053
      ==> gave -log_like_val of -1442.681[0m
      [33mINFO: Parameters
      muc=>2.979
      b=>0.593
      non_dec=>0.298
      range_non_dec=>0.054
      ==> gave -log_like_val of -1442.689[0m
      [33mINFO: Parameters
      muc=>2.972
      b=>0.592
      non_dec=>0.298
      range_non_dec=>0.053
      ==> gave -log_like_val of -1442.702[0m
      [33mINFO: Parameters
      muc=>2.974
      b=>0.593
      non_dec=>0.299
      range_non_dec=>0.053
      ==> gave -log_like_val of -1442.741[0m
      [33mINFO: Parameters
      muc=>2.966
      b=>0.593
      non_dec=>0.299
      range_non_dec=>0.053
      ==> gave -log_like_val of -1442.771[0m
      [33mINFO: Parameters
      muc=>2.97
      b=>0.594
      non_dec=>0.299
      range_non_dec=>0.053
      ==> gave -log_like_val of -1442.748[0m
      [33mINFO: Parameters
      muc=>2.959
      b=>0.593
      non_dec=>0.299
      range_non_dec=>0.052
      ==> gave -log_like_val of -1442.782[0m
      [33mINFO: Parameters
      muc=>2.945
      b=>0.593
      non_dec=>0.3
      range_non_dec=>0.051
      ==> gave -log_like_val of -1442.763[0m
      [33mINFO: Parameters
      muc=>2.95
      b=>0.591
      non_dec=>0.299
      range_non_dec=>0.052
      ==> gave -log_like_val of -1442.779[0m
      [33mINFO: Parameters
      muc=>2.951
      b=>0.594
      non_dec=>0.3
      range_non_dec=>0.051
      ==> gave -log_like_val of -1442.708[0m
      [33mINFO: Parameters
      muc=>2.956
      b=>0.594
      non_dec=>0.299
      range_non_dec=>0.052
      ==> gave -log_like_val of -1442.764[0m
      [33mINFO: Parameters
      muc=>2.945
      b=>0.591
      non_dec=>0.299
      range_non_dec=>0.052
      ==> gave -log_like_val of -1442.776[0m
      [33mINFO: Parameters
      muc=>2.954
      b=>0.591
      non_dec=>0.299
      range_non_dec=>0.052
      ==> gave -log_like_val of -1442.763[0m
      [33mINFO: Parameters
      muc=>2.956
      b=>0.593
      non_dec=>0.299
      range_non_dec=>0.052
      ==> gave -log_like_val of -1442.781[0m
      [33mINFO: Parameters
      muc=>2.939
      b=>0.591
      non_dec=>0.3
      range_non_dec=>0.051
      ==> gave -log_like_val of -1442.756[0m
      [33mINFO: Parameters
      muc=>2.959
      b=>0.593
      non_dec=>0.299
      range_non_dec=>0.052
      ==> gave -log_like_val of -1442.784[0m
      [33mINFO: Parameters
      muc=>2.967
      b=>0.594
      non_dec=>0.299
      range_non_dec=>0.052
      ==> gave -log_like_val of -1442.767[0m
      [33mINFO: Parameters
      muc=>2.951
      b=>0.592
      non_dec=>0.299
      range_non_dec=>0.052
      ==> gave -log_like_val of -1442.785[0m
      [33mINFO: Parameters
      muc=>2.963
      b=>0.594
      non_dec=>0.299
      range_non_dec=>0.052
      ==> gave -log_like_val of -1442.759[0m
      [33mINFO: Parameters
      muc=>2.953
      b=>0.592
      non_dec=>0.299
      range_non_dec=>0.052
      ==> gave -log_like_val of -1442.786[0m
      [33mINFO: Parameters
      muc=>2.956
      b=>0.592
      non_dec=>0.299
      range_non_dec=>0.052
      ==> gave -log_like_val of -1442.785[0m
      [33mINFO: Parameters
      muc=>2.95
      b=>0.591
      non_dec=>0.299
      range_non_dec=>0.052
      ==> gave -log_like_val of -1442.779[0m
      [33mINFO: Parameters
      muc=>2.957
      b=>0.593
      non_dec=>0.299
      range_non_dec=>0.052
      ==> gave -log_like_val of -1442.786[0m
      [33mINFO: Parameters
      muc=>2.949
      b=>0.592
      non_dec=>0.299
      range_non_dec=>0.052
      ==> gave -log_like_val of -1442.781[0m
      [33mINFO: Parameters
      muc=>2.957
      b=>0.592
      non_dec=>0.299
      range_non_dec=>0.052
      ==> gave -log_like_val of -1442.787[0m
      [33mINFO: Parameters
      muc=>2.953
      b=>0.593
      non_dec=>0.299
      range_non_dec=>0.052
      ==> gave -log_like_val of -1442.784[0m
      [33mINFO: Parameters
      muc=>2.955
      b=>0.592
      non_dec=>0.299
      range_non_dec=>0.052
      ==> gave -log_like_val of -1442.787[0m
      [33mINFO: Parameters
      muc=>2.96
      b=>0.593
      non_dec=>0.299
      range_non_dec=>0.052
      ==> gave -log_like_val of -1442.784[0m
      [33mINFO: Parameters
      muc=>2.953
      b=>0.592
      non_dec=>0.299
      range_non_dec=>0.052
      ==> gave -log_like_val of -1442.787[0m
      [33mINFO: Parameters
      muc=>2.952
      b=>0.592
      non_dec=>0.299
      range_non_dec=>0.052
      ==> gave -log_like_val of -1442.785[0m
      [33mINFO: Parameters
      muc=>2.956
      b=>0.592
      non_dec=>0.299
      range_non_dec=>0.052
      ==> gave -log_like_val of -1442.787[0m
      [33mINFO: Parameters
      muc=>2.957
      b=>0.593
      non_dec=>0.299
      range_non_dec=>0.052
      ==> gave -log_like_val of -1442.786[0m
      [33mINFO: Parameters
      muc=>2.954
      b=>0.592
      non_dec=>0.299
      range_non_dec=>0.052
      ==> gave -log_like_val of -1442.787[0m
      [33mINFO: Parameters
      muc=>2.955
      b=>0.593
      non_dec=>0.299
      range_non_dec=>0.052
      ==> gave -log_like_val of -1442.787[0m
      [33mINFO: Parameters
      muc=>2.955
      b=>0.592
      non_dec=>0.299
      range_non_dec=>0.052
      ==> gave -log_like_val of -1442.787[0m
      [33mINFO: Parameters
      muc=>2.958
      b=>0.593
      non_dec=>0.299
      range_non_dec=>0.052
      ==> gave -log_like_val of -1442.786[0m
      [33mINFO: Parameters
      muc=>2.954
      b=>0.592
      non_dec=>0.299
      range_non_dec=>0.052
      ==> gave -log_like_val of -1442.787[0m
      [33mINFO: Parameters
      muc=>2.953
      b=>0.592
      non_dec=>0.299
      range_non_dec=>0.052
      ==> gave -log_like_val of -1442.787[0m
      [33mINFO: Parameters
      muc=>2.956
      b=>0.592
      non_dec=>0.299
      range_non_dec=>0.052
      ==> gave -log_like_val of -1442.787[0m
      [33mINFO: Parameters
      muc=>2.956
      b=>0.593
      non_dec=>0.299
      range_non_dec=>0.052
      ==> gave -log_like_val of -1442.787[0m
      [33mINFO: Parameters
      muc=>2.955
      b=>0.592
      non_dec=>0.299
      range_non_dec=>0.052
      ==> gave -log_like_val of -1442.787[0m
      [33mINFO: Parameters
      muc=>2.955
      b=>0.592
      non_dec=>0.299
      range_non_dec=>0.052
      ==> gave -log_like_val of -1442.787[0m
      [33mINFO: Parameters
      muc=>2.954
      b=>0.592
      non_dec=>0.299
      range_non_dec=>0.052
      ==> gave -log_like_val of -1442.787[0m
      [33mINFO: Parameters
      muc=>2.955
      b=>0.592
      non_dec=>0.299
      range_non_dec=>0.052
      ==> gave -log_like_val of -1442.787[0m
      [33mINFO: Parameters
      muc=>2.954
      b=>0.592
      non_dec=>0.299
      range_non_dec=>0.052
      ==> gave -log_like_val of -1442.787[0m
      [33mINFO: Parameters
      muc=>2.955
      b=>0.592
      non_dec=>0.299
      range_non_dec=>0.052
      ==> gave -log_like_val of -1442.787[0m
      [33mINFO: Parameters
      muc=>2.956
      b=>0.592
      non_dec=>0.299
      range_non_dec=>0.052
      ==> gave -log_like_val of -1442.787[0m
      [33mINFO: Parameters
      muc=>2.955
      b=>0.592
      non_dec=>0.299
      range_non_dec=>0.052
      ==> gave -log_like_val of -1442.787[0m
      [33mINFO: Parameters
      muc=>2.955
      b=>0.592
      non_dec=>0.299
      range_non_dec=>0.052
      ==> gave -log_like_val of -1442.787[0m
      [33mINFO: Parameters
      muc=>2.955
      b=>0.592
      non_dec=>0.299
      range_non_dec=>0.052
      ==> gave -log_like_val of -1442.787[0m
      [33mINFO: Parameters
      muc=>2.955
      b=>0.592
      non_dec=>0.299
      range_non_dec=>0.052
      ==> gave -log_like_val of -1442.787[0m
      [33mINFO: Parameters
      muc=>2.955
      b=>0.592
      non_dec=>0.299
      range_non_dec=>0.052
      ==> gave -log_like_val of -1442.787[0m
      [33mINFO: Parameters
      muc=>2.955
      b=>0.592
      non_dec=>0.299
      range_non_dec=>0.052
      ==> gave -log_like_val of -1442.787[0m
      [33mINFO: Parameters
      muc=>2.955
      b=>0.592
      non_dec=>0.299
      range_non_dec=>0.052
      ==> gave -log_like_val of -1442.787[0m
      [33mINFO: Parameters
      muc=>2.955
      b=>0.592
      non_dec=>0.299
      range_non_dec=>0.052
      ==> gave -log_like_val of -1442.787[0m
      [33mINFO: Parameters
      muc=>2.955
      b=>0.592
      non_dec=>0.299
      range_non_dec=>0.052
      ==> gave -log_like_val of -1442.787[0m
      [33mINFO: Parameters
      muc=>2.955
      b=>0.592
      non_dec=>0.299
      range_non_dec=>0.052
      ==> gave -log_like_val of -1442.787[0m
      [33mINFO: Parameters
      muc=>2.955
      b=>0.592
      non_dec=>0.299
      range_non_dec=>0.052
      ==> gave -log_like_val of -1442.787[0m
      [33mINFO: Parameters
      muc=>2.955
      b=>0.592
      non_dec=>0.299
      range_non_dec=>0.052
      ==> gave -log_like_val of -1442.787[0m
      [33mINFO: Parameters
      muc=>2.955
      b=>0.592
      non_dec=>0.299
      range_non_dec=>0.052
      ==> gave -log_like_val of -1442.787[0m
      [33mINFO: Parameters
      muc=>2.955
      b=>0.592
      non_dec=>0.299
      range_non_dec=>0.052
      ==> gave -log_like_val of -1442.787[0m
      [33mINFO: Parameters
      muc=>2.955
      b=>0.592
      non_dec=>0.299
      range_non_dec=>0.052
      ==> gave -log_like_val of -1442.787[0m
      [33mINFO: Parameters
      muc=>2.955
      b=>0.592
      non_dec=>0.299
      range_non_dec=>0.052
      ==> gave -log_like_val of -1442.787[0m
      [33mINFO: Parameters
      muc=>2.955
      b=>0.592
      non_dec=>0.299
      range_non_dec=>0.052
      ==> gave -log_like_val of -1442.787[0m
      [33mINFO: Parameters
      muc=>2.955
      b=>0.592
      non_dec=>0.299
      range_non_dec=>0.052
      ==> gave -log_like_val of -1442.787[0m
      [33mINFO: Parameters
      muc=>2.955
      b=>0.592
      non_dec=>0.299
      range_non_dec=>0.052
      ==> gave -log_like_val of -1442.787[0m
      [33mINFO: Parameters
      muc=>2.955
      b=>0.592
      non_dec=>0.299
      range_non_dec=>0.052
      ==> gave -log_like_val of -1442.787[0m
      [33mINFO: Parameters
      muc=>2.955
      b=>0.592
      non_dec=>0.299
      range_non_dec=>0.052
      ==> gave -log_like_val of -1442.787[0m
      [33mINFO: Parameters
      muc=>2.955
      b=>0.592
      non_dec=>0.299
      range_non_dec=>0.052
      ==> gave -log_like_val of -1442.787[0m
      [33mINFO: Parameters
      muc=>2.955
      b=>0.592
      non_dec=>0.299
      range_non_dec=>0.052
      ==> gave -log_like_val of -1442.787[0m
      [33mINFO: Parameters
      muc=>2.955
      b=>0.592
      non_dec=>0.299
      range_non_dec=>0.052
      ==> gave -log_like_val of -1442.787[0m
      Class(es): ratcliff_dm, drift_dm
      
      Current Parameter Matrix:
             muc     b non_dec range_non_dec
      null 2.955 0.592   0.299         0.052
      
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
      Iteration: 1 bestvalit: -581.259603 bestmemit:    4.147303    0.314631
      Iteration: 2 bestvalit: -581.259603 bestmemit:    4.147303    0.314631
      Iteration: 3 bestvalit: -581.259603 bestmemit:    4.147303    0.314631
      Iteration: 4 bestvalit: -581.259603 bestmemit:    4.147303    0.314631
      Iteration: 5 bestvalit: -586.258657 bestmemit:    4.011332    0.303037
      Iteration: 6 bestvalit: -586.258657 bestmemit:    4.011332    0.303037
      Iteration: 7 bestvalit: -586.258657 bestmemit:    4.011332    0.303037
      Iteration: 8 bestvalit: -587.038018 bestmemit:    4.053700    0.305854
      Iteration: 9 bestvalit: -587.526129 bestmemit:    4.159118    0.305854
      Iteration: 10 bestvalit: -587.526129 bestmemit:    4.159118    0.305854
      Iteration: 11 bestvalit: -587.526129 bestmemit:    4.159118    0.305854
      Iteration: 12 bestvalit: -587.526129 bestmemit:    4.159118    0.305854
      Iteration: 13 bestvalit: -587.526129 bestmemit:    4.159118    0.305854
      Iteration: 14 bestvalit: -587.526129 bestmemit:    4.159118    0.305854
      Iteration: 15 bestvalit: -587.526129 bestmemit:    4.159118    0.305854
      Iteration: 16 bestvalit: -587.536456 bestmemit:    4.220199    0.306252
      Iteration: 17 bestvalit: -587.572096 bestmemit:    4.209832    0.307147
      Iteration: 18 bestvalit: -587.583207 bestmemit:    4.184888    0.306767
      Iteration: 19 bestvalit: -587.587134 bestmemit:    4.200942    0.306718
      Iteration: 20 bestvalit: -587.587134 bestmemit:    4.200942    0.306718
      Iteration: 21 bestvalit: -587.587134 bestmemit:    4.200942    0.306718
      Iteration: 22 bestvalit: -587.587134 bestmemit:    4.200942    0.306718
      Iteration: 23 bestvalit: -587.587710 bestmemit:    4.197117    0.306652
      Iteration: 24 bestvalit: -587.587793 bestmemit:    4.198219    0.306697
      Iteration: 25 bestvalit: -587.587793 bestmemit:    4.198219    0.306697
      Iteration: 26 bestvalit: -587.587993 bestmemit:    4.194349    0.306657
      Iteration: 27 bestvalit: -587.588018 bestmemit:    4.193857    0.306697
      Iteration: 28 bestvalit: -587.588073 bestmemit:    4.195461    0.306702
      Iteration: 29 bestvalit: -587.588073 bestmemit:    4.195461    0.306702
      Iteration: 30 bestvalit: -587.588073 bestmemit:    4.195461    0.306702
      Iteration: 31 bestvalit: -587.588073 bestmemit:    4.195461    0.306702
      Iteration: 32 bestvalit: -587.588073 bestmemit:    4.195461    0.306702
      Iteration: 33 bestvalit: -587.588073 bestmemit:    4.195461    0.306702
      Iteration: 34 bestvalit: -587.588079 bestmemit:    4.195302    0.306697
      Iteration: 35 bestvalit: -587.588079 bestmemit:    4.195302    0.306697
      Iteration: 36 bestvalit: -587.588079 bestmemit:    4.195302    0.306697
      Iteration: 37 bestvalit: -587.588080 bestmemit:    4.194953    0.306692
      Iteration: 38 bestvalit: -587.588080 bestmemit:    4.194953    0.306692
      Iteration: 39 bestvalit: -587.588081 bestmemit:    4.195048    0.306690
      Iteration: 40 bestvalit: -587.588081 bestmemit:    4.195048    0.306690
      Iteration: 41 bestvalit: -587.588081 bestmemit:    4.195048    0.306690
      Iteration: 42 bestvalit: -587.588081 bestmemit:    4.195165    0.306692
      Iteration: 43 bestvalit: -587.588081 bestmemit:    4.195165    0.306692
      Iteration: 44 bestvalit: -587.588081 bestmemit:    4.195165    0.306692
      Iteration: 45 bestvalit: -587.588081 bestmemit:    4.195165    0.306692
      Iteration: 46 bestvalit: -587.588081 bestmemit:    4.195134    0.306692
      Iteration: 47 bestvalit: -587.588081 bestmemit:    4.195134    0.306692
      Iteration: 48 bestvalit: -587.588081 bestmemit:    4.195134    0.306692
      Iteration: 49 bestvalit: -587.588081 bestmemit:    4.195134    0.306692
      Iteration: 50 bestvalit: -587.588081 bestmemit:    4.195134    0.306692
      Iteration: 51 bestvalit: -587.588081 bestmemit:    4.195134    0.306692
      Iteration: 52 bestvalit: -587.588081 bestmemit:    4.195134    0.306692
      Iteration: 53 bestvalit: -587.588081 bestmemit:    4.195121    0.306692
      Iteration: 54 bestvalit: -587.588081 bestmemit:    4.195121    0.306692
      Iteration: 55 bestvalit: -587.588081 bestmemit:    4.195121    0.306692
      Iteration: 56 bestvalit: -587.588081 bestmemit:    4.195131    0.306692
      Iteration: 57 bestvalit: -587.588081 bestmemit:    4.195131    0.306692
      Iteration: 58 bestvalit: -587.588081 bestmemit:    4.195127    0.306692
      Iteration: 59 bestvalit: -587.588081 bestmemit:    4.195127    0.306692
      Iteration: 60 bestvalit: -587.588081 bestmemit:    4.195130    0.306692
      Iteration: 61 bestvalit: -587.588081 bestmemit:    4.195128    0.306692
      Iteration: 62 bestvalit: -587.588081 bestmemit:    4.195128    0.306692
      Iteration: 63 bestvalit: -587.588081 bestmemit:    4.195129    0.306692
      Iteration: 64 bestvalit: -587.588081 bestmemit:    4.195129    0.306692
      Iteration: 65 bestvalit: -587.588081 bestmemit:    4.195130    0.306692
      Iteration: 66 bestvalit: -587.588081 bestmemit:    4.195130    0.306692
      Iteration: 67 bestvalit: -587.588081 bestmemit:    4.195130    0.306692
      Iteration: 68 bestvalit: -587.588081 bestmemit:    4.195129    0.306692
      Iteration: 69 bestvalit: -587.588081 bestmemit:    4.195129    0.306692
      Iteration: 70 bestvalit: -587.588081 bestmemit:    4.195129    0.306692
      Iteration: 71 bestvalit: -587.588081 bestmemit:    4.195129    0.306692
      Iteration: 72 bestvalit: -587.588081 bestmemit:    4.195129    0.306692
      Iteration: 73 bestvalit: -587.588081 bestmemit:    4.195129    0.306692
      Iteration: 74 bestvalit: -587.588081 bestmemit:    4.195129    0.306692
      Iteration: 75 bestvalit: -587.588081 bestmemit:    4.195129    0.306692
      Iteration: 76 bestvalit: -587.588081 bestmemit:    4.195129    0.306692
      Iteration: 77 bestvalit: -587.588081 bestmemit:    4.195129    0.306692
      Iteration: 78 bestvalit: -587.588081 bestmemit:    4.195129    0.306692
      Iteration: 79 bestvalit: -587.588081 bestmemit:    4.195129    0.306692
      Iteration: 80 bestvalit: -587.588081 bestmemit:    4.195129    0.306692
      Iteration: 81 bestvalit: -587.588081 bestmemit:    4.195129    0.306692
      Iteration: 82 bestvalit: -587.588081 bestmemit:    4.195129    0.306692
      Iteration: 83 bestvalit: -587.588081 bestmemit:    4.195129    0.306692
      Iteration: 84 bestvalit: -587.588081 bestmemit:    4.195129    0.306692
      Iteration: 85 bestvalit: -587.588081 bestmemit:    4.195129    0.306692
      Iteration: 86 bestvalit: -587.588081 bestmemit:    4.195129    0.306692
      Iteration: 87 bestvalit: -587.588081 bestmemit:    4.195129    0.306692
      Iteration: 88 bestvalit: -587.588081 bestmemit:    4.195129    0.306692
      Iteration: 89 bestvalit: -587.588081 bestmemit:    4.195129    0.306692
      Iteration: 90 bestvalit: -587.588081 bestmemit:    4.195129    0.306692
      [33mINFO: Parameters
      muc=>4.195
      non_dec=>0.307
      ==> gave -log_like_val of -587.5881[0m
      Class(es): dmc_dm, drift_dm
      
      Current Parameter Matrix:
               muc   b non_dec sd_non_dec  tau a    A alpha
      comp   4.195 0.6   0.307       0.02 0.04 2  0.1     4
      incomp 4.195 0.6   0.307       0.02 0.04 2 -0.1     4
      
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
      Iteration: 1 bestvalit: -581.259603 bestmemit:    4.147303    0.314631
      Iteration: 2 bestvalit: -581.259603 bestmemit:    4.147303    0.314631
      Iteration: 3 bestvalit: -581.259603 bestmemit:    4.147303    0.314631
      Iteration: 4 bestvalit: -581.259603 bestmemit:    4.147303    0.314631
      Iteration: 5 bestvalit: -586.258657 bestmemit:    4.011332    0.303037
      Iteration: 6 bestvalit: -586.258657 bestmemit:    4.011332    0.303037
      Iteration: 7 bestvalit: -586.258657 bestmemit:    4.011332    0.303037
      Iteration: 8 bestvalit: -587.038018 bestmemit:    4.053700    0.305854
      Iteration: 9 bestvalit: -587.526129 bestmemit:    4.159118    0.305854
      Iteration: 10 bestvalit: -587.526129 bestmemit:    4.159118    0.305854
      Iteration: 11 bestvalit: -587.526129 bestmemit:    4.159118    0.305854
      Iteration: 12 bestvalit: -587.526129 bestmemit:    4.159118    0.305854
      Iteration: 13 bestvalit: -587.526129 bestmemit:    4.159118    0.305854
      Iteration: 14 bestvalit: -587.526129 bestmemit:    4.159118    0.305854
      Iteration: 15 bestvalit: -587.526129 bestmemit:    4.159118    0.305854
      Iteration: 16 bestvalit: -587.536456 bestmemit:    4.220199    0.306252
      Iteration: 17 bestvalit: -587.572096 bestmemit:    4.209832    0.307147
      Iteration: 18 bestvalit: -587.583207 bestmemit:    4.184888    0.306767
      Iteration: 19 bestvalit: -587.587134 bestmemit:    4.200942    0.306718
      Iteration: 20 bestvalit: -587.587134 bestmemit:    4.200942    0.306718
      Iteration: 21 bestvalit: -587.587134 bestmemit:    4.200942    0.306718
      Iteration: 22 bestvalit: -587.587134 bestmemit:    4.200942    0.306718
      Iteration: 23 bestvalit: -587.587710 bestmemit:    4.197117    0.306652
      Iteration: 24 bestvalit: -587.587793 bestmemit:    4.198219    0.306697
      Iteration: 25 bestvalit: -587.587793 bestmemit:    4.198219    0.306697
      Iteration: 26 bestvalit: -587.587993 bestmemit:    4.194349    0.306657
      Iteration: 27 bestvalit: -587.588018 bestmemit:    4.193857    0.306697
      Iteration: 28 bestvalit: -587.588073 bestmemit:    4.195461    0.306702
      Iteration: 29 bestvalit: -587.588073 bestmemit:    4.195461    0.306702
      Iteration: 30 bestvalit: -587.588073 bestmemit:    4.195461    0.306702
      Iteration: 31 bestvalit: -587.588073 bestmemit:    4.195461    0.306702
      Iteration: 32 bestvalit: -587.588073 bestmemit:    4.195461    0.306702
      Iteration: 33 bestvalit: -587.588073 bestmemit:    4.195461    0.306702
      Iteration: 34 bestvalit: -587.588079 bestmemit:    4.195302    0.306697
      Iteration: 35 bestvalit: -587.588079 bestmemit:    4.195302    0.306697
      Iteration: 36 bestvalit: -587.588079 bestmemit:    4.195302    0.306697
      Iteration: 37 bestvalit: -587.588080 bestmemit:    4.194953    0.306692
      Iteration: 38 bestvalit: -587.588080 bestmemit:    4.194953    0.306692
      Iteration: 39 bestvalit: -587.588081 bestmemit:    4.195048    0.306690
      Iteration: 40 bestvalit: -587.588081 bestmemit:    4.195048    0.306690
      Iteration: 41 bestvalit: -587.588081 bestmemit:    4.195048    0.306690
      Iteration: 42 bestvalit: -587.588081 bestmemit:    4.195165    0.306692
      Iteration: 43 bestvalit: -587.588081 bestmemit:    4.195165    0.306692
      Iteration: 44 bestvalit: -587.588081 bestmemit:    4.195165    0.306692
      Iteration: 45 bestvalit: -587.588081 bestmemit:    4.195165    0.306692
      Iteration: 46 bestvalit: -587.588081 bestmemit:    4.195134    0.306692
      Iteration: 47 bestvalit: -587.588081 bestmemit:    4.195134    0.306692
      Iteration: 48 bestvalit: -587.588081 bestmemit:    4.195134    0.306692
      Iteration: 49 bestvalit: -587.588081 bestmemit:    4.195134    0.306692
      Iteration: 50 bestvalit: -587.588081 bestmemit:    4.195134    0.306692
      Iteration: 51 bestvalit: -587.588081 bestmemit:    4.195134    0.306692
      Iteration: 52 bestvalit: -587.588081 bestmemit:    4.195134    0.306692
      Iteration: 53 bestvalit: -587.588081 bestmemit:    4.195121    0.306692
      Iteration: 54 bestvalit: -587.588081 bestmemit:    4.195121    0.306692
      Iteration: 55 bestvalit: -587.588081 bestmemit:    4.195121    0.306692
      Iteration: 56 bestvalit: -587.588081 bestmemit:    4.195131    0.306692
      Iteration: 57 bestvalit: -587.588081 bestmemit:    4.195131    0.306692
      Iteration: 58 bestvalit: -587.588081 bestmemit:    4.195127    0.306692
      Iteration: 59 bestvalit: -587.588081 bestmemit:    4.195127    0.306692
      Iteration: 60 bestvalit: -587.588081 bestmemit:    4.195130    0.306692
      Iteration: 61 bestvalit: -587.588081 bestmemit:    4.195128    0.306692
      Iteration: 62 bestvalit: -587.588081 bestmemit:    4.195128    0.306692
      Iteration: 63 bestvalit: -587.588081 bestmemit:    4.195129    0.306692
      Iteration: 64 bestvalit: -587.588081 bestmemit:    4.195129    0.306692
      Iteration: 65 bestvalit: -587.588081 bestmemit:    4.195130    0.306692
      Iteration: 66 bestvalit: -587.588081 bestmemit:    4.195130    0.306692
      Iteration: 67 bestvalit: -587.588081 bestmemit:    4.195130    0.306692
      Iteration: 68 bestvalit: -587.588081 bestmemit:    4.195129    0.306692
      Iteration: 69 bestvalit: -587.588081 bestmemit:    4.195129    0.306692
      Iteration: 70 bestvalit: -587.588081 bestmemit:    4.195129    0.306692
      Iteration: 71 bestvalit: -587.588081 bestmemit:    4.195129    0.306692
      Iteration: 72 bestvalit: -587.588081 bestmemit:    4.195129    0.306692
      Iteration: 73 bestvalit: -587.588081 bestmemit:    4.195129    0.306692
      Iteration: 74 bestvalit: -587.588081 bestmemit:    4.195129    0.306692
      Iteration: 75 bestvalit: -587.588081 bestmemit:    4.195129    0.306692
      Iteration: 76 bestvalit: -587.588081 bestmemit:    4.195129    0.306692
      Iteration: 77 bestvalit: -587.588081 bestmemit:    4.195129    0.306692
      Iteration: 78 bestvalit: -587.588081 bestmemit:    4.195129    0.306692
      Iteration: 79 bestvalit: -587.588081 bestmemit:    4.195129    0.306692
      Iteration: 80 bestvalit: -587.588081 bestmemit:    4.195129    0.306692
      Iteration: 81 bestvalit: -587.588081 bestmemit:    4.195129    0.306692
      Iteration: 82 bestvalit: -587.588081 bestmemit:    4.195129    0.306692
      Iteration: 83 bestvalit: -587.588081 bestmemit:    4.195129    0.306692
      Iteration: 84 bestvalit: -587.588081 bestmemit:    4.195129    0.306692
      Iteration: 85 bestvalit: -587.588081 bestmemit:    4.195129    0.306692
      Iteration: 86 bestvalit: -587.588081 bestmemit:    4.195129    0.306692
      Iteration: 87 bestvalit: -587.588081 bestmemit:    4.195129    0.306692
      Iteration: 88 bestvalit: -587.588081 bestmemit:    4.195129    0.306692
      Iteration: 89 bestvalit: -587.588081 bestmemit:    4.195129    0.306692
      Iteration: 90 bestvalit: -587.588081 bestmemit:    4.195129    0.306692
      [33mINFO: Parameters
      muc=>4.195
      non_dec=>0.307
      ==> gave -log_like_val of -587.5881[0m
      Class(es): dmc_dm, drift_dm
      
      Current Parameter Matrix:
               muc   b non_dec sd_non_dec  tau a    A alpha
      comp   4.195 0.6   0.307       0.02 0.04 2  0.1     4
      incomp 4.195 0.6   0.307       0.02 0.04 2 -0.1     4
      
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

