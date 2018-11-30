
#### *Marcus W. Beck, marcusb@sccwrp.org, Raphael D. Mazor, raphaelm@sccwrp.org, Mark Engeln*

# PHABMetrics

[![Travis-CI Build Status](https://travis-ci.org/SCCWRP/PHABMetrics.svg?branch=master)](https://travis-ci.org/SCCWRP/PHABMetrics)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/SCCWRP/PHABMetrics?branch=master&svg=true)](https://ci.appveyor.com/project/SCCWRP/PHABMetrics)

This package provides functions to calculate PHAB metrics using field data.

# Installing the package

The development version of this package can be installed from Github:


```r
install.packages('devtools')
library(devtools)
install_github('SCCWRP/PHABMetrics')
library(PHABMetrics)
```

# Usage

The core  function is `phabmetrics()`.



```r
alldat <- phabmetrics(sampdat)
alldat
```

```
##                     XBKF_H.result XBKF_H.count XBKF_H.sd XKBF_W.result
## 402M00002_8/13/2015     0.6590909           11 0.0202260     15.545455
## SMC00710_5/20/2009      0.4918182           77 0.7186184      6.130909
## SMC00957_6/21/2010      0.3581818           22 0.1206189      3.029091
## SMC04383_6/9/2010       0.8645455           22 0.1197111     18.681818
##                     XKBF_W.count XKBF_W.sd XWDEPTH.result XWDEPTH.count
## 402M00002_8/13/2015           11 2.8762349      33.845361            97
## SMC00710_5/20/2009            77 1.7371500       7.733333           735
## SMC00957_6/21/2010            22 0.8329232       5.116667           240
## SMC04383_6/9/2010             22 5.4864019      14.600000           210
##                     XWDEPTH.sd XWIDTH.result XWIDTH.count XWIDTH.sd
## 402M00002_8/13/2015   24.49036      9.326190           21 16.252179
## SMC00710_5/20/2009    20.43468      3.179048          147 17.316949
## SMC00957_6/21/2010    22.07518      1.457083           48  8.350054
## SMC04383_6/9/2010     18.81383     10.595238           42 16.582599
##                     XWDR.result XWDR.count XWDA.result XWDA.count
## 402M00002_8/13/2015    27.55530         21   0.3629066         97
## SMC00710_5/20/2009     41.10837        147   0.2432594        735
## SMC00957_6/21/2010     28.47720         48   0.3511581        240
## SMC04383_6/9/2010      72.57012         42   0.1377978        210
##                     XWDM.result XWDM.count PCT_CF.result PCT_CF.count
## 402M00002_8/13/2015    60.17647         17           0.0           10
## SMC00710_5/20/2009     14.21429         42           0.0           70
## SMC00957_6/21/2010      9.47619         21           0.0           20
## SMC04383_6/9/2010      31.09524         21           1.5           20
##                     PCT_CF.sd PCT_DR.result PCT_DR.count PCT_DR.sd
## 402M00002_8/13/2015 0.0000000             0           10         0
## SMC00710_5/20/2009  0.0000000             0           70         0
## SMC00957_6/21/2010  0.0000000             0           20         0
## SMC04383_6/9/2010   0.3077935             0           20         0
##                     PCT_GL.result PCT_GL.count PCT_GL.sd PCT_POOL.result
## 402M00002_8/13/2015            38           10  4.817791            58.0
## SMC00710_5/20/2009             64           70  3.947481             4.5
## SMC00957_6/21/2010             55           20  6.190825            29.5
## SMC04383_6/9/2010              19           20  3.079645             4.5
##                     PCT_POOL.count PCT_POOL.sd PCT_RA.result PCT_RA.count
## 402M00002_8/13/2015             10    3.653005             0           10
## SMC00710_5/20/2009              70    1.410108             0           70
## SMC00957_6/21/2010              20    4.459172             0           20
## SMC04383_6/9/2010               20    1.436370             0           20
##                     PCT_RA.sd PCT_RI.result PCT_RI.count PCT_RI.sd
## 402M00002_8/13/2015         0           4.0           10 0.9660918
## SMC00710_5/20/2009          0          23.5           70 2.2273010
## SMC00957_6/21/2010          0           0.0           20 0.0000000
## SMC04383_6/9/2010           0          73.0           20 2.5567249
##                     PCT_RN.result PCT_RN.count PCT_RN.sd PCT_FAST.result
## 402M00002_8/13/2015           0.0           10  0.000000             4.0
## SMC00710_5/20/2009            8.0           70  2.526096            31.5
## SMC00957_6/21/2010           15.5           20  1.450953            15.5
## SMC04383_6/9/2010             2.0           20  2.430075            76.5
##                     PCT_SLOW.result PCT_CF_WT.result PCT_CF_WT.count
## 402M00002_8/13/2015            96.0                0              10
## SMC00710_5/20/2009             68.5                0              70
## SMC00957_6/21/2010             84.5                0              20
## SMC04383_6/9/2010              23.5                3              20
##                     PCT_GL_WT.result PCT_GL_WT.count PCT_POOL_WT.result
## 402M00002_8/13/2015               38              10               58.0
## SMC00710_5/20/2009               448              70               31.5
## SMC00957_6/21/2010               110              20               59.0
## SMC04383_6/9/2010                 38              20                9.0
##                     PCT_POOL_WT.count PCT_RA_WT.result PCT_RA_WT.count
## 402M00002_8/13/2015                10                0              10
## SMC00710_5/20/2009                 70                0              70
## SMC00957_6/21/2010                 20                0              20
## SMC04383_6/9/2010                  20                0              20
##                     PCT_RI_WT.result PCT_RI_WT.count PCT_RN_WT.result
## 402M00002_8/13/2015              4.0              10                0
## SMC00710_5/20/2009             164.5              70               56
## SMC00957_6/21/2010               0.0              20               31
## SMC04383_6/9/2010              146.0              20                4
##                     PCT_RN_WT.count PCT_FAST_WT.result PCT_SLOW_WT.result
## 402M00002_8/13/2015              10                4.0               96.0
## SMC00710_5/20/2009               70              220.5              479.5
## SMC00957_6/21/2010               20               31.0              169.0
## SMC04383_6/9/2010                20              153.0               47.0
##                     XSLOPE.result XSLOPE.count   XSLOPE.sd SLOPE_0.result
## 402M00002_8/13/2015   0.009090909           11 0.003015113         0.0000
## SMC00710_5/20/2009    0.000000000           12 0.000000000       666.6667
## SMC00957_6/21/2010    0.000000000           11 0.000000000       666.6667
## SMC04383_6/9/2010     0.000000000           11 0.000000000       666.6667
##                     SLOPE_0.count SLOPE_0_5.result SLOPE_0_5.count
## 402M00002_8/13/2015            10         666.6667              10
## SMC00710_5/20/2009             10           0.0000              10
## SMC00957_6/21/2010             10           0.0000              10
## SMC04383_6/9/2010              10           0.0000              10
##                     SLOPE_1.result SLOPE_1.count SLOPE_2.result
## 402M00002_8/13/2015       666.6667            10       666.6667
## SMC00710_5/20/2009          0.0000            10         0.0000
## SMC00957_6/21/2010          0.0000            10         0.0000
## SMC04383_6/9/2010           0.0000            10         0.0000
##                     SLOPE_2.count XBEARING.result XBEARING.count
## 402M00002_8/13/2015            10     0.009090909             11
## SMC00710_5/20/2009             10     0.007500000             12
## SMC00957_6/21/2010             10     0.009090909             11
## SMC04383_6/9/2010              10     0.009090909             11
##                     XBEARING.sd SINU.NOT_WORKING XCDENMID.result
## 402M00002_8/13/2015 0.003015113         28.64934        77.00535
## SMC00710_5/20/2009  0.004522670         31.83260        84.75936
## SMC00957_6/21/2010  0.003015113         28.64934        53.87701
## SMC04383_6/9/2010   0.003015113         28.64934        50.00000
##                     XCDENMID.count XCDENMID.sd XCDENBK.result
## 402M00002_8/13/2015             44    35.08415             NA
## SMC00710_5/20/2009              88    18.59386             NA
## SMC00957_6/21/2010              44    34.32184             NA
## SMC04383_6/9/2010               44    35.09978             NA
##                     XCDENBK.count XCDENBK.sd XFC_AQM.result XFC_AQM.count
## 402M00002_8/13/2015             0         NA      22.954545            11
## SMC00710_5/20/2009              0         NA       3.636364            77
## SMC00957_6/21/2010              0         NA      32.954545            22
## SMC04383_6/9/2010               0         NA       4.545455            22
##                     XFC_AQM.sd XFC_HUM.result XFC_HUM.count XFC_HUM.sd
## 402M00002_8/13/2015  23.500484      0.0000000            11   0.000000
## SMC00710_5/20/2009    2.241411      5.6818182            77  16.556504
## SMC00957_6/21/2010   35.670753      0.9090909            22   1.973855
## SMC04383_6/9/2010     1.471225      0.0000000            22   0.000000
##                     XFC_RCK.result XFC_RCK.count XFC_RCK.sd XFC_ALG.result
## 402M00002_8/13/2015           27.5            11   28.19574     12.2727273
## SMC00710_5/20/2009             0.0            77    0.00000     84.7727273
## SMC00957_6/21/2010             0.0            22    0.00000      0.4545455
## SMC04383_6/9/2010             62.5            22   22.75647     22.5000000
##                     XFC_ALG.count XFC_ALG.sd XFC_LWD.result XFC_LWD.count
## 402M00002_8/13/2015            11  10.090500      0.0000000            11
## SMC00710_5/20/2009             77   8.680948      0.4545455            77
## SMC00957_6/21/2010             22   1.471225      5.4545455            22
## SMC04383_6/9/2010              22  14.433757      0.0000000            22
##                     XFC_LWD.sd XFC_LTR.result XFC_LTR.count XFC_LTR.sd
## 402M00002_8/13/2015   0.000000      10.454545            11   9.341987
## SMC00710_5/20/2009    1.446825      20.681818            77  15.025397
## SMC00957_6/21/2010    9.625004       2.272727            22   7.356124
## SMC04383_6/9/2010     0.000000       4.545455            22   1.471225
##                     XFC_OHV.result XFC_OHV.count XFC_OHV.sd XFC_BRS.result
## 402M00002_8/13/2015      36.818182            11   16.39706      14.090909
## SMC00710_5/20/2009       28.409091            77   19.76386       5.000000
## SMC00957_6/21/2010       23.409091            22   23.53500       3.181818
## SMC04383_6/9/2010         8.636364            22    7.89542       5.000000
##                     XFC_BRS.count XFC_BRS.sd XFC_UCB.result XFC_UCB.count
## 402M00002_8/13/2015            11   10.44466      5.0000000            11
## SMC00710_5/20/2009             77    0.00000     16.5909091            77
## SMC00957_6/21/2010             22    7.32664      0.4545455            22
## SMC04383_6/9/2010              22    0.00000      0.0000000            22
##                     XFC_UCB.sd XFC_BIG.result XFC_NAT_EMAP.result
## 402M00002_8/13/2015   0.000000      32.500000            83.40909
## SMC00710_5/20/2009   16.352959      22.727273            50.45455
## SMC00957_6/21/2010    1.471225       6.818182            32.50000
## SMC04383_6/9/2010     0.000000      62.500000            76.13636
##                     XFC_NAT_SWAMP.result CFC_AQM.result CFC_HUM.result
## 402M00002_8/13/2015            116.81818             11              0
## SMC00710_5/20/2009              74.77273             56             14
## SMC00957_6/21/2010              67.72727             16              4
## SMC04383_6/9/2010               85.22727             20              0
##                     CFC_RCK.result CFC_ALG.result CFC_LWD.result
## 402M00002_8/13/2015             11             11              0
## SMC00710_5/20/2009               0             77              7
## SMC00957_6/21/2010               0              2              8
## SMC04383_6/9/2010               22             22              0
##                     CFC_LTR.result CFC_OHV.result CFC_BRS.result
## 402M00002_8/13/2015             11             11             11
## SMC00710_5/20/2009              77             77             77
## SMC00957_6/21/2010               2             16              6
## SMC04383_6/9/2010               20             22             22
##                     CFC_UCB.result CFC_ALL_EMAP.result
## 402M00002_8/13/2015             11                   6
## SMC00710_5/20/2009              70                   7
## SMC00957_6/21/2010               2                   7
## SMC04383_6/9/2010                0                   5
##                     CFC_ALL_SWAMP.result W1H_BRDG.result W1H_BRDG.count
## 402M00002_8/13/2015                    7               1             11
## SMC00710_5/20/2009                     8               7             11
## SMC00957_6/21/2010                     8               2             11
## SMC04383_6/9/2010                      6               2             11
##                     W1H_BRDG.sd W1H_BLDG.result W1H_BLDG.count W1H_BLDG.sd
## 402M00002_8/13/2015           0               1             11           0
## SMC00710_5/20/2009            0               7             11           0
## SMC00957_6/21/2010            0               2             11           0
## SMC04383_6/9/2010             0               2             11           0
##                     W1H_LDFL.result W1H_LDFL.count W1H_LDFL.sd
## 402M00002_8/13/2015               1             11           0
## SMC00710_5/20/2009                7             11           0
## SMC00957_6/21/2010                2             11           0
## SMC04383_6/9/2010                 2             11           0
##                     W1H_LOG.result W1H_LOG.count W1H_LOG.sd
## 402M00002_8/13/2015              1            11          0
## SMC00710_5/20/2009               7            11          0
## SMC00957_6/21/2010               2            11          0
## SMC04383_6/9/2010                2            11          0
##                     W1H_MINE.result W1H_MINE.count W1H_MINE.sd
## 402M00002_8/13/2015               1             11           0
## SMC00710_5/20/2009                7             11           0
## SMC00957_6/21/2010                2             11           0
## SMC04383_6/9/2010                 2             11           0
##                     W1H_ORVY.result W1H_ORVY.count W1H_ORVY.sd
## 402M00002_8/13/2015               1             11           0
## SMC00710_5/20/2009                7             11           0
## SMC00957_6/21/2010                2             11           0
## SMC04383_6/9/2010                 2             11           0
##                     W1H_PARK.result W1H_PARK.count W1H_PARK.sd
## 402M00002_8/13/2015               1             11           0
## SMC00710_5/20/2009                7             11           0
## SMC00957_6/21/2010                2             11           0
## SMC04383_6/9/2010                 2             11           0
##                     W1H_PSTR.result W1H_PSTR.count W1H_PSTR.sd
## 402M00002_8/13/2015               1             11           0
## SMC00710_5/20/2009                7             11           0
## SMC00957_6/21/2010                2             11           0
## SMC04383_6/9/2010                 2             11           0
##                     W1H_PVMT.result W1H_PVMT.count W1H_PVMT.sd
## 402M00002_8/13/2015               1             11           0
## SMC00710_5/20/2009                7             11           0
## SMC00957_6/21/2010                2             11           0
## SMC04383_6/9/2010                 2             11           0
##                     W1H_PIPE.result W1H_PIPE.count W1H_PIPE.sd
## 402M00002_8/13/2015               1             11           0
## SMC00710_5/20/2009                7             11           0
## SMC00957_6/21/2010                2             11           0
## SMC04383_6/9/2010                 2             11           0
##                     W1H_ROAD.result W1H_ROAD.count W1H_ROAD.sd
## 402M00002_8/13/2015               1             11           0
## SMC00710_5/20/2009                7             11           0
## SMC00957_6/21/2010                2             11           0
## SMC04383_6/9/2010                 2             11           0
##                     W1H_CROP.result W1H_CROP.count W1H_CROP.sd
## 402M00002_8/13/2015               1             11           0
## SMC00710_5/20/2009                7             11           0
## SMC00957_6/21/2010                2             11           0
## SMC04383_6/9/2010                 2             11           0
##                     W1H_VEGM.result W1H_VEGM.count W1H_VEGM.sd
## 402M00002_8/13/2015               1             11           0
## SMC00710_5/20/2009                7             11           0
## SMC00957_6/21/2010                2             11           0
## SMC04383_6/9/2010                 2             11           0
##                     W1H_WALL.result W1H_WALL.count W1H_WALL.sd
## 402M00002_8/13/2015               1             11           0
## SMC00710_5/20/2009                7             11           0
## SMC00957_6/21/2010                2             11           0
## SMC04383_6/9/2010                 2             11           0
##                     W1_HALL_EMAP.result W1_HALL_SWAMP.result FL_N_F.result
## 402M00002_8/13/2015                   4                    4            NA
## SMC00710_5/20/2009                   28                   28            NA
## SMC00957_6/21/2010                    8                    8   0.002344635
## SMC04383_6/9/2010                     8                    8            NA
##                     FL_N_M.result FL_Q_F.result FL_Q_M.result  FL_F.result
## 402M00002_8/13/2015            NA      2.855642    0.08086277  2.855641769
## SMC00710_5/20/2009             NA      0.000000    0.00000000  0.000000000
## SMC00957_6/21/2010         0.0828            NA            NA  0.002344635
## SMC04383_6/9/2010              NA     14.754147    0.41779091 14.754146820
##                     FL_M.result MWVM_F.result MWVM_M.result XWV_F.result
## 402M00002_8/13/2015  0.08086277          2.29      0.697992    0.5009091
## SMC00710_5/20/2009   0.00000000          0.58      0.176784    0.1973684
## SMC00957_6/21/2010   0.08280000          1.00      0.304800    1.0000000
## SMC04383_6/9/2010    0.41779091          0.88      0.268224    0.2287500
##                     XWV_M.result PWVZ.result NFC_DLU.result NFC_EFR.result
## 402M00002_8/13/2015   0.15267709    45.45455          Other             NO
## SMC00710_5/20/2009    0.06015789    15.78947 Suburban, Town             NO
## SMC00957_6/21/2010    0.30480000     0.00000         Forest             NO
## SMC04383_6/9/2010     0.06972300     0.00000 Suburban, Town             NO
##                     NFC_ERN.result RBP_CHN.result RBP_EPI.result
## 402M00002_8/13/2015             NO             NA             NA
## SMC00710_5/20/2009              NO             NA             NA
## SMC00957_6/21/2010              NO             17             17
## SMC04383_6/9/2010               NO             19             14
##                     RBP_SED.result PBM_S.result PBM_V.result PBM_E.result
## 402M00002_8/13/2015             NA     4.545455     81.81818     13.63636
## SMC00710_5/20/2009              NA     0.000000     59.33333     40.66667
## SMC00957_6/21/2010              18    45.454545     18.18182     36.36364
## SMC04383_6/9/2010               17   100.000000      0.00000      0.00000
##                     XWAK.result XWDO.result XWPH.result XWSL.result
## 402M00002_8/13/2015         341        3.90        7.62        0.92
## SMC00710_5/20/2009          200        7.19        7.57        1.20
## SMC00957_6/21/2010          310        9.70        8.51        0.60
## SMC04383_6/9/2010           198        8.86        7.91        0.45
##                     XWSC.result XWTC.result XWTF.result XWTB.result
## 402M00002_8/13/2015    1806.000       20.50      68.900          NA
## SMC00710_5/20/2009     1985.474       18.68      65.624          NA
## SMC00957_6/21/2010     1188.000       20.90      69.620          NA
## SMC04383_6/9/2010       764.000       16.22      61.196        10.8
##                     XGB.result XGB.count   XGB.sd XGH.result XGH.count
## 402M00002_8/13/2015   82.04545        22 11.84313   19.54545        22
## SMC00710_5/20/2009    14.88636        22 14.27780   12.27273        22
## SMC00957_6/21/2010    20.34091        22 34.86620   20.90909        22
## SMC04383_6/9/2010     33.86364        22 14.81487   21.02273        22
##                        XGH.sd XGW.result XGW.count    XGW.sd XM.result
## 402M00002_8/13/2015  9.116846   5.000000        22  0.000000  41.25000
## SMC00710_5/20/2009   9.847319  25.795455        22 15.047274  58.63636
## SMC00957_6/21/2010  26.865466   4.318182        22  8.632091  23.18182
## SMC04383_6/9/2010   12.165992  23.181818        22  5.884899  22.15909
##                     XM.count    XM.sd XC.result XC.count    XC.sd
## 402M00002_8/13/2015       22 16.63241  15.11364       22 14.06781
## SMC00710_5/20/2009        22 26.91175  58.75000       22 25.11580
## SMC00957_6/21/2010        22 23.74445  20.68182       22 24.53353
## SMC04383_6/9/2010         22 17.17029  28.97727       22 21.65282
##                     XG.result XCM.result XCMG.result XPMID.result
## 402M00002_8/13/2015  24.54545   56.36364    80.90909     1.000000
## SMC00710_5/20/2009   38.06818  117.38636   155.45455     1.000000
## SMC00957_6/21/2010   25.22727   43.86364    69.09091     1.466667
## SMC04383_6/9/2010    44.20455   51.13636    95.34091     1.000000
##                     XPCAN.result XPGVEG.result XPCM.result XPCMG.result
## 402M00002_8/13/2015    0.9090909             1           1            1
## SMC00710_5/20/2009     1.0000000             1           1            1
## SMC00957_6/21/2010     0.6818182             1           1            1
## SMC04383_6/9/2010      1.0000000             1           1            1
##                     XPMGVEG.result PCT_RS.result PCT_RR.result
## 402M00002_8/13/2015              0             0             0
## SMC00710_5/20/2009               0             0             0
## SMC00957_6/21/2010               0             0             0
## SMC04383_6/9/2010                0             0             0
##                     PCT_RC.result PCT_XB.result PCT_SB.result
## 402M00002_8/13/2015             0      0.000000     7.2164948
## SMC00710_5/20/2009              0      0.000000     0.7874016
## SMC00957_6/21/2010              0      0.000000     0.0000000
## SMC04383_6/9/2010               0      6.666667    20.0000000
##                     PCT_CB.result PCT_GC.result PCT_GF.result
## 402M00002_8/13/2015      11.34021      9.278351      4.123711
## SMC00710_5/20/2009       32.28346     39.370079     10.236220
## SMC00957_6/21/2010        0.00000      0.000000     21.904762
## SMC04383_6/9/2010        16.19048     13.333333     28.571429
##                     PCT_SA.result PCT_FN.result PCT_HP.result
## 402M00002_8/13/2015      9.278351     35.051546             0
## SMC00710_5/20/2009      14.960630      0.000000             0
## SMC00957_6/21/2010      78.095238      0.000000             0
## SMC04383_6/9/2010       13.333333      1.904762             0
##                     PCT_WD.result PCT_OT.result PCT_BDRK.result
## 402M00002_8/13/2015      4.123711     19.587629               0
## SMC00710_5/20/2009       0.000000      2.362205               0
## SMC00957_6/21/2010       0.000000      0.000000               0
## SMC04383_6/9/2010        0.000000      0.000000               0
##                     PCT_BIGR.result PCT_SFGF.result PCT_SAFN.result
## 402M00002_8/13/2015        27.83505        48.45361        44.32990
## SMC00710_5/20/2009         72.44094        25.19685        14.96063
## SMC00957_6/21/2010          0.00000       100.00000        78.09524
## SMC04383_6/9/2010          56.19048        43.80952        15.23810
##                     XSDGM.result XSPGM.result SB_PT_D50.result
## 402M00002_8/13/2015     1.377776     1.377776             1.03
## SMC00710_5/20/2009     31.377005    31.377005            40.00
## SMC00957_6/21/2010      1.655960     1.655960             1.03
## SMC04383_6/9/2010      39.827686    39.827686            40.00
##                     SB_PT_D10.result SB_PT_D25.result SB_PT_D75.result
## 402M00002_8/13/2015             0.03             0.03            40.00
## SMC00710_5/20/2009              1.03             9.00           157.00
## SMC00957_6/21/2010              1.03             1.03             1.03
## SMC04383_6/9/2010               1.03             9.00           625.00
##                     SB_PT_D90.result SB_PP_D50.result SB_PP_D10.result
## 402M00002_8/13/2015              157             1.03             0.03
## SMC00710_5/20/2009               157            40.00             1.03
## SMC00957_6/21/2010                 9             1.03             1.03
## SMC04383_6/9/2010                625            40.00             1.03
##                     SB_PP_D25.result SB_PP_D75.result SB_PP_D90.result
## 402M00002_8/13/2015             0.03            40.00              157
## SMC00710_5/20/2009              9.00           157.00              157
## SMC00957_6/21/2010              1.03             1.03                9
## SMC04383_6/9/2010               9.00           625.00              625
##                     XEMBED.result XEMBED.count XEMBED.sd PCT_CPOM.result
## 402M00002_8/13/2015      45.41667           12  30.70818        20.61856
## SMC00710_5/20/2009       13.29268           41  11.04591        56.69291
## SMC00957_6/21/2010             NA            0        NA        48.57143
## SMC04383_6/9/2010        44.28571           14  28.00118        26.66667
##                     XMIAT.result XMIAT.count  XMIAT.sd XMIATP.result
## 402M00002_8/13/2015    0.1032609          92 0.5372807     2.3750000
## SMC00710_5/20/2009     0.3245614         114 0.1725967     0.3737374
## SMC00957_6/21/2010     0.1666667         105 0.3319281     0.3888889
## SMC04383_6/9/2010      0.1173469          98 0.1613573     0.3026316
##                     XMIATP.count XMIATP.sd PCT_MIATP.result
## 402M00002_8/13/2015            4 1.2500000         4.347826
## SMC00710_5/20/2009            99 0.1256297        86.842105
## SMC00957_6/21/2010            45 0.4147684        42.857143
## SMC04383_6/9/2010             38 0.1032887        38.775510
##                     PCT_MIAT1.result PCT_MIAT1P.result PCT_MAA.result
## 402M00002_8/13/2015         3.260870         75.000000        2.12766
## SMC00710_5/20/2009          0.000000          0.000000       69.60000
## SMC00957_6/21/2010          0.952381          2.222222       10.47619
## SMC04383_6/9/2010           0.000000          0.000000       36.36364
##                     PCT_MCP.result PCT_MAU.result PCT_MAP.result
## 402M00002_8/13/2015      13.829787       0.000000        2.12766
## SMC00710_5/20/2009        3.937008       0.000000       69.60000
## SMC00957_6/21/2010       39.047619       0.952381       11.42857
## SMC04383_6/9/2010         0.000000       4.040404       39.39394
##                     PCT_NSA.result
## 402M00002_8/13/2015       5.319149
## SMC00710_5/20/2009       69.600000
## SMC00957_6/21/2010       12.380952
## SMC04383_6/9/2010        39.795918
```

The classes for the columns in the output data are as follows:


```r
unlist(lapply(alldat, class))
```

```
##        XBKF_H.result         XBKF_H.count            XBKF_H.sd 
##            "numeric"            "numeric"            "numeric" 
##        XKBF_W.result         XKBF_W.count            XKBF_W.sd 
##            "numeric"            "numeric"            "numeric" 
##       XWDEPTH.result        XWDEPTH.count           XWDEPTH.sd 
##            "numeric"            "numeric"            "numeric" 
##        XWIDTH.result         XWIDTH.count            XWIDTH.sd 
##            "numeric"            "numeric"            "numeric" 
##          XWDR.result           XWDR.count          XWDA.result 
##            "numeric"            "numeric"            "numeric" 
##           XWDA.count          XWDM.result           XWDM.count 
##            "numeric"            "numeric"            "numeric" 
##        PCT_CF.result         PCT_CF.count            PCT_CF.sd 
##            "numeric"            "numeric"            "numeric" 
##        PCT_DR.result         PCT_DR.count            PCT_DR.sd 
##            "numeric"            "numeric"            "numeric" 
##        PCT_GL.result         PCT_GL.count            PCT_GL.sd 
##            "numeric"            "numeric"            "numeric" 
##      PCT_POOL.result       PCT_POOL.count          PCT_POOL.sd 
##            "numeric"            "numeric"            "numeric" 
##        PCT_RA.result         PCT_RA.count            PCT_RA.sd 
##            "numeric"            "numeric"            "numeric" 
##        PCT_RI.result         PCT_RI.count            PCT_RI.sd 
##            "numeric"            "numeric"            "numeric" 
##        PCT_RN.result         PCT_RN.count            PCT_RN.sd 
##            "numeric"            "numeric"            "numeric" 
##      PCT_FAST.result      PCT_SLOW.result     PCT_CF_WT.result 
##            "numeric"            "numeric"            "numeric" 
##      PCT_CF_WT.count     PCT_GL_WT.result      PCT_GL_WT.count 
##            "numeric"            "numeric"            "numeric" 
##   PCT_POOL_WT.result    PCT_POOL_WT.count     PCT_RA_WT.result 
##            "numeric"            "numeric"            "numeric" 
##      PCT_RA_WT.count     PCT_RI_WT.result      PCT_RI_WT.count 
##            "numeric"            "numeric"            "numeric" 
##     PCT_RN_WT.result      PCT_RN_WT.count   PCT_FAST_WT.result 
##            "numeric"            "numeric"            "numeric" 
##   PCT_SLOW_WT.result        XSLOPE.result         XSLOPE.count 
##            "numeric"            "numeric"            "numeric" 
##            XSLOPE.sd       SLOPE_0.result        SLOPE_0.count 
##            "numeric"            "numeric"            "numeric" 
##     SLOPE_0_5.result      SLOPE_0_5.count       SLOPE_1.result 
##            "numeric"            "numeric"            "numeric" 
##        SLOPE_1.count       SLOPE_2.result        SLOPE_2.count 
##            "numeric"            "numeric"            "numeric" 
##      XBEARING.result       XBEARING.count          XBEARING.sd 
##            "numeric"            "numeric"            "numeric" 
##     SINU.NOT_WORKING      XCDENMID.result       XCDENMID.count 
##            "numeric"            "numeric"            "numeric" 
##          XCDENMID.sd       XCDENBK.result        XCDENBK.count 
##            "numeric"            "numeric"            "numeric" 
##           XCDENBK.sd       XFC_AQM.result        XFC_AQM.count 
##            "numeric"            "numeric"            "numeric" 
##           XFC_AQM.sd       XFC_HUM.result        XFC_HUM.count 
##            "numeric"            "numeric"            "numeric" 
##           XFC_HUM.sd       XFC_RCK.result        XFC_RCK.count 
##            "numeric"            "numeric"            "numeric" 
##           XFC_RCK.sd       XFC_ALG.result        XFC_ALG.count 
##            "numeric"            "numeric"            "numeric" 
##           XFC_ALG.sd       XFC_LWD.result        XFC_LWD.count 
##            "numeric"            "numeric"            "numeric" 
##           XFC_LWD.sd       XFC_LTR.result        XFC_LTR.count 
##            "numeric"            "numeric"            "numeric" 
##           XFC_LTR.sd       XFC_OHV.result        XFC_OHV.count 
##            "numeric"            "numeric"            "numeric" 
##           XFC_OHV.sd       XFC_BRS.result        XFC_BRS.count 
##            "numeric"            "numeric"            "numeric" 
##           XFC_BRS.sd       XFC_UCB.result        XFC_UCB.count 
##            "numeric"            "numeric"            "numeric" 
##           XFC_UCB.sd       XFC_BIG.result  XFC_NAT_EMAP.result 
##            "numeric"            "numeric"            "numeric" 
## XFC_NAT_SWAMP.result       CFC_AQM.result       CFC_HUM.result 
##            "numeric"            "numeric"            "numeric" 
##       CFC_RCK.result       CFC_ALG.result       CFC_LWD.result 
##            "numeric"            "numeric"            "numeric" 
##       CFC_LTR.result       CFC_OHV.result       CFC_BRS.result 
##            "numeric"            "numeric"            "numeric" 
##       CFC_UCB.result  CFC_ALL_EMAP.result CFC_ALL_SWAMP.result 
##            "numeric"            "numeric"            "numeric" 
##      W1H_BRDG.result       W1H_BRDG.count          W1H_BRDG.sd 
##            "numeric"            "numeric"            "numeric" 
##      W1H_BLDG.result       W1H_BLDG.count          W1H_BLDG.sd 
##            "numeric"            "numeric"            "numeric" 
##      W1H_LDFL.result       W1H_LDFL.count          W1H_LDFL.sd 
##            "numeric"            "numeric"            "numeric" 
##       W1H_LOG.result        W1H_LOG.count           W1H_LOG.sd 
##            "numeric"            "numeric"            "numeric" 
##      W1H_MINE.result       W1H_MINE.count          W1H_MINE.sd 
##            "numeric"            "numeric"            "numeric" 
##      W1H_ORVY.result       W1H_ORVY.count          W1H_ORVY.sd 
##            "numeric"            "numeric"            "numeric" 
##      W1H_PARK.result       W1H_PARK.count          W1H_PARK.sd 
##            "numeric"            "numeric"            "numeric" 
##      W1H_PSTR.result       W1H_PSTR.count          W1H_PSTR.sd 
##            "numeric"            "numeric"            "numeric" 
##      W1H_PVMT.result       W1H_PVMT.count          W1H_PVMT.sd 
##            "numeric"            "numeric"            "numeric" 
##      W1H_PIPE.result       W1H_PIPE.count          W1H_PIPE.sd 
##            "numeric"            "numeric"            "numeric" 
##      W1H_ROAD.result       W1H_ROAD.count          W1H_ROAD.sd 
##            "numeric"            "numeric"            "numeric" 
##      W1H_CROP.result       W1H_CROP.count          W1H_CROP.sd 
##            "numeric"            "numeric"            "numeric" 
##      W1H_VEGM.result       W1H_VEGM.count          W1H_VEGM.sd 
##            "numeric"            "numeric"            "numeric" 
##      W1H_WALL.result       W1H_WALL.count          W1H_WALL.sd 
##            "numeric"            "numeric"            "numeric" 
##  W1_HALL_EMAP.result W1_HALL_SWAMP.result        FL_N_F.result 
##            "numeric"            "numeric"            "numeric" 
##        FL_N_M.result        FL_Q_F.result        FL_Q_M.result 
##            "numeric"            "numeric"            "numeric" 
##          FL_F.result          FL_M.result        MWVM_F.result 
##            "numeric"            "numeric"            "numeric" 
##        MWVM_M.result         XWV_F.result         XWV_M.result 
##            "numeric"            "numeric"            "numeric" 
##          PWVZ.result       NFC_DLU.result       NFC_EFR.result 
##            "numeric"          "character"          "character" 
##       NFC_ERN.result       RBP_CHN.result       RBP_EPI.result 
##          "character"            "numeric"            "numeric" 
##       RBP_SED.result         PBM_S.result         PBM_V.result 
##            "numeric"            "numeric"            "numeric" 
##         PBM_E.result          XWAK.result          XWDO.result 
##            "numeric"            "numeric"            "numeric" 
##          XWPH.result          XWSL.result          XWSC.result 
##            "numeric"            "numeric"            "numeric" 
##          XWTC.result          XWTF.result          XWTB.result 
##            "numeric"            "numeric"            "numeric" 
##           XGB.result            XGB.count               XGB.sd 
##            "numeric"            "numeric"            "numeric" 
##           XGH.result            XGH.count               XGH.sd 
##            "numeric"            "numeric"            "numeric" 
##           XGW.result            XGW.count               XGW.sd 
##            "numeric"            "numeric"            "numeric" 
##            XM.result             XM.count                XM.sd 
##            "numeric"            "numeric"            "numeric" 
##            XC.result             XC.count                XC.sd 
##            "numeric"            "numeric"            "numeric" 
##            XG.result           XCM.result          XCMG.result 
##            "numeric"            "numeric"            "numeric" 
##         XPMID.result         XPCAN.result        XPGVEG.result 
##            "numeric"            "numeric"            "numeric" 
##          XPCM.result         XPCMG.result       XPMGVEG.result 
##            "numeric"            "numeric"            "numeric" 
##        PCT_RS.result        PCT_RR.result        PCT_RC.result 
##            "numeric"            "numeric"            "numeric" 
##        PCT_XB.result        PCT_SB.result        PCT_CB.result 
##            "numeric"            "numeric"            "numeric" 
##        PCT_GC.result        PCT_GF.result        PCT_SA.result 
##            "numeric"            "numeric"            "numeric" 
##        PCT_FN.result        PCT_HP.result        PCT_WD.result 
##            "numeric"            "numeric"            "numeric" 
##        PCT_OT.result      PCT_BDRK.result      PCT_BIGR.result 
##            "numeric"            "numeric"            "numeric" 
##      PCT_SFGF.result      PCT_SAFN.result         XSDGM.result 
##            "numeric"            "numeric"            "numeric" 
##         XSPGM.result     SB_PT_D50.result     SB_PT_D10.result 
##            "numeric"            "numeric"            "numeric" 
##     SB_PT_D25.result     SB_PT_D75.result     SB_PT_D90.result 
##            "numeric"            "numeric"            "numeric" 
##     SB_PP_D50.result     SB_PP_D10.result     SB_PP_D25.result 
##            "numeric"            "numeric"            "numeric" 
##     SB_PP_D75.result     SB_PP_D90.result        XEMBED.result 
##            "numeric"            "numeric"            "numeric" 
##         XEMBED.count            XEMBED.sd      PCT_CPOM.result 
##            "numeric"            "numeric"            "numeric" 
##         XMIAT.result          XMIAT.count             XMIAT.sd 
##            "numeric"            "numeric"            "numeric" 
##        XMIATP.result         XMIATP.count            XMIATP.sd 
##            "numeric"            "numeric"            "numeric" 
##     PCT_MIATP.result     PCT_MIAT1.result    PCT_MIAT1P.result 
##            "numeric"            "numeric"            "numeric" 
##       PCT_MAA.result       PCT_MCP.result       PCT_MAU.result 
##            "numeric"            "numeric"            "numeric" 
##       PCT_MAP.result       PCT_NSA.result 
##            "numeric"            "numeric"
```

# Required data checks

* For every function, make sure there are no duplicate or conflicting values for every unique combination of `id`, `LocationCode`, `AnalyteName`, and `VariableResult` (or `Result`).  This should be specific to the metric classes just to be safe.  For example, every combination should have only one entry in `VariableResult` for `AnalyteName %in% c('Microalgae Thickness', 'Macrophyte Cover', 'Macroalgae Cover, Attached', 'Macroalgae Cover, Unattached')` for the algae metrics.  The `algae.R` function will remove duplicate entries but a checker should be built that verifies a unique value can be determined.  

* Required column names, see those in `sampdat`.  

* Check for required values in `AnalyteName` (note that `chkinp()` can check of the columns exist but we'll need a checker on data input to check for these and only these): 
     * `c('Microalgae Thickness', 'Macrophyte Cover', 'Macroalgae Cover, Attached', 'Macroalgae Cover, Unattached')` for `algae()`
     * `c('Bankfull Height', 'Bankfull Width', 'StationWaterDepth', 'Wetted Width')` for `bankmorph()`
     * `c('Cascade/Falls', 'Dry', 'Glide', 'Pool', 'Rapid', 'Riffle', 'Run'))` for `channelmorph()` 
     * `c(Slope', 'Length, Segment', 'Elevation Difference', 'Bearing', 'Proportion', 'Length, Reach')` for `channelsinuosity()`
     * `c('Canopy Cover')` for `densiometer()`
     * `c('Distance from Bank', 'StationWaterDepth', 'Velocity', 'Distance, Float', 'Float Time', 'Wetted Width')` for `flow()`
     * `c('Fish Cover Macrophytes', 'Fish Cover Artificial Structures', 'Fish Cover Boulders', 'Fish Cover Filamentous Algae', 'Fish Cover Woody Debris >0.3 m', 'Fish Cover Live Trees/Roots', 'Fish Cover Overhang.Veg', 'Fish Cover Woody Debris <0.3 m', 'Fish Cover Undercut Banks')` for `habitat()`
     * `c('Riparian Bridges/Abutments', 'Riparian Buildings', 'Riparian Landfill/Trash', 'Riparian Logging', 'Riparian Mining', 'Riparian Orchards/Vineyards', 'Riparian Park/Lawn', 'Riparian Pasture/Range', 'Riparian Pavement', 'Riparian Pipes', 'Riparian Road', 'Riparian Row Crops', 'Riparian Vegetation Management', 'Riparian Wall/Dike')` for `disturbance()`
     * `c('Riffle/Run Channel Alteration', 'Riffle/Run Epifaunal Substrate', 'Riffle/Run Sediment Deposition', 'Dominant Land Use', 'Evidence of Fire', 'Evidence of Recent Rainfall')` for `misc()`
     * `c('Bank Stability')` for `bankstability()`
     * `c("Alkalinity as CaCO3", "Oxygen, Dissolved", "pH", "Salinity", "SpecificConductivity", "Temperature", "Turbidity")` for `quality()`
     * `c('Riparian GroundCover Barren', 'Riparian GroundCover NonWoody Plants', 'Riparian GroundCover Woody Shrubs', 'Riparian Lower Canopy All Vegetation', 'Riparian Upper Canopy All Trees', 'Riparian Lower Canopy All Vegetation', 'Riparian Upper Canopy All Trees', 'Riparian GroundCover Woody Shrubs', 'Riparian GroundCover NonWoody Plants')` for `ripveg()` 
     * `c('Substrate Size Class', 'Embeddedness', 'CPOM')` for `substrate()`
     
* Check for required values in `LocationCode` (note that `chkinp()` can check of the columns exist but we'll need a checker on data input to check for these and only these)

* Maybe we need to add a checker to make sure all values in each field are present but with appropriate NA values for `Result`, `VariableResult`, this can be done with `tidyr::complete()` but may be unnecessary since this will increase data volume 
