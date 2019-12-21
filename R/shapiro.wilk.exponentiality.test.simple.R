# Shapiro-Wilk Test for Exponentiality
# Implementer: Mike Burr
# Uses spline interpolation to to provide estimate of p value between points in the table.


# Source:
# An Analysis of Variance Test for the Exponential Distribution (Complete Samples) 
# Author(s): S. S. Shapiro and M. B. Wilk 
# Source: Technometrics, Vol. 14, No. 2 (May, 1972), pp. 355-370


.shapiro.wilk.exponentiality.table = matrix(
  c(
     c(0.2519,0.2538,0.2596,0.2697,0.2915,0.5714,0.9709,0.9926,0.9981,0.9997,0.9999)
    ,c(0.1241,0.1302,0.1434,0.1604,0.1891,0.3768,0.7514,0.8581,0.9236,0.968,0.9837)
    ,c(0.0845,0.0905,0.1048,0.1187,0.1442,0.2875,0.5547,0.6682,0.759,0.86,0.9192)
    ,c(0.061,0.0665,0.0802,0.0956,0.1173,0.2276,0.4292,0.5089,0.5842,0.6775,0.7501)
    ,c(0.0514,0.0591,0.07,0.081,0.0986,0.1874,0.3474,0.4162,0.4852,0.5706,0.6426)
    ,c(0.0454,0.0512,0.0614,0.071,0.0852,0.1625,0.2934,0.3497,0.4033,0.4848,0.5428)
    ,c(0.0404,0.0442,0.0537,0.0633,0.0751,0.1415,0.2553,0.3005,0.3454,0.4015,0.4433)
    ,c(0.0369,0.0404,0.0487,0.0568,0.0678,0.1225,0.2178,0.2525,0.2879,0.3391,0.3701)
    ,c(0.0339,0.038,0.0447,0.0528,0.0616,0.1112,0.1934,0.2265,0.2619,0.3039,0.3314)
    ,c(0.0311,0.0358,0.041,0.0494,0.0567,0.1009,0.1723,0.2019,0.2364,0.2716,0.2978)
    ,c(0.0287,0.0337,0.0382,0.046,0.0528,0.0925,0.1563,0.1829,0.2113,0.2422,0.2642)
    ,c(0.0265,0.0317,0.0362,0.0428,0.0496,0.0847,0.1417,0.1647,0.1862,0.2131,0.2315)
    ,c(0.0247,0.0298,0.0344,0.0398,0.0466,0.0778,0.1285,0.1485,0.1669,0.1926,0.2123)
    ,c(0.0233,0.028,0.0326,0.0374,0.0438,0.0728,0.1187,0.1355,0.1542,0.177,0.1931)
    ,c(0.0222,0.0264,0.031,0.0352,0.0412,0.0684,0.1099,0.1257,0.1423,0.1614,0.1794)
    ,c(0.0212,0.025,0.0294,0.0332,0.0388,0.064,0.1015,0.1164,0.1311,0.1483,0.1668)
    ,c(0.0203,0.0238,0.0278,0.0314,0.0368,0.06,0.0935,0.1071,0.1199,0.1374,0.1452)
    ,c(0.0196,0.0227,0.0264,0.0302,0.0353,0.057,0.0884,0.1002,0.1121,0.1286,0.1369)
    ,c(0.019,0.0217,0.025,0.029,0.0337,0.054,0.0839,0.0948,0.1054,0.1198,0.1288)
    ,c(0.0185,0.0208,0.0238,0.0278,0.0323,0.0516,0.0794,0.0894,0.0988,0.1118,0.1212)
    ,c(0.0181,0.0201,0.023,0.0266,0.031,0.0492,0.0749,0.0836,0.0933,0.1043,0.1142)
    ,c(0.0177,0.0194,0.0224,0.0256,0.0298,0.0468,0.0704,0.0788,0.0882,0.0984,0.1071)
    ,c(0.0173,0.0188,0.0218,0.0248,0.0286,0.0447,0.0668,0.0749,0.0836,0.0927,0.1)
    ,c(0.0169,0.0182,0.0213,0.024,0.0274,0.0426,0.0636,0.0712,0.0791,0.0885,0.0948)
    ,c(0.0165,0.0177,0.0208,0.0232,0.0264,0.0407,0.0606,0.0678,0.0747,0.0843,0.0896)
    ,c(0.0161,0.0172,0.0203,0.0225,0.0256,0.0391,0.0576,0.0649,0.0706,0.0801,0.0859)
    ,c(0.0157,0.0168,0.0198,0.0219,0.0249,0.0377,0.0555,0.0621,0.0671,0.0759,0.0822)
    ,c(0.0153,0.0164,0.0193,0.0213,0.0242,0.0364,0.0536,0.0593,0.0643,0.0719,0.0786)
    ,c(0.0149,0.016,0.0188,0.0207,0.0235,0.0352,0.0518,0.0569,0.0615,0.0719,0.0753)
    ,c(0.0145,0.0156,0.0183,0.0201,0.0229,0.034,0.0491,0.0547,0.0591,0.0686,0.0722)
    ,c(0.0141,0.0152,0.0178,0.0195,0.0223,0.0329,0.0475,0.0527,0.0573,0.0661,0.0691)
    ,c(0.0137,0.0148,0.0173,0.019,0.0217,0.0319,0.0459,0.0507,0.0555,0.0636,0.066)
    ,c(0.0133,0.0144,0.0168,0.0185,0.0211,0.0309,0.0444,0.0488,0.0537,0.0611,0.0639)
    ,c(0.0129,0.0141,0.0164,0.018,0.0205,0.03,0.0429,0.047,0.0519,0.0588,0.0608)
    ,c(0.0125,0.0138,0.016,0.0176,0.02,0.0291,0.0414,0.0454,0.0501,0.0567,0.0578)
    ,c(0.0122,0.0135,0.0156,0.0172,0.0195,0.0283,0.04,0.044,0.0483,0.0546,0.0553)
    ,c(0.012,0.0133,0.0152,0.0168,0.019,0.0275,0.0386,0.0426,0.0465,0.0525,0.0531)
    ,c(0.0118,0.0131,0.0148,0.0164,0.0186,0.0267,0.0375,0.0414,0.0447,0.0512,0.051)
    ,c(0.0116,0.0129,0.0144,0.0161,0.0182,0.026,0.0364,0.0402,0.043,0.0499,0.0493)
    ,c(0.0114,0.0127,0.014,0.0158,0.0178,0.0253,0.0355,0.0389,0.0417,0.0476,0.0482)
    ,c(0.0112,0.0125,0.0137,0.0155,0.0174,0.0248,0.0346,0.0379,0.0405,0.0464,0.0471)
    ,c(0.011,0.0123,0.0134,0.0152,0.017,0.0243,0.0338,0.0369,0.0394,0.0452,0.046)
    ,c(0.0108,0.0121,0.0131,0.0149,0.0166,0.0238,0.0329,0.0359,0.0385,0.044,0.0449)
    ,c(0.0106,0.0119,0.0129,0.0146,0.0162,0.0233,0.032,0.0349,0.0376,0.0423,0.0438)
    ,c(0.0104,0.0117,0.0127,0.0143,0.0158,0.0228,0.0311,0.034,0.0367,0.0416,0.0427)
    ,c(0.0103,0.0115,0.0125,0.0143,0.0158,0.0228,0.0311,0.034,0.0367,0.0394,0.0426)
    ,c(0.0102,0.0113,0.0123,0.0141,0.0155,0.0223,0.0303,0.0332,0.0358,0.0382,0.0416)
    ,c(0.0101,0.0111,0.0122,0.0137,0.0149,0.0213,0.0288,0.0317,0.034,0.036,0.0394)
    ,c(0.01,0.0109,0.012,0.0135,0.0147,0.0209,0.0282,0.031,0.0331,0.0349,0.0383)
    ,c(0.0099,0.0107,0.0119,0.0133,0.0145,0.0205,0.0276,0.0303,0.0323,0.0341,0.0373)
    ,c(0.0097,0.0106,0.0118,0.0131,0.0143,0.0201,0.027,0.0296,0.0315,0.0332,0.0363)
    ,c(0.0095,0.0104,0.0116,0.0129,0.0141,0.0197,0.0264,0.0289,0.0307,0.0329,0.0353)
    ,c(0.0094,0.0103,0.0115,0.0127,0.0139,0.0193,0.0258,0.0282,0.0299,0.0321,0.0343)
    ,c(0.0093,0.0102,0.0113,0.0125,0.0137,0.0189,0.0252,0.0275,0.0292,0.0313,0.0333)
    ,c(0.0092,0.0101,0.0112,0.0123,0.0135,0.0185,0.0247,0.0268,0.0285,0.0306,0.0324)
    ,c(0.0091,0.01,0.011,0.0121,0.0133,0.0182,0.0242,0.0262,0.0279,0.0301,0.0318)
    ,c(0.009,0.0098,0.0109,0.0119,0.0131,0.0179,0.0238,0.0257,0.0274,0.0296,0.0312)
    ,c(0.0089,0.0095,0.0108,0.0117,0.0129,0.0176,0.0234,0.0252,0.027,0.0291,0.0306)
    ,c(0.0088,0.0093,0.0107,0.0115,0.0127,0.0173,0.023,0.0247,0.0266,0.0286,0.0301)
    ,c(0.0087,0.0092,0.0105,0.0113,0.0125,0.017,0.0226,0.0242,0.0262,0.0281,0.0296)
    ,c(0.0086,0.0091,0.0104,0.0112,0.0123,0.0167,0.0222,0.0238,0.0257,0.0276,0.0291)
    ,c(0.0085,0.009,0.0102,0.0111,0.0121,0.0164,0.0218,0.0234,0.0252,0.0271,0.0286)
    ,c(0.0084,0.0089,0.0101,0.0109,0.0119,0.0161,0.0215,0.023,0.0247,0.0266,0.0281)
    ,c(0.0082,0.0088,0.0099,0.0108,0.0117,0.0159,0.0211,0.0225,0.0242,0.0261,0.0276)
    ,c(0.0081,0.0087,0.0098,0.0107,0.0115,0.0157,0.0207,0.0221,0.0237,0.0256,0.0271)
    ,c(0.008,0.0086,0.0096,0.0105,0.0114,0.0155,0.0204,0.0217,0.0232,0.0251,0.0266)
    ,c(0.0079,0.0085,0.0095,0.0104,0.0113,0.0152,0.0198,0.0213,0.0227,0.0246,0.0261)
    ,c(0.0078,0.0084,0.0094,0.0103,0.0111,0.015,0.0194,0.0209,0.0222,0.0241,0.0256)
    ,c(0.0077,0.0083,0.0093,0.0102,0.0109,0.0147,0.0191,0.0205,0.0218,0.0237,0.0251)
    ,c(0.0076,0.0082,0.0092,0.0101,0.0108,0.0145,0.0188,0.0201,0.0214,0.0232,0.0246)
    ,c(0.0075,0.0081,0.0091,0.01,0.0107,0.0143,0.0185,0.0198,0.0211,0.0228,0.0241)
    ,c(0.0074,0.008,0.009,0.0098,0.0106,0.0141,0.0182,0.0195,0.0208,0.0224,0.0236)
    ,c(0.0073,0.0079,0.0089,0.0097,0.0105,0.0139,0.0179,0.0192,0.0205,0.022,0.0231)
    ,c(0.0073,0.0078,0.0088,0.0096,0.0104,0.0137,0.0176,0.0189,0.0202,0.0217,0.0227)
    ,c(0.0072,0.0077,0.0087,0.0095,0.0103,0.0135,0.0173,0.0186,0.0199,0.0214,0.0223)
    ,c(0.0071,0.0077,0.0086,0.0093,0.0101,0.0134,0.017,0.0183,0.0196,0.0211,0.0219)
    ,c(0.007,0.0076,0.0085,0.0092,0.01,0.0132,0.0168,0.018,0.0193,0.0208,0.0215)
    ,c(0.007,0.0075,0.0084,0.0091,0.0099,0.0131,0.0166,0.0177,0.019,0.0205,0.0211)
    ,c(0.0069,0.0074,0.0083,0.009,0.0098,0.0129,0.0164,0.0175,0.0187,0.0202,0.0207)
    ,c(0.0068,0.0074,0.0082,0.0088,0.0097,0.0128,0.0162,0.0173,0.0184,0.0199,0.0203)
    ,c(0.0067,0.0073,0.0081,0.0087,0.0096,0.0126,0.016,0.017,0.0181,0.0196,0.0199)
    ,c(0.0067,0.0073,0.008,0.0086,0.0095,0.0125,0.0158,0.0168,0.0178,0.0193,0.0196)
    ,c(0.0065,0.0072,0.0079,0.0085,0.0094,0.0123,0.0156,0.0166,0.0174,0.019,0.0193)
    ,c(0.0066,0.0071,0.0078,0.0085,0.0093,0.0122,0.0154,0.0164,0.0172,0.0187,0.019)
    ,c(0.0065,0.0071,0.0077,0.0084,0.0092,0.012,0.0152,0.0162,0.017,0.0184,0.0187)
    ,c(0.0065,0.007,0.0077,0.0084,0.0091,0.0119,0.015,0.016,0.0168,0.0181,0.0185)
    ,c(0.0064,0.007,0.0076,0.0083,0.009,0.0117,0.0148,0.0158,0.0166,0.0179,0.0183)
    ,c(0.0064,0.0069,0.0075,0.0082,0.0089,0.0116,0.0147,0.0156,0.0164,0.0176,0.0181)
    ,c(0.0063,0.0068,0.0075,0.0082,0.0088,0.0114,0.0145,0.0154,0.0162,0.0173,0.0179)
    ,c(0.0063,0.0068,0.0074,0.0081,0.0087,0.0113,0.0143,0.0153,0.016,0.0171,0.0177)
    ,c(0.0062,0.0067,0.0073,0.0081,0.0086,0.0112,0.0141,0.0151,0.0158,0.0168,0.0175)
    ,c(0.0062,0.0067,0.0073,0.008,0.0085,0.011,0.0139,0.0149,0.0156,0.0165,0.0173)
    ,c(0.0061,0.0066,0.0072,0.0079,0.0084,0.0109,0.0138,0.0147,0.0154,0.0163,0.0171)
    ,c(0.0061,0.0065,0.0072,0.0078,0.0083,0.0108,0.0136,0.0145,0.0153,0.0161,0.0169)
    ,c(0.006,0.0065,0.0071,0.0077,0.0082,0.0107,0.0134,0.0143,0.0152,0.0159,0.0167)
    ,c(0.006,0.0064,0.007,0.0076,0.0081,0.0105,0.0133,0.0142,0.0151,0.0157,0.0165)
    ,c(0.0059,0.0064,0.007,0.0075,0.008,0.0104,0.0132,0.014,0.015,0.0155,0.0163)
    ,c(0.0059,0.0063,0.0069,0.0074,0.0079,0.0103,0.0131,0.0139,0.0149,0.0153,0.0161)
  )
  ,ncol = 11
  ,byrow = T
)

.shapiro.wilk.exponentiality.table <- cbind(.shapiro.wilk.exponentiality.table[,1]-(.shapiro.wilk.exponentiality.table[,2] - .shapiro.wilk.exponentiality.table[,1]),
                                            .shapiro.wilk.exponentiality.table
                                            )
                         


.shapiro.wilk.exponentiality.table <- cbind(.shapiro.wilk.exponentiality.table, 
                                            .shapiro.wilk.exponentiality.table[,ncol(.shapiro.wilk.exponentiality.table)]+(.shapiro.wilk.exponentiality.table[,ncol(.shapiro.wilk.exponentiality.table)] - .shapiro.wilk.exponentiality.table[,(ncol(.shapiro.wilk.exponentiality.table)-1)]))

colnames(.shapiro.wilk.exponentiality.table) <- c(0,.005,.01,.025,.05,.1,.5,.9,.95,.975,.99,.995,1)
rownames(.shapiro.wilk.exponentiality.table) <- 3:100

.shapiro.wilk.exponentiality.fn <- list()
.shapiro.wilk.exponentiality.fn[[1]] <- function(W) { NA}
.shapiro.wilk.exponentiality.fn[[2]] <- function(W) { NA}

for (i in 1:nrow(.shapiro.wilk.exponentiality.table)) {
  .shapiro.wilk.exponentiality.fn[[(i+2)]] <- splinefun(.shapiro.wilk.exponentiality.table[i,], 
                                                        colnames(.shapiro.wilk.exponentiality.table))
}

shapiro.wilk.exponentiality.test.simple <- function(W, sample.size, alternative = c("two.sided", "less")) {
  validate.htest.alternative(alternative = alternative)
  
  p.value <- .shapiro.wilk.exponentiality.fn[[sample.size]](W)
  gt.50.pct <- W > .shapiro.wilk.exponentiality.table[(sample.size-2), 7]
  
  p.value <- ifelse(p.value < 0, 0, p.value)
  p.value <- ifelse(p.value > 1, 1, p.value)
  
  if (gt.50.pct) {
    p.value <- 1-p.value
  }
  
  if (alternative[1] == "two.sided") {
    #See pg 359 for method illustration in Shapiro/Wilk 1972.
    p.value <- 2* p.value
  } else { 
    
  }
  
  p.value <- ifelse(p.value < 0, 0, p.value)
  p.value <- ifelse(p.value > 1, 1, p.value)
  
  retval<-list(data.name   = "input data",
               statistic   = c(W = W), 
               estimate    = c(W = W, sample.size = sample.size),
               parameter   = 1 ,
               p.value     = p.value,
               null.value  = 1,
               alternative = alternative[1],
               method      = "Shapiro-Wilk Test for Exponentiality"#,
               #                 conf.int    = c(NA,NA)
  )
  
  names(retval$null.value) <- "W statistic"
  names(retval$parameter) <- "null hypothesis W statistic"
  
  class(retval)<-"htest"
  retval
}
