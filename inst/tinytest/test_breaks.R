mtq <- mf_get_mtq()
x <- mtq$MED

expect_equal(mf_get_breaks(x = x, nbreaks = 6, breaks = "geom"),
             c(11929, 13186.1043108293, 14575.6850445193, 16111.7028584815,
               17809.5896149738, 19686.40341991, 21761))
expect_equal(mf_get_breaks(x = x,  breaks = "arith"),
             c(11929, 12397.1904761905, 12865.380952381, 13333.5714285714,
               13801.7619047619, 14269.9523809524, 21761))
expect_equal(mf_get_breaks(x = x,  breaks = "q6"),
             c(11929, 13168.65, 14468.275, 15685.5, 17750.25, 20159.35,
               21761))
expect_equal(mf_get_breaks(x = x, nbreaks = 8,breaks = "em"),
             c(11929, 13334.5, 14369.1578947368, 15121.6363636364,
               16137.7058823529, 17121.875, 18377.8666666667,
               19813.2857142857, 21761))
expect_equal(mf_get_breaks(x = x, breaks = "msd", k = 1, central = TRUE),
             c(11929, 12548.5835933576, 14941.3317860212, 17334.0799786847,
               19726.8281713483, 21761))
expect_equal(mf_get_breaks(x = x, breaks = "msd", k = 1),
             c(11929, 13744.9576896894, 16137.7058823529, 18530.4540750165,
               20923.2022676801, 21761))
expect_error(mf_get_breaks(x = x, nbreaks = 7, breaks = "em"))
expect_equal(mf_get_breaks(x = x, breaks = c(1,10,20,100), k = 1),
             c(1,10,20,100))
expect_equal(mf_get_breaks(x = x, breaks = "fisher"),
             c(11929, 13953, 15685.5, 17372, 18622, 20354.5, 21761))
