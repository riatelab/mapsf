mtq <- mf_get_mtq()
mf_map(mtq)
expect_silent(mf_scale(pos = c(713709.9, 1596117)))
expect_silent(mf_scale())
expect_silent(mf_scale(pos = "bottomleft", size = 5))

expect_message(mf_scale(pos = "bottomleft", size = 5000, unit = "ft"))

expect_error(mf_scale(crs_units = "pop"))
expect_error(mf_scale(scale_units = "pop"))

expect_silent(mf_scale(scale_units = "m"))
expect_silent(mf_scale(scale_units = "mi"))
expect_silent(mf_scale(scale_units = "ft"))

mtqx <- st_transform(mtq, 2264)
mf_map(mtqx)
expect_silent(mf_scale(crs_units = "ft", scale_units = "m"))
expect_silent(mf_scale(crs_units = "ft", scale_units = "km"))
expect_silent(mf_scale(crs_units = "ft", scale_units = "mi"))
expect_silent(mf_scale(crs_units = "ft", scale_units = "ft"))
expect_silent(mf_scale(x = mtqx, scale_units = "m"))

mtqy <- st_transform(mtq, 4326)
mf_map(mtqy)
expect_message(mf_scale(x = mtqy))
st_crs(mtqy) <- NA_crs_
expect_message(mf_scale(x = mtqy))

