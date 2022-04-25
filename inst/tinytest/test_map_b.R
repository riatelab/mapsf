mtq <- mf_get_mtq()
st_geometry(mtq) <- st_centroid(st_geometry(mtq))
expect_silent(mf_map(mtq, type = "base"))
