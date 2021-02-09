# init
mtq <- mf_get_mtq()
mf_map(mtq)


## mf_arrow
for (i in list('topleft', 'top', 'topright', 'right', 'bottomright', 'bottom',
               'bottomleft', 'left', c(701012.7,1613554 ))){
  expect_silent(mf_arrow(pos = i, col = "red"))
}
expect_silent(mf_arrow(pos = i+c(10000,10000), adjust = mtq))

