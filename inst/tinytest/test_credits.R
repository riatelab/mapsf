# init
mtq <- mf_get_mtq()
mf_map(mtq)

## mf_credits
for (i in  c("bottomleft", "bottomright", "rightbottom")){
  expect_silent(mf_credits(txt = "credits line with \n a jump",
                           pos = i, col = "red"))
}
expect_silent(mf_credits(txt = "credits line with \n a jump\n\n."))
