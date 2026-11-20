mf_map(mtq)
expect_silent(mf_inset_on(x = mtq))
mf_map(mtq)
expect_silent(mf_inset_off())
expect_silent(mf_inset_on(fig = c(0, .5, 0, .5)))
mf_map(mtq)
expect_silent(mf_inset_off())


expect_silent(mf_inset_on(x = "worldmap", pos = "topleft"))
mf_worldmap(mtq)
expect_silent(mf_inset_off())
posposs <- c(
  "bottomleft", "left", "topleft", "top", "bottom",
  "bottomright", "right", "topright"
)

mf_map(mtq)
for(i in posposs){
  expect_silent({
    mf_inset_on(x = "worldmap", pos = i)
    plot(1)
    mf_inset_off()
  })
}

