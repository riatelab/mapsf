mtq <- mf_get_mtq()
expect_silent(mf_prop_typo(mtq, c("POP", "STATUS"), add = FALSE))

mf_map(mtq)
expect_silent(mf_prop_typo(mtq, c("POP", "STATUS")))

mtq[6, "STATUS"] <- NA
expect_silent(
  mf_prop_typo(
    x = mtq, var = c("POP", "STATUS"), inches = .35, border = "tomato4",
    val_max = 90000, symbol = "square", col_na = "grey", pal = "Dynamic",
    lwd = 2,
    leg_pos = c("bottomright", "bottomleft"),
    leg_title = c("Population", "Municipality\nstatus"),
    leg_title_cex = c(0.9, 0.9),
    leg_val_cex = c(.7, .7),
    val_order = c("Prefecture", "Sub-prefecture", "Simple municipality"),
    leg_no_data = "No dada",
    leg_frame = c(TRUE, TRUE),
    add = TRUE)
)


mob <- read.csv(system.file("csv/mob.csv", package = "mapsf"))
mob_97209 <- mob[mob$i == 97209, ]
mob_links <- mf_get_links(x = mtq, df = mob_97209)
mf_map(mtq)
expect_silent(mf_map(mob_links, c("fij", "sj"), "prop_typo", add = TRUE))
mob_links[2, "sj"] <- NA
expect_silent(mf_map(mob_links, c("fij", "sj"), "prop_typo",
                     val_max = 5000, add = FALSE, col_na = "red"))
