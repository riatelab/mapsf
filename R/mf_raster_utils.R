get_the_raster_pal <- function(pal, nbreaks, alpha = 1, rev = TRUE) {
  if (length(pal) == 1) {
    if (pal %in% hcl.pals()) {
      cols <- hcl.colors(n = nbreaks, palette = pal, alpha = alpha, rev = rev)
    } else {
      stop("This is not a palette name", call. = FALSE)
    }
  } else {
    cols <- colorRampPalette(pal, alpha = TRUE)(nbreaks)
  }
  return(cols)
}


get_continuous_pal <- function(breaks, pal){
  # get a palette repartitionthat match classes size
  etendu <- max(breaks) - min(breaks)
  lb <- length(breaks)
  dd <- data.frame(from = breaks[1:(lb-1)], to = breaks[2:lb])
  dd$diff <- dd$to - dd$from
  dd$ncol <- round(dd$diff * 1000 / etendu)
  dd$colfrom <- pal[1:(lb-1)]
  dd$colto <- pal[2:lb]
  l <- list()
  for(i in 1:(lb-1)){
    l[[i]] <-  colorRampPalette(c(dd$colfrom[i], dd$colto[i]))(dd$ncol[i])
  }
  p <- do.call(c, l)
  p
}



mf_raster_multiband <- function(ops, expandBB, add){
  ops$smooth <- ifelse(is.null(ops$smooth), TRUE, ops$smooth)
  if (add == FALSE) {
    mf_init(ops$x, expandBB = expandBB)
  }
  do.call(terra::plotRGB, ops)
}

mf_raster_interval <- function(ops, ops_leg, pal, breaks, nbreaks, alpha,
                               rev, add, expandBB){
  if (missing(pal)){
    pal <- "Dark Mint"
  }
  # set breaks and palette
  ops$breaks <- mf_get_breaks(x = terra::values(ops$x), nbreaks = nbreaks,
                              breaks = breaks)
  ops$col <- get_the_pal(pal = pal, nbreaks = length(ops$breaks) - 1,
                         alpha = alpha, rev = !rev)
  # init
  if (add == FALSE) {
    mf_init(ops$x, expandBB = expandBB)
  }
  # plot
  do.call(terra::plot, ops)
  # legend
  leg(
    type = "choro",
    box_cex = ops_leg$leg_box_cex,
    val = ops$breaks,
    horiz = ops_leg$leg_horiz,
    box_border = ops_leg$leg_box_border,
    pos = ops_leg$leg_pos,
    pal = ops$col,
    title = ops_leg$leg_title,
    title_cex = ops_leg$leg_title_cex,
    val_cex = ops_leg$leg_val_cex,
    val_rnd = ops_leg$leg_val_rnd,
    frame = ops_leg$leg_frame,
    bg = ops_leg$leg_bg,
    fg = ops_leg$leg_fg,
    frame_border = ops_leg$leg_frame_border,
    adj = ops_leg$leg_adj,
    size = ops_leg$leg_size
  )
}



mf_raster_continuous <- function(ops, ops_leg, breaks, pal, expandBB, add,
                                 alpha, rev){
  if (missing(pal)){
    pal <- "Dark Mint"
  }
  val <- terra::values(ops$x, na.rm = TRUE)

  # with breaks
  lb <- length(breaks)
  if (lb > 1) {
    if (length(pal) != (lb)) {
      stop(paste0("'pal' should be a vector of ", lb, " colors"), call. = FALSE)
    }
    pal <- get_continuous_pal(breaks, pal)
    p_pal <- pal
    # this for vmin superior to lmin or/and vmax inferior to lmax
    # other cases are missing
    val_min <- min(val)
    val_max <- max(val)
    bks_min <- min(breaks)
    bks_max <- max(breaks)
    one_unit <- length(pal) / (bks_max - bks_min)
    d_min <- bks_min - val_min
    d_max <- bks_max - val_max
    if (d_min > 0) {
      p_pal <- c(rep(NA, round(d_min * one_unit, 0)), p_pal)
    }
    if (d_min < 0) {
      p_pal <- p_pal[-(1:round(-d_min * one_unit, 0))]
    }
    if (d_max > 0) {
      p_pal <- p_pal[1:(length(p_pal) - round(d_max * one_unit, 0))]
    }
    if (d_max < 0) {
      p_pal <- c(p_pal, rep(NA, round(-d_max * one_unit, 0)))
    }
    ops$col <- p_pal
    vv <- breaks
  } else {
    pal <- get_the_raster_pal(
      pal = pal, nbreaks = 255, alpha = alpha,
      rev = !rev
    )
    ops$col <- pal[-1]
    # For the legend
    v <- mf_get_breaks(x = val, nbreaks = 4, breaks = "pretty")
    vmin <- min(val)
    vmax <- max(val)
    lim <- (vmax - vmin) / 10
    vv <- c(vmin, v[v > vmin & v < vmax], vmax)
    lvv <- length(vv)
    if (vv[2] - vv[1] < lim){
      vv <- vv[-2]
    }
    lvv <- length(vv)
    if ((vv[lvv] - vv[(lvv - 1)]) < lim){
      vv <- vv[-(lvv - 1)]
    }
  }

  if (add == FALSE) {
    mf_init(ops$x, expandBB = expandBB)
  }

  do.call(terra::plot, ops)

  leg(
    type = "cont",
    box_cex = c(1.5, 2) * ops_leg$leg_box_cex,
    val = vv,
    horiz = ops_leg$leg_horiz,
    pos = ops_leg$leg_pos,
    pal = pal,
    title = ops_leg$leg_title,
    title_cex = ops_leg$leg_title_cex,
    val_cex = ops_leg$leg_val_cex,
    val_rnd = ops_leg$leg_val_rnd,
    frame = ops_leg$leg_frame,
    bg = ops_leg$leg_bg,
    fg = ops_leg$leg_fg,
    frame_border = ops_leg$leg_frame_border,
    adj = ops_leg$leg_adj,
    size = ops_leg$leg_size
  )
}


mf_raster_classes <- function(ops, ops_leg, pal, val_order, expandBB,
                              add, alpha, rev){
  modalities <- terra::cats(ops$x)[[1]]
  if (is.null(modalities)) {
    ops$x <- terra::as.factor(ops$x)
    modalities <- terra::cats(ops$x)[[1]]
  }

  if (missing(pal)){
    pal <- "Dark 2"
  }
  if(missing(val_order)){
    val_order <- modalities[,2]
  }
  pal <- get_the_pal(
    pal = pal, nbreaks = length(val_order),
    alpha = alpha, rev = rev
  )
  refcol <- data.frame(mod = val_order, col = pal)

  mod <- merge(x = modalities, y = refcol,
               by.x = names(modalities)[2],
               by.y = "mod", all.x = TRUE)
  mod <- mod[, c("ID", "col")]
  mod <- mod[order(mod$ID), 'col']

  ops$col <- mod

  if (add == FALSE) {
    mf_init(ops$x, expandBB = expandBB)
  }
  do.call(terra::plot, ops)

  leg(
    type = "typo",
    pos = ops_leg$leg_pos,
    val = refcol$mod,
    title = ops_leg$leg_title,
    title_cex = ops_leg$leg_title_cex,
    val_cex = ops_leg$leg_val_cex,
    no_data = FALSE,
    size = ops_leg$leg_size,
    box_border = ops_leg$leg_box_border,
    box_cex = ops_leg$leg_box_cex,
    frame_border = ops_leg$leg_frame_border,
    frame = ops_leg$leg_frame,
    pal = refcol$col,
    bg = ops_leg$leg_bg,
    fg = ops_leg$leg_fg,
    adj = ops_leg$leg_adj
  )
}
