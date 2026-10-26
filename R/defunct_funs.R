#' @title Defunct - Plot an sf object
#' @description
#' This function is defunct. Please use `mf_map()` with `type = "base"`
#' instead.
#' @param ... ignored
#' @keywords internal
#' @return nothing is returned
#' @export
#' @examples
#' mtq <- mf_get_mtq()
#' mf_map(mtq)
mf_base <- function(...) {
  .Defunct(msg = "'mf_base()' is defunct.\nUse 'mf_map()' instead.")
}

#' @title Defunct - Plot a choropleth map
#' @description
#' This function is defunct. Please use `mf_map()` with `type = "choro"`
#' instead.
#' @param ... ignored
#' @keywords internal
#' @return nothing is returned
#' @export
#' @examples
#' mtq <- mf_get_mtq()
#' mf_map(mtq, var = "MED", type = "choro")
mf_choro <- function(...) {
  .Defunct(msg = "'mf_choro()' is defunct.\nUse 'mf_map()' instead.")
}


#' @title Defunct - Plot graduated symbols
#' @description
#' This function is defunct. Please use `mf_map()` with `type = "grad"`
#' instead.
#' @param ... ignored
#' @keywords internal
#' @return nothing is returned
#' @export
#' @examples
#' mtq <- mf_get_mtq()
#' mf_map(mtq)
#' mf_map(mtq, "POP", "grad", pch = 22, add = TRUE)
mf_grad <- function(...) {
  .Defunct(msg = "'mf_grad()' is defunct.\nUse 'mf_map()' instead.")
}


#' @title Defunct - Plot proportional symbols using choropleth coloration
#' @description
#' This function is defunct. Please use `mf_map()` with `type = "prop_choro"`
#' instead.
#' @param ... ignored
#' @keywords internal
#' @return nothing is returned
#' @export
#' @examples
#' mtq <- mf_get_mtq()
#' mf_map(mtq)
#' mf_map(mtq, c("POP", "MED"), "prop_choro", add = TRUE)
mf_prop_choro <- function(...) {
  .Defunct(msg = "'mf_prop_choro()' is defunct.\nUse 'mf_map()' instead.")
}

#' @title Defunct - Plot proportional symbols using typology coloration
#' @description
#' This function is defunct. Please use `mf_map()` with `type = "prop_typo"`
#' instead.
#' @param ... ignored
#' @keywords internal
#' @return nothing is returned
#' @export
#' @examples
#' mtq <- mf_get_mtq()
#' mf_map(mtq)
#' mf_map(mtq, c("POP", "STATUS"), "prop_typo", add = TRUE)
mf_prop_typo <- function(...) {
  .Defunct(msg = "'mf_prop_typo()' is defunct.\nUse 'mf_map()' instead.")
}

#' @title Defunct - Plot proportional symbols
#' @description
#' This function is defunct. Please use `mf_map()` with `type = "prop"`
#' instead.
#' @param ... ignored
#' @keywords internal
#' @return nothing is returned
#' @export
#' @examples
#' mtq <- mf_get_mtq()
#' mf_map(mtq)
#' mf_map(mtq, "POP", "prop", add = TRUE)
mf_prop <- function(...) {
  .Defunct(msg = "'mf_prop()' is defunct.\nUse 'mf_map()' instead.")
}


#' @title Defunct - Plot symbols using choropleth coloration
#' @description
#' This function is defunct. Please use `mf_map()` with `type = "symb_choro"`
#' instead.
#' @param ... ignored
#' @keywords internal
#' @return nothing is returned
#' @export
#' @examples
#' mtq <- mf_get_mtq()
#' mf_map(mtq)
#' mf_map(mtq, c("STATUS", "MED"), "symb_choro", add = TRUE)
mf_symb_choro <- function(...) {
  .Defunct(msg = "'mf_symb_choro()' is defunct.\nUse 'mf_map()' instead.")
}

#' @title Defunct - Plot symbols
#' @description
#' This function is defunct. Please use `mf_map()` with `type = "symb"`
#' instead.
#' @param ... ignored
#' @keywords internal
#' @return nothing is returned
#' @export
#' @examples
#' mtq <- mf_get_mtq()
#' mf_map(mtq)
#' mf_map(mtq, "STATUS", "symb", add = TRUE)
mf_symb <- function(...) {
  .Defunct(msg = "'mf_symb()' is defunct.\nUse 'mf_map()' instead.")
}

#' @title Defunct - Plot a typology map
#' @description
#' This function is defunct. Please use `mf_map()` with `type = "typo"`
#' instead.
#' @param ... ignored
#' @keywords internal
#' @return nothing is returned
#' @export
#' @examples
#' mtq <- mf_get_mtq()
#' mf_map(mtq, "STATUS", "typo")
mf_typo <- function(...) {
  .Defunct(msg = "'mf_typo()' is defunct.\nUse 'mf_map()' instead.")
}


#' @title Defunct - Initialize a map with a specific extent
#' @name mf_init
#' @description
#' This function is defunct. Please use `mf_map()` instead.
#' @param ... ignored
#' @keywords internal
#' @return nothing is returned
#' @export
#' @examples
#' mtq <- mf_get_mtq()
#' target <- mtq[30, ]
#' mf_map(target, type = "base", col = NA, border = NA)
#' mf_map(mtq, add = TRUE)
#' # or
#' mf_map(mtq, extent = target)
mf_init <- function(...) {
  .Defunct(
    msg = "'mf_init()' is defunct.\nUse 'mf_map()' instead."
  )
}
