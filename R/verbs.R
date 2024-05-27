

#' Connection class for a geopkg flie
#'
#' @slot CRS tbl.
#'
#' @export
#'
setClass("GeopkgConnection",
         contains="SQLiteConnection",
         slots=c(CRS="tbl"))

############

#' create connection to a geopackage database file
#'
#' @param gpkgname the filename, passed to DBI::dbConnect
#'
#' @return a connection object including structures required to create sf objects
#' @export
#'
#' @importFrom rlang .data
#' @examples
#' library(dplyr)
#' ncf <- system.file("gpkg/nc.gpkg", package="sf")
#' nc <- connectGPKG(ncf)
#' nc
#' tbl(nc, "nc.gpkg")
#' nicenames <- tibble::tibble(NAME=c("Northampton", "Stokes"))
#' f <- tbl(nc, "nc.gpkg")
#' collect(semi_join(f, nicenames, copy=TRUE))
#'
connectGPKG <- function(gpkgname) {
  db <- DBI::dbConnect(RSQLite::SQLite(), gpkgname)
  t1 <- tbl(db, "gpkg_geometry_columns")
  t2 <- tbl(db, "gpkg_spatial_ref_sys")

  db <- methods::as(db, "GeopkgConnection")
  db@CRS <- collect(left_join(t1, t2, by="srs_id"))
  db@CRS <- mutate(db@CRS, CRS=paste(.data$organization, .data$organization_coordsys_id, sep=":"))
  return(db)
}

#' show method for geopackage connection object
#' @rdname GeopkgConnection-methods
#' @method show GeopkgConnection
#' @param object the connection object
#' @return prints a summary of the object
#'
#' @export
#'
setMethod(f = "show",
          signature = "GeopkgConnection",
          definition = function(object) {
            callNextMethod()
            cat("Tables in this geopackage:\n")
            cat(paste(object@CRS$table_name), sep=", ")
          })


#' Create table from geopackage src
#'
#' @param src the geopkg connection
#' @param from table name
#' @param ... passed to methods
#'
#' @export
#'
tbl.GeopkgConnection <- function(src, from, ...) {
  crstbl <- filter(src@CRS, .data$table_name == from)
  thiscrs <- crstbl[1,"CRS", drop=TRUE]
  geomname <- crstbl[1, "column_name", drop=TRUE]
  res <- NextMethod()
  res$CRS <- thiscrs
  res$geomcolname <- geomname
  return(res)
}


#' Retrieves data into a local tibble
#'
#' @param x A lazy data frame backed by a database query.
#' @param ... other parameters passed to methods
#' @param n number of rows to fetch, default all
#' @param warn_incomplete Warn if n is less than the number of result rows?
#' @param cte use common table expressions
#'
#' @return a local sf tibble
#' @export
#'
collect.tbl_GeopkgConnection <- function(x, ..., n = Inf, warn_incomplete = TRUE, cte = FALSE) {
  res <- NextMethod()
  gname <- x[["geomcolname"]]
  # could use mutate, but then we'd have to do rlang stuff
  if (!is.null(res[[gname]])) {
    # some operations can remove geometry columns
    # check for missing geometries and filter. There doesn't seem to be a nice way
    # of converting them to empty. Note that st_read will do that.
    msng <- is.na(res[[gname]])
    if (any(msng)) {
      M <- res[msng, ]
      res <- res[!msng,]
      warning("Filtered missing geometries - check if this is OK")
    }
    res[[gname]] <-  sf::st_as_sfc(res[[gname]], crs = x[["CRS"]])
    res <- sf::st_as_sf(res)
  }
  return(res)
}
