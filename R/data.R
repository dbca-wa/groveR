# Functions to make directories and example data

#' A Function to create folders for user data
#'
#' \code{make_folders}
#'
#' @details This function creates the directories and sub-directories required
#'     to store user data (or example data if working through the vignettes).
#'     These are:
#' \itemize{
#'     \item `raster_masks/`
#'     \item `raster_masks/cloud_masks/`
#'     \item `supplementary/`
#'     \item `vectors/`
#'     }
#'
#' @param p Character file path for the processing folder (top level directory).
#'     Defaults to current location, i.e. ".".
#'
#' @return Creates directories and sub-directories for user data.
#'
#' @author Bart Huntley, \email{bart.huntley@@dbca.wa.gov.au}
#'
#' @examples
#' \dontrun{
#' make_folders(p = ".")
#' }
#'
#' @export
make_folders <- function(p = "."){
  a <- file.path(p, "raster_masks")
  if (!file.exists(a)) {dir.create(a)}
  b <- file.path(a, "cloud_masks")
  if (!file.exists(b)) {dir.create(b)}
  c <- file.path(p, "supplementary")
  if (!file.exists(c)) {dir.create(c)}
  d <- file.path(p, "vectors")
  if (!file.exists(d)) {dir.create(d)}
}


#' A function to set up example data for practice
#'
#' \code{make_example_data}
#'
#' @details This function extracts the internal example data, shipped with the
#'     package, and saves it into the processing folder structure as created by
#'     \code{link{make_folders}}.
#'
#' @param p Character file path for the processing folder (top level directory).
#'     Defaults to current location, i.e. ".".
#'
#' @return Creates dummy user data.
#'
#' @author Bart Huntley, \email{bart.huntley@@dbca.wa.gov.au}
#'
#' @examples
#' \dontrun{
#' make_example_data(p = ".")
#' }
#'
#' @importFrom terra rast writeRaster
#' @importFrom readr read_csv write_csv
#' @importFrom sf st_read st_write
#'
#' @export
make_example_data <- function(p = "."){
  suppressWarnings({
    #land mask
    lmaskp <- system.file("extdata/raster_masks", "land_msk.tif", package = "groveR")
    lmask <- terra::rast(lmaskp)
    fname <- file.path(p, "raster_masks/land_msk.tif")
    terra::writeRaster(lmask, filename = fname, datatype = 'INT1U')

    #csvs
    calp <- system.file("extdata/supplementary", "calibration.csv", package = "groveR")
    cal <- readr::read_csv(calp)
    cname <- file.path(p, "supplementary/calibration.csv")
    readr::write_csv(cal, path = cname)

    denp <- system.file("extdata/supplementary", "density_classes.csv", package = "groveR")
    den <- readr::read_csv(denp)
    cname <- file.path(p, "supplementary/density_classes.csv")
    readr::write_csv(den, path = cname)

    trnp <- system.file("extdata/supplementary", "trend_classes.csv", package = "groveR")
    trn <- readr::read_csv(trnp)
    cname <- file.path(p, "supplementary/trend_classes.csv")
    readr::write_csv(trn, path = cname)

    #shapefile
    shpp <- system.file("extdata/vectors", "regions.shp", package = "groveR")
    shp <- sf::st_read(shpp)
    sname <- file.path(p, "vectors/regions.shp")
    sf::st_write(shp, dsn = sname)

    shp2p <- system.file("extdata/vectors", "cloud_vectors.shp", package = "groveR")
    shp2 <- sf::st_read(shp2p)
    sname2 <- file.path(p, "vectors/cloud_vectors.shp")
    sf::st_write(shp2, dsn = sname2)

  })
}

#' A function to create a folder and set up example index data
#'
#' \code{make_index_rasters}
#'
#' @details This function firstly creates a folder (`index_rasters/`) and then
#'     extracts the internal index example data, shipped with the package, and
#'     writes it to this location. This is handled separately to the other
#'     `make_` functions as this data and its location are only applicable to
#'     following examples in the package vignettes.
#'
#' @param p Character file path for the processing folder (top level directory).
#'     Defaults to current location, i.e. ".".
#'
#' @return Creates a folder `index_rasters/` and dummy index data.
#'
#' @author Bart Huntley, \email{bart.huntley@@dbca.wa.gov.au}
#'
#' @examples
#' \dontrun{
#' make_index_rasters(p = ".")
#' }
#'
#' @importFrom raster raster writeRaster
#'
#' @export
make_index_rasters <- function(p = "."){
  suppressWarnings({
    #make folder
    ifol <- file.path(p, "index_rasters")
    if (!file.exists(ifol)) {dir.create(ifol)}

    #data
    a <- system.file("extdata/index_rasters",
                     "lgcsmp_ndvi_2007.tif", package = "groveR")
    ra <- terra::rast(a)
    dname <- file.path(p, "index_rasters/lgcsmp_ndvi_2007.tif")
    terra::writeRaster(ra, filename = dname)

    b <- system.file("extdata/index_rasters",
                     "lgcsmp_ndvi_2008.tif", package = "groveR")
    rb <- terra::rast(b)
    dname <- file.path(p, "index_rasters/lgcsmp_ndvi_2008.tif")
    terra::writeRaster(rb, filename = dname)

    c <- system.file("extdata/index_rasters",
                     "lgcsmp_ndvi_2009.tif", package = "groveR")
    rc <- terra::rast(c)
    dname <- file.path(p, "index_rasters/lgcsmp_ndvi_2009.tif")
    terra::writeRaster(rc, filename = dname)

    d <- system.file("extdata/index_rasters",
                     "lgcsmp_ndvi_2010.tif", package = "groveR")
    rd <- terra::rast(d)
    dname <- file.path(p, "index_rasters/lgcsmp_ndvi_2010.tif")
    terra::writeRaster(rd, filename = dname)

    e <- system.file("extdata/index_rasters",
                     "lgcsmp_ndvi_2011.tif", package = "groveR")
    re <- terra::rast(e)
    dname <- file.path(p, "index_rasters/lgcsmp_ndvi_2011.tif")
    terra::writeRaster(re, filename = dname)

    g <- system.file("extdata/index_rasters",
                     "lgcsmp_ndvi_2013.tif", package = "groveR")
    rg <- terra::rast(g)
    dname <- file.path(p, "index_rasters/lgcsmp_ndvi_2013.tif")
    terra::writeRaster(rg, filename = dname)

    h <- system.file("extdata/index_rasters",
                     "lgcsmp_ndvi_2014.tif", package = "groveR")
    rh <- terra::rast(h)
    dname <- file.path(p, "index_rasters/lgcsmp_ndvi_2014.tif")
    terra::writeRaster(rh, filename = dname)

    i <- system.file("extdata/index_rasters",
                     "lgcsmp_ndvi_2015.tif", package = "groveR")
    ri <- terra::rast(i)
    dname <- file.path(p, "index_rasters/lgcsmp_ndvi_2015.tif")
    terra::writeRaster(ri, filename = dname)

    j <- system.file("extdata/index_rasters",
                     "lgcsmp_ndvi_2016.tif", package = "groveR")
    rj <- terra::rast(j)
    dname <- file.path(p, "index_rasters/lgcsmp_ndvi_2016.tif")
    terra::writeRaster(rj, filename = dname)

    k <- system.file("extdata/index_rasters",
                     "lgcsmp_ndvi_2017.tif", package = "groveR")
    rk <- terra::rast(k)
    dname <- file.path(p, "index_rasters/lgcsmp_ndvi_2017.tif")
    terra::writeRaster(rk, filename = dname)

    l <- system.file("extdata/index_rasters",
                     "lgcsmp_ndvi_2018.tif", package = "groveR")
    rl <- terra::rast(l)
    dname <- file.path(p, "index_rasters/lgcsmp_ndvi_2018.tif")
    terra::writeRaster(rl, filename = dname)

    m <- system.file("extdata/index_rasters",
                     "lgcsmp_ndvi_2019.tif", package = "groveR")
    rm <- terra::rast(m)
    dname <- file.path(p, "index_rasters/lgcsmp_ndvi_2019.tif")
    terra::writeRaster(rm, filename = dname)

    n <- system.file("extdata/index_rasters",
                     "lgcsmp_ndvi_2020.tif", package = "groveR")
    rn <- terra::rast(n)
    dname <- file.path(p, "index_rasters/lgcsmp_ndvi_2020.tif")
    terra::writeRaster(rn, filename = dname)

    o <- system.file("extdata/index_rasters",
                     "lgcsmp_ndvi_2021.tif", package = "groveR")
    ro <- terra::rast(o)
    dname <- file.path(p, "index_rasters/lgcsmp_ndvi_2021.tif")
    terra::writeRaster(ro, filename = dname)

    q <- system.file("extdata/index_rasters",
                     "lgcsmp_ndvi_2022.tif", package = "groveR")
    rq <- terra::rast(q)
    dname <- file.path(p, "index_rasters/lgcsmp_ndvi_2022.tif")
    terra::writeRaster(rq, filename = dname)

    r <- system.file("extdata/index_rasters",
                     "lgcsmp_ndvi_2023.tif", package = "groveR")
    rr <- terra::rast(r)
    dname <- file.path(p, "index_rasters/lgcsmp_ndvi_2023.tif")
    terra::writeRaster(rr, filename = dname)

  })
}

