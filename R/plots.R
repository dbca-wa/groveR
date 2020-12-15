# Plotting functions

#' A function to generate a stacked bar chart for mangrove density classes.
#'
#' \code{veg_dens_class_plot} creates stacked bar chart of vegetation densities.
#'
#' @details Takes a file path to an extent summary csv file as created by
#'     \code{link{veg_class_area}} located in `extent_summaries\` and produce
#'     a stacked bar chart representing the same.
#'
#' @param icsv Character representation of the name of the extent summary
#'     csv file including file path.
#'
#' @param cap Character string of caption (not title) to supply to the plot. If
#'     none required use empty quotes i.e. "".
#'
#' @return A PNG format graphic will be written to the `extent_summaries\`
#'     directory.
#'
#' @author Bart Huntley, \email{bart.huntley@@dbca.wa.gov.au}
#'
#' @examples
#' \dontrun{
#' veg_dens_class_plot(icsv = "./extent_summaries/NatPark_2005-2009_extent_summaries.csv",
#'     cap = "RSSA")
#' }
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @import ggplot2
#' @importFrom readr read_csv
#'
#' @export
veg_dens_class_plot <- function(icsv, cap){
  dens_class_cols <- c('10-19%' = "#33FE31",
                       '20-29%' = "#20CB27",
                       '30-49%' = "#109A1D",
                       '50-69%' = "#046C13",
                       '70-100%' = "#004108",
                       'Cloud' = "black",
                       'Cloud 10-19%' = "#AFEF5A",
                       'Cloud 20-29%' = "#8EC045",
                       'Cloud 30-49%' = "#6E9232",
                       'Cloud 50-69%' = "#4F6820",
                       'Cloud 70-100%' = "#314010")
  # summarise data
  df <-  readr::read_csv(icsv) %>%
    dplyr::filter(DensityClass != "Other") %>%
    dplyr::group_by(Region, Site, Year, DensityClass) %>%
    dplyr::summarise(a = sum(Area))

  sites <- unique(df$Site)
  for(i in seq_along(sites)){
    site <- sites[i]
    df2 <- df %>%
      dplyr::filter(Site == site)
    # helpers
    yr_range <- paste0(min(df$Year), '-', max(df$Year))
    scalex <- min(df$Year):max(df$Year)


    p  <- ggplot(df2, aes(x = Year, y = a, fill = DensityClass)) +
      geom_bar(position="stack", stat="identity") +
      scale_fill_manual(values = dens_class_cols) +
      scale_x_continuous(breaks = scalex) +
      labs(title = paste("Mangrove density class summary", yr_range),
           subtitle = paste(df2$Region[1], df2$Site[1]),
           y = "Area (ha)",
           x = "",
           caption = bquote(italic(.(cap)))) +
      theme_bw() +
      theme(axis.text.x = element_text(angle=70, vjust = 0.5))

    pname <- paste0(dirname(icsv), "/", gsub(" ", "_", df2$Site[1]),"_",
                    yr_range, ".png")
    ggsave(p, filename = pname, width = 10, height = 7)
  }

}

#' A function to generate a stacked bar chart for mangrove change summaries.
#'
#' \code{ext_chng_plot} creates stacked bar chart of vegetation change summaries.
#'
#' @details Takes a file path to an extent change csv file as created by
#'     \code{link{change_extent}} located in `extent_change\` and produce
#'     a stacked bar chart representing the same.
#'
#' @param icsv Character representation of the name of the extent change
#'     csv file including file path.
#'
#' @param cap Character string of caption (not title) to supply to the plot. If
#'     none required use empty quotes i.e. "".
#'
#' @return A PNG format graphic will be written to the `extent_change\`
#'     directory.
#'
#' @author Bart Huntley, \email{bart.huntley@@dbca.wa.gov.au}
#'
#' @examples
#' \dontrun{
#' ext_chng_plot(icsv = "./extent_change/NatPark_2005-2009_extent_summaries.csv",
#'     cap = "RSSA")
#' }
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @import ggplot2
#' @importFrom readr read_csv
#'
#' @export
ext_chng_plot <- function(icsv, cap){
  ext_chng_cols <- c('gain' = '#0000CC',
                     'stable' = '#808080',
                     'loss' = '#FF0000',
                     'cloud no data' = 'black',
                     'cloud likely gain' = '#00CCFF',
                     'cloud likely stable' = '#D9D9D9',
                     'cloud likely loss' = "#FF9999"
  )
  # summarise data - factors for plotting order for Status
  df <-  readr::read_csv(icsv) %>%
    dplyr::filter(!is.na(Status)) %>%
    dplyr::group_by(Region, Site, Period, Status) %>%
    dplyr::summarise(a = sum(Area)) %>%
    dplyr::mutate(Status = factor(Status,
                                  levels = c('loss', 'cloud likely loss','gain', 'cloud likely gain',
                                             'stable', 'cloud likely stable',
                                             'cloud no data', NA)))
  sites <- unique(df$Site)
  for(i in seq_along(sites)){
    site <- sites[i]
    df2 <- df %>%
      dplyr::filter(Site == site)
    # helpers
    as_of <- Sys.Date()

    p  <- ggplot(df2, aes(x = Period, y = a, fill = Status)) +
      geom_bar(position="stack", stat="identity") +
      scale_fill_manual(values = ext_chng_cols) +
      labs(title = "Mangrove change summaries",
           subtitle = paste(df2$Region[1], df2$Site[i]),
           y = "Area (ha)",
           x = "",
           caption = bquote(italic(.(cap)))) +
      theme_bw() +
      theme(axis.text.x = element_text(angle=70, vjust = 0.5))

    pname <- paste0(dirname(icsv), "/", df2$Region[1], "_",
                    gsub(" ", "_", df2$Site[i]), ".png")
    ggsave(p, filename = pname, width = 9, height = 7)
  }
}

