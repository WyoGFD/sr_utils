#' addSR
#'
#' Add seasonal range polygons to a \code{leaflet} map
#'
#' @param lf \code{leaflet} object or proxy
#' @param sr \code{sf} object containing seasonal range polygons
#' @param leg logical - add a legend?
#' @param hide logical - hide seasonal range polygons on render?
#'
#' @return \code{leaflet} object
#' @export
#'
#' @examples
#'
#' sr <- wgfd_agol_data("Antelope_Seasonal_Range")
#'
#' leaflet::leaflet() |>
#'   leaflet::addProviderTiles(leaflet::providers$Esri.WorldTopoMap) |>
#'   addSR(sr, hide = FALSE)
#'
addSR <- function(lf, sr, leg = TRUE, hide = TRUE) {

  bright_blue <- "#1AA7EC"
  neutral_gray <- "#F0F3F3"

  sr_pal <- grDevices::colorRampPalette(c(bright_blue, neutral_gray))

  sr_palette_tbl <- tibble::tribble(
    ~ range, ~ color, ~ opacity,
    "OUT", "#FFFFFF", 0,
    "CRUSWR", bright_blue, 1,
    "CRUWIN", bright_blue, 1,
    "CRUWYL", bright_blue, 1,
    "WIN", sr_pal(4)[2], 0.75,
    "WYL", sr_pal(4)[2], 0.75,
    "SWR", sr_pal(4)[3], 0.5,
    "YRL", neutral_gray, 0.25,
    "SSF", neutral_gray, 0.25
  )

  sr_legend <- sr_palette_tbl  |>
    dplyr::filter(.data$range != "OUT")  |>
    dplyr::group_by(.data$color, .data$opacity) |>
    dplyr::summarize(
      label = paste(.data$range, collapse = "/"),
      .groups = "drop"
    )

  out <- lf |>
    leaflet::addPolygons(
      data = sr |>
        dplyr::filter(.data$RANGE != "OUT") |>
        sf::st_transform(4326) |>
        dplyr::inner_join(sr_palette_tbl, by = c("RANGE" = "range")),
      color = ~ color,
      fillOpacity = ~ opacity,
      label = ~ RANGE,
      group = "Existing Seasonal Range"
    )

  if (leg) {
    out <- out |>
      leaflet::addLegend(
        labels = sr_legend$label,
        colors = sr_legend$color,
        opacity = sr_legend$opacity,
        group = "Existing Seasonal Range"
      )
  }

  if (hide) {
    out <- out |>
      leaflet::hideGroup("Existing Seasonal Range")
  }

  out

}
