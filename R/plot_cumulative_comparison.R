#' Title
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
plot_cumulative_comparison <- function(data) { # add month over month, add change in activity
  summary <- data %>%
    mutate(
      date = lubridate::date(time)
    ) %>%
    group_by(date) %>%
    summarise(
      distance = sum(dist_to_prev),
      duration = sum(time_diff_to_prev) / 60^2
    ) %>%
    tidyr::complete(
      date = seq(lubridate::floor_date(min(date), unit = "year"), max(date), by = "1 day"),
      fill = list(distance = 0, duration = 0)
    ) %>%
    mutate(
      year = lubridate::year(date),
      day_of_year = lubridate::yday(date)
    )

  summary_cumsum <- summary %>%
    group_by(year) %>%
    mutate(
      distance_cumsum = cumsum(distance),
      duration_cumsum = cumsum(duration)
    )

  summary_cumsum %>%
    group_by(year) %>%
    mutate(
      color = if_else(
        year == lubridate::year(Sys.Date()),
        "#FE5502",
        "#FFE6D6"
      ),
      label = if_else(
        date == max(date),
        as.character(year),
        NA_character_
      )
    ) %>%
    ggplot(aes(day_of_year, duration_cumsum, color = color, group = year)) +
    geom_line(size = 2) +
    scale_y_continuous(position = "right") +
    scale_color_identity() +
    theme_void() +
    ggrepel::geom_label_repel(aes(label = label), na.rm = TRUE) +
    labs(title = "So far so good") +
    # theme(panel.background = element_rect(fill = "#252323"))
    theme(
      plot.title = element_text(size = 22, family = "Tahoma", face = "bold"), axis.ticks = element_blank()
    )
  # axis.title.x=element_blank(), axis.title.y=element_blank(), legend.position="none", panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
  # panel.grid.minor=element_blank(),plot.background=element_blank()

  # data <- strava::process_data("../strava_test/export_14617031/activities/")
}
