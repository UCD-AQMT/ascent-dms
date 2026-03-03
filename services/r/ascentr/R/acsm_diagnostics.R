
#' Title
#'
#' @param site
#' @param start_dt
#' @param end_dt
#' @param con
#'
#' @returns
#' @export
#'
#' @import ggplot2
#' @import patchwork
#'
#' @examples
acsm_diagnostics <- function(site, start_dt, end_dt, con) {

  acsm_sa <- tbl(con, I("acsm.sample_analysis"))
  diag_calib <- tbl(con, I("acsm.diag_calib"))
  mass_loadings <- tbl(con, I("acsm.mass_loadings"))
  tps <- tbl(con, I("acsm.tps"))
  sites <- tbl(con, I("common.sites"))

  df <- acsm_sa |>
    inner_join(select(sites, site_number, site_code), by = "site_number") |>
    inner_join(select(tps, sample_analysis_id, hb, filament_emm),
               by = c("id"="sample_analysis_id")) |>
    inner_join(select(mass_loadings, sample_analysis_id, rie_nh4, rie_so4),
               by = c("id"="sample_analysis_id")) |>
    inner_join(select(diag_calib, sample_analysis_id, ie_ionspg, abref, ab_total, flow_ccs),
               by = c("id"="sample_analysis_id")) |>
    filter(site_code == site,
           start_date >= start_dt,
           stop_date <= end_dt,
           status == 0) |>
    arrange(start_date) |>
    collect()

  # Censor some truly out there filament emissions
  df <- df |>
    mutate(filament_emm = if_else(filament_emm > 1, NA, filament_emm))

  # reduce the calibration parameters to the first point
  calibs <- df |>
    select(start_date, ie_ionspg, abref, rie_nh4, rie_so4) |>
    arrange(start_date) |>
    mutate(across(ie_ionspg:rie_so4, dplyr::lag, .names = "next_{.col}")) |>
    mutate(ie_diff = ie_ionspg - next_ie_ionspg,
           ab_diff = abref - next_abref,
           nh4_diff = rie_nh4 - next_rie_nh4,
           so4_diff = rie_so4 - next_rie_so4,
           Diff = ie_diff + ab_diff + nh4_diff + so4_diff) |>
    filter(is.na(Diff) | Diff != 0) |>
    select(start_date:rie_so4) |>
    mutate(ie_ab = ie_ionspg / abref)

  # Create dummy time records to be able to show gaps when data is collected
  # Find gaps
  gaps <- df |>
    select(id, start_date) |>
    mutate(next_date = dplyr::lead(start_date),
           time_diff = next_date - start_date)

  time_res <- gaps |>
    summarise(res = median(time_diff, na.rm = TRUE)) |>
    pull(res)

  gaps <- gaps |>
    mutate(gap_time = start_date + time_res) |>
    filter(time_diff > 3 * time_res) |>
    select(start_date = gap_time)

  # Insert dummy records
  df <- df |>
    full_join(gaps, by = "start_date") |>
    arrange(start_date)


  x_range <- c(min(df$start_date), max(df$start_date))

  baseplot <- ggplot(df, aes(x = start_date)) +
    scale_x_datetime(limits = x_range,
                     labels = scales::label_date_short(),
                     date_breaks = "1 month") +
    theme_classic() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.line.x = element_blank())

  baseplot_calib <- ggplot(calibs, aes(x = start_date)) +
    scale_x_datetime(limits = x_range,
                     labels = scales::label_date_short(),
                     date_breaks = "1 month") +
    theme_classic() +
    theme(panel.grid.major.y = element_line(color = "grey95"),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.line.x = element_blank())

  # The last plot needs an x-axis
  bottomplot <- ggplot(df, aes(x = start_date)) +
    scale_x_datetime(limits = x_range,
                     labels = scales::label_date_short(),
                     date_breaks = "1 month") +
    theme_classic() +
    theme(axis.title.x = element_blank())

  # plots to stack
  ab <- baseplot +
    geom_line(aes(y = ab_total), na.rm = TRUE) +
    # scale_y_continuous(limits = c(0, 300e3),
    #                    labels = scales::label_number(scale_cut = scales::cut_short_scale()),
    #                    oob = scales::squish) +
    scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
    labs(y = "Air Beam (Hz)")

  ie_no3 <- baseplot_calib +
    geom_point(aes(y = ie_ionspg), size = 3, color = "dodgerblue") +
    labs(y = expression(IE[NO3^"-"]~(ions/pg))) +
    theme(axis.title.y=element_text(color="dodgerblue"),
          axis.text.y=element_text(color="dodgerblue"))

  ie_ab <- baseplot_calib +
    geom_point(aes(y = ie_ab), size = 3) +
    # scale_y_continuous(labels = scales::label_number(scale = 1e6),
    #                    oob = scales::squish, limits = c(0, 1000 * 1e-6)) +
    scale_y_continuous(labels = scales::label_number(scale = 1e6)) +
    labs(y = expression(IE[NO3^"-"]/AB~x10^-6))

  scale_factor_rie <- max(calibs$rie_nh4) / max(calibs$rie_so4)
  rie <- baseplot_calib +
    geom_point(aes(y = rie_nh4), color = "goldenrod", size = 3) +
    geom_point(aes(y = rie_so4 * scale_factor_rie), color = "red", size = 3) +
    scale_y_continuous(name="RIE_NH4", sec.axis=sec_axis(~./scale_factor_rie, name="RIE_SO4")) +
    theme(
      axis.title.y.left=element_text(color="goldenrod"),
      axis.text.y.left=element_text(color="goldenrod"),
      axis.title.y.right=element_text(color="red"),
      axis.text.y.right=element_text(color="red")
    )

  # Scale primary y-axis to a reasonable amount
  hb_limits <- c(min(df$hb, na.rm = TRUE) * 0.95, max(df$hb, na.rm = TRUE) * 1.05)
  scale_factor_hb <- max(df$hb, na.rm = TRUE) / max(df$filament_emm, na.rm = TRUE)
  hb_fil <- baseplot +
    geom_line(aes(y = filament_emm * scale_factor_hb), color = "blue") +
    geom_line(aes(y = hb), color = "orange") +
    scale_y_continuous(name = "Heater Bias (V)", limits = hb_limits,
                       sec.axis=sec_axis(~./scale_factor_hb, name="Filament Emission (mA)")) +
    theme(
      axis.title.y.left=element_text(color="orange"),
      axis.text.y.left=element_text(color="orange"),
      axis.title.y.right=element_text(color="blue"),
      axis.text.y.right=element_text(color="blue")
    )

  flow <- bottomplot +
    geom_line(aes(y = flow_ccs)) +
    scale_y_continuous(limits = c(0, 2), oob = scales::squish) +
    labs(y = "Flow (cc/s)") +
    theme(axis.text.x = element_text())

  # Compose the final plot using notation from the patchwork package
  the_title <- paste("ACSM Diagnostics -", site, start_dt, "to", end_dt)

  plot <- ie_no3 / ab / ie_ab /  hb_fil / rie / flow
  plot <- plot +
    patchwork::plot_annotation(title = the_title,
                               theme = theme(plot.title = element_text(hjust = 0.5)))


}
