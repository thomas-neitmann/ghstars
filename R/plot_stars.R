#' @export
plot.ghstars_history_tbl <- function(x, ..., geom = "step") {
  geom <- match.arg(geom, c("step", "line"))
  n_repos <- length(unique(x$repo))
  if (n_repos == 1L) {
    plot_single_repo(x, geom)
  } else {
    plot_multiple_repos(x, geom)
  }
}

plot_single_repo <- function(ghstars_tbl, geom = "step") {
  repo <- ghstars_tbl$repo[1L]
  geom <- paste0("geom_", geom)
  ggplot(ghstars_tbl, aes_string("date", "cumulative_stars")) +
    eval(call(geom, size = 1, color = "darkorange")) +
    ggcharts::theme_ng(grid = "xy", ticks = "x") +
    theme(legend.position = c(.05, .95)) +
    labs(
      x = NULL,
      y = NULL,
      title = paste("GitHub Star History of", repo)
    )
}

plot_multiple_repos <- function(ghstars_tbl, geom = "step") {
  geom <- paste0("geom_", geom)
  ggplot(ghstars_tbl, aes_string("day", "cumulative_stars", color = "repo", group = "repo")) +
    eval(call(geom, size = 1)) +
    scale_color_brewer(palette = "Set2", name = NULL) +
    ggcharts::theme_ng(grid = "XY") +
    theme(
      legend.position = c(.03, 1),
      legend.justification = c("left", "top")
    ) +
    labs(
      x = "Days Since 1st Star",
      y = NULL,
      title = "GitHub Star History"
    )
}

#' @export
plot.ghmetrics_tbl <- function(x, ..., metric = c("stars", "forks", "watchers", "open_issues")) {
  metric <- match.arg(metric)
  args <- list(
    data = x,
    x = quote(repo),
    y = as.symbol(metric)
  )

  old_theme <- ggcharts::ggcharts_set_theme("theme_ng")
  on.exit(ggcharts::ggcharts_set_theme(old_theme))
  do.call(ggcharts::bar_chart, args) +
    labs(x = NULL, y = tools::toTitleCase(gsub("_", " ", metric)))
}
