#' @export
plot.ghstars_history_tbl <- function(object, geom = "step") {
  geom <- match.arg(geom, c("step", "line"))
  n_repos <- length(unique(object$repo))
  if (n_repos == 1L) {
    plot_single_repo(object, geom)
  } else {
    plot_multiple_repos(object, geom)
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
      x = "Date",
      y = "Cumulative Number of Stars",
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
      y = "Cumulative Number of Stars",
      title = "GitHub Star History"
    )
}

plot.ghstars_tbl <- function(object) {
  # To avoid the 'no visible binding for global variable' note when
  # running R CMD check
  repo <- NULL
  stars <- NULL
  old_theme <- ggcharts::ggcharts_set_theme("theme_ng")
  on.exit(ggcharts::ggcharts_set_theme(old_theme))
  ggcharts::bar_chart(object, repo, stars) +
    labs(x = "Repository", y = "Stars")
}
