get_pkg_repo <- function(pkg) {
  repo <- vapply(pkg, get_pkg_repo_single, "")
  pkg_without_repo <- pkg[is.na(repo)]

  if (length(pkg_without_repo)) {
    msg <- paste0(
      "Could not find a GitHub repo for package ",
      paste(pkg_without_repo, collapse = ", "), "."
    )
    if (length(pkg_without_repo) == length(pkg)) {
      rlang::abort(msg)
    }
    rlang::warn(msg)
  }

  repo
}

get_pkg_repo_single <- memoise::memoise(function(pkg) {
  stopifnot(length(pkg) == 1L)
  if (is.null(ghstars_global$pkg_db)) {
    ghstars_global$pkg_db <- tools::CRAN_package_db()
  }
  pkg_db <- ghstars_global$pkg_db
  url <- pkg_db$URL[pkg_db$Package == pkg]
  if (length(url) == 0L) {
    return(structure(NA_character_, names = pkg))
  }

  url <- unlist(strsplit(url, ", "))
  gh_url <- url[grepl("github", url)]
  if (length(gh_url) == 0L) {
    return(structure(NA_character_, names = pkg))
  }

  match <- gregexpr("github.com/", gh_url)[[1L]]
  repo <- substr(gh_url, match + attr(match, "match.length"), nchar(gh_url))
  # Remove trailing slash if present
  if (substr(repo, nchar(repo), nchar(repo)) == "/") {
    repo <- substr(repo, 1L, nchar(repo)-1L)
  }
  names(repo) <- pkg
  repo
})
