get_pkg_repo <- function(pkg) {
  vapply(pkg, get_pkg_repo_single, "")
}

get_pkg_repo_single <- function(pkg) {
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
}
