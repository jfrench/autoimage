.onAttach <- function(...) {
  date <- date()
  # x <- regexpr('[0-9]{4}', date) yr <- substr(date, x[1], x[1] +
  # attr(x, 'match.length') - 1)
  greet <- paste("# This package was created for research supported by NSF Grant DMS-1463642 and NIH Grant R01 CA157528")
  packageStartupMessage(greet)
}
