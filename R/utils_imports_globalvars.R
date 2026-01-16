#' @keywords internal
"_PACKAGE"

#' Internal imports
#'
#' Declare imports so R CMD check recognizes usage of packages listed in
#' DESCRIPTION Imports. Keep only functions you actually use.
#'
#' @keywords internal
#' @noRd
#'
#' @import rExpertQuery
NULL

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL

# Silence NSE binding notes for common data.table symbols
utils::globalVariables(c(
  "ATTAINS.AssessmentUnitIdentifier",
  "ATTAINS.WaterType",
  "TADA.AURefSource",
  "TADA.MonitoringLocationIdentifier",
  "name"
))
