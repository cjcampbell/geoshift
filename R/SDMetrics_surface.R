#' SDMetrics surface
#'
#' An organizing class to group probability-of-origin surfaces with their temporal grouping (i.e., "Summer") and presence-only points.
#'
#' @slot rast A rasterLayer probability-of-occurrence surface
#' @slot group A character identifying the time interval or other attribute associated with the surface.
#' @slot coords Presence points used to create the 'rast' surface.
#'
#' @name SDMetrics_surface-class
#' @rdname SDMetrics_surface-class
#' @export
