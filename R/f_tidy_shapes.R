# helper functions to sync stops and shapes

# split_lines ---------------------------------------------------------------------------------

#' split a linestring 

#' @param lines A `data.frame` with class `sf` with the `LINESTRING`s to be splited
#' @param points A `data.frame` with class `sf` with the `POINT`s to split `lines`
#' @param snap_tol Snap tolerance in meters
#' @param temp_crs A temporary CRS to use for the operations
#' @description
#' `split_lines()` Splits linestrings between two or more points.
#' @details
#'  `snap_tol` guarantees that ponts are located along the lines. However, this operation requires
#'  a UTM coordinate reference system, hence `tmp_crs` allows a temporary conversion.
#' @returns a `data.frame` with class `sf`
#' @export

split_lines <- function(lines, points, snap_tol = 10, tmp_crs = NULL) {
  
  if(!is.null(tmp_crs)) {
    old_crs <- st_crs(lines)
    lines <- st_transform(lines, crs = tmp_crs)
    points <- st_transform(points, crs = tmp_crs)
  }
  
  lines <- st_snap(lines, points, snap_tol)
  lines <- lwgeom::st_split(lines, points)
  lines <- lines |> st_collection_extract("LINESTRING") |> 
    st_cast("LINESTRING")
  
  if(!is.null(tmp_crs)) {
    lines <- lines |> 
      st_transform(crs = old_crs)
  }
  
  return(lines)
}



# find_endpoints ------------------------------------------------------------------------------

#' Identify endpoints in a line segment

#' @param lines A `data.frame` with class `sf` with the `LINESTRING`s to be splited
#' @param points A `data.frame` with class `sf` with the `POINT`s to split `lines`
#' @param point_name A character indicating the name column in `points` (optional)
#' @param order_name A character indicating the order column in `points` (optional)
#' @param reverse Should start and endpoints be reversed? Defautls to FALSE.
#' @description
#' `find_endpoints()` determines the feature in `points` that is closest to each segment in `lines`.
#' @details
#'  Finding the nearest feature is a good, but not perfect proxy, since it not necessarily lies 
#'  in the line. Therefore, providing either `point_name` or `point_order` is highly recommended  
#'  both for usability and for identifying false positives downstream.
#' @returns a `data.frame` with class `sf`
#' @export

find_endpoints <- function(lines, points, point_name = NULL, point_order = NULL, 
                           reverse = FALSE) {
  
  point_name <- rlang::sym(point_name)
  point_order <- rlang::sym(point_order)
  
  if(reverse) {
    lines <- lines |> 
      mutate(from = lwgeom::st_endpoint(geometry), to = lwgeom::st_startpoint(geometry)) 
  } else {
    lines <- lines |> 
      mutate(from = lwgeom::st_startpoint(geometry), to = lwgeom::st_endpoint(geometry)) 
  } 
  
  if(!is.null(point_name)) {
    df <- points |> select(x = {{point_name}})
    lines <- lines |>
      mutate(
        from_name = df$x[st_nearest_feature(from, df)],
        to_name = df$x[st_nearest_feature(to, df)]
      )
  }
  
  if(!is.null(point_order)) {
    df <- points |> select(x = {{point_order}})
    lines <- lines |>
      mutate(
        from_order = df$x[st_nearest_feature(from, df)],
        to_order = df$x[st_nearest_feature(to, df)]
      )
  }
  
  return(lines)
}

