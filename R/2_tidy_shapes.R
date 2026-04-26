#' sync stops and shapes
#' we'll use the helper functions in "f_tidy_shapes.R", but first we need to do some manual changes
#' relevant to the local context. skip directly to `spatial operations` at your discretion 


# setup ---------------------------------------------------------------------------------------

library(dplyr)
library(sf)
library(geoarrow)
source("R/f_tidy_shapes.R")

shapes_sf <- arrow::open_dataset("data/shapes_sf.parquet") |> 
  st_as_sf()
stops_sf <- arrow::open_dataset("data/stops_sf.parquet") |> 
  st_as_sf()


# manipulation --------------------------------------------------------------------------------

##' we unite segments first because they're neither evenly distributed along the stations nor 
##' a single linestring for each service. 
##' IMPORTANT: here we have one linestring for each track. Otherwise, double caution is needed 
##' since you might need to reverse the order of `from` and `to` in `find_endpoints()`.
shapes_sf <- shapes_sf |> 
  group_by(name, direction) |> 
  summarise(geometry = st_union(geometry), .groups = "drop")


##' separate services and directions
##' our case is a little more complicated because it's a Y operation with two branches and one trunk.
##' Therefore, we'll create a list with 4 `df`s
shapes_list <- list(
  "L1_1" = shapes_sf |> filter(name == "Linha 1" & direction == 1),
  "L1_0" = shapes_sf |> filter(name == "Linha 1" & direction == 0),
  "L2_1" = shapes_sf |> filter(direction == 1),
  "L2_0" = shapes_sf |> filter(direction == 0)
)

##' the stops case requires even more "creativity", i.e. we need some workarounds
stops_list <- list()

stops_list[["L1_1"]] = stops_sf |> filter(segment < 2) |> arrange(order)

stops_list[["L1_0"]] <- stops_sf |> 
  filter(segment < 2) |> 
  mutate(order = 16 - order) |> 
  arrange(order)

###' ATTENTION to this example in case of Y operations:
###' we have to intentionally include segment 0 because we cannot exclude it from the shapes df.
###' hence, the only way to exclude it after `split_lines` is by indicating that it belongs to 
###' "Terminal Porto" station
stops_list[["L2_1"]] <-  stops_sf |> 
  filter(segment < 2 | (segment == 2 & order < 21)) |> 
  arrange(order)

stops_list[["L2_2"]] <- stops_sf |> 
  filter((segment == 1 & between(order, 1, 14)) | (segment == 2 & order > 20)) |> 
  mutate(order = case_when(
    segment == 2 ~ order - 20,
    segment == 1 ~ abs(15 - order) + 6
  )) |> 
  arrange(order)


# spatial operations --------------------------------------------------------------------------

shapes_list <- purrr::map2(
  shapes_list, stops_list, 
  \(x, y) split_lines(x, y, tmp_crs = 31983)
)

shapes_list <- purrr::map2(
  shapes_list, stops_list, 
  \(x, y) find_endpoints(x, y, "name", "order") |> 
    mutate(from = from_name, to = to_name, .keep = "unused") |> 
    filter(from != to) |> 
    arrange(from_order)
  )

## visual inspection if needed
# library(ggplot2)
# sh[[3]] |> 
#   filter(from_order == 14) |> 
#   ggplot(aes(color =to)) +
#   geom_sf()

## quick workaround because of the Y operation
shapes_list[[3]] <- shapes_list[[3]] |> 
  filter(to != "Terminal Porto")

## bind lists --- this is our desired output for the shapes table.
shapes_sf <- bind_rows(shapes_list, .id = "service")

## calculate lengths --- important for time calculations
shapes_sf <- shapes_sf |> 
  mutate(length_km = st_length(geometry) |> units::set_units("km"))

## finally, unique shape ids
shapes_sf <- shapes_sf |> 
  tibble::rowid_to_column() |> 
  mutate(shape_id = paste(service, rowid, sep = "_"), .keep = "unused", .before = everything()) |> 
  relocate(geometry, .after = everything())

arrow::write_parquet(shapes_sf, "data/shapes_sf_adjusted.parquet")
