#' Since there's no official shapefile, we'll download osm data for reproductibility instead of
#' just drawing it by hand

library(dplyr)
library(sf)


# get data ------------------------------------------------------------------------------------


region_sf <- geobr::read_urban_concentrations(year = 2015) |> 
  filter(code_urban_concentration == 3548500) |> 
  st_transform(crs = 4326)

bbox <- st_bbox(region_sf)

osmdata::available_features()
osmdata::available_tags("railway") |> View()
osmdata::available_tags("public_transport")

osm_rail <- bbox |> 
  osmdata::opq() |> 
  osmdata::add_osm_feature(key = "railway") |> 
  osmdata::osmdata_sf()

saveRDS(osm_rail, "data-raw/osm_query_raw.rds")

# ## visual inspection: there's a little gap which is "Túnel do José Menino".
# mapview::mapviewOptions(platform = "leafgl")
# mapview::mapview(
#   osm_rail$osm_lines |> filter(stringr::str_detect(name, "VLT|Menino")) |> select(name, geometry), 
#   zcol = "name"
#   ) +
#   mapview::mapview(
#     osm_rail$osm_points |> 
#       filter(public_transport %in% c("station", "stop_position") & station == "light_rail")
#   )

## filter
shapes_sf <- osm_rail$osm_lines |> 
  filter(stringr::str_detect(name, "VLT|Menino") & !stringr::str_detect(name, "3")) |> 
  select(osm_id, name, geometry)

## standardize names
shapes_sf <- shapes_sf |> 
  mutate(name = recode_values(name, "Linha 2 do VLT" ~ "Linha 2", default = "Linha 1"))

stops_sf <- osm_rail$osm_points |> 
  filter(public_transport %in% c("station", "stop_position") & station == "light_rail") |> 
  select(osm_id, name, geometry)

# mapview::mapview(shapes_sf, zcol = "name") +
#   mapview::mapview(stops_sf)



# fine tuning ---------------------------------------------------------------------------------

##' send it to qgis 
##' shapes: remove railway switches and visually attribute direction to tracks
##' stops: attribute segment, i.e. 0 = line 1 only, 1 = trunk, 2 = line 2 only
write_sf(shapes_sf, "data/shapes_osm.gpkg")
write_sf(stops_sf, "data/stops_osm.gpkg")

library(geoarrow)
st_read("data/shapes_qgis.gpkg") |> 
  arrow::write_parquet("data/shapes_sf.parquet")

st_read("data/stops_qgis.gpkg") |> 
  filter(!is.na(segment)) |> # stop São Bento doesn't exist, so we attributed NA to segment 
  arrow::write_parquet("data/stops_sf.parquet")
