#' timetables and tariffs are available at:
#' https://www.emtu.sp.gov.br/emtu/itinerarios-e-tarifas/encontre-uma-linha/pelo-numero-da-linha.fss
#' search for line 953, which is tram line 1. there we have timetables, tariff integration, and
#' itinerary. i'm too lazy to webscrape this so i'll just copy to a spreadsheet and save it in
#' data-raw, but feel free to do it more technologically if you will


# setup ---------------------------------------------------------------------------------------

library(dplyr)
library(sf)
library(geoarrow)

shapes_sf <- arrow::open_dataset("data/shapes_sf_adjusted.parquet") |> 
  st_as_sf()
stops_sf <- arrow::open_dataset("data/stops_sf.parquet") |> 
  st_as_sf()
departures <- readxl::read_xlsx("data-raw/departures.xlsx", sheet = "long")



# calendar ------------------------------------------------------------------------------------

## we'll visually inspect this and hard-code calendar and frequencies using the table as a proxy
departures <- departures |>
  mutate(departure = hms::as_hms(departure)) |> 
  # mutate(departure = as.character(departure) |> stringr::str_sub(-8, -1)) |> 
  group_by(proto_service, stop_name) |> 
  mutate(headway = lead(departure) - departure) |> 
  filter(!is.na(headway) & headway > 0)

departures |> 
  mutate(main_service = stringr::str_sub(proto_service, 1, 1)) |> 
  ggplot() +
  geom_line(aes(x = departure, y = headway, color = proto_service)) +
  facet_wrap(vars(main_service))

calendar <- tibble::tribble(
  ~proto_service_id, ~monday, ~tuesday, ~wednesday, ~thursday, ~friday, ~saturday, ~sunday, 
  "U_1",     1, 1, 1, 1, 1, 0, 0,  # valley 1 weekdays
  "U_2",     1, 1, 1, 1, 1, 0, 0,  # peak 1 weekdays
  "U_3",     1, 1, 1, 1, 1, 0, 0,  # valley 2 weekdays
  "U_4",     1, 1, 1, 1, 1, 0, 0,  # peak 2 weekdays
  "U_5",     1, 1, 1, 1, 1, 0, 0,  # valley 3 weekdays
  "S_1",     0, 0, 0, 0, 0, 1, 0,  # valley 1 saturday
  "S_2",     0, 0, 0, 0, 0, 1, 0,  # peak 1 saturday
  "S_3",     0, 0, 0, 0, 0, 1, 0,  # valley 2 saturday
  "S_4",     0, 0, 0, 0, 0, 1, 0,  # peak 2 saturday
  "S_5",     0, 0, 0, 0, 0, 1, 0,  # valley 3 saturday
  "D_1",     0, 0, 0, 0, 0, 0, 1,  # valley 1 sunday
  "D_2",     0, 0, 0, 0, 0, 0, 1,  # peak sunday
  "D_3",     0, 0, 0, 0, 0, 0, 1,  # valley 2 sunday
)



# frequencies ---------------------------------------------------------------------------------
frequencies <- tibble::tribble(
  ~proto_service_id, ~start_time, ~end_time,  ~headway_secs,
  "U_1",           "05:30:00",  "06:29:59", 720,
  "U_2",           "06:30:00",  "08:34:59", 360,
  "U_3",           "08:35:00",  "15:54:59", 1200,
  "U_4",           "15:55:00",  "19:54:59", 360,
  "U_5",           "19:55:00",  "23:59:59", 1200,
  "S_1",           "05:30:00",  "06:19:59", 1500,
  "S_2",           "06:20:00",  "08:15:59", 1200,
  "S_3",           "08:16:00",  "14:39:59", 1920,
  "S_4",           "14:40:00",  "16:29:59", 1080,
  "S_5",           "16:30:00",  "23:59:59", 1800,
  "D_1",           "05:30:00",  "10:29:59", 1800,
  "D_2",           "10:30:00",  "20:29:59", 1500,
  "D_3",           "20:30:00",  "23:59:59", 1800
)

block_time_l1 <- 40/60
length_l1 <- shapes_sf |> 
  st_drop_geometry() |> 
  filter(stringr::str_detect(shape_id, "L1_0")) |> 
  summarise(length_km = sum(length_km)) |> 
  pull()

avg_speed <- units::drop_units(length_l1)/block_time_l1
avg_speed <- round(avg_speed)

# 666666666666666
zel <- shapes_sf |>
  st_drop_geometry() |> 
  cross_join(frequencies) |> 
  arrange(proto_service_id, start_time, shape_id, from_order) |>
  group_by(name, direction, proto_service_id, start_time) |> 
  mutate(
    speed = avg_speed,
    time = units::drop_units(length_km/speed), 
    elapsed = hms::hms(hours = cumsum(time))
  ) |> 
  ungroup()

stop_times <- full_join(stop_times, frequencies, relationship = "many-to-many") |> 
  mutate(start_time = hms::as_hms(start_time)) |> 
  arrange(trip_id, start_time, stop_sequence) |> 
  group_by(trip_id, start_time) |> 
  mutate(
    arrival_time = hms::hms(lubridate::seconds_to_period(start_time + elapsed)),
    departure_time = arrival_time
  ) |> 
  ungroup()