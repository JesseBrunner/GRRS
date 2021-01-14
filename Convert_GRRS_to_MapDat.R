library(tidyverse)
library(measurements)

# Load and process data
grrs_map <- read_csv("GRRS_Map_Data - Sheet1.csv",
                     na=c("", "NA", "undefined")
)

grrs_map <- grrs_map %>%
  mutate(eventDate = parse_date(eventDate, "%m/%d/%Y"),
         Year = lubridate::year(eventDate),
         speciesGenus = sub(" .*", "", speciesName)
  )

# Load in Genus table
genera <- read_csv("Genera_good.csv")

Not_in_GenusTable <- unique(grrs_map$speciesGenus)[!unique(grrs_map$speciesGenus) %in% genera$speciesGenus]

if(length(Not_in_GenusTable) > 1) print("WAIT! There are genera missing from the Genus Table!")

# merge in info
grrs_map <- left_join(grrs_map, genera, by = c("speciesGenus" = "speciesGenus"))




# Fill in missing coordinates
grrs_map <- grrs_map %>%
  mutate(
    # add in decimal degree latitude when missing
    lat = case_when(is.na(lat) ~ as.numeric(
      conv_unit(
        paste(eventLocation.degreesLat,
              eventLocation.minutesLat,
              eventLocation.secondsLat),
        from = 'deg_min_sec',
        to = 'dec_deg') ),
    TRUE ~ lat),
    # add in decimal degree longitude when missing
    long = case_when(is.na(long) ~ as.numeric(
      conv_unit(
        paste(eventLocation.degreesLon,
              eventLocation.minutesLon,
              eventLocation.secondsLon),
        from = 'deg_min_sec',
        to = 'dec_deg')
    ),
    TRUE ~ long),

    # add in deg, min, sec of latitutde for NAs
    temp = conv_unit(lat, from = 'dec_deg', to = "deg_min_sec"),

    eventLocation.degreesLat =
      case_when(is.na(eventLocation.degreesLat) ~ as.numeric(sub(" .*", "", temp)),
                TRUE ~ eventLocation.degreesLat),
    eventLocation.minutesLat =
      case_when(is.na(eventLocation.minutesLat) ~ as.numeric(sapply(strsplit(temp, " "), `[`, 2)),
                TRUE ~ eventLocation.minutesLat),
    eventLocation.secondsLat =
      case_when(is.na(eventLocation.secondsLat) ~as.numeric(sapply(strsplit(temp, " "), `[`, 3)),
                TRUE ~ eventLocation.secondsLat),

    # add in deg, min, sec of longitidue for NAs
    temp = conv_unit(long, from = 'dec_deg', to = "deg_min_sec"),

    eventLocation.degreesLon =
      case_when(is.na(eventLocation.degreesLon) ~ as.numeric(sub(" .*", "", temp)),
                TRUE ~ eventLocation.degreesLon),
    eventLocation.minutesLon =
      case_when(is.na(eventLocation.minutesLon) ~ as.numeric(sapply(strsplit(temp, " "), `[`, 2)),
                TRUE ~ eventLocation.minutesLon),
    eventLocation.secondsLon =
      case_when(is.na(eventLocation.secondsLon) ~as.numeric(sapply(strsplit(temp, " "), `[`, 3)),
                TRUE ~ eventLocation.secondsLon),
  ) %>%
  select(-temp)




write_csv(grrs_map, "GRRS_Map.csv")

