getLOESSCases <- function(dates, count_data, days_incl = 21, degree = 1, truncation = 0) {

  if (truncation != 0) {
    dates <- dates[1:(length(dates) - truncation)]
    count_data <- count_data[1:(length(count_data) - truncation)]
  }

  n_points <- length(unique(dates))
  sel_span <- days_incl / n_points

  n_pad <- round(length(count_data) * sel_span * 0.5)

  c_data <- data.frame(value = c(rep(0, n_pad), count_data),
                       date_num = c(seq(as.numeric(dates[1]) - n_pad, as.numeric(dates[1]) - 1),
                                    as.numeric(dates)))
  c_data.lo <- loess(value ~ date_num, data = c_data, span = sel_span, degree = degree)
  smoothed <- predict(c_data.lo)
  smoothed[smoothed < 0] <- 0
  raw_smoothed_counts <- smoothed[(n_pad + 1):length(smoothed)]
  normalized_smoothed_counts <-
    raw_smoothed_counts * sum(count_data, na.rm = T) / sum(raw_smoothed_counts, na.rm = T)

  if (truncation != 0) {
    normalized_smoothed_counts <- append(normalized_smoothed_counts, rep(NA, truncation))
  }
  return(normalized_smoothed_counts)
}


# new data function -> move to data repo

getMobilityDataGoogle <- function(countries = NULL, tempFileName = NULL, tReload = 300) {
  urlfile <- "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv"

  if (is.null(tempFileName)) {
    csvPath <- urlfile
  } else {
    fileMod <- file.mtime(tempFileName)
    fileReload <- if_else(file.exists(tempFileName), now() > fileMod + minutes(tReload), TRUE)
    if (fileReload) {
      downloadOK <- try(download.file(urlfile, destfile = tempFileName))
      if ("try-error" %in% class(downloadOK)) {
        if (file.exists(tempFileName)) {
          warning("Couldn't fetch new OWID Vaccination data. Using data from ", fileMod)
        } else {
          warning("Couldn't fetch new OWID Vaccination data.")
          return(NULL)
        }
      }
    }
    csvPath <- tempFileName
  }

  world_data <- try(read_csv(csvPath,
    na = c(""),
    col_types = cols_only(
      country_region_code = col_character(),
      sub_region_1 = col_character(),
      sub_region_2 = col_character(),
      metro_area = col_character(),
      date = col_date(format = ""),
      retail_and_recreation_percent_change_from_baseline = col_double(),
      grocery_and_pharmacy_percent_change_from_baseline = col_double(),
      parks_percent_change_from_baseline = col_double(),
      transit_stations_percent_change_from_baseline = col_double(),
      workplaces_percent_change_from_baseline = col_double(),
      residential_percent_change_from_baseline = col_double()
    )
  ))
  
  if ("try-error" %in% class(world_data)) {
    warning(str_c("couldn't get OWID Vaccination data from ", url, "."))
    return(NULL)
  }

  longData <- world_data %>%
    filter(is.na(sub_region_1), is.na(sub_region_2), is.na(metro_area)) %>%
    mutate(
      countryIso3 = countrycode::countrycode(country_region_code, origin = "iso2c", destination = "iso3c")
    ) %>%
    transmute(
      date,
      countryIso3,
      region = countryIso3,
      retail_and_recreation = retail_and_recreation_percent_change_from_baseline / 100,
      grocery_and_pharmacy = grocery_and_pharmacy_percent_change_from_baseline / 100,
      parks = parks_percent_change_from_baseline / 100,
      transit_stations = transit_stations_percent_change_from_baseline / 100,
      workplaces = workplaces_percent_change_from_baseline / 100,
      residential = residential_percent_change_from_baseline / 100
    ) %>%
    pivot_longer(retail_and_recreation:residential, names_to = "placeCategory", values_to = "change")

  if (!is.null(countries)) {
    longData <- longData %>%
      filter(countryIso3 %in% countries)
  }
  return(longData)
}


dataUpdatesString <- function(latestData, name = "Data Source", dateFormat = "%Y-%m-%d") {
  latestDataSum <- latestData %>%
    mutate(lastChanged = format(lastChanged, dateFormat)) %>%
    group_by(source, lastChanged) %>%
    summarize(
      data_type = str_c(data_type, collapse = ", "),
      .groups = "keep")

  outList <- list(str_c(name, ": "))
  nSources <- dim(latestDataSum)[1]
  if (nSources == 1) {
    outList[[2]] <- str_c(
      latestDataSum$source, ", ", latestDataSum$lastChanged,
      "; ")
  } else {
    for (i in seq_len(nSources)) {
      outList[[i + 1]] <- str_c(
        latestDataSum[i, ]$source, ", ", latestDataSum[i, ]$lastChanged,
        " (", latestDataSum[i, ]$data_type, ")",
        "; ")
    }
  }
  return(str_sub(str_c(outList, collapse = ""), end = -3))
}
