mapLabels <- function(shapeFileData, mainLabel = "cases14d") {
  if (mainLabel == "cases14d") {
    labelOrder <- c("cases14d", "re")
  } else (
    labelOrder <- c("re", "cases14d")
  )

  mapLabels <- as_tibble(shapeFileData) %>%
    transmute(
      name = str_c("<strong>", NAME, "</strong>"),
      cases14d = if_else(is.na(cases14d),
        "<br>No data available",
        str_c("<br>", round(cases14d, 3), " cases / 100'000 / 14d (", dateCases, ")")),
      re = if_else(is.na(median_R_mean),
        "<br>No R<sub>e</sub> estimate available",
        str_c("<br>R<sub>e</sub>: ", round(median_R_mean, 3), " ",
        "(", round(median_R_lowHPD, 3), " - ", round(median_R_highHPD, 3), ") (", dateEstimates, ")" ))
    ) %>%
    transmute(
      label = str_c(name, .data[[labelOrder[1]]], .data[[labelOrder[2]]])) %>%
    .$label %>%
    lapply(htmltools::HTML)
  return(mapLabels)
}

addPolygonLayer <- function(map, shapeFile, fillColor, group, labels, options = pathOptions(), layerId = NULL) {
  map <- map %>%
    addPolygons(
      layerId = layerId,
      data = shapeFile,
      fillColor = fillColor,
      weight = 0,
      opacity = 0,
      color = "transparent",
      dashArray = "",
      fillOpacity = 0.7,
      highlight = highlightOptions(
        weight = 1,
        opacity = 1,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE),
      label = labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"),
      group = group,
      options = options)

  return(map)
}

divergentColorPal <- function(palette, domain, midpoint, na.color = "#808080", alpha = FALSE, reverse = FALSE) {
  rng <- NULL
  if (length(domain) > 0) {
    rng <- range(domain, na.rm = TRUE)
    if (!all(is.finite(rng))) {
      stop("Wasn't able to determine range of domain")
    }
  }

  pf <- leaflet:::safePaletteFunc(palette, na.color, alpha)

  leaflet:::withColorAttr("numeric", list(na.color = na.color), function(x) {
    if (length(x) == 0 || all(is.na(x))) {
      return(pf(x))
    }

    if (is.null(rng)) rng <- range(x, na.rm = TRUE)

    rescaled <- scales::rescale_mid(x, from = rng, mid = midpoint)
    rescaled[rescaled > 1] <- 1
    if (any(rescaled < 0 | rescaled > 1, na.rm = TRUE))
      warning("Some values were outside the color scale and will be treated as NA")

    if (reverse) {
      rescaled <- 1 - rescaled
    }
    pf(rescaled)
  })
}

casesLegendLabels <- function(type, cuts) {
  out <- format(cuts, scientific = FALSE, big.mark = ",")
  out[length(out)] <- str_c("≥", out[length(out)])
  return(out)
}

regionCheckboxInput <- function(checkboxGroupId, label, choices, selected, zoomLabel) {
  out <- list()

  header <- glue::glue(
    "<div id='{checkboxGroupId}' class='shiny-input-checkboxgroup shiny-input-container shiny-bound-input'>
      <label class='control-label' for='{checkboxGroupId}'>{label}</label>
      <div class='shiny-options-group'>")


  checkboxes <- list()
  for (i in seq_along(choices)) {
    checkboxes[[i]] <- glue::glue(
      "<div class='checkbox'>",
        "<label>",
            "<input type='checkbox' name='{checkboxGroupId}' value='{choiceValue}' {checked}>",
              "<span>{choiceName}",
              "<a id='zoom{choiceValue}' href='#' class='action-button shiny-bound-input' style='display: inline;margin-left:10px;'>{zoomLabel}</a>",
              "</span>",
        "</label>",
      "</div>",
      checked = if_else(choices[i] %in% selected, "checked='checked'", ""),
      choiceValue = choices[i],
      choiceName = names(choices)[i])
  }

  return(HTML(str_c(header, str_c(checkboxes, collapse = ""), "</div></div>")))   
}
