e_tooltip_incidence_formatter <- function(
  style = c("decimal", "percent", "currency"),
  digits = 0,
  locale = "en-GB",
  currency = "USD"
) {

  style <- match.arg(style)
  opts <- list(
    style = style,
    maximumFractionDigits = digits,
    currency = currency
  )

  tip <- htmlwidgets::JS(sprintf("function(params, ticket, callback) {
        var fmt = new Intl.NumberFormat('de-CH', %s);
        var fmtDate = new Intl.DateTimeFormat('%s');
        var fmtDay = new Intl.DateTimeFormat('%s', {weekday: 'short'})
        var res = fmtDate.format(Date.parse(params[0].value[0])) + ' (' +
          fmtDay.formatToParts(Date.parse(params[0].value[0]))[0].value + ')';
        for (i = 0; i < params.length; i++) {
            res += '<br />' +
                  params[i].marker + ' ' +
                  params[i].seriesName +
                  '<span style=\"float:right;margin-left:20px;font-size:14px;color:#666;font-weight:900\">' +
                  fmt.format(params[i].value[1]) +
                  '</span>';
        }
        return res;
    }", jsonlite::toJSON(opts, auto_unbox = TRUE), locale, locale))
  tip <- structure(tip, class = c("JS_EVAL", "item_formatter"))
  return(tip)
}


e_tooltip_estimate_formatter <- function(
  style = c("decimal", "percent", "currency"),
  digits = 0,
  locale = "en-GB",
  currency = "USD",
  data_type = "Test"
) {

  style <- match.arg(style)
  opts <- list(
    style = style,
    minimumFractionDigits = 0,
    maximumFractionDigits = digits,
    currency = currency
  )

  tip <- htmlwidgets::JS(sprintf("function(params, ticket, callback) {
        var fmt = new Intl.NumberFormat('de-CH', %s);
        var fmtDate = new Intl.DateTimeFormat('%s');
        var fmtDay = new Intl.DateTimeFormat('%s', {weekday: 'short'})
        var res = fmtDate.format(Date.parse(params[0].value[0])) + ' (' +
          fmtDay.formatToParts(Date.parse(params[0].value[0]))[0].value + ')<br>' +
          'Estimated R<sub>e</sub> based on %s';
        for (i = 0; i < params.length/2; i++) {
            res += '<br />' +
                  params[i].marker + ' ' +
                  params[i].seriesName +
                  '<span style=\"float:right;margin-left:20px;font-size:14px;color:#666;font-weight:900\">' +
                  fmt.format(params[i].value[1]) + ' (' +
                  fmt.format(params[i + params.length/2].value[1]) + ' - ' +
                  fmt.format(params[i + params.length/2].value[2]) + ')' +
                  '</span>';
        }
        return res;
    }", jsonlite::toJSON(opts, auto_unbox = TRUE), locale, locale, data_type))
  tip <- structure(tip, class = c("JS_EVAL", "item_formatter"))
  return(tip)
}

header_formatter <- function(header, class = "tooltip-header") {
  str_c("<br><span class='", class, "'>", header, "</span>")
}

e_tooltip_independentvar_formatter <- function(
  style = c("decimal", "percent", "currency"),
  digits = 0,
  locale = "en-GB",
  currency = "USD",
  multiple = FALSE
) {

  style <- match.arg(style)
  opts <- list(
    style = style,
    maximumFractionDigits = digits,
    currency = currency
  )

  tip <- htmlwidgets::JS(sprintf("function(params, ticket, callback) {
        var fmt = new Intl.NumberFormat('%s', %s);
        var fmtDate = new Intl.DateTimeFormat('%s');
        var fmtDay = new Intl.DateTimeFormat('%s', {weekday: 'short'})
        var res = fmtDate.format(Date.parse(params[0].value[0])) + ' (' +
          fmtDay.formatToParts(Date.parse(params[0].value[0]))[0].value +')';
        var header = ''
        for (i = 0; i < params.length; i++) {
            if (params[i].name != header) {
              header = params[i].name;
              res += header
            }
            if (params[i].value[1] === null) {
              var valueStr = '<span style=\"float:right;margin-left:20px;font-size:14px;color:#999;font-weight:900\">' +
                'NA' + '</span>'
            } else {
              var valueStr = '<span style=\"float:right;margin-left:20px;font-size:14px;color:#666;font-weight:900\">' +
                fmt.format(params[i].value[1]) +
                '</span>'
            }
            res += '<br />' +
                  params[i].marker + ' ' +
                  params[i].seriesName +
                  valueStr;
        }
        return res;
    }", locale, jsonlite::toJSON(opts, auto_unbox = TRUE), locale, locale))
  tip <- structure(tip, class = c("JS_EVAL", "item_formatter"))
  return(tip)
}
