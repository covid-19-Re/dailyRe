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
        var fmt = new Intl.NumberFormat('%s', %s);
        var fmtDate = new Intl.DateTimeFormat('%s');
        var res = fmtDate.format(Date.parse(params[0].value[0]));
        for (i = 0; i < params.length; i++) {
            res += '<br />' +
                  params[i].marker + ' ' +
                  params[i].seriesName +
                  '<span style=\"float:right;margin-left:20px;font-size:14px;color:#666;font-weight:900\">' +
                  fmt.format(parseFloat(params[i].value[1])) +
                  '</span>';
        }
        return res;
    }", locale, jsonlite::toJSON(opts, auto_unbox = TRUE), locale))
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
        var fmt = new Intl.NumberFormat('en-GB', %s);
        var fmtDate = new Intl.DateTimeFormat('%s');
        var res = fmtDate.format(Date.parse(params[0].value[0])) + '<br>' +
          'Estimated R<sub>e</sub> based on %s';
        for (i = 0; i < params.length/2; i++) {
            res += '<br />' +
                  params[i].marker + ' ' +
                  params[i].seriesName +
                  '<span style=\"float:right;margin-left:20px;font-size:14px;color:#666;font-weight:900\">' +
                  fmt.format(parseFloat(params[i].value[1])) + ' (' +
                  fmt.format(parseFloat(params[i + params.length/2].value[1])) + ' - ' +
                  fmt.format(parseFloat(params[i + params.length/2].value[2])) + ')' +
                  '</span>';
        }
        return res;
    }", jsonlite::toJSON(opts, auto_unbox = TRUE), locale, data_type))
  tip <- structure(tip, class = c("JS_EVAL", "item_formatter"))
  return(tip)
}

e_tooltip_independentvar_formatter <- function(
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
        var fmt = new Intl.NumberFormat('%s', %s);
        var fmtDate = new Intl.DateTimeFormat('%s');
        var res = fmtDate.format(Date.parse(params[0].value[0]));
        var header = params[0].seriesName
        for (i = 0; i < params.length; i++) {

            res += '<br />' +
                  params[i].marker + ' ' +
                  params[i].seriesName +
                  '<span style=\"float:right;margin-left:20px;font-size:14px;color:#666;font-weight:900\">' +
                  fmt.format(parseFloat(params[i].value[1])) +
                  '</span>';
        }
        return res;
    }", locale, jsonlite::toJSON(opts, auto_unbox = TRUE), locale))
  tip <- structure(tip, class = c("JS_EVAL", "item_formatter"))
  return(tip)
}
