# Native Excel chart XML (DrawingML) for openxlsx2::wb_add_chart_xml().
# Charts reference worksheet ranges and carry no cached values, so Excel draws
# them from the live formulas. Styling follows the shared GARY/NELLY theme.

.esc <- function(x) { x <- gsub("&", "&amp;", x, fixed = TRUE); x <- gsub("<", "&lt;", x, fixed = TRUE); gsub(">", "&gt;", x, fixed = TRUE) }
.fill <- function(col) sprintf('<a:solidFill><a:srgbClr val="%s"/></a:solidFill>', col)
.txt  <- function(sz = 800, col = "6B7280", bold = FALSE)
  sprintf('<c:txPr><a:bodyPr/><a:lstStyle/><a:p><a:pPr><a:defRPr sz="%d" b="%d">%s<a:latin typeface="Arial"/></a:defRPr></a:pPr><a:endParaRPr lang="en-AU"/></a:p></c:txPr>',
          sz, as.integer(bold), .fill(col))
.title <- function(text, sz = 1100)
  sprintf('<c:title><c:tx><c:rich><a:bodyPr/><a:lstStyle/><a:p><a:pPr><a:defRPr sz="%d" b="1">%s<a:latin typeface="Arial"/></a:defRPr></a:pPr><a:r><a:rPr lang="en-AU" sz="%d" b="1">%s<a:latin typeface="Arial"/></a:rPr><a:t>%s</a:t></a:r></a:p></c:rich></c:tx><c:overlay val="0"/></c:title>',
          sz, .fill("1A1D21"), sz, .fill("1A1D21"), .esc(text))
.tx <- function(name) sprintf("<c:tx><c:v>%s</c:v></c:tx>", .esc(name))
.ref <- function(sheet, rng) sprintf("'%s'!%s", sheet, rng)
.axis_title <- function(t) if (is.null(t)) "" else .title(t, 900)

.val_ax <- function(id, cross, pos = "l", fmt = "General", title = NULL, grid = TRUE, crosses = "autoZero")
  sprintf('<c:valAx><c:axId val="%d"/><c:scaling><c:orientation val="minMax"/></c:scaling><c:delete val="0"/><c:axPos val="%s"/>%s%s<c:numFmt formatCode="%s" sourceLinked="0"/><c:majorTickMark val="none"/><c:minorTickMark val="none"/><c:tickLblPos val="low"/><c:spPr><a:ln><a:noFill/></a:ln></c:spPr>%s<c:crossAx val="%d"/><c:crosses val="%s"/><c:crossBetween val="between"/></c:valAx>',
          id, pos, if (grid) sprintf('<c:majorGridlines><c:spPr><a:ln w="6350">%s</a:ln></c:spPr></c:majorGridlines>', .fill("E3E6EA")) else "",
          .axis_title(title), .esc(fmt), .txt(), cross, crosses)

.cat_ax <- function(id, cross, date = FALSE, fmt = "mmm-yy", reverse = FALSE, pos = "b")
  if (date) {
    sprintf('<c:dateAx><c:axId val="%d"/><c:scaling><c:orientation val="minMax"/></c:scaling><c:delete val="0"/><c:axPos val="b"/><c:numFmt formatCode="%s" sourceLinked="0"/><c:majorTickMark val="none"/><c:minorTickMark val="none"/><c:tickLblPos val="low"/><c:spPr><a:ln w="6350">%s</a:ln></c:spPr>%s<c:crossAx val="%d"/><c:crosses val="autoZero"/><c:auto val="1"/><c:lblOffset val="100"/><c:baseTimeUnit val="months"/></c:dateAx>',
            id, .esc(fmt), .fill("C8CDD3"), .txt(), cross)
  } else {
    sprintf('<c:catAx><c:axId val="%d"/><c:scaling><c:orientation val="%s"/></c:scaling><c:delete val="0"/><c:axPos val="%s"/><c:numFmt formatCode="General" sourceLinked="0"/><c:majorTickMark val="none"/><c:minorTickMark val="none"/><c:tickLblPos val="low"/><c:spPr><a:ln w="6350">%s</a:ln></c:spPr>%s<c:crossAx val="%d"/><c:crosses val="autoZero"/><c:auto val="1"/><c:lblAlgn val="ctr"/><c:lblOffset val="100"/><c:noMultiLvlLbl val="0"/></c:catAx>',
            id, if (reverse) "maxMin" else "minMax", pos, .fill("C8CDD3"), .txt(), cross)
  }

.wrap <- function(title, plot, legend = TRUE)
  paste0('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
         '<c:chartSpace xmlns:c="http://schemas.openxmlformats.org/drawingml/2006/chart" xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships">',
         '<c:roundedCorners val="0"/><c:chart>', .title(title), '<c:autoTitleDeleted val="0"/><c:plotArea><c:layout/>', plot,
         '<c:spPr><a:noFill/></c:spPr></c:plotArea>',
         if (legend) paste0('<c:legend><c:legendPos val="b"/><c:overlay val="0"/>', .txt(800, "6B7280"), '</c:legend>') else "",
         '<c:plotVisOnly val="1"/><c:dispBlanksAs val="gap"/></c:chart>',
         sprintf('<c:spPr>%s<a:ln w="9525">%s</a:ln></c:spPr>', .fill("FFFFFF"), .fill("E3E6EA")),
         .txt(900, "1A1D21"), '</c:chartSpace>')

# series: list of list(name, ref, colour, dash = FALSE); cats: range for x
chart_line <- function(title, sheet, cats, series, y_fmt = "0%", y_title = NULL, date = TRUE) {
  sers <- vapply(seq_along(series), function(i) {
    s <- series[[i]]
    sprintf('<c:ser><c:idx val="%d"/><c:order val="%d"/>%s<c:spPr><a:ln w="22225" cap="rnd">%s%s<a:round/></a:ln></c:spPr><c:marker><c:symbol val="none"/></c:marker><c:cat><c:numRef><c:f>%s</c:f></c:numRef></c:cat><c:val><c:numRef><c:f>%s</c:f></c:numRef></c:val><c:smooth val="0"/></c:ser>',
            i - 1, i - 1, .tx(s$name), .fill(s$colour), if (isTRUE(s$dash)) '<a:prstDash val="dash"/>' else "",
            .ref(sheet, cats), .ref(sheet, s$ref))
  }, "")
  .wrap(title, paste0('<c:lineChart><c:grouping val="standard"/><c:varyColors val="0"/>', paste(sers, collapse = ""),
                      '<c:marker val="1"/><c:axId val="101"/><c:axId val="102"/></c:lineChart>',
                      .cat_ax(101, 102, date = date), .val_ax(102, 101, fmt = y_fmt, title = y_title)))
}

# dir "col" or "bar"; grouping "clustered" or "stacked"; cats text (strRef) or dates
chart_bar <- function(title, sheet, cats, series, y_fmt = "#,##0", dir = "col", grouping = "clustered", date = FALSE,
                      legend = TRUE, top_down = FALSE) {
  sers <- vapply(seq_along(series), function(i) {
    s <- series[[i]]
    sprintf('<c:ser><c:idx val="%d"/><c:order val="%d"/>%s<c:spPr>%s<a:ln w="6350">%s</a:ln></c:spPr><c:invertIfNegative val="0"/><c:cat><c:%s><c:f>%s</c:f></c:%s></c:cat><c:val><c:numRef><c:f>%s</c:f></c:numRef></c:val></c:ser>',
            i - 1, i - 1, .tx(s$name), .fill(s$colour), .fill("FFFFFF"), if (date) "numRef" else "strRef",
            .ref(sheet, cats), if (date) "numRef" else "strRef", .ref(sheet, s$ref))
  }, "")
  plot <- paste0(sprintf('<c:barChart><c:barDir val="%s"/><c:grouping val="%s"/><c:varyColors val="0"/>', dir, grouping),
                 paste(sers, collapse = ""),
                 sprintf('<c:gapWidth val="%d"/>', if (grouping == "stacked") 40 else 60),
                 if (grouping == "stacked") '<c:overlap val="100"/>' else "",
                 '<c:axId val="201"/><c:axId val="202"/></c:barChart>',
                 .cat_ax(201, 202, date = date, reverse = top_down, pos = if (dir == "bar") "l" else "b"),
                 .val_ax(202, 201, pos = if (dir == "bar") "b" else "l", fmt = y_fmt, crosses = if (top_down) "max" else "autoZero"))
  .wrap(title, plot, legend)
}

chart_scatter <- function(title, sheet, x, y, colour, x_title, y_title, x_fmt = "$#,##0", y_fmt = "0%") {
  ser <- sprintf('<c:ser><c:idx val="0"/><c:order val="0"/>%s<c:spPr><a:ln w="19050"><a:noFill/></a:ln></c:spPr><c:marker><c:symbol val="circle"/><c:size val="6"/><c:spPr>%s<a:ln w="6350">%s</a:ln></c:spPr></c:marker><c:xVal><c:numRef><c:f>%s</c:f></c:numRef></c:xVal><c:yVal><c:numRef><c:f>%s</c:f></c:numRef></c:yVal><c:smooth val="0"/></c:ser>',
                 .tx(title), .fill(colour), .fill("FFFFFF"), .ref(sheet, x), .ref(sheet, y))
  plot <- paste0('<c:scatterChart><c:scatterStyle val="lineMarker"/><c:varyColors val="0"/>', ser,
                 '<c:axId val="301"/><c:axId val="302"/></c:scatterChart>',
                 .val_ax(301, 302, pos = "b", fmt = x_fmt, title = x_title, grid = FALSE),
                 .val_ax(302, 301, pos = "l", fmt = y_fmt, title = y_title))
  .wrap(title, plot, legend = FALSE)
}
