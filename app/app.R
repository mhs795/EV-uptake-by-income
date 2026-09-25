# EV × Income — Shiny dashboard.
# Design and palette follow the GARY gas-market-model dashboard (shared flat
# light Material theme). Data come from processed/dashboard.rds, built by
# R/dashboard_data.R. Launch with:  Rscript run_dashboard.R
suppressPackageStartupMessages({
  library(shiny); library(bslib); library(leaflet); library(plotly); library(data.table)
  library(sf); library(htmltools); library(openxlsx2)
})

ROOT <- normalizePath(Sys.getenv("EV_ROOT", ".."))
CFG <- yaml::read_yaml(file.path(ROOT, "config.yaml"))
PROC <- file.path(ROOT, Sys.getenv("EV_PROCESSED", CFG$paths$processed))
D <- readRDS(file.path(PROC, "dashboard.rds"))
WORKBOOK <- file.path(ROOT, Sys.getenv("EV_WORKBOOK", D$workbook))

# ---- palette (GARY colorway) ---------------------------------------------------
COL <- list(primary = "#1F7AE0", text = "#1A1D21", med = "#6B7280", low = "#9AA5B1", divider = "#E3E6EA",
            nsw = "#1976D2", qld = "#F57C00", vic = "#00897B", other = "#CFD6DE", band = "rgba(245,124,0,0.10)",
            na = "#E3E6EA", land = "#FFFFFF", coast = "#B8C1CC")
STATE_COL <- c(NSW = COL$nsw, QLD = COL$qld, VIC = COL$vic)
SEQ <- c("#E3F2FD", "#BBDEFB", "#90CAF9", "#5AA2EE", "#1F7AE0", "#1565C0", "#0D47A1")
GROUP_COL <- c("#90CAF9", "#5AA2EE", "#1F7AE0", "#1565C0", "#0D47A1")
NG <- D$n_groups
QLAB <- paste0("Q", seq_len(NG), c(" low", rep("", NG - 2), " high"))

# ---- formatting ----------------------------------------------------------------
na_or <- function(x, f) ifelse(is.na(x), "–", f(x))
pct <- function(x) na_or(x, function(v) sprintf("%.1f%%", 100 * v))
pp <- function(x) na_or(x, function(v) sprintf("%+.1f pp", v))
mult <- function(x) na_or(x, function(v) sprintf("%.2f×", v))
num1 <- function(x) na_or(x, function(v) sprintf("%.1f", v))
int <- function(x) na_or(x, function(v) formatC(round(v), format = "d", big.mark = ","))
usd <- function(x) na_or(x, function(v) paste0("$", formatC(round(v), format = "d", big.mark = ",")))
mon <- function(ym) format(as.Date(paste0(ym, "-01")), "%b %y")
qmon <- function(q) mon(sprintf("%s-%02d", substr(q, 1, 4), as.integer(substr(q, 6, 6)) * 3L))
crisis_label <- sprintf("%s–%s", mon(D$crisis$start), mon(D$crisis$end))

METRICS <- list(
  lga = list(
    share = list(label = sprintf("BEV share of new private cars (%s–%s)", mon(D$recent$start), mon(D$recent$end)), fmt = pct),
    share_c = list(label = sprintf("BEV share during the fuel crisis (%s)", crisis_label), fmt = pct),
    chg = list(label = "Change in BEV share: crisis vs same months a year earlier", fmt = pp),
    mult = list(label = "Crisis BEV share ÷ year-earlier share", fmt = mult),
    bev = list(label = "New BEVs registered, last 12 months (count)", fmt = int),
    per1000veh = list(label = sprintf("BEVs per 1,000 light vehicles — fleet, %s (NSW only)", mon(D$fleet_dates$NSW)), fmt = num1),
    fleet_bev = list(label = "BEVs in the fleet (count; QLD: BEVs seen since 2022)", fmt = int),
    income = list(label = "Median total income of earners, 2022-23", fmt = usd)),
  vic = list(
    per1000veh = list(label = sprintf("BEVs per 1,000 vehicles — fleet, %s", qmon(D$fleet_dates$VIC)), fmt = num1),
    rshare = list(label = "BEV share of recent-model vehicles (new-car proxy)", fmt = pct),
    add_c1000 = list(label = sprintf("BEVs added in the crisis quarter (to %s) per 1,000 vehicles", qmon(D$crisis$vic_quarter)), fmt = num1),
    bev = list(label = "BEVs in the fleet (count)", fmt = int),
    add_c = list(label = "BEVs added in the crisis quarter (count)", fmt = int),
    income = list(label = "Median taxable income, 2023-24", fmt = usd)))
short <- function(lab) sub(" —.*| \\(.*", "", lab)

# ---- plotly theme (GARY) -----------------------------------------------------------
theme_plot <- function(p, yfmt = NULL, ytitle = NULL, legend = TRUE, xtitle = NULL) {
  p |> layout(
    font = list(family = "Inter, 'Segoe UI', sans-serif", color = COL$text, size = 12),
    paper_bgcolor = "#FFFFFF", plot_bgcolor = "#FFFFFF", margin = list(l = 56, r = 12, t = 8, b = 40),
    xaxis = list(gridcolor = COL$divider, linecolor = COL$divider, zeroline = FALSE, tickfont = list(color = COL$med, size = 11),
                 title = list(text = xtitle, font = list(size = 11, color = COL$med))),
    yaxis = list(gridcolor = COL$divider, linecolor = COL$divider, zeroline = FALSE, tickfont = list(color = COL$med, size = 11),
                 tickformat = yfmt, title = list(text = ytitle, font = list(size = 11, color = COL$med))),
    legend = list(orientation = "h", x = 0, y = -0.18, font = list(size = 11, color = COL$med)), showlegend = legend,
    hoverlabel = list(bgcolor = "#FFFFFF", bordercolor = COL$divider, font = list(family = "Inter", size = 12, color = COL$text))) |>
    config(displayModeBar = FALSE)
}
crisis_band <- function(x0, x1) list(type = "rect", xref = "x", yref = "paper", x0 = x0, x1 = x1, y0 = 0, y1 = 1,
                                     fillcolor = COL$band, line = list(width = 0), layer = "below")
mdate <- function(ym) as.Date(paste0(ym, "-01"))
qdate <- function(q) as.Date(sprintf("%s-%02d-01", substr(q, 1, 4), as.integer(substr(q, 6, 6)) * 3L))

# ---- UI ----------------------------------------------------------------------------
css <- "
:root { --md-bg:#F5F6F8; --md-surface:#FFFFFF; --md-primary:#1F7AE0; --md-primary-dim:rgba(31,122,224,0.08); --md-hover:rgba(31,122,224,0.05);
  --md-text:#1A1D21; --md-text-med:#6B7280; --md-text-low:#9AA5B1; --md-divider:#E3E6EA; --font:'Inter','Segoe UI',Roboto,system-ui,sans-serif; }
html, body { background: var(--md-bg) !important; color: var(--md-text); font-family: var(--font) !important; font-size: 14px; -webkit-font-smoothing: antialiased; }
.app { display: flex; min-height: 100vh; }
.md-sidebar { width: 280px; flex-shrink: 0; background: var(--md-surface); border-right: 1px solid var(--md-divider); position: sticky; top: 0; height: 100vh; overflow-y: auto; display: flex; flex-direction: column; }
.md-sidebar-brand { padding: 18px 20px 16px; border-bottom: 1px solid var(--md-divider); display: flex; align-items: center; gap: 12px; }
.md-sidebar-brand-icon { width: 34px; height: 34px; background: var(--md-primary-dim); border: 1px solid var(--md-divider); border-radius: 8px; display: flex; align-items: center; justify-content: center; color: var(--md-primary); }
.md-sidebar-brand-text { font-size: 20px; font-weight: 700; letter-spacing: -0.3px; line-height: 1.2; }
.md-sidebar-brand-sub { font-size: 12px; color: var(--md-text-med); }
.md-sidebar-body { padding: 16px 20px; flex: 1; }
.md-section-label { font-size: 11px; font-weight: 600; letter-spacing: .6px; text-transform: uppercase; color: var(--md-text-low); margin: 0 0 10px; }
.md-divider { border: none; border-top: 1px solid var(--md-divider); margin: 16px 0; opacity: 1; }
.md-input-hint { font-size: 11px; color: var(--md-text-med); margin: 4px 0 12px; line-height: 1.4; }
.md-sidebar label.control-label { font-size: 11px; text-transform: uppercase; letter-spacing: .6px; font-weight: 600; color: var(--md-text-med); margin-bottom: 5px; }
.md-sidebar .form-select, .md-sidebar .selectize-input { font-size: 13px; border-radius: 8px; border-color: var(--md-divider); }
.seg { display: flex; border: 1px solid var(--md-divider); border-radius: 8px; overflow: hidden; margin-bottom: 14px; }
.seg button { flex: 1; border: 0; background: transparent; color: var(--md-text-med); padding: 8px 6px; font-size: 13px; font-weight: 600; }
.seg button:hover { background: var(--md-hover); }
.seg button.on { background: var(--md-primary-dim); color: var(--md-primary); }
.md-btn { display: block; width: 100%; padding: 9px 16px; border-radius: 8px; font-size: 13px; font-weight: 600; border: 1px solid var(--md-divider);
  background: transparent; color: var(--md-text); text-align: center; margin-bottom: 8px; text-decoration: none; }
.md-btn:hover { background: var(--md-hover); color: var(--md-text); }
.md-btn-filled { background: var(--md-primary); color: #fff; border-color: var(--md-primary); }
.md-btn-filled:hover { background: var(--md-primary); color: #fff; opacity: .9; }
.md-sidebar-foot { padding: 12px 20px 18px; font-size: 11px; color: var(--md-text-low); border-top: 1px solid var(--md-divider); line-height: 1.5; }
.md-main { flex: 1; min-width: 0; display: flex; flex-direction: column; }
.md-header { background: var(--md-surface); padding: 14px 22px; border-bottom: 1px solid var(--md-divider); display: flex; align-items: center; justify-content: space-between; gap: 12px; flex-wrap: wrap; }
.md-header-title { font-size: 16px; font-weight: 700; letter-spacing: -.2px; }
.md-header-sub { font-size: 12px; color: var(--md-text-med); margin-top: 2px; }
.md-chip { font-size: 11px; font-weight: 600; padding: 5px 14px; border-radius: 14px; background: var(--md-primary-dim); color: var(--md-primary); border: 1px solid var(--md-divider); white-space: nowrap; }
.md-chip.warn { background: rgba(245,124,0,.08); color: #B45309; }
.md-content { padding: 18px 22px; }
.md-kpi-row { display: flex; gap: 12px; margin-bottom: 16px; flex-wrap: wrap; }
.md-kpi-card { flex: 1; background: var(--md-surface); border: 1px solid var(--md-divider); border-radius: 10px; padding: 12px 16px; min-width: 160px; }
.md-kpi-label { font-size: 10px; font-weight: 600; letter-spacing: .6px; text-transform: uppercase; color: var(--md-text-low); margin-bottom: 3px; }
.md-kpi-value { font-size: 22px; font-weight: 700; line-height: 1.2; }
.md-kpi-sub { display: block; font-size: 12px; font-weight: 600; color: var(--md-text-low); margin-top: 2px; }
.nav-underline { border-bottom: 1px solid var(--md-divider); gap: 0; margin-bottom: 16px; }
.nav-underline .nav-link { color: var(--md-text-med); font-size: 13px; font-weight: 600; padding: 10px 14px; border-bottom-width: 2px; }
.nav-underline .nav-link:hover { color: var(--md-primary); }
.nav-underline .nav-link.active { color: var(--md-primary); border-bottom-color: var(--md-primary); }
.card2 { background: var(--md-surface); border: 1px solid var(--md-divider); border-radius: 12px; padding: 16px 18px; min-width: 0; margin-bottom: 14px; }
.card2 h2 { font-size: 14px; margin: 0 0 2px; font-weight: 600; }
.card2 .note { color: var(--md-text-med); font-size: 12px; margin: 0 0 10px; }
.grid-2 { display: grid; grid-template-columns: minmax(0,1.45fr) minmax(0,1fr); gap: 14px; }
.grid-even { display: grid; grid-template-columns: minmax(0,1fr) minmax(0,1fr); gap: 14px; }
.leaflet-container { background: #EEF2F6 !important; border-radius: 8px; font-family: var(--font); }
.sel-name { font-size: 17px; font-weight: 700; }
.sel-meta { color: var(--md-text-med); font-size: 12px; }
.kpis { display: grid; grid-template-columns: repeat(3, minmax(0,1fr)); gap: 8px; margin: 10px 0 6px; }
.kpi { background: #F5F6F8; border: 1px solid var(--md-divider); border-radius: 8px; padding: 8px 10px; }
.kpi b { display: block; font-size: 17px; font-weight: 700; }
.kpi span { font-size: 11px; color: var(--md-text-med); }
table.md { width: 100%; border-collapse: collapse; font-size: 12.5px; font-variant-numeric: tabular-nums; }
table.md th { text-align: left; color: var(--md-text-low); font-weight: 600; font-size: 11px; text-transform: uppercase; letter-spacing: .4px; border-bottom: 1px solid var(--md-divider); padding: 6px; }
table.md td { padding: 6px; border-bottom: 1px solid var(--md-divider); }
table.md .num { text-align: right; }
table.md tbody tr { cursor: pointer; }
table.md tbody tr:hover { background: var(--md-hover); }
.findings ul { color: var(--md-text-med); line-height: 1.55; padding-left: 18px; }
.findings b { color: var(--md-text); }
.legend-row { display:flex; flex-wrap:wrap; font-size:11px; color: var(--md-text-med); margin-top:8px; }
.legend-row .sw { width: 70px; } .legend-row .sw i { display:block; height:10px; } .legend-row .sw span { display:block; padding-top:2px; white-space:nowrap; }
@media (max-width: 1100px) { .app { flex-direction: column; } .md-sidebar { width: 100%; height: auto; position: static; }
  .grid-2, .grid-even { grid-template-columns: 1fr; } }
"

seg_js <- "
$(document).on('click', '.seg button', function() {
  $('.seg button').removeClass('on'); $(this).addClass('on');
  Shiny.setInputValue('geo', $(this).data('geo'));
});
Shiny.addCustomMessageHandler('pick', function(id) { Shiny.setInputValue('pick', id, {priority: 'event'}); });
"

card <- function(title, note = NULL, ...) div(class = "card2", h2(title), if (!is.null(note)) p(class = "note", note), ...)

ui <- page(
  theme = bs_theme(version = 5, primary = COL$primary, bg = "#F5F6F8", fg = COL$text,
                   base_font = font_collection("Inter", "Segoe UI", "Roboto", "system-ui", "sans-serif")),
  tags$head(tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap"),
            tags$style(HTML(css)), tags$script(HTML(seg_js)), tags$title("EV × Income")),
  div(class = "app",
    tags$aside(class = "md-sidebar",
      div(class = "md-sidebar-brand",
          div(class = "md-sidebar-brand-icon", HTML('<svg width="18" height="18" viewBox="0 0 24 24" fill="currentColor"><path d="M13 2 4 14h6l-1 8 9-12h-6l1-8z"/></svg>')),
          div(div(class = "md-sidebar-brand-text", "EV × Income"), div(class = "md-sidebar-brand-sub", "BEV take-up by area income"))),
      div(class = "md-sidebar-body",
          p(class = "md-section-label", "View"),
          tags$label(class = "control-label", "Geography"),
          div(class = "seg", tags$button(class = "on", `data-geo` = "lga", "NSW + QLD"), tags$button(`data-geo` = "vic", "VIC")),
          selectInput("metric", "Map metric", choices = NULL, width = "100%"),
          conditionalPanel("input.geo != 'vic'", selectInput("st", "State", c("NSW + QLD" = "ALL", "NSW", "QLD"), width = "100%")),
          actionButton("reset", "Reset view", class = "md-btn"),
          p(class = "md-input-hint", "NSW and QLD are by council area (LGA); VIC by postcode, named by its suburbs. Click any area, dot or table row."),
          hr(class = "md-divider"),
          p(class = "md-section-label", "Download to Excel"),
          downloadButton("dl_workbook", "Full workbook", class = "md-btn md-btn-filled", icon = NULL),
          downloadButton("dl_areas", "All areas in this view", class = "md-btn", icon = NULL),
          downloadButton("dl_top", "Top & bottom 10", class = "md-btn", icon = NULL),
          downloadButton("dl_groups", "Income-group summary", class = "md-btn", icon = NULL),
          downloadButton("dl_series", "Monthly series + fuel prices", class = "md-btn", icon = NULL)),
      div(class = "md-sidebar-foot",
          "Public data: TfNSW, QLD TMR, VIC DTP registrations; ABS Personal Income 2022-23 (ATO-based); ATO Taxation Statistics 2023-24; ",
          "NSW FuelCheck, QLD fuel prices, AIP; ABS boundaries. Sources, adjustments and method: see the workbook.")),
    div(class = "md-main",
      div(class = "md-header",
          div(div(class = "md-header-title", "EV uptake by regional income — NSW, QLD, VIC"),
              div(class = "md-header-sub", "Battery-electric vehicles in state registration data against ATO/ABS median income by area")),
          div(style = "display:flex;gap:8px;flex-wrap:wrap",
              span(class = "md-chip", sprintf("Last 12 months: %s–%s", mon(D$recent$start), mon(D$recent$end))),
              span(class = "md-chip warn", sprintf("Fuel crisis: %s", crisis_label)))),
      div(class = "md-content",
        uiOutput("kpi_row"),
        navset_underline(id = "tab",
          nav_panel("Map", value = "map", div(class = "grid-2",
            div(class = "card2", h2(textOutput("map_title", inline = TRUE)), p(class = "note", textOutput("map_note", inline = TRUE)),
                leafletOutput("map", height = 600), uiOutput("legend")),
            div(div(class = "card2", uiOutput("sel_head"), h2(textOutput("line_title", inline = TRUE), style = "margin-top:8px"),
                    p(class = "note", textOutput("line_note", inline = TRUE)), plotlyOutput("sel_line", height = 230)),
                div(class = "card2", h2(textOutput("sc_title", inline = TRUE)),
                    p(class = "note", "Each dot is an area (small areas hidden). Dashed line: least-squares fit per state."),
                    plotlyOutput("scatter", height = 290))))),
          nav_panel("Income groups", value = "income", div(class = "grid-even",
            card(textOutput("grp_title", inline = TRUE), "Income groups each hold about a fifth of the state's earners (Q1 = lowest-income areas). Weighted totals, not averages of areas.",
                 plotlyOutput("grp_bars", height = 280)),
            card("Fleet: BEVs per 1,000 vehicles by income group",
                 sprintf("Latest snapshot. NSW light vehicles (%s); VIC all vehicles (%s). QLD publishes no regional fleet data.", mon(D$fleet_dates$NSW), qmon(D$fleet_dates$VIC)),
                 plotlyOutput("fleet_bars", height = 280)))),
          nav_panel("Fuel crisis", value = "crisis", div(class = "grid-even",
            card("Pump prices and the 2026 fuel crisis", "Monthly average retail price, c/L. Shaded: crisis months (onset detected from terminal gate prices).", plotlyOutput("fuel", height = 260)),
            card("BEV share of new private registrations — all areas", "Same months as the price chart.", plotlyOutput("share_all", height = 260)),
            card("NSW — BEV share by income group: crisis vs a year earlier", textOutput("crisis_note_nsw"), plotlyOutput("crisis_nsw", height = 260)),
            card("QLD — BEV share by income group: crisis vs a year earlier", textOutput("crisis_note_qld"), plotlyOutput("crisis_qld", height = 260)))),
          nav_panel("Raw numbers", value = "raw", div(class = "grid-even",
            card("NSW — new private registrations per month", textOutput("raw_note"), plotlyOutput("raw_nsw", height = 260)),
            card("QLD — new private registrations per month", "BEV vs other fuels (count).", plotlyOutput("raw_qld", height = 260)),
            card("New BEVs registered by income group — last 12 months (count)", "NSW and QLD new private registrations.", plotlyOutput("raw_groups", height = 260)),
            card("New BEVs by income group — crisis months vs a year earlier (count)", sprintf("%s vs %s–%s.", crisis_label, mon(D$crisis$py_start), mon(D$crisis$py_end)),
                 plotlyOutput("raw_crisis", height = 260)))),
          nav_panel("Top 10", value = "top", div(class = "grid-even",
            div(class = "card2", h2(textOutput("tbl_title", inline = TRUE)),
                radioButtons("ord", NULL, c("Top 10" = "top", "Bottom 10" = "bottom"), inline = TRUE), uiOutput("tbl"),
                p(class = "note", style = "margin-top:10px", "Ranks the map metric chosen in the sidebar. Click a row to see it on the map.")),
            div(class = "card2", h2(textOutput("tbl2_title", inline = TRUE)), p(class = "note", "Raw count, same geography."), uiOutput("tbl2")))),
          nav_panel("Findings", value = "about", div(class = "card2 findings", h2("What the data show"), uiOutput("findings"))))))))

# ---- server -------------------------------------------------------------------------
server <- function(input, output, session) {
  geo <- reactive(if (is.null(input$geo)) "lga" else input$geo)
  sel <- reactiveVal(NULL)
  observe({
    m <- METRICS[[geo()]]
    updateSelectInput(session, "metric", choices = setNames(names(m), vapply(m, `[[`, "", "label")),
                      selected = if (isolate(input$metric) %in% names(m)) isolate(input$metric) else names(m)[1])
  })
  metric <- reactive({ m <- METRICS[[geo()]]; k <- if (input$metric %in% names(m)) input$metric else names(m)[1]; c(key = k, m[[k]]) })
  areas <- reactive({
    if (geo() == "vic") return(D$vic)
    if (input$st == "ALL") D$lga else D$lga[state == input$st]
  })
  val <- function(a) a[[metric()$key]]
  eligible <- reactive({ a <- areas(); a[(metric()$key %in% c("income") | elig) & !is.na(val(a))] })

  # ---- KPI row
  output$kpi_row <- renderUI({
    K <- D$kpi; G <- D$groups
    card <- function(l, v, s) div(class = "md-kpi-card", div(class = "md-kpi-label", l), div(class = "md-kpi-value", v), span(class = "md-kpi-sub", s))
    g5 <- G[state == "NSW" & group == NG, share]; g1 <- G[state == "NSW" & group == 1, share]
    div(class = "md-kpi-row",
        card("New BEVs, last 12 months", int(K$NSW$bev + K$QLD$bev), sprintf("NSW %s · QLD %s", int(K$NSW$bev), int(K$QLD$bev))),
        card("BEV share of new private cars", pct(K$NSW$share), sprintf("NSW · QLD %s", pct(K$QLD$share))),
        card("Richest vs poorest areas", sprintf("%.1f×", g5 / g1), "NSW BEV share, Q5 vs Q1"),
        card("BEVs in the fleet", int(K$NSW$fleet + K$VIC$fleet), sprintf("NSW %s · VIC %s", int(K$NSW$fleet), int(K$VIC$fleet))),
        card("Fuel crisis BEV share", sprintf("%s → %s", pct(K$NSW$share_p), pct(K$NSW$share_c)), sprintf("NSW, %s vs a year earlier", crisis_label)))
  })

  # ---- map
  shapes <- reactive({
    g <- if (geo() == "vic") D$geo_poa else D$geo_lga
    a <- areas()
    g <- g[g$id %in% a$id, ]
    cbind(g, a[match(g$id, a$id)])
  })
  bins <- reactive({
    v <- sort(val(eligible()))
    if (!length(v)) return(NULL)
    unique(quantile(v, probs = seq(0, 1, length.out = length(SEQ) + 1), names = FALSE, type = 1))
  })
  colour_of <- function(d) {
    b <- bins(); v <- d[[metric()$key]]
    ok <- (metric()$key == "income" | d$elig) & !is.na(v)
    out <- rep(COL$na, length(v))
    if (!is.null(b) && length(b) > 1) out[ok] <- SEQ[pmin(findInterval(v[ok], b, rightmost.closed = TRUE, all.inside = TRUE), length(SEQ))]
    out
  }
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomSnap = 0.25, attributionControl = TRUE)) |>
      addPolygons(data = D$geo_aus, fillColor = COL$land, fillOpacity = 1, color = COL$coast, weight = 0.8, options = pathOptions(interactive = FALSE)) |>
      fitBounds(112.9, -43.8, 153.8, -10.4) |>
      addControl(html = "Boundaries © ABS (ASGS)", position = "bottomright", className = "leaflet-control-attribution")
  })
  observe({
    d <- shapes(); m <- metric()
    lab <- sprintf("<b>%s (%s)</b><br>%s: %s<br>Median income: %s<br>Income group: Q%d", htmlEscape(d$name), d$state, short(m$label),
                   ifelse((m$key == "income" | d$elig) & !is.na(d[[m$key]]), m$fmt(d[[m$key]]), "too few registrations"), usd(d$income), d$group)
    leafletProxy("map") |> clearGroup("areas") |>
      addPolygons(data = d, layerId = ~id, group = "areas", fillColor = colour_of(d), fillOpacity = 0.85, color = "#FFFFFF", weight = 0.6,
                  label = lapply(lab, HTML), highlightOptions = highlightOptions(weight = 2, color = COL$text, bringToFront = TRUE),
                  labelOptions = labelOptions(style = list("font-family" = "Inter", "font-size" = "12px")))
  })
  fit_view <- function() {
    p <- leafletProxy("map")
    if (geo() == "vic") p |> fitBounds(144.3, -38.6, 145.6, -37.4)
    else if (input$st == "ALL") p |> fitBounds(112.9, -43.8, 153.8, -10.4)
    else { b <- st_bbox(shapes()); p |> fitBounds(b[["xmin"]], b[["ymin"]], b[["xmax"]], b[["ymax"]]) }
  }
  observeEvent(list(geo(), input$st), { sel(NULL); fit_view() }, ignoreInit = TRUE)
  observeEvent(input$reset, { sel(NULL); fit_view() })
  observeEvent(input$map_shape_click, sel(input$map_shape_click$id))
  observeEvent(input$pick, { sel(input$pick); nav_select("tab", "map") })
  observeEvent(event_data("plotly_click", source = "sc"), { e <- event_data("plotly_click", source = "sc"); if (!is.null(e$customdata)) sel(e$customdata) })
  observeEvent(sel(), {
    p <- leafletProxy("map") |> clearGroup("sel")
    if (!is.null(sel())) {
      g <- shapes(); g <- g[g$id == sel(), ]
      if (nrow(g)) p |> addPolylines(data = g, group = "sel", color = COL$text, weight = 2.5)
    }
  }, ignoreNULL = FALSE)
  output$map_title <- renderText(metric()$label)
  output$map_note <- renderText(if (geo() == "lga") "Council areas (LGAs) in NSW and QLD; other states in outline. Colour bins are sevenths of the areas shown. Grey = fewer than the minimum new registrations."
                                else "Postcodes, named by their ABS suburbs. Opens on Greater Melbourne — zoom out for regional Victoria. Colour bins are sevenths of postcodes.")
  output$legend <- renderUI({
    b <- bins(); if (is.null(b)) return(NULL); f <- metric()$fmt
    div(class = "legend-row", lapply(seq_len(length(b) - 1), function(i)
      div(class = "sw", tags$i(style = sprintf("background:%s", SEQ[i])), span(if (i < length(b) - 1) paste("≤", f(b[i + 1])) else paste(">", f(b[i]))))),
      div(style = "margin-left:12px;display:flex;gap:6px;align-items:center", tags$i(style = sprintf("width:14px;height:10px;background:%s;display:inline-block", COL$na)),
          "Too few registrations / no data"))
  })

  # ---- selection panel
  selected <- reactive({ a <- areas(); if (is.null(sel())) NULL else a[id == sel()][1] })
  output$sel_head <- renderUI({
    a <- selected()
    if (is.null(a) || is.na(a$id)) return(tagList(div(class = "sel-name", if (geo() == "lga") "All NSW and QLD council areas" else "All VIC postcodes"),
                                                 div(class = "sel-meta", "Click the map, the scatter or a table row to see one area.")))
    k <- function(l, v) div(class = "kpi", tags$b(v), span(l))
    ks <- if (geo() == "lga") list(k("BEV share, last 12 months", pct(a$share)), k(sprintf("Crisis %s (year earlier)", crisis_label), sprintf("%s (%s)", pct(a$share_c), pct(a$share_p))),
                                   if (a$state == "NSW") k("BEVs per 1,000 light vehicles", num1(a$per1000veh)) else k("BEVs seen per 1,000 earners", num1(a$per1000pop)))
          else list(k("BEVs per 1,000 vehicles", num1(a$per1000veh)), k("BEV share, recent-model", pct(a$rshare)), k("Added in crisis qtr (yr earlier)", sprintf("%s (%s)", int(a$add_c), int(a$add_p))))
    tagList(div(class = "sel-name", if (geo() == "vic") sprintf("%s — %s", a$name, a$id) else a$name),
            div(class = "sel-meta", sprintf("%s · median income %s · income group Q%d of %d%s", a$state, usd(a$income), a$group, NG, if (a$elig) "" else " · small area, treat with care")),
            div(class = "kpis", ks))
  })
  output$line_title <- renderText(if (geo() == "lga") "BEV share of new private registrations, monthly" else "BEVs per 1,000 registered vehicles, quarterly")
  output$line_note <- renderText({ a <- selected(); if (is.null(a) || is.na(a$id)) "State averages. Shaded: fuel crisis." else sprintf("%s vs %s average. Shaded: fuel crisis.", a$name, a$state) })
  output$sel_line <- renderPlotly({
    a <- selected(); p <- plot_ly()
    if (geo() == "lga") {
      x <- mdate(D$months); sm <- D$state_month
      if (is.null(a) || is.na(a$id)) {
        for (s in c("NSW", "QLD")) p <- p |> add_lines(x = x, y = sm[state == s][match(D$months, month), share], name = s, line = list(color = STATE_COL[[s]], width = 2))
      } else {
        y <- unlist(D$lga_series[state == a$state & lga_name == a$name, -(1:2)])
        p <- p |> add_lines(x = x, y = y, name = a$name, line = list(color = STATE_COL[[a$state]], width = 2)) |>
          add_lines(x = x, y = sm[state == a$state][match(D$months, month), share], name = paste(a$state, "average"), line = list(color = COL$low, width = 2, dash = "dash"))
      }
      p |> theme_plot(yfmt = ".0%") |> layout(shapes = list(crisis_band(mdate(D$crisis$start), mdate(D$crisis$end))), hovermode = "x unified")
    } else {
      x <- qdate(D$quarters)
      p <- p |> add_lines(x = x, y = D$vic_q[match(D$quarters, quarter), per1000], name = "VIC average", line = list(color = if (is.null(a) || is.na(a$id)) COL$vic else COL$low, width = 2, dash = if (is.null(a) || is.na(a$id)) "solid" else "dash"))
      if (!is.null(a) && !is.na(a$id)) p <- p |> add_lines(x = x, y = unlist(D$vic_series[postcode == as.integer(a$id), -1]), name = a$id, line = list(color = COL$vic, width = 2))
      p |> theme_plot(yfmt = ".0f") |> layout(shapes = list(crisis_band(mdate(D$crisis$start), qdate(D$crisis$vic_quarter))), hovermode = "x unified")
    }
  })

  # ---- scatter
  output$sc_title <- renderText({ m <- metric(); if (m$key == "income") "Median income vs BEV share" else paste("Median income vs", if (startsWith(m$label, "BEV")) m$label else sub("^(.)", "\\L\\1", m$label, perl = TRUE)) })
  output$scatter <- renderPlotly({
    m <- metric(); key <- if (m$key == "income") (if (geo() == "lga") "share" else "per1000veh") else m$key
    fmt <- METRICS[[geo()]][[key]]$fmt
    a <- areas()[elig == TRUE & !is.na(get(key))]
    p <- plot_ly(source = "sc")
    for (s in unique(a$state)) {
      d <- a[state == s]
      p <- p |> add_markers(data = d, x = ~income, y = d[[key]], customdata = ~id, name = s,
                            marker = list(color = STATE_COL[[s]], size = 8, opacity = 0.8, line = list(color = "#FFFFFF", width = 1)),
                            text = sprintf("<b>%s</b><br>%s<br>Median income %s", d$name, fmt(d[[key]]), usd(d$income)), hoverinfo = "text")
      if (nrow(d) > 2) { f <- lm(d[[key]] ~ d$income); xr <- range(d$income)
        p <- p |> add_lines(x = xr, y = coef(f)[1] + coef(f)[2] * xr, name = paste(s, "fit"), showlegend = FALSE, hoverinfo = "skip",
                            line = list(color = STATE_COL[[s]], dash = "dash", width = 2)) }
    }
    if (!is.null(sel())) { d <- a[id == sel()]; if (nrow(d)) p <- p |> add_markers(x = d$income, y = d[[key]], name = "Selected", showlegend = FALSE, hoverinfo = "skip",
                                                                          marker = list(size = 13, color = "rgba(0,0,0,0)", line = list(color = COL$text, width = 2.5))) }
    yf <- if (identical(fmt, pct)) ".0%" else if (identical(fmt, usd)) "$,.0f" else ",.1f"
    p |> theme_plot(yfmt = yf, xtitle = "Area median income") |> layout(xaxis = list(tickprefix = "$", tickformat = ",.0f"), showlegend = FALSE) |> event_register("plotly_click")
  })

  # ---- group bars
  bars <- function(series, fmt_axis, hover_fmt) {
    p <- plot_ly()
    for (s in series) p <- p |> add_bars(x = QLAB, y = s$vals, name = s$name, marker = list(color = s$colour),
                                         text = hover_fmt(s$vals), hoverinfo = "text+name", textposition = "none")
    p |> theme_plot(yfmt = fmt_axis) |> layout(barmode = "group", bargap = 0.3, xaxis = list(categoryorder = "array", categoryarray = QLAB))
  }
  axis_of <- function(f) if (identical(f, pct)) ".0%" else if (identical(f, usd)) "$,.0f" else ",.0f"
  output$grp_title <- renderText({ m <- metric(); key <- if (m$key == "income") (if (geo() == "lga") "share" else "per1000veh") else m$key; paste(METRICS[[geo()]][[key]]$label, "— by income group") })
  output$grp_bars <- renderPlotly({
    m <- metric(); key <- if (m$key == "income") (if (geo() == "lga") "share" else "per1000veh") else m$key
    if (key == "fleet_bev") key <- "fleet"
    if (key == "add_c") key <- "add_c1000"
    fmt <- (METRICS[[geo()]][[m$key]] %||% METRICS[[geo()]]$per1000veh)$fmt
    sts <- if (geo() == "vic") "VIC" else if (input$st == "ALL") c("NSW", "QLD") else input$st
    series <- lapply(sts, function(s) list(name = s, colour = STATE_COL[[s]], vals = D$groups[state == s][order(group)][[key]] %||% rep(NA, NG)))
    bars(series, axis_of(fmt), fmt)
  })
  output$fleet_bars <- renderPlotly(bars(list(list(name = "NSW (per 1,000 light vehicles)", colour = COL$nsw, vals = D$groups[state == "NSW"][order(group), per1000veh]),
                                              list(name = "VIC (per 1,000 vehicles)", colour = COL$vic, vals = D$groups[state == "VIC"][order(group), per1000veh])), ",.0f", num1))

  # ---- fuel crisis
  output$fuel <- renderPlotly({
    F <- D$fuel; x <- mdate(F$month)
    plot_ly() |> add_lines(x = x, y = F$NSW_ULP, name = "NSW ULP", line = list(color = COL$nsw, width = 2)) |>
      add_lines(x = x, y = F$QLD_ULP, name = "QLD ULP", line = list(color = COL$qld, width = 2)) |>
      add_lines(x = x, y = F$NSW_Diesel, name = "NSW diesel", line = list(color = COL$low, width = 2, dash = "dash")) |>
      theme_plot(yfmt = ",.0f", ytitle = "c/L") |> layout(shapes = list(crisis_band(mdate(D$crisis$start), mdate(D$crisis$end))), hovermode = "x unified")
  })
  output$share_all <- renderPlotly({
    sm <- D$state_month; p <- plot_ly()
    for (s in c("NSW", "QLD")) p <- p |> add_lines(x = mdate(sm[state == s, month]), y = sm[state == s, share], name = s, line = list(color = STATE_COL[[s]], width = 2))
    p |> theme_plot(yfmt = ".0%") |> layout(shapes = list(crisis_band(mdate(D$crisis$start), mdate(D$crisis$end))), hovermode = "x unified")
  })
  crisis_bars <- function(s) {
    g <- D$groups[state == s][order(group)]
    bars(list(list(name = sprintf("%s–%s", mon(D$crisis$py_start), mon(D$crisis$py_end)), colour = COL$other, vals = g$share_p),
              list(name = paste("Crisis", crisis_label), colour = STATE_COL[[s]], vals = g$share_c)), ".0%", pct)
  }
  output$crisis_nsw <- renderPlotly(crisis_bars("NSW"))
  output$crisis_qld <- renderPlotly(crisis_bars("QLD"))
  crisis_note <- function(s) { g <- D$groups[state == s][order(group)]; sprintf("BEV share rose %s in the lowest-income group vs %s in the highest.", mult(g$mult[1]), mult(g$mult[NG])) }
  output$crisis_note_nsw <- renderText(crisis_note("NSW"))
  output$crisis_note_qld <- renderText(crisis_note("QLD"))

  # ---- raw numbers
  stacked <- function(s) {
    sm <- D$state_month[state == s]; x <- mdate(sm$month)
    plot_ly() |> add_bars(x = x, y = sm$bev, name = "BEV", marker = list(color = STATE_COL[[s]])) |>
      add_bars(x = x, y = sm$other, name = "Other fuels", marker = list(color = COL$other)) |>
      theme_plot(yfmt = ",.0f") |> layout(barmode = "stack", bargap = 0.15, hovermode = "x unified",
                                          shapes = list(crisis_band(mdate(D$crisis$start) - 15, mdate(D$crisis$end) + 15)))
  }
  output$raw_nsw <- renderPlotly(stacked("NSW"))
  output$raw_qld <- renderPlotly(stacked("QLD"))
  output$raw_note <- renderText(sprintf("BEV vs other fuels (count). Low months (%s) are TfNSW data gaps: blocks of rows with no make or fuel, left out of these counts.",
                                        paste(mon(D$gaps), collapse = ", ")))
  output$raw_groups <- renderPlotly(bars(lapply(c("NSW", "QLD"), function(s) list(name = s, colour = STATE_COL[[s]], vals = D$groups[state == s][order(group), bev])), ",.0f", int))
  output$raw_crisis <- renderPlotly(bars(list(
    list(name = "NSW yr earlier", colour = "#B3CCE8", vals = D$groups[state == "NSW"][order(group), p_bev]),
    list(name = "NSW crisis", colour = COL$nsw, vals = D$groups[state == "NSW"][order(group), c_bev]),
    list(name = "QLD yr earlier", colour = "#F8D2A8", vals = D$groups[state == "QLD"][order(group), p_bev]),
    list(name = "QLD crisis", colour = COL$qld, vals = D$groups[state == "QLD"][order(group), c_bev])), ",.0f", int))

  # ---- top 10 tables
  rank_table <- function(rows, key, fmt, lab) {
    if (!nrow(rows)) return(p(class = "note", "No areas meet the size threshold for this metric."))
    tags$table(class = "md", tags$thead(tags$tr(tags$th("#"), tags$th("Area"), tags$th(class = "num", "Income"), tags$th(class = "num", lab))),
      tags$tbody(lapply(seq_len(nrow(rows)), function(i) { a <- rows[i]
        tags$tr(onclick = sprintf("Shiny.setInputValue('pick', '%s', {priority: 'event'})", a$id),
                tags$td(i), tags$td(sprintf("%s (%s)", a$name, if (geo() == "vic") a$id else a$state)), tags$td(class = "num", usd(a$income)),
                tags$td(class = "num", fmt(a[[key]]))) })))
  }
  output$tbl_title <- renderText(sprintf("%s: %s", if (input$ord == "top") "Highest" else "Lowest", metric()$label))
  output$tbl <- renderUI({
    e <- eligible(); k <- metric()$key
    rows <- e[order(if (input$ord == "top") -get(k) else get(k))][seq_len(min(10, .N))]
    rank_table(rows, k, metric()$fmt, short(metric()$label))
  })
  cnt_key <- reactive(if (geo() == "lga") "bev" else "bev")
  output$tbl2_title <- renderText(if (geo() == "lga") "Most BEVs: new BEVs, last 12 months" else "Most BEVs: BEVs in the fleet")
  output$tbl2 <- renderUI({ a <- areas()[!is.na(bev)][order(-bev)][seq_len(min(10, .N))]; rank_table(a, "bev", int, "BEVs") })

  # ---- findings
  output$findings <- renderUI({
    G <- D$groups; g <- function(s, q, k) G[state == s & group == q][[k]]
    items <- list(
      c("Richer areas buy more BEVs. ", sprintf("Over the last 12 months the richest fifth of NSW council areas registered BEVs at %s of new private cars, vs %s in the poorest (%.1f×). In the fleet the gap is wider: %s vs %s BEVs per 1,000 light vehicles.",
                                               pct(g("NSW", NG, "share")), pct(g("NSW", 1, "share")), g("NSW", NG, "share") / g("NSW", 1, "share"), num1(g("NSW", NG, "per1000veh")), num1(g("NSW", 1, "per1000veh")))),
      c("VIC postcodes show the same gradient. ", sprintf("%s BEVs per 1,000 vehicles in the top income group vs %s in the bottom.", num1(g("VIC", NG, "per1000veh")), num1(g("VIC", 1, "per1000veh")))),
      c("QLD is flatter at council level. ", sprintf("Its LGAs are large (Brisbane alone is about a quarter of QLD earners), and lower-income coastal retiree areas such as the Sunshine Coast take up BEVs strongly. Top vs bottom group: %s vs %s.",
                                                    pct(g("QLD", NG, "share")), pct(g("QLD", 1, "share")))),
      c("The fuel crisis narrowed the gap in relative terms. ", sprintf("Comparing %s with the same months a year earlier, BEV share rose %s in NSW's lowest-income group vs %s in the highest (QLD: %s vs %s). In percentage points richer areas still gained more (%s vs %s in NSW).",
                                                                        crisis_label, mult(g("NSW", 1, "mult")), mult(g("NSW", NG, "mult")), mult(g("QLD", 1, "mult")), mult(g("QLD", NG, "mult")),
                                                                        pp(g("NSW", NG, "chg")), pp(g("NSW", 1, "chg")))),
      c("Regional coastal areas moved most. ", "Switch the map metric to the crisis change: the biggest NSW jumps were in lower-income coastal LGAs such as Bellingen, Kiama, Byron, Eurobodalla and Ballina, alongside the wealthy North Shore."),
      c("Caveat. ", "This compares areas, not people: it shows where BEVs are registered, not who bought them. Novated leases, retirees' wealth and business fleets all blur the link to income."))
    tags$ul(lapply(items, function(x) tags$li(tags$b(x[1]), x[2])))
  })

  # ---- downloads
  about <- reactive(data.frame(Item = c("Exported from", "Geography", "State filter", "Recent window", "Fuel crisis window", "Year-earlier comparison", "Sources and adjustments"),
                               Value = c("EV × Income dashboard", if (geo() == "lga") "NSW + QLD council areas (LGAs)" else "VIC postcodes",
                                         if (geo() == "lga") input$st else "VIC", paste(D$recent$start, "to", D$recent$end), paste(D$crisis$start, "to", D$crisis$end),
                                         paste(D$crisis$py_start, "to", D$crisis$py_end), "See the full workbook (Sources, Data_Adjustments, Notes sheets)")))
  cols_lga <- c(state = "State", name = "LGA", id = "LGA code", income = "Median total income 2022-23 ($)", pop = "Earners", group = "Income group (1 = lowest)",
                new = "New private regos, last 12 months", bev = "BEV, last 12 months", share = "BEV share, last 12 months", p_new = "New regos, year before crisis",
                p_bev = "BEV, year before crisis", share_p = "BEV share, year before crisis", c_new = "New regos, crisis months", c_bev = "BEV, crisis months",
                share_c = "BEV share, crisis months", chg = "Change (pp)", mult = "Crisis ÷ year earlier", fleet_bev = "BEV fleet (NSW est.; QLD BEVs seen since 2022)",
                fleet_veh = "Light-vehicle fleet (NSW)", per1000veh = "BEV per 1,000 light vehicles (NSW)", per1000pop = "BEV fleet per 1,000 earners", elig = "Above size threshold")
  cols_vic <- c(id = "Postcode", name = "Suburbs", income = "Median taxable income 2023-24 ($)", pop = "Individuals", group = "Income group (1 = lowest)",
                vehicles = "Vehicles (latest)", bev = "BEV (latest)", per1000veh = "BEV per 1,000 vehicles", recent_vehicles = "Recent-model vehicles",
                recent_bev = "Recent-model BEV", rshare = "BEV share of recent-model", add_c = "BEV added, crisis quarter", add_c1000 = "Added per 1,000 vehicles, crisis quarter",
                add_p = "BEV added, same quarter a year earlier", add_p1000 = "Added per 1,000, year earlier", elig = "Above size threshold")
  area_table <- function(a) { cols <- if (geo() == "lga") cols_lga else cols_vic; x <- as.data.frame(a[, names(cols), with = FALSE]); names(x) <- cols; x }
  output$dl_workbook <- downloadHandler(filename = function() basename(WORKBOOK), content = function(f) file.copy(WORKBOOK, f))
  output$dl_areas <- downloadHandler(filename = function() sprintf("EV_by_income_%s.xlsx", if (geo() == "lga") paste0("LGAs_", input$st) else "VIC_postcodes"),
                                     content = function(f) write_xlsx(list(Areas = area_table(areas()), About = about()), f))
  output$dl_top <- downloadHandler(filename = function() sprintf("EV_top10_%s.xlsx", if (geo() == "lga") input$st else "VIC"), content = function(f) {
    sheets <- list()
    for (k in names(METRICS[[geo()]])) {
      a <- areas()[(k == "income" | elig) & !is.na(get(k))]
      mk <- function(d) data.frame(Rank = seq_len(nrow(d)), Area = d$name, Code = d$id, State = d$state, `Median income ($)` = d$income, Value = d[[k]], check.names = FALSE) |>
        setNames(c("Rank", "Area", "Code", "State", "Median income ($)", METRICS[[geo()]][[k]]$label))
      sheets[[paste("Top", k)]] <- mk(a[order(-get(k))][seq_len(min(10, .N))])
      sheets[[paste("Bottom", k)]] <- mk(a[order(get(k))][seq_len(min(10, .N))])
    }
    write_xlsx(c(sheets, list(About = about())), f)
  })
  output$dl_groups <- downloadHandler(filename = "EV_income_groups.xlsx", content = function(f) {
    g <- copy(D$groups)[, `Income group` := paste0("Q", group)]
    write_xlsx(c(lapply(split(as.data.frame(g), g$state), function(x) x[, c("Income group", setdiff(names(x), c("Income group", "group", "state")))]), list(About = about())), f)
  })
  output$dl_series <- downloadHandler(filename = function() sprintf("EV_series_%s.xlsx", if (geo() == "lga") "monthly_BEV_share" else "quarterly_BEV_per_1000"), content = function(f) {
    s <- if (geo() == "lga") as.data.frame(D$lga_series) else as.data.frame(D$vic_series)
    write_xlsx(list(Series = s, `State totals` = as.data.frame(D$state_month), `Fuel prices (c per L)` = as.data.frame(D$fuel), About = about()), f)
  })
}

`%||%` <- function(a, b) if (is.null(a)) b else a
shinyApp(ui, server)
