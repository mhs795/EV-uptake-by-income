# EV × Income — Shiny dashboard. Tabs are self-contained (own controls, own selection).
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
CT_COL <- c(Private = "#1F7AE0", Business = "#F57C00", `Dealer demonstrator` = "#00897B", Government = "#9AA5B1", Organisation = "#F57C00")
SEQ <- c("#E3F2FD", "#BBDEFB", "#90CAF9", "#5AA2EE", "#1F7AE0", "#1565C0", "#0D47A1")
GROUP_COL <- c("#90CAF9", "#5AA2EE", "#1F7AE0", "#1565C0", "#0D47A1")
NG <- D$n_groups
RW <- D$windows$recent   # recent window as months, e.g. "Sep 2025–Aug 2026"
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
    share = list(label = sprintf("BEV share of new cars (%s)", RW), fmt = pct),
    share_c = list(label = sprintf("BEV share during the fuel crisis (%s)", crisis_label), fmt = pct),
    chg = list(label = "Change in BEV share: crisis vs same months a year earlier", fmt = pp),
    mult = list(label = "Crisis BEV share ÷ year-earlier share", fmt = mult),
    bev = list(label = sprintf("New BEVs registered, %s (count)", RW), fmt = int),
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
    config(displayModeBar = FALSE, responsive = TRUE)
}
crisis_band <- function(x0, x1) list(type = "rect", xref = "x", yref = "paper", x0 = x0, x1 = x1, y0 = 0, y1 = 1,
                                     fillcolor = COL$band, line = list(width = 0), layer = "below")
mdate <- function(ym) as.Date(paste0(ym, "-01"))
qdate <- function(q) as.Date(sprintf("%s-%02d-01", substr(q, 1, 4), as.integer(substr(q, 6, 6)) * 3L))


# ---- UI ----------------------------------------------------------------------------
# Every tab is self-contained: its own controls sit in a toolbar at the top of the tab,
# and clicking an area (map, scatter or table row) only changes that tab.
css <- "
:root { --md-bg:#F5F6F8; --md-surface:#FFFFFF; --md-primary:#1F7AE0; --md-primary-dim:rgba(31,122,224,0.08); --md-hover:rgba(31,122,224,0.05);
  --md-text:#1A1D21; --md-text-med:#6B7280; --md-text-low:#9AA5B1; --md-divider:#E3E6EA; --font:'Inter','Segoe UI',Roboto,system-ui,sans-serif; }
html, body { background: var(--md-bg) !important; color: var(--md-text); font-family: var(--font) !important; font-size: 14px; -webkit-font-smoothing: antialiased; }
.md-header { background: var(--md-surface); padding: 12px 22px; border-bottom: 1px solid var(--md-divider); display: flex; align-items: center; gap: 14px; flex-wrap: wrap; }
.md-brand-icon { width: 34px; height: 34px; background: var(--md-primary-dim); border: 1px solid var(--md-divider); border-radius: 8px; display: flex; align-items: center; justify-content: center; color: var(--md-primary); flex-shrink: 0; }
.md-header-title { font-size: 16px; font-weight: 700; letter-spacing: -.2px; }
.md-header-sub { font-size: 12px; color: var(--md-text-med); margin-top: 1px; }
.md-header-right { margin-left: auto; display: flex; gap: 8px; align-items: center; flex-wrap: wrap; }
.md-chip { font-size: 11px; font-weight: 600; padding: 5px 12px; border-radius: 14px; background: var(--md-primary-dim); color: var(--md-primary); border: 1px solid var(--md-divider); white-space: nowrap; }
.md-chip.warn { background: rgba(245,124,0,.08); color: #B45309; }
.md-btn { display: inline-block; padding: 7px 14px; border-radius: 8px; font-size: 13px; font-weight: 600; border: 1px solid var(--md-divider);
  background: transparent; color: var(--md-text); text-align: center; text-decoration: none; }
.md-btn:hover { background: var(--md-hover); color: var(--md-text); }
.md-btn-filled { background: var(--md-primary); color: #fff; border-color: var(--md-primary); }
.md-btn-filled:hover, .md-btn-filled.show { background: var(--md-primary); color: #fff; opacity: .9; }
.dropdown-menu { font-size: 13px; border-color: var(--md-divider); border-radius: 10px; padding: 6px; box-shadow: 0 6px 24px rgba(0,0,0,.08); }
.dropdown-item { border-radius: 6px; padding: 7px 12px; }
.dropdown-item small { display: block; color: var(--md-text-med); }
.md-content { padding: 0 22px 22px; }
.nav-underline { border-bottom: 1px solid var(--md-divider); gap: 0; margin-bottom: 14px; }
.nav-underline .nav-link { color: var(--md-text-med); font-size: 13px; font-weight: 600; padding: 12px 14px; border-bottom-width: 2px; }
.nav-underline .nav-link:hover { color: var(--md-primary); }
.nav-underline .nav-link.active { color: var(--md-primary); border-bottom-color: var(--md-primary); }
.toolbar { display: flex; gap: 14px; align-items: flex-end; flex-wrap: wrap; background: var(--md-surface); border: 1px solid var(--md-divider);
  border-radius: 12px; padding: 10px 14px; margin-bottom: 14px; }
.toolbar .form-group { margin-bottom: 0; }
.toolbar .shiny-input-container { width: auto !important; }
.toolbar label.control-label, .tb-label { display: block; font-size: 10px; text-transform: uppercase; letter-spacing: .6px; font-weight: 600; color: var(--md-text-low); margin-bottom: 4px; }
.toolbar .form-select, .toolbar .selectize-input { font-size: 13px; border-radius: 8px; border-color: var(--md-divider); min-height: 36px; }
.toolbar .tb-metric .selectize-control { min-width: 380px; }
.toolbar .selectize-control.single .selectize-input { padding-right: 32px !important; }   /* room for the dropdown arrow */
.toolbar .tb-state .selectize-control { min-width: 150px; }
.toolbar .tb-hint { font-size: 12px; color: var(--md-text-med); margin-left: auto; align-self: center; max-width: 340px; line-height: 1.4; }
.seg { display: inline-flex; border: 1px solid var(--md-divider); border-radius: 8px; overflow: hidden; height: 36px; }
.seg button { border: 0; background: transparent; color: var(--md-text-med); padding: 0 14px; font-size: 13px; font-weight: 600; white-space: nowrap; }
.seg button + button { border-left: 1px solid var(--md-divider); }
.seg button:hover { background: var(--md-hover); }
.seg button.on { background: var(--md-primary-dim); color: var(--md-primary); }
.md-kpi-row { display: grid; grid-template-columns: repeat(auto-fit, minmax(180px, 1fr)); gap: 12px; margin-bottom: 14px; }
.md-kpi-card { background: var(--md-surface); border: 1px solid var(--md-divider); border-radius: 10px; padding: 12px 16px; }
.md-kpi-label { font-size: 10px; font-weight: 600; letter-spacing: .6px; text-transform: uppercase; color: var(--md-text-low); margin-bottom: 3px; }
.md-kpi-value { font-size: 22px; font-weight: 700; line-height: 1.2; }
.md-kpi-sub { display: block; font-size: 12px; font-weight: 600; color: var(--md-text-low); margin-top: 2px; }
.card2 { background: var(--md-surface); border: 1px solid var(--md-divider); border-radius: 12px; padding: 16px 18px; min-width: 0; margin-bottom: 14px; overflow: hidden; }
.card2 .html-widget { max-width: 100%; }
.card2 h2 { font-size: 14px; margin: 0 0 2px; font-weight: 600; }
.card2 .note { color: var(--md-text-med); font-size: 12px; margin: 0 0 10px; }
.grid-2 { display: grid; grid-template-columns: minmax(0,1.45fr) minmax(0,1fr); gap: 14px; align-items: start; }
.grid-even { display: grid; grid-template-columns: minmax(0,1fr) minmax(0,1fr); gap: 14px; align-items: stretch; }   /* side-by-side cards share a height */
.grid-even > .card2, .grid-2 > .card2 { margin-bottom: 0; }
.stack { display: flex; flex-direction: column; gap: 14px; min-width: 0; }
.stack > .card2 { margin-bottom: 0; }
.leaflet-container { background: #EEF2F6 !important; border-radius: 8px; font-family: var(--font); }
.sel-name { font-size: 17px; font-weight: 700; display: flex; align-items: center; gap: 8px; }
.sel-meta { color: var(--md-text-med); font-size: 12px; }
.sel-clear { font-size: 12px; font-weight: 600; color: var(--md-primary); cursor: pointer; margin-left: auto; }
.kpis { display: grid; grid-template-columns: repeat(3, minmax(0,1fr)); gap: 8px; margin: 10px 0 6px; }
.kpi { background: #F5F6F8; border: 1px solid var(--md-divider); border-radius: 8px; padding: 8px 10px; }
.kpi b { display: block; font-size: 17px; font-weight: 700; }
.kpi span { font-size: 11px; color: var(--md-text-med); }
table.md { width: 100%; border-collapse: collapse; font-size: 12.5px; font-variant-numeric: tabular-nums; }
table.md th { text-align: left; color: var(--md-text-low); font-weight: 600; font-size: 11px; text-transform: uppercase; letter-spacing: .4px; border-bottom: 1px solid var(--md-divider); padding: 6px; }
table.md td { padding: 7px 6px; border-bottom: 1px solid var(--md-divider); }
table.md .num { text-align: right; }
table.md.pick tbody tr { cursor: pointer; }
table.md.pick tbody tr:hover { background: var(--md-hover); }
table.md tbody tr.on { background: var(--md-primary-dim); box-shadow: inset 3px 0 0 var(--md-primary); }
.findings ul { color: var(--md-text-med); line-height: 1.55; padding-left: 18px; margin-bottom: 0; }
.findings li + li { margin-top: 6px; }
.findings b { color: var(--md-text); }
.sources { font-size: 12px; color: var(--md-text-med); line-height: 1.5; }
.legend-row { display:flex; flex-wrap:wrap; font-size:11px; color: var(--md-text-med); margin-top:8px; }
.legend-row .sw { width: 70px; } .legend-row .sw i { display:block; height:10px; } .legend-row .sw span { display:block; padding-top:2px; white-space:nowrap; }
@media (max-width: 1100px) { .grid-2, .grid-even { grid-template-columns: 1fr; } .toolbar .tb-metric .selectize-control { min-width: 0; } .toolbar .tb-hint { margin-left: 0; } }
"

# Segmented buttons: <div class='seg' data-input='x'><button data-value='a'>…  → input$x
seg_js <- "
// Keep every Plotly chart the width of its card: re-fit when a card changes size, and when a tab
// opens (charts drawn while their tab was hidden start at Plotly's default width)
function fitPlots(root) {
  $(root || document).find('.js-plotly-plot').each(function() { if (this.offsetParent !== null && window.Plotly) Plotly.Plots.resize(this); });
}
if (window.ResizeObserver) {
  const ro = new ResizeObserver(entries => entries.forEach(e => fitPlots(e.target)));
  $(document).on('shiny:value', function(ev) { setTimeout(() => $('.card2').each(function() { ro.observe(this); }), 0); });
}
$(document).on('shown.bs.tab', function() { setTimeout(() => { fitPlots(); window.dispatchEvent(new Event('resize')); }, 50); });
$(document).on('click', '.seg button', function() {
  $(this).addClass('on').siblings().removeClass('on');
  Shiny.setInputValue($(this).parent().data('input'), $(this).data('value'));
});
"
seg <- function(id, choices, label = NULL)
  div(if (!is.null(label)) span(class = "tb-label", label),
      div(class = "seg", `data-input` = id, lapply(seq_along(choices), function(i)
        tags$button(class = if (i == 1) "on", `data-value` = unname(choices[i]), names(choices)[i]))))
geo_seg <- function(id) seg(id, c("NSW + QLD" = "lga", "VIC" = "vic"), "Geography")
state_sel <- function(id, geo_id) conditionalPanel(sprintf("input.%s != 'vic'", geo_id),
                                                   div(class = "tb-state", selectInput(id, "State", c("NSW + QLD" = "ALL", "NSW", "QLD"))))
metric_choices <- function(g, drop = NULL) { m <- METRICS[[g]][setdiff(names(METRICS[[g]]), drop)]; setNames(names(m), vapply(m, `[[`, "", "label")) }
# all customers or private buyers only (NSW + QLD; VIC data have no customer type)
cust_seg <- function(id, geo_id) conditionalPanel(sprintf("input.%s != 'vic'", geo_id), seg(id, c("All" = "all", "Private only" = "private"), "Customers"))
metric_sel <- function(id, label, drop = NULL) div(class = "tb-metric", selectInput(id, label, metric_choices("lga", drop)))

card <- function(title, note = NULL, ...) div(class = "card2", h2(title), if (!is.null(note)) p(class = "note", note), ...)
# area detail: name + KPIs + trend line, used on the Map and Rankings tabs
detail_ui <- function(id) div(class = "card2", uiOutput(paste0(id, "_head")), h2(textOutput(paste0(id, "_line_title"), inline = TRUE), style = "margin-top:10px"),
                              p(class = "note", textOutput(paste0(id, "_line_note"), inline = TRUE)), plotlyOutput(paste0(id, "_line"), height = 230))

dl_item <- function(id, label, sub) tags$li(downloadLink(id, class = "dropdown-item", label, tags$small(sub)))

ui <- page(
  theme = bs_theme(version = 5, primary = COL$primary, bg = "#F5F6F8", fg = COL$text,
                   base_font = font_collection("Inter", "Segoe UI", "Roboto", "system-ui", "sans-serif")),
  tags$head(tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap"),
            tags$style(HTML(css)), tags$script(HTML(seg_js)), tags$title("EV × Income")),
  div(class = "md-header",
      div(class = "md-brand-icon", HTML('<svg width="18" height="18" viewBox="0 0 24 24" fill="currentColor"><path d="M13 2 4 14h6l-1 8 9-12h-6l1-8z"/></svg>')),
      div(div(class = "md-header-title", "EV uptake by regional income — NSW, QLD, VIC"),
          div(class = "md-header-sub", "Battery-electric vehicles in state registration data against ATO/ABS median income by area")),
      div(class = "md-header-right",
          span(class = "md-chip", sprintf("Recent window: %s", RW)),
          span(class = "md-chip warn", sprintf("Fuel crisis: %s", crisis_label)),
          div(class = "dropdown",
              tags$button(class = "md-btn md-btn-filled dropdown-toggle", type = "button", `data-bs-toggle` = "dropdown", "Download"),
              tags$ul(class = "dropdown-menu dropdown-menu-end",
                      dl_item("dl_workbook", "Full workbook", "Every table, chart, source and adjustment"),
                      dl_item("dl_areas", "All areas", "NSW + QLD councils and VIC postcodes"),
                      dl_item("dl_top", "Top & bottom 10s", "Every metric, both geographies"),
                      dl_item("dl_groups", "Income-group summary", "Totals by income group and customer type, and how groups are built"),
                      dl_item("dl_series", "Monthly series + fuel prices", "Per-area trends and state totals"))))),
  div(class = "md-content",
    navset_underline(id = "tab",
      nav_panel("Overview", value = "overview",
        uiOutput("kpi_row"),
        div(class = "card2 findings", h2("What the data show"), uiOutput("findings")),
        div(class = "card2 sources", h2("Data"),
            "Public data: TfNSW, QLD TMR and VIC DTP vehicle registrations; ABS Personal Income 2022-23 (ATO-based); ATO Taxation Statistics 2023-24; ",
            "NSW FuelCheck, QLD fuel prices, AIP; ABS boundaries. NSW and QLD are by council area (LGA); VIC by postcode, named by its suburbs. ",
            "Sources, data adjustments and method are in the full workbook (Download, top right).")),

      nav_panel("Map", value = "map",
        div(class = "toolbar", geo_seg("map_geo"), state_sel("map_st", "map_geo"), cust_seg("map_cust", "map_geo"), metric_sel("map_metric", "Colour areas by"),
            actionButton("map_reset", "Reset view", class = "md-btn"),
            span(class = "tb-hint", "Click an area on the map or a dot on the scatter to see its trend.")),
        div(class = "grid-2",
            div(class = "card2", h2(textOutput("map_title", inline = TRUE)), p(class = "note", textOutput("map_note", inline = TRUE)),
                leafletOutput("map", height = 600), uiOutput("legend")),
            div(class = "stack", detail_ui("map_sel"),
                div(class = "card2", h2(textOutput("sc_title", inline = TRUE)),
                    p(class = "note", "Each dot is an area (small areas hidden). Dashed line: least-squares fit per state."),
                    plotlyOutput("scatter", height = 290))))),

      nav_panel("Income groups", value = "income",
        div(class = "toolbar", geo_seg("grp_geo"), cust_seg("grp_cust", "grp_geo"), metric_sel("grp_metric", "Measure", drop = c("income", "add_c")),
            span(class = "tb-hint", "Areas are grouped into fifths of each state's earners, from the lowest-income areas (Q1) to the highest (Q5).")),
        div(class = "grid-even",
          card(textOutput("grp_title", inline = TRUE), "Weighted totals for each group, not averages of areas. Method below.", plotlyOutput("grp_bars", height = 280)),
          card("Fleet: BEVs per 1,000 vehicles by income group",
               sprintf("Latest snapshot. NSW light vehicles (%s); VIC all vehicles (%s). QLD publishes no regional fleet data.", mon(D$fleet_dates$NSW), qmon(D$fleet_dates$VIC)),
               plotlyOutput("fleet_bars", height = 280)),
          div(class = "card2 findings", style = "grid-column: 1 / -1;", h2("How the income groups are built"), uiOutput("grp_method")))),

      nav_panel("Fuel crisis", value = "crisis", div(class = "grid-even",
        card("Pump prices and the 2026 fuel crisis", "Monthly average retail price, c/L. Shaded: crisis months (onset detected from terminal gate prices).", plotlyOutput("fuel", height = 260)),
        card("BEV share of new registrations — all areas and customers", "Same months as the price chart.", plotlyOutput("share_all", height = 260)),
        card("NSW — BEV share by income group: crisis vs a year earlier", textOutput("crisis_note_nsw"), plotlyOutput("crisis_nsw", height = 260)),
        card("QLD — BEV share by income group: crisis vs a year earlier", textOutput("crisis_note_qld"), plotlyOutput("crisis_qld", height = 260)))),

      nav_panel("Customer types", value = "cust",
        div(class = "toolbar", seg("ct_st", c(NSW = "NSW", QLD = "QLD"), "State"),
            span(class = "tb-hint", style = "max-width:560px", "NSW splits private, business, dealer (demonstrator) and government buyers; QLD only individuals and organisations. ",
                 "Organisations register vehicles at their own address, not where the driver lives, so their BEVs cluster in a few councils.")),
        div(class = "grid-even",
          card(sprintf("BEV take-up by customer type — %s", RW), textOutput("ct_note"), uiOutput("ct_tbl")),
          card("Where each type's BEVs are registered", "Share of each type's new BEVs registered in its top 5 councils, against those councils' share of the state's earners.",
               uiOutput("ct_conc")),
          card("BEV share of new registrations by customer type, monthly", "Shaded: fuel crisis.", plotlyOutput("ct_share", height = 260)),
          card("New BEVs per month by customer type (count)", "Shaded: fuel crisis.", plotlyOutput("ct_count", height = 260)),
          card(sprintf("BEV share by income group and customer type — %s", RW), "Income group of the council where the vehicle is registered.", plotlyOutput("ct_grp", height = 260)),
          card(sprintf("Share of each income group's new BEVs not bought privately — %s", RW), "Business, dealer and government (QLD: organisations).",
               plotlyOutput("ct_notpriv", height = 260)),
          div(class = "card2", style = "grid-column: 1 / -1;", h2(sprintf("Councils with the most BEVs bought by organisations — %s", RW)),
              p(class = "note", "Head offices, fleet and leasing companies and dealers show up here, not where the cars are driven."), uiOutput("ct_top")))),

      nav_panel("Registrations", value = "raw", div(class = "grid-even",
        card("NSW — new registrations per month", textOutput("raw_note"), plotlyOutput("raw_nsw", height = 260)),
        card("QLD — new registrations per month", "BEV vs other fuels (count).", plotlyOutput("raw_qld", height = 260)),
        card(sprintf("New BEVs registered by income group — %s (count)", RW), "NSW and QLD new registrations, all customer types.", plotlyOutput("raw_groups", height = 260)),
        card("New BEVs by income group — crisis months vs a year earlier (count)", sprintf("%s vs %s–%s.", crisis_label, mon(D$crisis$py_start), mon(D$crisis$py_end)),
             plotlyOutput("raw_crisis", height = 260)))),

      nav_panel("Rankings", value = "rank",
        div(class = "toolbar", geo_seg("rank_geo"), state_sel("rank_st", "rank_geo"), cust_seg("rank_cust", "rank_geo"), metric_sel("rank_metric", "Rank by"),
            seg("rank_ord", c("Highest 10" = "top", "Lowest 10" = "bottom"), "Show"),
            span(class = "tb-hint", "Click a row to see that area's trend alongside.")),
        div(class = "grid-even",
            div(class = "card2", h2(textOutput("rank_title", inline = TRUE)), p(class = "note", textOutput("rank_note", inline = TRUE)), uiOutput("rank_tbl")),
            detail_ui("rank_sel"))))))

# ---- server -------------------------------------------------------------------------
server <- function(input, output, session) {
  # ---- per-tab view: geography, state filter, metric
  view <- function(prefix, drop = NULL) {
    geo <- reactive(input[[paste0(prefix, "_geo")]] %||% "lga")
    st <- reactive(if (geo() == "vic") "VIC" else input[[paste0(prefix, "_st")]] %||% "ALL")
    cust <- reactive(input[[paste0(prefix, "_cust")]] %||% "all")
    dv <- reactive(D$views[[cust()]])   # lga, lga_series, groups, state_month for the chosen customers
    mid <- paste0(prefix, "_metric")
    observeEvent(geo(), {
      ch <- metric_choices(geo(), drop); cur <- isolate(input[[mid]])
      updateSelectInput(session, mid, choices = ch, selected = if (isTRUE(cur %in% ch)) cur else ch[[1]])
    }, ignoreInit = TRUE)
    metric <- reactive({ m <- METRICS[[geo()]]; k <- input[[mid]]; if (!isTRUE(k %in% setdiff(names(m), drop))) k <- setdiff(names(m), drop)[1]; c(key = k, m[[k]]) })
    areas <- reactive(if (geo() == "vic") D$vic else if (st() == "ALL") dv()$lga else dv()$lga[state == st()])
    eligible <- reactive({ a <- areas(); k <- metric()$key; a[(k == "income" | elig) & !is.na(a[[k]])] })
    list(geo = geo, st = st, cust = cust, dv = dv, metric = metric, areas = areas, eligible = eligible)
  }
  mv <- view("map"); gv <- view("grp", drop = c("income", "add_c")); rv <- view("rank")

  # ---- area detail panel (Map and Rankings tabs each have their own)
  detail_server <- function(id, v, sel) {
    selected <- reactive({ s <- sel(); if (is.null(s)) NULL else { a <- v$areas()[id == s]; if (nrow(a)) a[1] else NULL } })
    output[[paste0(id, "_head")]] <- renderUI({
      a <- selected()
      if (is.null(a)) return(tagList(div(class = "sel-name", if (v$geo() == "lga") sprintf("All %s council areas%s", if (v$st() == "ALL") "NSW and QLD" else v$st(),
                                                                                           if (v$cust() == "private") " — private buyers" else "") else "All VIC postcodes"),
                                     div(class = "sel-meta", "No area selected: showing state averages.")))
      k <- function(l, x) div(class = "kpi", tags$b(x), span(l))
      ks <- if (v$geo() == "lga") list(k(sprintf("BEV share, %s", RW), pct(a$share)), k(sprintf("Crisis %s (year earlier)", crisis_label), sprintf("%s (%s)", pct(a$share_c), pct(a$share_p))),
                                       if (a$state == "NSW") k("BEVs per 1,000 light vehicles", num1(a$per1000veh)) else k("BEVs seen per 1,000 earners", num1(a$per1000pop)))
            else list(k("BEVs per 1,000 vehicles", num1(a$per1000veh)), k("BEV share, recent-model", pct(a$rshare)), k("Added in crisis qtr (yr earlier)", sprintf("%s (%s)", int(a$add_c), int(a$add_p))))
      tagList(div(class = "sel-name", if (v$geo() == "vic") sprintf("%s — %s", a$name, a$id) else a$name,
                  span(class = "sel-clear", onclick = sprintf("Shiny.setInputValue('%s_clear', Math.random())", id), "Clear ×")),
              div(class = "sel-meta", sprintf("%s · median income %s · income group Q%d of %d%s", a$state, usd(a$income), a$group, NG, if (a$elig) "" else " · small area, treat with care")),
              div(class = "kpis", ks))
    })
    output[[paste0(id, "_line_title")]] <- renderText(if (v$geo() == "lga") "BEV share of new registrations, monthly" else "BEVs per 1,000 registered vehicles, quarterly")
    output[[paste0(id, "_line_note")]] <- renderText({ a <- selected(); if (is.null(a)) "State averages. Shaded: fuel crisis." else sprintf("%s vs %s average. Shaded: fuel crisis.", a$name, a$state) })
    output[[paste0(id, "_line")]] <- renderPlotly({
      a <- selected(); p <- plot_ly()
      if (v$geo() == "lga") {
        x <- mdate(D$months); sm <- v$dv()$state_month
        if (is.null(a)) {
          for (s in if (v$st() == "ALL") c("NSW", "QLD") else v$st())
            p <- p |> add_lines(x = x, y = sm[state == s][match(D$months, month), share], name = s, line = list(color = STATE_COL[[s]], width = 2))
        } else {
          y <- unlist(v$dv()$lga_series[state == a$state & lga_name == a$name, -(1:2)])
          p <- p |> add_lines(x = x, y = y, name = a$name, line = list(color = STATE_COL[[a$state]], width = 2)) |>
            add_lines(x = x, y = sm[state == a$state][match(D$months, month), share], name = paste(a$state, "average"), line = list(color = COL$low, width = 2, dash = "dash"))
        }
        p |> theme_plot(yfmt = ".0%") |> layout(shapes = list(crisis_band(mdate(D$crisis$start), mdate(D$crisis$end))), hovermode = "x unified")
      } else {
        x <- qdate(D$quarters)
        p <- p |> add_lines(x = x, y = D$vic_q[match(D$quarters, quarter), per1000], name = "VIC average",
                            line = list(color = if (is.null(a)) COL$vic else COL$low, width = 2, dash = if (is.null(a)) "solid" else "dash"))
        if (!is.null(a)) p <- p |> add_lines(x = x, y = unlist(D$vic_series[postcode == as.integer(a$id), -1]), name = a$id, line = list(color = COL$vic, width = 2))
        p |> theme_plot(yfmt = ".0f") |> layout(shapes = list(crisis_band(mdate(D$crisis$start), qdate(D$crisis$vic_quarter))), hovermode = "x unified")
      }
    })
    observeEvent(input[[paste0(id, "_clear")]], sel(NULL))
  }

  # ---- Overview
  output$kpi_row <- renderUI({
    K <- D$kpi; G <- D$groups
    card <- function(l, v, s) div(class = "md-kpi-card", div(class = "md-kpi-label", l), div(class = "md-kpi-value", v), span(class = "md-kpi-sub", s))
    g5 <- G[state == "NSW" & group == NG, share]; g1 <- G[state == "NSW" & group == 1, share]
    div(class = "md-kpi-row",
        card(sprintf("New BEVs, %s", RW), int(K$NSW$bev + K$QLD$bev), sprintf("NSW %s · QLD %s", int(K$NSW$bev), int(K$QLD$bev))),
        card("BEV share of new cars", pct(K$NSW$share), sprintf("NSW · QLD %s", pct(K$QLD$share))),
        card("Richest vs poorest areas", sprintf("%.1f×", g5 / g1), "NSW BEV share, Q5 vs Q1"),
        card("BEVs in the fleet", int(K$NSW$fleet + K$VIC$fleet), sprintf("NSW %s · VIC %s", int(K$NSW$fleet), int(K$VIC$fleet))),
        card("Fuel crisis BEV share", sprintf("%s → %s", pct(K$NSW$share_p), pct(K$NSW$share_c)), sprintf("NSW, %s vs a year earlier", crisis_label)))
  })
  output$findings <- renderUI({
    G <- D$groups; g <- function(s, q, k) G[state == s & group == q][[k]]
    items <- list(
      c("Richer areas buy more BEVs. ", sprintf("In %s, counting every customer type, the richest fifth of NSW council areas registered BEVs at %s of new cars, vs %s in the poorest (%.1f×). In the fleet the gap is wider: %s vs %s BEVs per 1,000 light vehicles.",
                                               RW, pct(g("NSW", NG, "share")), pct(g("NSW", 1, "share")), g("NSW", NG, "share") / g("NSW", 1, "share"), num1(g("NSW", NG, "per1000veh")), num1(g("NSW", 1, "per1000veh")))),
      c("VIC postcodes show the same gradient. ", sprintf("%s BEVs per 1,000 vehicles in the top income group vs %s in the bottom.", num1(g("VIC", NG, "per1000veh")), num1(g("VIC", 1, "per1000veh")))),
      c("QLD is flatter at council level. ", sprintf("Its LGAs are large (Brisbane alone is about a quarter of QLD earners), and lower-income coastal retiree areas such as the Sunshine Coast take up BEVs strongly. Top vs bottom group: %s vs %s.",
                                                    pct(g("QLD", NG, "share")), pct(g("QLD", 1, "share")))),
      c("The fuel crisis narrowed the gap in relative terms. ", sprintf("Comparing %s with the same months a year earlier, BEV share rose %s in NSW's lowest-income group vs %s in the highest (QLD: %s vs %s). In percentage points richer areas still gained more (%s vs %s in NSW).",
                                                                        crisis_label, mult(g("NSW", 1, "mult")), mult(g("NSW", NG, "mult")), mult(g("QLD", 1, "mult")), mult(g("QLD", NG, "mult")),
                                                                        pp(g("NSW", NG, "chg")), pp(g("NSW", 1, "chg")))),
      c("Regional coastal areas moved most. ", "On the Map tab, colour areas by the crisis change: the biggest NSW jumps were in lower-income coastal LGAs such as Bellingen, Kiama, Byron, Eurobodalla and Ballina, alongside the wealthy North Shore."),
      local({ C <- D$cust$recent; cn <- function(t, k) C[state == "NSW" & customer == t][[k]]; pv <- D$views$private$groups
        c("Business buyers take up BEVs more slowly, and register them in a few councils. ",
          sprintf("In NSW, %s of new BEVs went to business, dealer and government buyers. Their BEV share was %s for businesses vs %s for private buyers, and a business's top 5 councils (%s) held %s of its BEVs but only %s of the state's earners. Counting private buyers only, the richest fifth of NSW areas reaches %s vs %s in the poorest (%.1f×) — see the Customer types tab and the Customers switch.",
                  pct(1 - cn(D$private_label, "of_bev")), pct(cn("Business", "share")), pct(cn(D$private_label, "share")), cn("Business", "top5"),
                  pct(cn("Business", "top5_bev")), pct(cn("Business", "top5_earners")),
                  pct(pv[state == "NSW" & group == NG, share]), pct(pv[state == "NSW" & group == 1, share]),
                  pv[state == "NSW" & group == NG, share] / pv[state == "NSW" & group == 1, share])) }),
      c("Caveat. ", "This compares areas, not people: it shows where BEVs are registered, not who bought them. Business, dealer and government vehicles are registered at the organisation's address, and novated leases and retirees' wealth also blur the link to income."))
    tags$ul(lapply(items, function(x) tags$li(tags$b(x[1]), x[2])))
  })

  # ---- Map tab
  map_sel <- reactiveVal(NULL)
  detail_server("map_sel", mv, map_sel)
  shapes <- reactive({
    g <- if (mv$geo() == "vic") D$geo_poa else D$geo_lga
    a <- mv$areas()
    g <- g[g$id %in% a$id, ]
    cbind(g, a[match(g$id, a$id)])
  })
  bins <- reactive({
    v <- sort(mv$eligible()[[mv$metric()$key]])
    if (!length(v)) return(NULL)
    unique(quantile(v, probs = seq(0, 1, length.out = length(SEQ) + 1), names = FALSE, type = 1))
  })
  colour_of <- function(d) {
    b <- bins(); k <- mv$metric()$key; v <- d[[k]]
    ok <- (k == "income" | d$elig) & !is.na(v)
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
  outputOptions(output, "map", suspendWhenHidden = FALSE)
  observe({
    d <- shapes(); m <- mv$metric()
    lab <- sprintf("<b>%s (%s)</b><br>%s: %s<br>Median income: %s<br>Income group: Q%d", htmlEscape(d$name), d$state, short(m$label),
                   ifelse((m$key == "income" | d$elig) & !is.na(d[[m$key]]), m$fmt(d[[m$key]]), "too few registrations"), usd(d$income), d$group)
    leafletProxy("map") |> clearGroup("areas") |>
      addPolygons(data = d, layerId = ~id, group = "areas", fillColor = colour_of(d), fillOpacity = 0.85, color = "#FFFFFF", weight = 0.6,
                  label = lapply(lab, HTML), highlightOptions = highlightOptions(weight = 2, color = COL$text, bringToFront = TRUE),
                  labelOptions = labelOptions(style = list("font-family" = "Inter", "font-size" = "12px")))
  })
  fit_view <- function() {
    p <- leafletProxy("map")
    if (mv$geo() == "vic") p |> fitBounds(144.3, -38.6, 145.6, -37.4)
    else if (mv$st() == "ALL") p |> fitBounds(112.9, -43.8, 153.8, -10.4)
    else { b <- st_bbox(shapes()); p |> fitBounds(b[["xmin"]], b[["ymin"]], b[["xmax"]], b[["ymax"]]) }
  }
  # the map is drawn while its tab is hidden, so fit it again the first time the tab opens
  map_seen <- FALSE
  observeEvent(input$tab, if (input$tab == "map" && !map_seen) { map_seen <<- TRUE; fit_view() })
  observeEvent(list(mv$geo(), mv$st()), { map_sel(NULL); fit_view() }, ignoreInit = TRUE)
  observeEvent(mv$cust(), { s <- map_sel(); if (!is.null(s) && !nrow(mv$areas()[id == s])) map_sel(NULL) }, ignoreInit = TRUE)
  observeEvent(input$map_reset, { map_sel(NULL); fit_view() })
  observeEvent(input$map_shape_click, map_sel(input$map_shape_click$id))
  observeEvent(event_data("plotly_click", source = "sc"), { e <- event_data("plotly_click", source = "sc"); if (!is.null(e$customdata)) map_sel(e$customdata) })
  observe({
    s <- map_sel(); p <- leafletProxy("map") |> clearGroup("sel")
    if (!is.null(s)) { g <- shapes(); g <- g[g$id == s, ]; if (nrow(g)) p |> addPolylines(data = g, group = "sel", color = COL$text, weight = 2.5) }
  })
  output$map_title <- renderText(paste0(mv$metric()$label, if (mv$geo() != "vic" && mv$cust() == "private") " — private buyers only" else ""))
  output$map_note <- renderText(if (mv$geo() == "lga") "Council areas (LGAs) in NSW and QLD; other states in outline. Colour bins are sevenths of the areas shown. Grey = fewer than the minimum new registrations."
                                else "Postcodes, named by their ABS suburbs. Opens on Greater Melbourne — zoom out for regional Victoria. Colour bins are sevenths of postcodes.")
  output$legend <- renderUI({
    b <- bins(); if (is.null(b)) return(NULL); f <- mv$metric()$fmt
    div(class = "legend-row", lapply(seq_len(length(b) - 1), function(i)
      div(class = "sw", tags$i(style = sprintf("background:%s", SEQ[i])), span(if (i < length(b) - 1) paste("≤", f(b[i + 1])) else paste(">", f(b[i]))))),
      div(style = "margin-left:12px;display:flex;gap:6px;align-items:center", tags$i(style = sprintf("width:14px;height:10px;background:%s;display:inline-block", COL$na)),
          "Too few registrations / no data"))
  })
  # scatter: income vs the map metric (vs BEV take-up when the map shows income itself)
  sc_key <- reactive({ k <- mv$metric()$key; if (k == "income") (if (mv$geo() == "lga") "share" else "per1000veh") else k })
  output$sc_title <- renderText({ l <- METRICS[[mv$geo()]][[sc_key()]]$label; paste("Median income vs", if (startsWith(l, "BEV")) l else sub("^(.)", "\\L\\1", l, perl = TRUE)) })
  output$scatter <- renderPlotly({
    key <- sc_key(); fmt <- METRICS[[mv$geo()]][[key]]$fmt
    a <- mv$areas()[elig == TRUE & !is.na(get(key))]
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
    if (!is.null(map_sel())) { d <- a[id == map_sel()]; if (nrow(d)) p <- p |> add_markers(x = d$income, y = d[[key]], name = "Selected", showlegend = FALSE, hoverinfo = "skip",
                                                                                  marker = list(size = 13, color = "rgba(0,0,0,0)", line = list(color = COL$text, width = 2.5))) }
    p |> theme_plot(yfmt = axis_of(fmt), xtitle = "Area median income") |> layout(xaxis = list(tickprefix = "$", tickformat = ",.0f"), showlegend = FALSE) |> event_register("plotly_click")
  })
  outputOptions(output, "scatter", suspendWhenHidden = FALSE)  # render it while the Map tab is hidden, so its click event is registered

  # ---- Income groups tab
  bars <- function(series, fmt_axis, hover_fmt) {
    p <- plot_ly()
    for (s in series) p <- p |> add_bars(x = QLAB, y = s$vals, name = s$name, marker = list(color = s$colour),
                                         text = hover_fmt(s$vals), hoverinfo = "text+name", textposition = "none")
    p |> theme_plot(yfmt = fmt_axis) |> layout(barmode = "group", bargap = 0.3, xaxis = list(categoryorder = "array", categoryarray = QLAB))
  }
  grp_col <- c(fleet_bev = "fleet", add_c = "add_c1000")   # area column -> income-group column
  output$grp_title <- renderText(paste0(gv$metric()$label, " — by income group", if (gv$geo() != "vic" && gv$cust() == "private") ", private buyers" else ""))
  output$grp_bars <- renderPlotly({
    m <- gv$metric(); key <- grp_col[m$key] %|NA|% m$key
    sts <- if (gv$geo() == "vic") "VIC" else c("NSW", "QLD")
    G <- gv$dv()$groups
    bars(lapply(sts, function(s) list(name = s, colour = STATE_COL[[s]], vals = G[state == s][order(group)][[key]] %||% rep(NA, NG))), axis_of(m$fmt), m$fmt)
  })
  output$grp_method <- renderUI({
    M <- D$group_method
    steps <- tags$ol(lapply(M$steps, function(t) tags$li(tags$b(sub("\\. .*$", ".", t)), " ", sub("^[^.]*\\. ", "", t))))
    tbl <- function(st) {
      g <- M$comp[state == st][order(group)]; who <- if (st == "VIC") "individuals" else "earners"
      tagList(h2(style = "margin-top:14px", sprintf("%s — %s", st, if (st == "VIC") "postcodes" else "council areas (LGAs)")),
              tags$table(class = "md", tags$thead(tags$tr(tags$th("Group"), tags$th(class = "num", "Areas"), tags$th(class = "num", tools::toTitleCase(who)),
                                                          tags$th(class = "num", paste("Share of", who)), tags$th(class = "num", "Median income range"),
                                                          tags$th("Largest areas"))),
                         tags$tbody(lapply(seq_len(nrow(g)), function(i) tags$tr(
                           tags$td(QLAB[g$group[i]]), tags$td(class = "num", int(g$areas[i])), tags$td(class = "num", int(g$people[i])),
                           tags$td(class = "num", pct(g$share[i])), tags$td(class = "num", paste(usd(g$inc_lo[i]), "–", usd(g$inc_hi[i]))),
                           tags$td(g$largest[i]))))),
              p(class = "note", style = "margin-top:6px", M$lumpy[[st]]))
    }
    tagList(steps, lapply(if (gv$geo() == "vic") "VIC" else c("NSW", "QLD"), tbl))
  })
  output$fleet_bars <- renderPlotly(bars(list(list(name = "NSW (per 1,000 light vehicles)", colour = COL$nsw, vals = D$groups[state == "NSW"][order(group), per1000veh]),
                                              list(name = "VIC (per 1,000 vehicles)", colour = COL$vic, vals = D$groups[state == "VIC"][order(group), per1000veh])), ",.0f", num1))

  # ---- Fuel crisis tab
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

  # ---- Registrations tab
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

  # ---- Customer types tab
  ct_st <- reactive(input$ct_st %||% "NSW")
  ct_types <- reactive(intersect(D$cust$order, D$cust$recent[state == ct_st(), customer]))
  output$ct_note <- renderText({ r <- D$cust$recent[state == ct_st()]
    sprintf("%s of %s's new BEVs were bought by customers other than private buyers.", pct(1 - r[customer == D$private_label, of_bev]), ct_st()) })
  output$ct_tbl <- renderUI({
    r <- D$cust$recent[state == ct_st()]
    tags$table(class = "md", tags$thead(tags$tr(tags$th("Customer type"), tags$th(class = "num", "New regos"), tags$th(class = "num", "BEVs"),
                                                tags$th(class = "num", "BEV share"), tags$th(class = "num", "Share of BEVs"),
                                                tags$th(class = "num", sprintf("Crisis %s (year earlier)", crisis_label)))),
      tags$tbody(lapply(seq_len(nrow(r)), function(i) tags$tr(tags$td(r$customer[i]), tags$td(class = "num", int(r$new[i])), tags$td(class = "num", int(r$bev[i])),
        tags$td(class = "num", pct(r$share[i])), tags$td(class = "num", pct(r$of_bev[i])),
        tags$td(class = "num", sprintf("%s (%s)", pct(r$share_c[i]), pct(r$share_p[i])))))))
  })
  output$ct_conc <- renderUI({
    r <- D$cust$recent[state == ct_st()]
    tags$table(class = "md", tags$thead(tags$tr(tags$th("Customer type"), tags$th("Top 5 councils by BEVs"), tags$th(class = "num", "Their share of the BEVs"),
                                                tags$th(class = "num", "Their share of earners"))),
      tags$tbody(lapply(seq_len(nrow(r)), function(i) tags$tr(tags$td(r$customer[i]), tags$td(r$top5[i]),
        tags$td(class = "num", pct(r$top5_bev[i])), tags$td(class = "num", pct(r$top5_earners[i]))))))
  })
  ct_band <- function() list(crisis_band(mdate(D$crisis$start), mdate(D$crisis$end)))
  output$ct_share <- renderPlotly({
    m <- D$cust$month[state == ct_st()]; p <- plot_ly()
    for (t in ct_types()) { d <- m[customer == t]; p <- p |> add_lines(x = mdate(d$month), y = d$share, name = t, line = list(color = CT_COL[[t]], width = 2)) }
    p |> theme_plot(yfmt = ".0%") |> layout(shapes = ct_band(), hovermode = "x unified")
  })
  output$ct_count <- renderPlotly({
    m <- D$cust$month[state == ct_st()]; p <- plot_ly()
    for (t in ct_types()) { d <- m[customer == t]; p <- p |> add_bars(x = mdate(d$month), y = d$bev, name = t, marker = list(color = CT_COL[[t]])) }
    p |> theme_plot(yfmt = ",.0f") |> layout(barmode = "stack", bargap = 0.15, hovermode = "x unified",
                                             shapes = list(crisis_band(mdate(D$crisis$start) - 15, mdate(D$crisis$end) + 15)))
  })
  output$ct_grp <- renderPlotly({
    g <- D$cust$group[state == ct_st()]
    bars(lapply(ct_types(), function(t) list(name = t, colour = CT_COL[[t]], vals = g[customer == t][order(group), share])), ".0%", pct)
  })
  output$ct_notpriv <- renderPlotly({
    g <- D$cust$group[state == ct_st()]
    v <- g[, .(x = 1 - sum(bev[customer == D$private_label]) / sum(bev)), keyby = group]$x
    bars(list(list(name = "Not private", colour = STATE_COL[[ct_st()]], vals = v)), ".0%", pct) |> layout(showlegend = FALSE)
  })
  output$ct_top <- renderUI({
    t <- D$cust$top[state == ct_st()][order(-bev)][seq_len(min(12, .N))]
    tags$table(class = "md", tags$thead(tags$tr(tags$th("#"), tags$th("Council"), tags$th("Customer type"), tags$th(class = "num", "New BEVs"),
                                                tags$th(class = "num", "Median income"), tags$th(class = "num", "Income group"))),
      tags$tbody(lapply(seq_len(nrow(t)), function(i) tags$tr(tags$td(i), tags$td(t$lga_name[i]), tags$td(t$customer[i]), tags$td(class = "num", int(t$bev[i])),
        tags$td(class = "num", usd(t$income[i])), tags$td(class = "num", sprintf("Q%d", t$group[i]))))))
  })

  # ---- Rankings tab: clicking a row fills the detail panel beside it, nothing else
  rank_sel <- reactiveVal(NULL)
  detail_server("rank_sel", rv, rank_sel)
  observeEvent(list(rv$geo(), rv$st()), rank_sel(NULL), ignoreInit = TRUE)
  observeEvent(input$rank_pick, rank_sel(if (identical(input$rank_pick, rank_sel())) NULL else input$rank_pick))
  rank_ord <- reactive(input$rank_ord %||% "top")
  output$rank_title <- renderText(sprintf("%s: %s%s", if (rank_ord() == "top") "Highest 10" else "Lowest 10", rv$metric()$label,
                                           if (rv$geo() != "vic" && rv$cust() == "private") " — private buyers" else ""))
  output$rank_note <- renderText(if (rv$metric()$key == "income") "All areas." else "Areas with too few registrations are left out.")
  output$rank_tbl <- renderUI({
    e <- rv$eligible(); k <- rv$metric()$key; fmt <- rv$metric()$fmt; s <- rank_sel()
    rows <- e[order(if (rank_ord() == "top") -get(k) else get(k))][seq_len(min(10, .N))]
    if (!nrow(rows)) return(p(class = "note", "No areas meet the size threshold for this metric."))
    tags$table(class = "md pick", tags$thead(tags$tr(tags$th("#"), tags$th("Area"), tags$th(class = "num", "Median income"), tags$th(class = "num", short(rv$metric()$label)))),
      tags$tbody(lapply(seq_len(nrow(rows)), function(i) { a <- rows[i]
        tags$tr(class = if (identical(a$id, s)) "on", onclick = sprintf("Shiny.setInputValue('rank_pick', '%s', {priority: 'event'})", a$id),
                tags$td(i), tags$td(sprintf("%s (%s)", a$name, if (rv$geo() == "vic") a$id else a$state)), tags$td(class = "num", usd(a$income)),
                tags$td(class = "num", fmt(a[[k]]))) })))
  })

  # ---- downloads (always both geographies, so they don't depend on any tab's settings)
  about <- data.frame(Item = c("Exported from", "Recent window", "Fuel crisis window", "Year-earlier comparison", "Sources and adjustments"),
                      Value = c("EV × Income dashboard", RW, paste(D$crisis$start, "to", D$crisis$end),
                                paste(D$crisis$py_start, "to", D$crisis$py_end), "See the full workbook (Sources, Data_Adjustments, Notes sheets)"))
  cols <- list(
    lga = c(state = "State", name = "LGA", id = "LGA code", income = "Median total income 2022-23 ($)", pop = "Earners", group = "Income group (1 = lowest)",
            new = paste("New regos (all customer types),", RW), bev = paste("BEV,", RW), share = paste("BEV share,", RW), p_new = "New regos, year before crisis",
            p_bev = "BEV, year before crisis", share_p = "BEV share, year before crisis", c_new = "New regos, crisis months", c_bev = "BEV, crisis months",
            share_c = "BEV share, crisis months", chg = "Change (pp)", mult = "Crisis ÷ year earlier", fleet_bev = "BEV fleet (NSW est.; QLD BEVs seen since 2022)",
            fleet_veh = "Light-vehicle fleet (NSW)", per1000veh = "BEV per 1,000 light vehicles (NSW)", per1000pop = "BEV fleet per 1,000 earners", elig = "Above size threshold"),
    vic = c(id = "Postcode", name = "Suburbs", income = "Median taxable income 2023-24 ($)", pop = "Individuals", group = "Income group (1 = lowest)",
            vehicles = "Vehicles (latest)", bev = "BEV (latest)", per1000veh = "BEV per 1,000 vehicles", recent_vehicles = "Recent-model vehicles",
            recent_bev = "Recent-model BEV", rshare = "BEV share of recent-model", add_c = "BEV added, crisis quarter", add_c1000 = "Added per 1,000 vehicles, crisis quarter",
            add_p = "BEV added, same quarter a year earlier", add_p1000 = "Added per 1,000, year earlier", elig = "Above size threshold"))
  area_table <- function(g) { a <- D[[g]]; cl <- cols[[g]]; x <- as.data.frame(a[, names(cl), with = FALSE]); names(x) <- cl; x }
  output$dl_workbook <- downloadHandler(filename = function() basename(WORKBOOK), content = function(f) file.copy(WORKBOOK, f))
  output$dl_areas <- downloadHandler(filename = "EV_by_income_all_areas.xlsx",
                                     content = function(f) write_xlsx(list(`NSW + QLD councils` = area_table("lga"), `VIC postcodes` = area_table("vic"), About = about), f))
  output$dl_top <- downloadHandler(filename = "EV_top_and_bottom_10.xlsx", content = function(f) {
    sheets <- list()
    for (g in c("lga", "vic")) for (k in names(METRICS[[g]])) {
      a <- D[[g]][(k == "income" | elig) & !is.na(get(k))]
      mk <- function(d) data.frame(Rank = seq_len(nrow(d)), Area = d$name, Code = d$id, State = d$state, `Median income ($)` = d$income, Value = d[[k]], check.names = FALSE) |>
        setNames(c("Rank", "Area", "Code", "State", "Median income ($)", METRICS[[g]][[k]]$label))
      tag <- if (g == "lga") "" else "VIC "
      sheets[[paste0(tag, "Top ", k)]] <- mk(a[order(-get(k))][seq_len(min(10, .N))])
      sheets[[paste0(tag, "Bottom ", k)]] <- mk(a[order(get(k))][seq_len(min(10, .N))])
    }
    write_xlsx(c(sheets, list(About = about)), f)
  })
  group_method_sheet <- function() {
    M <- D$group_method; c_ <- M$comp[order(state, group)]
    rbind(data.frame(Section = "Method", State = "", Group = "", Detail = M$steps),
          data.frame(Section = "Group make-up", State = c_$state, Group = QLAB[c_$group],
                     Detail = sprintf("%s areas; %s people (%s of state); median income %s–%s; largest: %s",
                                      int(c_$areas), int(c_$people), pct(c_$share), usd(c_$inc_lo), usd(c_$inc_hi), c_$largest)),
          data.frame(Section = "Why sizes differ", State = names(M$lumpy), Group = "", Detail = unname(M$lumpy)))
  }
  output$dl_groups <- downloadHandler(filename = "EV_income_groups.xlsx", content = function(f) {
    g <- copy(D$groups)[, `Income group` := paste0("Q", group)]
    write_xlsx(c(lapply(split(as.data.frame(g), g$state), function(x) x[, c("Income group", setdiff(names(x), c("Income group", "group", "state")))]),
                 list(`Customer types` = as.data.frame(D$cust$recent), `Customer type x income group` = as.data.frame(D$cust$group),
                      `How groups are built` = group_method_sheet(), About = about)), f)
  })
  output$dl_series <- downloadHandler(filename = "EV_series_and_fuel_prices.xlsx", content = function(f) {
    write_xlsx(list(`NSW + QLD monthly BEV share` = as.data.frame(D$lga_series), `VIC quarterly BEV per 1000` = as.data.frame(D$vic_series),
                    `State totals` = as.data.frame(D$state_month), `Fuel prices (c per L)` = as.data.frame(D$fuel), About = about), f)
  })
  # the links sit in a closed dropdown, and Shiny doesn't wire up hidden download links
  for (id in c("dl_workbook", "dl_areas", "dl_top", "dl_groups", "dl_series")) outputOptions(output, id, suspendWhenHidden = FALSE)
}

`%||%` <- function(a, b) if (is.null(a)) b else a
`%|NA|%` <- function(a, b) if (is.null(a) || is.na(a)) b else unname(a)
axis_of <- function(f) if (identical(f, pct)) ".0%" else if (identical(f, pp)) "+.1f" else if (identical(f, usd)) "$,.0f" else if (identical(f, int)) ",.0f" else ",.1f"
shinyApp(ui, server)
