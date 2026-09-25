# Write EV_uptake_by_income.xlsx from the processed tables.
#
# Data sheets hold counts as values; every share, ratio, group total and
# imputed count is an Excel formula, so changing an input on the Inputs sheet
# (e.g. the value given to a suppressed NSW "<=5" cell) flows through every
# table and chart. Charts are native Excel charts linked to the tables.
#
# Run:  Rscript R/build_workbook.R   (after build_data.R and fetch_boundaries.R)
# common.R sits next to this script: found via Rscript's --file, or source()'s ofile (RStudio's Source button)
source(file.path(local({ f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  if (!length(f)) f <- sys.frames()[[1]]$ofile
  if (length(f)) dirname(normalizePath(f)) else "R" }), "common.R"))
source(file.path(HERE, "R", "xlsx_charts.R"))
suppressPackageStartupMessages(library(openxlsx2))

NG <- CFG$income$n_groups
GROUPS <- seq_len(NG)
GROUP_LABEL <- paste0("Q", GROUPS, ifelse(GROUPS == 1, " (lowest)", ifelse(GROUPS == NG, " (highest)", "")))
L <- function(i) int2col(i)
FML <- function(v) { v <- sub("^=", "", v); class(v) <- c(class(v), "formula"); v }

# GARY / NELLY palette
PRIMARY <- "1F7AE0"; TEXT <- "1A1D21"; MUTED <- "6B7280"; BAND <- "F5F6F8"; OTHER <- "C8CDD3"
STATE_COL <- c(NSW = "1976D2", QLD = "F57C00", VIC = "00897B")
GROUP_COL <- c("90CAF9", "5AA2EE", "1F7AE0", "1565C0", "0D47A1")
PCT <- "0.0%"; NUM <- "#,##0"; DEC <- "0.00"; MON <- "mmm-yy"; USD <- "$#,##0"; ONE <- "0.0"

# ---- data -------------------------------------------------------------------
rd <- function(f) fread(file.path(OUT, f))
lga <- rd("lga_income.csv"); pcode <- rd("postcode_income.csv"); flow <- rd("flow_lga_month.csv")
snsw <- rd("stock_nsw_lga.csv"); sqld <- rd("stock_qld_proxy_lga.csv"); vic <- rd("vic_postcode_quarter.csv")
supp <- rd("suppression.csv"); fuel <- rd("fuel_prices.csv"); onset <- rd("fuel_crisis_onset.csv")
adjust <- rd("adjustments.csv"); suburbs <- rd("vic_postcode_suburbs.csv")
for (k in names(CFG$map$postcode_name_overrides)) suburbs[postcode == as.integer(k), suburbs := CFG$map$postcode_name_overrides[[k]]]
ym_date <- function(x) as.Date(paste0(x, "-01"))
grp <- setNames(lga$income_group, paste(lga$state, lga$lga_name))
flow[, `:=`(mdate = ym_date(month), group = grp[paste(state, lga_name)])]
snsw[, `:=`(mdate = ym_date(month), group = grp[paste("NSW", lga_name)])]
sqld[, `:=`(mdate = ym_date(month), group = grp[paste("QLD", lga_name)])]
vic[, qdate := as.Date(sprintf("%s-%02d-01", substr(quarter, 1, 4), as.integer(substr(quarter, 6, 6)) * 3L))]
vic[, group := pcode$income_group[match(postcode, pcode$postcode)]]
q_months <- sort(unique(sqld$month))
sqld_q <- sqld[month %in% unique(c(q_months[as.integer(substr(q_months, 6, 7)) %in% CFG$nsw$snapshot_months_of_year], max(q_months)))]
kv <- function(t, f, cl = if (t == "stock") "all" else "private") supp[table == t & fuel_group == f & customers == cl]
WIN <- analysis_windows(flow)

# ---- workbook + helpers -------------------------------------------------------
SHEETS <- c("Charts", "Summary", "Raw_Numbers", "Top10", "Fuel_Crisis", "Customer_Types", "Inputs", "Income_Groups", "Flow_Group_Summary", "Flow_Group_Month",
            "Stock_Group", "LGA_Summary", "VIC_Postcode", "Suppression", "Sources", "Data_Adjustments", "Notes",
            "Flow_LGA_Month", "Stock_NSW_LGA", "Stock_QLD_LGA", "VIC_Postcode_Qtr", "Fuel_Prices", "Rank_Keys")
wb <- wb_workbook(creator = "EV_income")
wb$set_base_font(font_size = 10, font_name = "Arial")
for (s in SHEETS) wb$add_worksheet(s, grid_lines = !(s %in% c("Charts", "Summary")))

put <- function(sheet, x, row, col = 1) {
  if (is.atomic(x) && !inherits(x, "formula")) x <- as.data.frame(as.list(x), stringsAsFactors = FALSE)
  if (inherits(x, "formula")) x <- data.frame(v = x)
  wb$add_data(sheet = sheet, x = x, start_row = row, start_col = col, col_names = FALSE, na.strings = "")
}
# a vertical vector (values or formulas) starting at (row, col)
putv <- function(sheet, v, row, col) {
  df <- data.frame(v = seq_along(v)); df$v <- v
  wb$add_data(sheet = sheet, x = df, start_row = row, start_col = col, col_names = FALSE, na.strings = "")
}
dims <- function(r1, c1 = 1, r2 = r1, c2 = c1) sprintf("%s%d:%s%d", L(c1), r1, L(c2), r2)
nf <- function(sheet, d, f) wb$add_numfmt(sheet = sheet, dims = d, numfmt = f)
bold <- function(sheet, d, col = TEXT, size = 10) wb$add_font(sheet = sheet, dims = d, bold = TRUE, name = "Arial", size = size, color = wb_color(col))
link <- function(sheet, d) wb$add_font(sheet = sheet, dims = d, name = "Arial", size = 10, color = wb_color("008000"))
head <- function(sheet, row, labels, col = 1) {
  put(sheet, labels, row, col)
  d <- dims(row, col, row, col + length(labels) - 1)
  wb$add_fill(sheet = sheet, dims = d, color = wb_color(PRIMARY))
  wb$add_font(sheet = sheet, dims = d, bold = TRUE, name = "Arial", size = 10, color = wb_color("FFFFFF"))
  wb$add_cell_style(sheet = sheet, dims = d, wrap_text = TRUE, horizontal = "center", vertical = "center")
  wb$set_row_heights(sheet = sheet, rows = row, heights = 42)
}
title <- function(sheet, text, sub = NULL) {
  put(sheet, text, 1); bold(sheet, "A1", size = 14)
  if (!is.null(sub)) { put(sheet, sub, 2); wb$add_font(sheet = sheet, dims = "A2", italic = TRUE, name = "Arial", size = 10, color = wb_color(MUTED)) }
}
widths <- function(sheet, w) wb$set_col_widths(sheet = sheet, cols = seq_along(w), widths = w)
rows_of <- function(n, r0) r0 + seq_len(n) - 1L

# ============================================================== Inputs ======
s <- "Inputs"
title(s, "Inputs and assumptions",
      "Blue text on yellow = assumption you can change. Black = formula. Everything else in the workbook recalculates from these.")
head(s, 4, c("Item", "Value", "How it was set"))
inputs <- data.table(
  item = c("NSW new regos, private buyers: value of a suppressed '<=5' cell — BEV", "NSW new regos, private buyers: value of a suppressed '<=5' cell — PHEV",
           "NSW new regos, private buyers: value of a suppressed '<=5' cell — all other fuels", "NSW fleet: value of a suppressed '<=5' cell — BEV",
           "NSW fleet: value of a suppressed '<=5' cell — PHEV", "NSW fleet: value of a suppressed '<=5' cell — all other fuels",
           "Recent window length (months)", "Minimum new regos in window for an LGA to appear on the scatter",
           "Minimum recent-model vehicles for a VIC postcode to appear on the scatter"),
  value = c(kv("flow", "bev")$k, kv("flow", "phev")$k, kv("flow", "other")$k, kv("stock", "bev")$k, kv("stock", "phev")$k,
            kv("stock", "other")$k, CFG$period$recent_window_months, CFG$analysis$min_new_regs_scatter, CFG$analysis$min_recent_vehicles_vic),
  how = c(kv("flow", "bev")$method, kv("flow", "phev")$method, kv("flow", "other")$method, kv("stock", "bev")$method,
          kv("stock", "phev")$method, kv("stock", "other")$method, sprintf("config.yaml period.recent_window_months. Built as %s (baseline %s); titles and headers show these months. If you change the length, B18–B21 give the new dates.", WIN$recent, WIN$base),
          "config.yaml analysis.min_new_regs_scatter", "config.yaml analysis.min_recent_vehicles_vic"))
put(s, inputs, 5)
wb$add_fill(sheet = s, dims = "B5:B13", color = wb_color("FFFF00"))
wb$add_font(sheet = s, dims = "B5:B13", name = "Arial", size = 10, color = wb_color("0000FF"))
nf(s, "B5:B10", DEC)
IN <- list(kf_bev = "Inputs!$B$5", kf_phev = "Inputs!$B$6", kf_other = "Inputs!$B$7", ks_bev = "Inputs!$B$8",
           ks_phev = "Inputs!$B$9", ks_other = "Inputs!$B$10", window = "Inputs!$B$11", min_lga = "Inputs!$B$12", min_vic = "Inputs!$B$13")
head(s, 15, c("Derived date", "Value", "Formula"))
derived <- c(
  "Latest month of new-registration data — NSW" = '_xlfn.MAXIFS(Flow_LGA_Month!$C:$C,Flow_LGA_Month!$A:$A,"NSW")',
  "Latest month of new-registration data — QLD" = '_xlfn.MAXIFS(Flow_LGA_Month!$C:$C,Flow_LGA_Month!$A:$A,"QLD")',
  "Recent window start — NSW" = sprintf("EDATE(B16,-(%s-1))", IN$window),
  "Recent window start — QLD" = sprintf("EDATE(B17,-(%s-1))", IN$window),
  "First month of new-registration data — both states" = "MIN(Flow_LGA_Month!$C:$C)",
  "Baseline window end (first window of the same length)" = sprintf("EDATE(B20,%s-1)", IN$window),
  "Latest NSW fleet snapshot" = "MAX(Stock_NSW_LGA!$B:$B)",
  "Latest QLD BEV-seen month" = "MAX(Stock_QLD_LGA!$B:$B)",
  "Latest VIC fleet snapshot (quarter)" = "MAX(VIC_Postcode_Qtr!$B:$B)",
  "VIC snapshot one year earlier" = "EDATE(B24,-12)")
putv(s, names(derived), 16, 1); putv(s, FML(unname(derived)), 16, 2); putv(s, unname(derived), 16, 3); nf(s, "B16:B25", MON)
D <- list(nsw_last = "Inputs!$B$16", qld_last = "Inputs!$B$17", nsw_start = "Inputs!$B$18", qld_start = "Inputs!$B$19",
          first = "Inputs!$B$20", base_end = "Inputs!$B$21", nsw_stock = "Inputs!$B$22", qld_stock = "Inputs!$B$23",
          vic_last = "Inputs!$B$24", vic_prev = "Inputs!$B$25")
head(s, 27, c("Fuel crisis window", "Value", "How it was set"))
put(s, "Crisis start month (2026 Middle East / Strait of Hormuz fuel crisis)", 28, 1)
put(s, data.frame(d = ym_date(onset$onset_month)), 28, 2)
put(s, sprintf("Derived: first month %s was >= %d%% above its trailing 12-month mean (%.1f vs %.1f c/L). config.yaml fuel.*",
               onset$onset_series, round(100 * onset$threshold), onset$onset_price, onset$trailing_mean), 28, 3)
wb$add_fill(sheet = s, dims = "B28", color = wb_color("FFFF00"))
wb$add_font(sheet = s, dims = "B28", name = "Arial", size = 10, color = wb_color("0000FF"))
crisis <- c("Crisis window end (latest month with data in both states)" = sprintf("MIN(%s,%s)", D$nsw_last, D$qld_last),
            "Same months a year earlier — start" = "EDATE(B28,-12)", "Same months a year earlier — end" = "EDATE(B29,-12)",
            "Crisis window length (months)" = "(YEAR(B29)-YEAR(B28))*12+MONTH(B29)-MONTH(B28)+1",
            "Pre-crisis window of the same length — start" = "EDATE(B28,-B32)", "Pre-crisis window — end" = "EDATE(B28,-1)")
putv(s, names(crisis), 29, 1); putv(s, FML(unname(crisis)), 29, 2); putv(s, unname(crisis), 29, 3)
nf(s, "B28:B31", MON); nf(s, "B32", "0"); nf(s, "B33:B34", MON)
D <- c(D, list(cr_start = "Inputs!$B$28", cr_end = "Inputs!$B$29", py_start = "Inputs!$B$30", py_end = "Inputs!$B$31",
               pre_start = "Inputs!$B$33", pre_end = "Inputs!$B$34"))
head(s, 36, c("Suppressed cells — business, dealer and government buyers", "Value", "How it was set"))
oth <- data.table(item = c("NSW new regos, business/dealer/government: value of a suppressed '<=5' cell — BEV",
                           "NSW new regos, business/dealer/government: value of a suppressed '<=5' cell — PHEV",
                           "NSW new regos, business/dealer/government: value of a suppressed '<=5' cell — all other fuels"),
                  value = c(kv("flow", "bev", "other")$k, kv("flow", "phev", "other")$k, kv("flow", "other", "other")$k),
                  how = c(kv("flow", "bev", "other")$method, kv("flow", "phev", "other")$method, kv("flow", "other", "other")$method))
put(s, oth, 37)
wb$add_fill(sheet = s, dims = "B37:B39", color = wb_color("FFFF00"))
wb$add_font(sheet = s, dims = "B37:B39", name = "Arial", size = 10, color = wb_color("0000FF"))
nf(s, "B37:B39", DEC)
IN <- c(IN, list(kfo_bev = "Inputs!$B$37", kfo_phev = "Inputs!$B$38", kfo_other = "Inputs!$B$39"))
widths(s, c(70, 14, 110))

# ======================================================= data sheets ========
s <- "Flow_LGA_Month"
setorder(flow, state, lga_name, month)
head(s, 1, c("State", "LGA", "Month", "Income group", "BEV exact", "BEV suppressed cells", "PHEV exact", "PHEV suppressed cells",
             "Other exact", "Other suppressed cells", "BEV (est.)", "PHEV (est.)", "Other (est.)", "New regos (est.)", "Customer type"))
r <- rows_of(nrow(flow), 2)
x <- flow[, .(state, lga_name, mdate, group, bev_exact, bev_supp, phev_exact, phev_supp, other_exact, other_supp)]
# a suppressed cell's value depends on the customer class: private rows are cut finer (gender x age)
kk <- function(priv, oth) sprintf('IF($O%d="%s",%s,%s)', r, PRIVATE, priv, oth)
x[, k := FML(sprintf("E%d+%s*F%d", r, kk(IN$kf_bev, IN$kfo_bev), r))]; x[, l := FML(sprintf("G%d+%s*H%d", r, kk(IN$kf_phev, IN$kfo_phev), r))]
x[, m := FML(sprintf("I%d+%s*J%d", r, kk(IN$kf_other, IN$kfo_other), r))]; x[, n := FML(sprintf("K%d+L%d+M%d", r, r, r))]
x[, o := flow$customer]
put(s, x, 2); NF <- max(r)
nf(s, dims(2, 3, NF), MON); nf(s, dims(2, 11, NF, 14), "#,##0.0")
widths(s, c(7, 26, 10, 9, rep(11, 10), 20)); wb$freeze_pane(sheet = s, first_row = TRUE)

s <- "Stock_NSW_LGA"
setorder(snsw, lga_name, month)
head(s, 1, c("LGA", "Month", "Income group", "BEV exact", "BEV suppressed cells", "PHEV exact", "PHEV suppressed cells",
             "Other exact", "Other suppressed cells", "BEV (est.)", "PHEV (est.)", "Other (est.)", "Light vehicles (est.)"))
r <- rows_of(nrow(snsw), 2)
x <- snsw[, .(lga_name, mdate, group, bev_exact, bev_supp, phev_exact, phev_supp, other_exact, other_supp)]
x[, j := FML(sprintf("D%d+%s*E%d", r, IN$ks_bev, r))]; x[, k := FML(sprintf("F%d+%s*G%d", r, IN$ks_phev, r))]
x[, l := FML(sprintf("H%d+%s*I%d", r, IN$ks_other, r))]; x[, m := FML(sprintf("J%d+K%d+L%d", r, r, r))]
put(s, x, 2)
nf(s, dims(2, 2, max(r)), MON); nf(s, dims(2, 10, max(r), 13), NUM)
widths(s, c(26, 10, 9, rep(11, 10))); wb$freeze_pane(sheet = s, first_row = TRUE)

s <- "Stock_QLD_LGA"
setorder(sqld, lga_name, month)
head(s, 1, c("LGA", "Month", "Income group", "BEVs seen since Jan 2022, at last known LGA"))
put(s, sqld[, .(lga_name, mdate, group, bev_seen)], 2)
nf(s, dims(2, 2, nrow(sqld) + 1), MON); widths(s, c(26, 10, 9, 22)); wb$freeze_pane(sheet = s, first_row = TRUE)

s <- "VIC_Postcode_Qtr"
setorder(vic, postcode, qdate)
head(s, 1, c("Postcode", "Quarter (last month)", "Income group", "Vehicles", "BEV", "Hybrid (HEV+PHEV)", "Recent-model vehicles", "Recent-model BEV"))
put(s, vic[, .(postcode, qdate, group, vehicles, bev, hybrid, recent_vehicles, recent_bev)], 2)
nf(s, dims(2, 2, nrow(vic) + 1), MON); nf(s, dims(2, 4, nrow(vic) + 1, 8), NUM)
widths(s, c(10, 12, 9, 11, 9, 11, 12, 11)); wb$freeze_pane(sheet = s, first_row = TRUE)

s <- "Fuel_Prices"
FSER <- c(CFG$fuel$retail_series, CFG$fuel$tgp_series)
FLAB <- c(NSW_ULP = "NSW retail ULP", NSW_Diesel = "NSW retail diesel", QLD_ULP = "QLD retail ULP", QLD_Diesel = "QLD retail diesel",
          TGP_petrol_national = "Terminal gate petrol (national)", TGP_diesel_national = "Terminal gate diesel (national)")
head(s, 1, c("Month", paste0(FLAB[FSER], " (c/L)")))
fx <- fuel[, c("month", FSER), with = FALSE][, month := ym_date(month)]
for (c in FSER) set(fx, j = c, value = round(fx[[c]], 2))
put(s, fx, 2)
nf(s, dims(2, 1, nrow(fx) + 1), MON); nf(s, dims(2, 2, nrow(fx) + 1, 1 + length(FSER)), ONE)
widths(s, c(10, rep(16, length(FSER))))
FCOL <- setNames(L(1 + seq_along(FSER)), FSER)

# ========================================================= LGA summary =====
s <- "LGA_Summary"
title(s, "LGA summary — NSW and QLD",
      paste0("Income: ABS Personal Income in Australia 2022-23 (ATO-based), median total income of earners. New regos = new vehicles, all customer types (private, business, dealer, government), in ", WIN$recent, ". Fleet = latest snapshot."))
head(s, 4, c("State", "LGA code", "LGA", "Median income ($)", "Earners", "Income group", paste0("New regos (", WIN$recent, ")"),
             paste0("BEV (", WIN$recent, ")"), paste0("PHEV (", WIN$recent, ", NSW)"), "BEV share of new", paste0("New regos (", WIN$base, ")"),
             paste0("BEV (", WIN$base, ")"), paste0("BEV share (", WIN$base, ")"), "Change (pp)", "BEV fleet (latest)", "Light-vehicle fleet (NSW)",
             "BEV per 1,000 light vehicles (NSW)", "BEV fleet per 1,000 earners", "On scatter? (1 = yes)",
             "BEV share — fuel crisis months", "BEV share — same months a year earlier", "Change during crisis (pp)"))
# eligible LGAs first within each state so the scatter series are contiguous
fl <- copy(flow)
fl[, cl := fifelse(customer == PRIVATE, "private", "other")]
fl[, new := bev_exact + phev_exact + other_exact + fifelse(cl == "private", kv("flow", "bev")$k, kv("flow", "bev", "other")$k) * bev_supp +
             fifelse(cl == "private", kv("flow", "phev")$k, kv("flow", "phev", "other")$k) * phev_supp +
             fifelse(cl == "private", kv("flow", "other")$k, kv("flow", "other", "other")$k) * other_supp]
fl[, last := max(month), by = state]
recent <- fl[ym_to_int(month) > ym_to_int(last) - CFG$period$recent_window_months, .(new = sum(new)), by = .(state, lga_name)]
lga[, elig := recent$new[match(paste(state, lga_name), paste(recent$state, recent$lga_name))] >= CFG$analysis$min_new_regs_scatter]
lga[is.na(elig), elig := FALSE]
ls_ <- lga[order(state, -elig, median_income)]
R0 <- 5; r <- rows_of(nrow(ls_), R0); LN <- max(r)
st <- sprintf('IF(A%d="NSW",%s,%s)', r, D$nsw_start, D$qld_start)
crit <- sprintf("Flow_LGA_Month!$A:$A,A%d,Flow_LGA_Month!$B:$B,C%d", r, r)
rw <- sprintf(',Flow_LGA_Month!$C:$C,">="&%s', st)
bw <- sprintf(',Flow_LGA_Month!$C:$C,"<="&%s', D$base_end)
win <- function(a, b) sprintf(',Flow_LGA_Month!$C:$C,">="&%s,Flow_LGA_Month!$C:$C,"<="&%s', a, b)
x <- ls_[, .(state, lga_code, lga_name, median_income, earners, income_group)]
x[, g := FML(sprintf("SUMIFS(Flow_LGA_Month!$N:$N,%s%s)", crit, rw))]
x[, h := FML(sprintf("SUMIFS(Flow_LGA_Month!$K:$K,%s%s)", crit, rw))]
x[, i := FML(sprintf('IF(A%d="NSW",SUMIFS(Flow_LGA_Month!$L:$L,%s%s),"n/a")', r, crit, rw))]
x[, j := FML(sprintf('IF(G%d>0,H%d/G%d,"")', r, r, r))]
x[, k := FML(sprintf("SUMIFS(Flow_LGA_Month!$N:$N,%s%s)", crit, bw))]
x[, l := FML(sprintf("SUMIFS(Flow_LGA_Month!$K:$K,%s%s)", crit, bw))]
x[, m := FML(sprintf('IF(K%d>0,L%d/K%d,"")', r, r, r))]
x[, n := FML(sprintf('IF(AND(G%d>0,K%d>0),(J%d-M%d)*100,"")', r, r, r, r))]
x[, o := FML(sprintf('IF(A%d="NSW",SUMIFS(Stock_NSW_LGA!$J:$J,Stock_NSW_LGA!$A:$A,C%d,Stock_NSW_LGA!$B:$B,%s),SUMIFS(Stock_QLD_LGA!$D:$D,Stock_QLD_LGA!$A:$A,C%d,Stock_QLD_LGA!$B:$B,%s))',
                     r, r, D$nsw_stock, r, D$qld_stock))]
x[, p := FML(sprintf('IF(A%d="NSW",SUMIFS(Stock_NSW_LGA!$M:$M,Stock_NSW_LGA!$A:$A,C%d,Stock_NSW_LGA!$B:$B,%s),"n/a")', r, r, D$nsw_stock))]
x[, q := FML(sprintf('IF(A%d="NSW",IF(P%d>0,O%d/P%d*1000,""),"n/a")', r, r, r, r))]
x[, rr := FML(sprintf("O%d/E%d*1000", r, r))]
x[, s_ := FML(sprintf("IF(G%d>=%s,1,0)", r, IN$min_lga))]
x[, t := FML(sprintf("IFERROR(SUMIFS(Flow_LGA_Month!$K:$K,%s%s)/SUMIFS(Flow_LGA_Month!$N:$N,%s%s),\"\")",
                     crit, win(D$cr_start, D$cr_end), crit, win(D$cr_start, D$cr_end)))]
x[, u := FML(sprintf("IFERROR(SUMIFS(Flow_LGA_Month!$K:$K,%s%s)/SUMIFS(Flow_LGA_Month!$N:$N,%s%s),\"\")",
                     crit, win(D$py_start, D$py_end), crit, win(D$py_start, D$py_end)))]
x[, v := FML(sprintf('IF(AND(ISNUMBER(T%d),ISNUMBER(U%d)),(T%d-U%d)*100,"")', r, r, r, r))]
put(s, x, R0)
nf(s, dims(R0, 4, LN), USD); nf(s, dims(R0, 5, LN), NUM); nf(s, dims(R0, 7, LN, 9), NUM); nf(s, dims(R0, 10, LN), PCT)
nf(s, dims(R0, 11, LN, 12), NUM); nf(s, dims(R0, 13, LN), PCT); nf(s, dims(R0, 14, LN), ONE); nf(s, dims(R0, 15, LN, 16), NUM)
nf(s, dims(R0, 17, LN, 18), ONE); nf(s, dims(R0, 20, LN, 21), PCT); nf(s, dims(R0, 22, LN), ONE)
widths(s, c(7, 9, 26, 12, 10, 8, 12, 10, 10, 10, 12, 10, 10, 9, 10, 12, 12, 12, 9, 11, 11, 10))
wb$freeze_pane(sheet = s, first_active_row = R0, first_active_col = 4)
wb$add_filter(sheet = s, rows = 4, cols = 1:22)
scat <- list()
for (st_ in c("NSW", "QLD")) { i <- which(ls_$state == st_ & ls_$elig); scat[[st_]] <- R0 - 1 + range(i) }

# ====================================================== VIC postcode ========
s <- "VIC_Postcode"
title(s, "VIC postcodes — fleet snapshot (quarterly) against ATO median taxable income",
      "Income: ATO Taxation Statistics 2023-24, Individuals Table 8. 'Recent-model' = year of manufacture within one year of the snapshot year — a proxy for recent new-vehicle take-up.")
head(s, 4, c("Postcode", "Median taxable income ($)", "Individuals", "Income group", "Vehicles (latest)", "BEV (latest)",
             "BEV per 1,000 vehicles", "Recent-model vehicles", "Recent-model BEV", "BEV share of recent-model", "BEV one year earlier",
             "Vehicles one year earlier", "BEV added in year per 1,000 vehicles", "On scatter? (1 = yes)", "Suburbs (ABS localities in postcode)",
             "BEV added in latest quarter (crisis)", "Added per 1,000 vehicles (latest quarter)", "BEV added same quarter a year earlier",
             "Added per 1,000 vehicles (year earlier)"))
vl <- vic[qdate == max(qdate)]
pcode[, elig := vl$recent_vehicles[match(postcode, vl$postcode)] >= CFG$analysis$min_recent_vehicles_vic &
                individuals >= CFG$analysis$min_individuals_vic_postcode]
pcode[is.na(elig), elig := FALSE]
ps <- pcode[order(-elig, median_income)]
r <- rows_of(nrow(ps), R0); PN <- max(r)
c1 <- sprintf("VIC_Postcode_Qtr!$A:$A,A%d,VIC_Postcode_Qtr!$B:$B,%s", r, D$vic_last)
c0 <- sprintf("VIC_Postcode_Qtr!$A:$A,A%d,VIC_Postcode_Qtr!$B:$B,%s", r, D$vic_prev)
cq <- function(off) sprintf("VIC_Postcode_Qtr!$A:$A,A%d,VIC_Postcode_Qtr!$B:$B,EDATE(%s,%d)", r, D$vic_last, off)
x <- ps[, .(postcode, median_income, individuals, income_group)]
x[, e := FML(sprintf("SUMIFS(VIC_Postcode_Qtr!$D:$D,%s)", c1))]
x[, f := FML(sprintf("SUMIFS(VIC_Postcode_Qtr!$E:$E,%s)", c1))]
x[, g := FML(sprintf('IF(E%d>0,F%d/E%d*1000,"")', r, r, r))]
x[, h := FML(sprintf("SUMIFS(VIC_Postcode_Qtr!$G:$G,%s)", c1))]
x[, i := FML(sprintf("SUMIFS(VIC_Postcode_Qtr!$H:$H,%s)", c1))]
x[, j := FML(sprintf('IF(H%d>0,I%d/H%d,"")', r, r, r))]
x[, k := FML(sprintf("SUMIFS(VIC_Postcode_Qtr!$E:$E,%s)", c0))]
x[, l := FML(sprintf("SUMIFS(VIC_Postcode_Qtr!$D:$D,%s)", c0))]
x[, m := FML(sprintf('IF(L%d>0,(F%d-K%d)/L%d*1000,"")', r, r, r, r))]
x[, n := FML(sprintf("IF(AND(H%d>=%s,C%d>=%d),1,0)", r, IN$min_vic, r, CFG$analysis$min_individuals_vic_postcode))]
x[, o := fifelse(is.na(suburbs$suburbs[match(postcode, suburbs$postcode)]), paste("Postcode", postcode),
                 suburbs$suburbs[match(postcode, suburbs$postcode)])]
x[, p := FML(sprintf("F%d-SUMIFS(VIC_Postcode_Qtr!$E:$E,%s)", r, cq(-3)))]
x[, q := FML(sprintf('IF(E%d>0,P%d/E%d*1000,"")', r, r, r))]
x[, rr := FML(sprintf("SUMIFS(VIC_Postcode_Qtr!$E:$E,%s)-SUMIFS(VIC_Postcode_Qtr!$E:$E,%s)", cq(-12), cq(-15)))]
x[, s_ := FML(sprintf('IF(L%d>0,R%d/L%d*1000,"")', r, r, r))]
put(s, x, R0)
nf(s, dims(R0, 2, PN), USD); nf(s, dims(R0, 3, PN), NUM); nf(s, dims(R0, 5, PN, 6), NUM); nf(s, dims(R0, 7, PN), ONE)
nf(s, dims(R0, 8, PN, 9), NUM); nf(s, dims(R0, 10, PN), PCT); nf(s, dims(R0, 11, PN, 12), NUM); nf(s, dims(R0, 13, PN), ONE)
nf(s, dims(R0, 16, PN), NUM); nf(s, dims(R0, 17, PN), ONE); nf(s, dims(R0, 18, PN), NUM); nf(s, dims(R0, 19, PN), ONE)
widths(s, c(10, 13, 11, 8, 11, 9, 11, 11, 10, 11, 11, 12, 13, 9, 34, 11, 11, 11, 11))
wb$freeze_pane(sheet = s, first_active_row = R0, first_active_col = 2)
wb$add_filter(sheet = s, rows = 4, cols = 1:19)
scat$VIC <- R0 - 1 + range(which(ps$elig))

# ============================================ Flow by income group, monthly ==
s <- "Flow_Group_Month"
title(s, "New registrations by LGA income group — monthly",
      "Groups are earner-weighted: each holds about a fifth of the state's earners. Q1 = lowest-income LGAs. Counts are SUMIFS over Flow_LGA_Month.")
months <- sort(unique(flow$mdate))
blocks <- list(c("NSW", "new", "N"), c("NSW", "BEV", "K"), c("QLD", "new", "N"), c("QLD", "BEV", "K"))
hdr <- c("Month", unlist(lapply(blocks, function(b) paste(b[1], b[2], GROUP_LABEL))),
         unlist(lapply(c("NSW", "QLD"), function(st_) c(paste(st_, "BEV share", GROUP_LABEL), paste(st_, "BEV share all")))))
head(s, 4, hdr)
M0 <- 5; r <- rows_of(length(months), M0); MN <- max(r)
putv(s, months, M0, 1); nf(s, dims(M0, 1, MN), MON)
col <- 2
for (b in blocks) for (g in GROUPS) {
  putv(s, FML(sprintf('SUMIFS(Flow_LGA_Month!$%s:$%s,Flow_LGA_Month!$A:$A,"%s",Flow_LGA_Month!$D:$D,%d,Flow_LGA_Month!$C:$C,$A%d)',
                      b[3], b[3], b[1], g, r)), M0, col)
  col <- col + 1
}
nf(s, dims(M0, 2, MN, col - 1), NUM)
SH <- list()
for (bi in 0:1) {
  new0 <- 2 + bi * 2 * NG; bev0 <- new0 + NG
  SH[[c("NSW", "QLD")[bi + 1]]] <- col
  for (g in GROUPS) {
    putv(s, FML(sprintf('IF(%s%d>0,%s%d/%s%d,"")', L(new0 + g - 1), r, L(bev0 + g - 1), r, L(new0 + g - 1), r)), M0, col); col <- col + 1
  }
  putv(s, FML(sprintf('IF(SUM(%s%d:%s%d)>0,SUM(%s%d:%s%d)/SUM(%s%d:%s%d),"")', L(new0), r, L(new0 + NG - 1), r,
                      L(bev0), r, L(bev0 + NG - 1), r, L(new0), r, L(new0 + NG - 1), r)), M0, col); col <- col + 1
}
nf(s, dims(M0, SH$NSW, MN, col - 1), PCT)
widths(s, c(9, rep(9, length(hdr) - 1))); wb$freeze_pane(sheet = s, first_active_row = M0, first_active_col = 2)

# ================================================ How income groups are built ==
s <- "Income_Groups"
title(s, sprintf("How the income groups (Q1–Q%d) are built", NG),
      "Every group table and chart in this workbook uses these groups. Computed in R/build_data.R (income_groups); number of groups in config.yaml income.n_groups.")
head(s, 4, c("Step", "How it works"))
meth <- income_group_method()
mrow <- rows_of(length(meth), 5)
put(s, data.table(step = sub("\\. .*$", "", meth), body = sub("^[^.]*\\. ", "", meth)), 5)
for (r in mrow) wb$merge_cells(sheet = s, dims = dims(r, 2, r, 7))
wb$add_cell_style(sheet = s, dims = dims(5, 1, max(mrow), 7), wrap_text = TRUE, vertical = "top")
bold(s, dims(5, 1, max(mrow), 1))
wb$set_row_heights(sheet = s, rows = mrow, heights = 14 * ceiling(nchar(meth) / 120) + 4)
r0 <- max(mrow) + 2
put(s, "What the groups contain", r0); bold(s, dims(r0), size = 12)
put(s, "Areas, people and shares are formulas on LGA_Summary / VIC_Postcode. A group's share of earners is only roughly 1/n because whole areas are kept together.", r0 + 1)
wb$add_font(sheet = s, dims = dims(r0 + 1), italic = TRUE, name = "Arial", size = 10, color = wb_color(MUTED))
r0 <- r0 + 3
grp_block <- function(r0, label, crit, src, cols, people, comp, note) {
  put(s, label, r0); bold(s, dims(r0))
  head(s, r0 + 1, c("Income group", "Areas", people, sprintf("Share of %s", tolower(people)), "Lowest area median ($)",
                    "Highest area median ($)", sprintf("Largest areas in the group (by %s)", tolower(people))))
  r1 <- r0 + 2; r <- rows_of(NG + 1, r1); allr <- r1 + NG
  gc <- c(sprintf(",%s!$%s:$%s,%d", src, cols$grp, cols$grp, GROUPS), "")
  cr <- sprintf("%s!$A:$A,%s", src, crit)
  x <- data.table(g = c(GROUP_LABEL, "All"),
                  a = FML(sprintf("COUNTIFS(%s%s)", cr, gc)),
                  b = FML(sprintf("SUMIFS(%s!$%s:$%s,%s%s)", src, cols$w, cols$w, cr, gc)),
                  c = FML(sprintf("C%d/C$%d", r, allr)),
                  d = FML(sprintf("_xlfn.MINIFS(%s!$%s:$%s,%s%s)", src, cols$inc, cols$inc, cr, gc)),
                  e = FML(sprintf("_xlfn.MAXIFS(%s!$%s:$%s,%s%s)", src, cols$inc, cols$inc, cr, gc)),
                  f = c(comp$largest, ""))
  put(s, x, r1); bold(s, dims(allr, 1, allr, 7))
  nf(s, dims(r1, 2, allr, 3), NUM); nf(s, dims(r1, 4, allr), PCT); nf(s, dims(r1, 5, allr, 6), USD)
  wb$add_cell_style(sheet = s, dims = dims(r1, 7, allr, 7), wrap_text = TRUE, vertical = "top")
  put(s, note, allr + 1); wb$merge_cells(sheet = s, dims = dims(allr + 1, 1, allr + 1, 7))
  wb$add_cell_style(sheet = s, dims = dims(allr + 1), wrap_text = TRUE, vertical = "top")
  wb$add_font(sheet = s, dims = dims(allr + 1), italic = TRUE, name = "Arial", size = 10, color = wb_color(MUTED))
  wb$set_row_heights(sheet = s, rows = allr + 1, heights = 30)
  allr + 3
}
for (st_ in c("NSW", "QLD")) {
  d <- lga[state == st_]
  r0 <- grp_block(r0, sprintf("%s — council areas (LGAs), median total income %s", st_, CFG$income$lga_year), sprintf('"%s"', st_),
                  "LGA_Summary", list(grp = "F", w = "E", inc = "D"), "Earners",
                  income_group_composition(d, "lga_name", "median_income", "earners"),
                  income_group_lumpiness(d, st_, "lga_name", "earners"))
}
pn <- copy(pcode)[, nm := postcode_label(postcode, suburbs)]
r0 <- grp_block(r0, sprintf("VIC — postcodes, median taxable income %s", CFG$income$postcode_year), '">0"',
                "VIC_Postcode", list(grp = "D", w = "C", inc = "B"), "Individuals",
                income_group_composition(pn, "nm", "median_income", "individuals"),
                income_group_lumpiness(pn, "VIC", "nm", "individuals"))
widths(s, c(22, 9, 12, 12, 14, 14, 70))

# ============================================== Flow by income group, window ==
s <- "Flow_Group_Summary"
title(s, sprintf("Who is buying the new BEVs? — %s vs %s", WIN$recent, WIN$base),
      sprintf("Recent window = the latest %d months of data (%s). Baseline = the first %d months of data (%s). Window length is on Inputs. How the income groups are built: see Income_Groups.",
              WIN$months, WIN$recent, WIN$months, WIN$base))
head(s, 4, c("State", "Income group", "Lowest LGA median ($)", "Highest LGA median ($)", "Earners", "New regos", "BEV",
             "BEV share of new", "Share of state's new BEVs", "Share of state's new regos", "BEV per 1,000 earners",
             paste0(c("New regos, ", "BEV, ", "BEV share, "), WIN$base)))
grow <- list(); r0 <- 5
for (st_ in c("NSW", "QLD")) {
  start <- if (st_ == "NSW") D$nsw_start else D$qld_start
  r <- rows_of(NG + 1, r0); allr <- r0 + NG
  gc <- c(sprintf(",Flow_LGA_Month!$D:$D,%d", GROUPS), "")
  lc <- c(sprintf(",LGA_Summary!$F:$F,%d", GROUPS), "")
  sm <- function(colf, extra) sprintf('SUMIFS(Flow_LGA_Month!$%s:$%s,Flow_LGA_Month!$A:$A,"%s"%s%s)', colf, colf, st_, gc, extra)
  x <- data.table(state = st_, g = c(GROUP_LABEL, "All"),
                  c = FML(sprintf('_xlfn.MINIFS(LGA_Summary!$D:$D,LGA_Summary!$A:$A,"%s"%s)', st_, lc)),
                  d = FML(sprintf('_xlfn.MAXIFS(LGA_Summary!$D:$D,LGA_Summary!$A:$A,"%s"%s)', st_, lc)),
                  e = FML(sprintf('SUMIFS(LGA_Summary!$E:$E,LGA_Summary!$A:$A,"%s"%s)', st_, lc)),
                  f = FML(sm("N", sprintf(',Flow_LGA_Month!$C:$C,">="&%s', start))),
                  g2 = FML(sm("K", sprintf(',Flow_LGA_Month!$C:$C,">="&%s', start))),
                  h = FML(sprintf("G%d/F%d", r, r)), i = FML(sprintf("G%d/G$%d", r, allr)), j = FML(sprintf("F%d/F$%d", r, allr)),
                  k = FML(sprintf("G%d/E%d*1000", r, r)),
                  l = FML(sm("N", sprintf(',Flow_LGA_Month!$C:$C,"<="&%s', D$base_end))),
                  m = FML(sm("K", sprintf(',Flow_LGA_Month!$C:$C,"<="&%s', D$base_end))),
                  n = FML(sprintf("M%d/L%d", r, r)))
  put(s, x, r0); bold(s, dims(allr, 1, allr, 14))
  tr <- allr + 1
  put(s, c(st_, "Top ÷ bottom group"), tr, 1)
  put(s, FML(sprintf("H%d/H%d", r0 + NG - 1, r0)), tr, 8); put(s, FML(sprintf("K%d/K%d", r0 + NG - 1, r0)), tr, 11)
  put(s, FML(sprintf("N%d/N%d", r0 + NG - 1, r0)), tr, 14); bold(s, dims(tr, 1, tr, 14))
  nf(s, dims(r0, 3, allr, 4), USD); nf(s, dims(r0, 5, allr, 7), NUM); nf(s, dims(r0, 8, allr, 10), PCT)
  nf(s, dims(r0, 11, allr), ONE); nf(s, dims(r0, 12, allr, 13), NUM); nf(s, dims(r0, 14, allr), PCT)
  nf(s, dims(tr, 8, tr, 14), DEC)
  grow[[st_]] <- r0; r0 <- tr + 2
}
r0 <- r0 + 1
put(s, "VIC — postcode income groups (ATO 2023-24). Flow proxy = BEV share of recent-model vehicles in the latest snapshot.", r0); bold(s, dims(r0, 1))
r0 <- r0 + 1
head(s, r0, c("State", "Income group", "Lowest postcode median ($)", "Highest postcode median ($)", "Individuals",
              "Recent-model vehicles", "Recent-model BEV", "BEV share of recent-model", "Share of VIC recent-model BEVs",
              "Share of VIC recent-model vehicles", "Recent-model BEV per 1,000 individuals", "BEV added in last year",
              "BEV added per 1,000 individuals"))
r0 <- r0 + 1; r <- rows_of(NG + 1, r0); allr <- r0 + NG
gc <- c(sprintf(",VIC_Postcode!$D:$D,%d", GROUPS), "")
vs <- function(c_) sprintf('SUMIFS(VIC_Postcode!$%s:$%s,VIC_Postcode!$A:$A,">0"%s)', c_, c_, gc)
x <- data.table(state = "VIC", g = c(GROUP_LABEL, "All"),
                c = FML(sprintf('_xlfn.MINIFS(VIC_Postcode!$B:$B,VIC_Postcode!$A:$A,">0"%s)', gc)),
                d = FML(sprintf('_xlfn.MAXIFS(VIC_Postcode!$B:$B,VIC_Postcode!$A:$A,">0"%s)', gc)),
                e = FML(vs("C")), f = FML(vs("H")), g2 = FML(vs("I")),
                h = FML(sprintf("G%d/F%d", r, r)), i = FML(sprintf("G%d/G$%d", r, allr)), j = FML(sprintf("F%d/F$%d", r, allr)),
                k = FML(sprintf("G%d/E%d*1000", r, r)), l = FML(paste0(vs("F"), "-", vs("K"))), m = FML(sprintf("L%d/E%d*1000", r, r)))
put(s, x, r0); bold(s, dims(allr, 1, allr, 13))
tr <- allr + 1
put(s, c("VIC", "Top ÷ bottom group"), tr, 1); put(s, FML(sprintf("H%d/H%d", r0 + NG - 1, r0)), tr, 8)
put(s, FML(sprintf("M%d/M%d", r0 + NG - 1, r0)), tr, 13); bold(s, dims(tr, 1, tr, 13))
nf(s, dims(r0, 3, allr, 4), USD); nf(s, dims(r0, 5, allr, 7), NUM); nf(s, dims(r0, 8, allr, 10), PCT)
nf(s, dims(r0, 11, allr), ONE); nf(s, dims(r0, 12, allr), NUM); nf(s, dims(r0, 13, allr), ONE); nf(s, dims(tr, 8, tr, 13), DEC)
grow$VIC <- r0
widths(s, c(7, 14, 12, 12, 11, 12, 10, 10, 11, 11, 11, 11, 11, 10))

# =============================================== Fleet (stock) by group =====
s <- "Stock_Group"
title(s, "Registered fleet (stock) by income group",
      "NSW: TfNSW registration snapshot, light vehicles, quarter ends. QLD: BEVs seen in any transaction since Jan 2022 at their last known LGA (QLD publishes no regional fleet-by-fuel snapshot). VIC: DTP whole-fleet snapshot by postcode.")
stock_table <- function(r0, st_, parts, dates, ratios) {
  hdr <- c("Month", unlist(lapply(parts, function(p) paste(st_, p$lab, GROUP_LABEL))),
           unlist(lapply(ratios, function(q) c(paste(st_, q$lab, GROUP_LABEL), paste(st_, q$lab, "all")))))
  head(s, r0, hdr)
  first <- r0 + 1; r <- rows_of(length(dates), first)
  putv(s, dates, first, 1); nf(s, dims(first, 1, max(r)), MON)
  col <- 2
  for (p in parts) for (g in GROUPS) {
    putv(s, FML(sprintf("SUMIFS(%s!$%s:$%s,%s!$C:$C,%d,%s!$B:$B,$A%d)", p$sheet, p$col, p$col, p$sheet, g, p$sheet, r)), first, col)
    col <- col + 1
  }
  nf(s, dims(first, 2, max(r), col - 1), NUM)
  for (q in ratios) {
    n0 <- 2 + q$num * NG
    for (g in GROUPS) {
      den <- if (is.numeric(q$den)) sprintf("%s%d", L(2 + q$den * NG + g - 1), r) else
        sprintf('SUMIFS(LGA_Summary!$E:$E,LGA_Summary!$A:$A,"%s",LGA_Summary!$F:$F,%d)', st_, g)
      putv(s, FML(sprintf('IF(%s>0,%s%d/%s*%d,"")', den, L(n0 + g - 1), r, den, q$scale)), first, col); col <- col + 1
    }
    dall <- if (is.numeric(q$den)) sprintf("SUM(%s%d:%s%d)", L(2 + q$den * NG), r, L(2 + q$den * NG + NG - 1), r) else
      sprintf('SUMIFS(LGA_Summary!$E:$E,LGA_Summary!$A:$A,"%s")', st_)
    putv(s, FML(sprintf("SUM(%s%d:%s%d)/%s*%d", L(n0), r, L(n0 + NG - 1), r, dall, q$scale)), first, col); col <- col + 1
    nf(s, dims(first, col - NG - 1, max(r), col - 1), if (q$scale > 1) ONE else PCT)
  }
  c(first, max(r))
}
STK <- list()
STK$NSW <- stock_table(4, "NSW", list(list(lab = "light vehicles", sheet = "Stock_NSW_LGA", col = "M"), list(lab = "BEV", sheet = "Stock_NSW_LGA", col = "J")),
                       sort(unique(snsw$mdate)), list(list(lab = "BEV per 1,000 light vehicles", num = 1, den = 0, scale = 1000),
                                                      list(lab = "BEV per 1,000 earners", num = 1, den = "earners", scale = 1000)))
STK$QLD <- stock_table(STK$NSW[2] + 3, "QLD", list(list(lab = "BEV seen", sheet = "Stock_QLD_LGA", col = "D")),
                       sort(unique(sqld_q$mdate)), list(list(lab = "BEV per 1,000 earners", num = 0, den = "earners", scale = 1000)))
STK$VIC <- stock_table(STK$QLD[2] + 3, "VIC", list(list(lab = "vehicles", sheet = "VIC_Postcode_Qtr", col = "D"),
                                                  list(lab = "BEV", sheet = "VIC_Postcode_Qtr", col = "E"),
                                                  list(lab = "recent-model vehicles", sheet = "VIC_Postcode_Qtr", col = "G"),
                                                  list(lab = "recent-model BEV", sheet = "VIC_Postcode_Qtr", col = "H")),
                       sort(unique(vic$qdate)), list(list(lab = "BEV per 1,000 vehicles", num = 1, den = 0, scale = 1000),
                                                     list(lab = "BEV share of recent-model", num = 3, den = 2, scale = 1)))
widths(s, c(9, rep(10, 40)))
nl <- STK$NSW[2]; ql <- STK$QLD[2]; vl_ <- STK$VIC[2]

# ============================================================ Fuel crisis ==
s <- "Fuel_Crisis"
title(s, "The 2026 fuel crisis — did the price shock change who buys BEVs?",
      "Crisis = months from the detected onset (Inputs) to the latest month, compared with the same calendar months a year earlier (controls for seasonality such as the June EOFY peak).")
head(s, 4, c("Month", "NSW retail ULP (c/L)", "NSW retail diesel (c/L)", "QLD retail ULP (c/L)", "QLD retail diesel (c/L)",
             "Terminal gate petrol (c/L)", "Crisis month? (1 = yes)", "NSW BEV share — all areas", "QLD BEV share — all areas"))
X0 <- 5; r <- rows_of(length(months), X0); XN <- max(r); fr <- rows_of(length(months), M0)
price_cols <- c("NSW_ULP", "NSW_Diesel", "QLD_ULP", "QLD_Diesel", "TGP_petrol_national")
putv(s, FML(sprintf("Flow_Group_Month!A%d", fr)), X0, 1)
for (j in seq_along(price_cols))
  putv(s, FML(sprintf('IFERROR(INDEX(Fuel_Prices!$%s:$%s,MATCH($A%d,Fuel_Prices!$A:$A,0)),"")', FCOL[price_cols[j]], FCOL[price_cols[j]], r)), X0, j + 1)
putv(s, FML(sprintf("IF(AND(A%d>=%s,A%d<=%s),1,0)", r, D$cr_start, r, D$cr_end)), X0, 7)
putv(s, FML(sprintf("Flow_Group_Month!%s%d", L(SH$NSW + NG), fr)), X0, 8)
putv(s, FML(sprintf("Flow_Group_Month!%s%d", L(SH$QLD + NG), fr)), X0, 9)
nf(s, dims(X0, 1, XN), MON); nf(s, dims(X0, 2, XN, 6), ONE); nf(s, dims(X0, 8, XN, 9), PCT); link(s, dims(X0, 8, XN, 9))
r0 <- XN + 3
put(s, "New registrations by income group — crisis months vs same months a year earlier", r0); bold(s, dims(r0, 1))
r0 <- r0 + 1
head(s, r0, c("State", "Income group", "New regos (crisis)", "BEV (crisis)", "BEV share (crisis)", "New regos (year earlier)",
              "BEV (year earlier)", "BEV share (year earlier)", "Change vs year earlier (pp)", "BEV share multiple (× year earlier)",
              "Extra BEVs vs year earlier", "Share of the extra BEVs", "Share of crisis new regos", "Non-BEV new regos, change vs year earlier"))
r0 <- r0 + 1; xgrow <- list()
for (st_ in c("NSW", "QLD")) {
  r <- rows_of(NG + 1, r0); allr <- r0 + NG
  gc <- c(sprintf(",Flow_LGA_Month!$D:$D,%d", GROUPS), "")
  w <- function(colf, a, b) sprintf('SUMIFS(Flow_LGA_Month!$%s:$%s,Flow_LGA_Month!$A:$A,"%s"%s,Flow_LGA_Month!$C:$C,">="&%s,Flow_LGA_Month!$C:$C,"<="&%s)',
                                    colf, colf, st_, gc, a, b)
  x <- data.table(st = st_, g = c(GROUP_LABEL, "All"), c = FML(w("N", D$cr_start, D$cr_end)), d = FML(w("K", D$cr_start, D$cr_end)),
                  e = FML(sprintf("D%d/C%d", r, r)), f = FML(w("N", D$py_start, D$py_end)), g2 = FML(w("K", D$py_start, D$py_end)),
                  h = FML(sprintf("G%d/F%d", r, r)), i = FML(sprintf("(E%d-H%d)*100", r, r)), j = FML(sprintf("E%d/H%d", r, r)),
                  k = FML(sprintf("D%d-G%d", r, r)), l = FML(sprintf("K%d/K$%d", r, allr)), m = FML(sprintf("C%d/C$%d", r, allr)),
                  n = FML(sprintf("(C%d-D%d)/(F%d-G%d)-1", r, r, r, r)))
  put(s, x, r0); bold(s, dims(allr, 1, allr, 14))
  tr <- allr + 1
  put(s, c(st_, "Top ÷ bottom group"), tr, 1)
  for (cc in c("E", "H", "J")) put(s, FML(sprintf("%s%d/%s%d", cc, r0 + NG - 1, cc, r0)), tr, col2int(cc))
  put(s, FML(sprintf("I%d-I%d", r0 + NG - 1, r0)), tr, 9); bold(s, dims(tr, 1, tr, 14))
  nf(s, dims(r0, 3, allr, 4), NUM); nf(s, dims(r0, 5, allr), PCT); nf(s, dims(r0, 6, allr, 7), NUM); nf(s, dims(r0, 8, allr), PCT)
  nf(s, dims(r0, 9, allr), ONE); nf(s, dims(r0, 10, allr), DEC); nf(s, dims(r0, 11, allr), NUM); nf(s, dims(r0, 12, allr, 14), PCT)
  nf(s, dims(tr, 5, tr, 10), DEC); nf(s, dims(tr, 9), ONE)
  xgrow[[st_]] <- r0; r0 <- tr + 2
}
r0 <- r0 + 1
put(s, "VIC — BEVs added to the fleet in the latest quarter vs the same quarter a year earlier (VIC has no monthly regional series; the latest quarter falls inside the crisis)", r0)
bold(s, dims(r0, 1)); r0 <- r0 + 1
head(s, r0, c("State", "Income group", "BEV fleet latest quarter", "BEV fleet quarter before", "Added (latest quarter)",
              "BEV fleet same quarter a year earlier", "BEV fleet quarter before that", "Added (year earlier)", "Multiple (× year earlier)",
              "Vehicles (latest)", "Added per 1,000 vehicles (latest quarter)", "Added per 1,000 vehicles (year earlier)", "Share of added BEVs (latest quarter)"))
r0 <- r0 + 1; r <- rows_of(NG + 1, r0); allr <- r0 + NG
gc <- c(sprintf(",VIC_Postcode_Qtr!$C:$C,%d", GROUPS), "")
vq <- function(c_, off) sprintf("SUMIFS(VIC_Postcode_Qtr!$%s:$%s,VIC_Postcode_Qtr!$B:$B,EDATE(%s,%d)%s)", c_, c_, D$vic_last, off, gc)
x <- data.table(st = "VIC", g = c(GROUP_LABEL, "All"), c = FML(vq("E", 0)), d = FML(vq("E", -3)), e = FML(sprintf("C%d-D%d", r, r)),
                f = FML(vq("E", -12)), g2 = FML(vq("E", -15)), h = FML(sprintf("F%d-G%d", r, r)), i = FML(sprintf("E%d/H%d", r, r)),
                j = FML(vq("D", 0)), k = FML(sprintf("E%d/J%d*1000", r, r)), l = FML(sprintf("H%d/(%s)*1000", r, vq("D", -12))),
                m = FML(sprintf("E%d/E$%d", r, allr)))
put(s, x, r0); bold(s, dims(allr, 1, allr, 13))
tr <- allr + 1
put(s, c("VIC", "Top ÷ bottom group"), tr, 1); put(s, FML(sprintf("I%d/I%d", r0 + NG - 1, r0)), tr, 9)
put(s, FML(sprintf("K%d/K%d", r0 + NG - 1, r0)), tr, 11); put(s, FML(sprintf("L%d/L%d", r0 + NG - 1, r0)), tr, 12); bold(s, dims(tr, 1, tr, 13))
nf(s, dims(r0, 3, allr, 8), NUM); nf(s, dims(r0, 9, allr), DEC); nf(s, dims(r0, 10, allr), NUM); nf(s, dims(r0, 11, allr, 12), ONE)
nf(s, dims(r0, 13, allr), PCT); nf(s, dims(tr, 9, tr, 12), DEC)
xgrow$VIC <- r0
widths(s, c(9, 14, rep(12, 17))); wb$freeze_pane(sheet = s, first_active_row = 5, first_active_col = 3)

# ========================================================= Customer types ===
s <- "Customer_Types"
title(s, "New registrations by customer type — NSW and QLD",
      paste0("Every table in this workbook counts all customer types. NSW splits private, business, dealer (demonstrator) and government buyers; QLD only individuals vs organisations. ",
             "Business, dealer and government vehicles are registered at the organisation's address, not where the driver lives, so they sit less well against area income."))
CT <- list(NSW = unname(unlist(CFG$nsw$customer_types)), QLD = unname(unlist(CFG$qld$customer_types)))
fsum <- function(colf, st_, extra) sprintf('SUMIFS(Flow_LGA_Month!$%s:$%s,Flow_LGA_Month!$A:$A,"%s"%s)', colf, colf, st_, extra)
ctc <- function(t) if (is.na(t)) "" else sprintf(',Flow_LGA_Month!$O:$O,"%s"', t)
since <- function(st_) sprintf(',Flow_LGA_Month!$C:$C,">="&%s', if (st_ == "NSW") D$nsw_start else D$qld_start)
between <- function(a, b) sprintf(',Flow_LGA_Month!$C:$C,">="&%s,Flow_LGA_Month!$C:$C,"<="&%s', a, b)
put(s, paste0("A. BEV take-up by customer type — ", WIN$recent, " and the fuel crisis"), 4); bold(s, "A4")
head(s, 5, c("State", "Customer type", "New regos", "BEV", "BEV share", "Share of state's new regos", "Share of state's BEVs",
             "BEV share — crisis months", "BEV share — same months a year earlier", "Change (pp)"))
r0 <- 6; ctrow <- list()
for (st_ in c("NSW", "QLD")) {
  ty <- c(CT[[st_]], NA); r <- rows_of(length(ty), r0); allr <- max(r)
  x <- data.table(a = st_, b = ifelse(is.na(ty), "All customers", ty),
                  c = FML(vapply(ty, function(t) fsum("N", st_, paste0(ctc(t), since(st_))), "")),
                  d = FML(vapply(ty, function(t) fsum("K", st_, paste0(ctc(t), since(st_))), "")),
                  e = FML(sprintf("IFERROR(D%d/C%d,\"\")", r, r)), f = FML(sprintf("C%d/C$%d", r, allr)), g = FML(sprintf("D%d/D$%d", r, allr)),
                  h = FML(vapply(ty, function(t) sprintf("IFERROR(%s/%s,\"\")", fsum("K", st_, paste0(ctc(t), between(D$cr_start, D$cr_end))),
                                                        fsum("N", st_, paste0(ctc(t), between(D$cr_start, D$cr_end)))), "")),
                  i = FML(vapply(ty, function(t) sprintf("IFERROR(%s/%s,\"\")", fsum("K", st_, paste0(ctc(t), between(D$py_start, D$py_end))),
                                                        fsum("N", st_, paste0(ctc(t), between(D$py_start, D$py_end)))), "")),
                  j = FML(sprintf("IFERROR((H%d-I%d)*100,\"\")", r, r)))
  put(s, x, r0); bold(s, dims(allr, 1, allr, 10))
  nf(s, dims(r0, 3, allr, 4), NUM); nf(s, dims(r0, 5, allr, 9), PCT); nf(s, dims(r0, 10, allr), ONE)
  ctrow[[st_]] <- c(r0, allr - 1); r0 <- allr + 2
}
put(s, paste0("B. BEV share of new regos by income group and customer type — ", WIN$recent), r0); bold(s, dims(r0, 1)); r0 <- r0 + 1
head(s, r0, c("State", "Customer type", GROUP_LABEL, "Top ÷ bottom group")); r0 <- r0 + 1; ctgrp <- list()
for (st_ in c("NSW", "QLD")) {
  ty <- c(NA, CT[[st_]]); r <- rows_of(length(ty), r0)
  put(s, data.table(a = st_, b = ifelse(is.na(ty), "All customers", ty)), r0)
  for (g in GROUPS) putv(s, FML(vapply(ty, function(t) {
    ex <- paste0(ctc(t), sprintf(",Flow_LGA_Month!$D:$D,%d", g), since(st_))
    sprintf("IFERROR(%s/%s,\"\")", fsum("K", st_, ex), fsum("N", st_, ex)) }, "")), r0, 2 + g)
  putv(s, FML(sprintf('IFERROR(%s%d/C%d,"")', L(2 + NG), r, r)), r0, 3 + NG)
  nf(s, dims(r0, 3, max(r), 2 + NG), PCT); nf(s, dims(r0, 3 + NG, max(r)), DEC)
  ctgrp[[st_]] <- r0; r0 <- max(r) + 2
}
put(s, paste0("C. Share of each income group's new BEVs bought by customers other than private buyers — ", WIN$recent), r0); bold(s, dims(r0, 1)); r0 <- r0 + 1
head(s, r0, c("State", "", GROUP_LABEL, "All groups")); r0 <- r0 + 1
for (st_ in c("NSW", "QLD")) {
  put(s, data.table(a = st_, b = "Not private"), r0)
  for (g in c(GROUPS, NA)) {
    gx <- if (is.na(g)) "" else sprintf(",Flow_LGA_Month!$D:$D,%d", g)
    put(s, FML(sprintf("IFERROR(1-%s/%s,\"\")", fsum("K", st_, paste0(ctc(PRIVATE), gx, since(st_))), fsum("K", st_, paste0(gx, since(st_))))),
        r0, if (is.na(g)) 3 + NG else 2 + g)
  }
  nf(s, dims(r0, 3, r0, 3 + NG), PCT); r0 <- r0 + 1
}
widths(s, c(9, 24, rep(13, 8)))

# ============================================================ Raw numbers ===
s <- "Raw_Numbers"
title(s, "Raw numbers — vehicle counts, not shares",
      "New registrations per month (NSW, QLD) and BEV counts by income group. NSW counts include the estimate for suppressed '<=5' cells (Inputs).")
head(s, 4, c("Month", "NSW new regos", "NSW BEV", "NSW other fuels", "QLD new regos", "QLD BEV", "QLD other fuels"))
RN0 <- 5; r <- rows_of(length(months), RN0); RNN <- max(r)
putv(s, FML(sprintf("Flow_Group_Month!A%d", fr)), RN0, 1); nf(s, dims(RN0, 1, RNN), MON)
for (bi in 0:1) {
  new0 <- 2 + bi * 2 * NG; bev0 <- new0 + NG; c0 <- 2 + bi * 3
  putv(s, FML(sprintf("SUM(Flow_Group_Month!%s%d:%s%d)", L(new0), fr, L(new0 + NG - 1), fr)), RN0, c0)
  putv(s, FML(sprintf("SUM(Flow_Group_Month!%s%d:%s%d)", L(bev0), fr, L(bev0 + NG - 1), fr)), RN0, c0 + 1)
  putv(s, FML(sprintf("%s%d-%s%d", L(c0), r, L(c0 + 1), r)), RN0, c0 + 2)
}
nf(s, dims(RN0, 2, RNN, 7), NUM)
GC <- 10
head(s, 4, c("Income group", paste0("NSW new BEVs (", WIN$recent, ")"), paste0("QLD new BEVs (", WIN$recent, ")"), "VIC recent-model BEVs in fleet",
             "NSW BEV fleet (latest)", "QLD BEVs seen since 2022 (latest)", "VIC BEV fleet (latest)", "NSW new BEVs — year before crisis",
             "NSW new BEVs — crisis months", "QLD new BEVs — year before crisis", "QLD new BEVs — crisis months"), col = GC)
gi <- GROUPS - 1
x <- data.table(g = GROUP_LABEL,
                a = FML(sprintf("Flow_Group_Summary!G%d", grow$NSW + gi)), b = FML(sprintf("Flow_Group_Summary!G%d", grow$QLD + gi)),
                c = FML(sprintf("Flow_Group_Summary!G%d", grow$VIC + gi)), d = FML(sprintf("Stock_Group!%s%d", L(2 + NG + gi), nl)),
                e = FML(sprintf("Stock_Group!%s%d", L(2 + gi), ql)), f = FML(sprintf("Stock_Group!%s%d", L(2 + NG + gi), vl_)),
                h = FML(sprintf("Fuel_Crisis!G%d", xgrow$NSW + gi)), i = FML(sprintf("Fuel_Crisis!D%d", xgrow$NSW + gi)),
                j = FML(sprintf("Fuel_Crisis!G%d", xgrow$QLD + gi)), k = FML(sprintf("Fuel_Crisis!D%d", xgrow$QLD + gi)))
put(s, x, RN0, GC)
RG_ALL <- RN0 + NG
put(s, "All", RG_ALL, GC)
put(s, as.data.frame(lapply(1:10, function(j) FML(sprintf("SUM(%s%d:%s%d)", L(GC + j), RN0, L(GC + j), RN0 + NG - 1))), col.names = paste0("v", 1:10)), RG_ALL, GC + 1)
nf(s, dims(RN0, GC + 1, RG_ALL, GC + 10), NUM); bold(s, dims(RG_ALL, GC, RG_ALL, GC + 10)); link(s, dims(RN0, GC + 1, RN0 + NG - 1, GC + 10))
widths(s, c(9, 12, 10, 12, 12, 10, 12, 3, 3, 14, rep(14, 10)))
wb$freeze_pane(sheet = s, first_active_row = RN0, first_active_col = 2)

# ============================================================= Top 10s =====
TOPN <- CFG$analysis$top_n
put("Rank_Keys", "Helper keys for the Top10 sheet: metric value plus a tiny row-number tie-break, or ±1E9 when the area is not eligible. Do not edit.", 1)
s <- "Top10"
title(s, sprintf("Top and bottom %d areas", TOPN),
      "Suburb-level EV data is not published by any state: VIC is by postcode (named by its ABS suburbs), NSW and QLD by LGA. Only areas above the size thresholds on Inputs are ranked. Live formulas.")
lga_show <- list(c("LGA", "C", ""), c("Median income ($)", "D", USD), c("Income group", "F", "0"))
vic_show <- list(c("Postcode", "A", "0"), c("Suburbs", "O", ""), c("Median taxable income ($)", "B", USD), c("Income group", "D", "0"))
specs <- list(
  list(paste0("NSW LGAs — highest BEV share of new cars (", WIN$recent, ")"), "LGA", "NSW", "J", -1, list(c("New regos", "G", NUM), c("BEV share", "J", PCT))),
  list(paste0("NSW LGAs — lowest BEV share of new cars (", WIN$recent, ")"), "LGA", "NSW", "J", 1, list(c("New regos", "G", NUM), c("BEV share", "J", PCT))),
  list(paste0("NSW LGAs — most new BEVs registered (", WIN$recent, ", count)"), "LGA", "NSW", "H", -1, list(c("New regos", "G", NUM), c("New BEVs", "H", NUM))),
  list("NSW LGAs — largest BEV fleet (count)", "LGA", "NSW", "O", -1, list(c("Per 1,000 vehicles", "Q", ONE), c("BEV fleet", "O", NUM))),
  list("NSW LGAs — biggest rise in BEV share during the fuel crisis", "LGA", "NSW", "V", -1,
       list(c("Year earlier", "U", PCT), c("Crisis", "T", PCT), c("Change (pp)", "V", ONE))),
  list(paste0("QLD LGAs — highest BEV share of new cars (", WIN$recent, ")"), "LGA", "QLD", "J", -1, list(c("New regos", "G", NUM), c("BEV share", "J", PCT))),
  list(paste0("QLD LGAs — most new BEVs registered (", WIN$recent, ", count)"), "LGA", "QLD", "H", -1, list(c("New regos", "G", NUM), c("New BEVs", "H", NUM))),
  list("VIC suburbs (postcodes) — most BEVs per 1,000 vehicles", "VIC", NA, "G", -1, list(c("BEVs", "F", NUM), c("Per 1,000", "G", ONE))),
  list("VIC suburbs (postcodes) — most BEVs registered (fleet count)", "VIC", NA, "F", -1, list(c("Vehicles", "E", NUM), c("BEVs", "F", NUM))),
  list("VIC suburbs (postcodes) — most BEVs added in the crisis quarter (count)", "VIC", NA, "P", -1,
       list(c("Added year earlier", "R", NUM), c("Added (crisis qtr)", "P", NUM))))
top_tables <- list(); row <- 4
for (k in seq_along(specs)) {
  sp <- specs[[k]]; src <- sp[[2]]; kc <- L(k + 1)
  sheet <- if (src == "LGA") "LGA_Summary" else "VIC_Postcode"
  last <- if (src == "LGA") LN else PN
  rr <- R0:last
  elig <- if (src == "LGA") sprintf("%s!$S%d=1", sheet, rr) else sprintf("%s!$N%d=1", sheet, rr)
  stc <- if (!is.na(sp[[3]])) sprintf('%s!$A%d="%s",', sheet, rr, sp[[3]]) else ""
  v <- sprintf("%s!$%s%d", sheet, sp[[4]], rr)
  cond <- sprintf("AND(%s%s,ISNUMBER(%s))", stc, elig, v)
  key <- if (sp[[5]] < 0) sprintf("IF(%s,%s+ROW()/1E9,-1E9)", cond, v) else sprintf("IF(%s,%s-ROW()/1E9,1E9)", cond, v)
  put("Rank_Keys", sp[[1]], 3, k + 1); putv("Rank_Keys", FML(key), R0, k + 1)
  show <- c(if (src == "LGA") lga_show else vic_show, sp[[6]])
  col0 <- if (k %% 2 == 1) 1 else 10
  put(s, sp[[1]], row, col0); bold(s, dims(row, col0))
  head(s, row + 1, c("#", vapply(show, `[`, "", 1)), col = col0)
  pick <- if (sp[[5]] < 0) "LARGE" else "SMALL"
  rows <- row + 1 + seq_len(TOPN)
  putv(s, seq_len(TOPN), row + 2, col0)
  mref <- sprintf("MATCH(%s(Rank_Keys!$%s:$%s,%d),Rank_Keys!$%s:$%s,0)", pick, kc, kc, seq_len(TOPN), kc, kc)
  for (j in seq_along(show)) {
    putv(s, FML(sprintf('IFERROR(INDEX(%s!$%s:$%s,%s),"")', sheet, show[[j]][2], show[[j]][2], mref)), row + 2, col0 + j)
    if (nzchar(show[[j]][3])) nf(s, dims(row + 2, col0 + j, row + 1 + TOPN), show[[j]][3])
  }
  link(s, dims(row + 2, col0 + 1, row + 1 + TOPN, col0 + length(show)))
  top_tables[[k]] <- list(title = sp[[1]], r1 = row + 2, r2 = row + 1 + TOPN, c1 = col0 + 1, c2 = col0 + length(show), src = src,
                          st = sp[[3]], fmt = tail(sp[[6]], 1)[[1]][3])
  if (k %% 2 == 0) row <- row + TOPN + 4
}
wb$set_col_widths(sheet = s, cols = 1:18, widths = c(5, 24, 14, rep(12, 6), 5, 24, 14, rep(12, 6)))
wb$set_sheet_visibility(sheet = "Rank_Keys", value = "hidden")

# ============================================================= Suppression ==
s <- "Suppression"
title(s, "NSW small-cell suppression — how the '<=5' cells are valued",
      "TfNSW publishes every count of 5 or fewer as '<=5'. Because each row is also split by colour, gender, age group etc., almost every row is suppressed, so the value given to a '<=5' cell sets the level of every NSW count.")
head(s, 4, c("Table", "Customers", "Fuel group", "Estimated value per '<=5' cell", "Method"))
put(s, supp[, .(table, customers, fuel_group, k, method)], 5); SR <- 4 + nrow(supp); nf(s, dims(5, 4, SR), DEC)
notes <- c(
  "Why it matters less than it looks: every NSW comparison in this workbook is a ratio within NSW (BEV share of new regos, BEVs per 1,000 vehicles), so a common scaling of all cells cancels. Only a difference between the size of BEV cells and other cells moves the income gradient.",
  "Flow estimate, private buyers: QLD publishes unit records. Each QLD individual's new vehicle was given a synthetic gender x age group drawn from the NSW private new-vehicle mix for its fuel group, then aggregated to NSW's grain (month x LGA x make x fuel x colour x gender x age). The mean of the cells that NSW would have suppressed is the estimate.",
  "Flow estimate, business, dealer and government buyers: NSW publishes these rows without gender or age, so their cells are coarser (month x LGA x make x fuel x colour) and larger. QLD organisations' new vehicles, aggregated to that grain, give the estimate the same way.",
  "Fleet estimates: BEV — the value that makes growth in the NSW BEV fleet between the first and latest snapshots equal new BEV registrations (all customer types) over the same period. Other fuels — the value that makes the whole detailed snapshot add to the total in TfNSW's coarser 'Age of Registered Vehicles' snapshot for the same month.",
  "Sensitivity: change the yellow cells on Inputs (e.g. set all to 1 or to 3) and every table and chart recalculates.")
NR <- SR + 2 + seq_along(notes) - 1
putv(s, notes, NR[1], 1)
for (i in NR) wb$merge_cells(sheet = s, dims = dims(i, 1, i, 5))
wb$add_cell_style(sheet = s, dims = dims(NR[1], 1, max(NR)), wrap_text = TRUE, vertical = "top")
wb$set_row_heights(sheet = s, rows = NR, heights = 62); widths(s, c(10, 12, 12, 16, 100))

# ================================================================= Charts ===
s <- "Charts"
title(s, "Charts", "Native Excel charts linked to the tables — change an input and they update. Q1 = lowest-income fifth of areas (by earners), Q5 = highest.")
rng <- function(c1, r1, r2, c2 = c1) sprintf("$%s$%d:$%s$%d", L(c1), r1, L(c2), r2)
grp_series <- function(first_col, r1, r2) lapply(GROUPS, function(g) list(name = GROUP_LABEL[g], ref = rng(first_col + g - 1, r1, r2), colour = GROUP_COL[g]))
top_chart <- function(k) {
  t <- top_tables[[k]]
  cat_col <- if (t$src == "VIC") t$c1 + 1 else t$c1
  chart_bar(t$title, "Top10", rng(cat_col, t$r1, t$r2),
            list(list(name = sub(".* — ", "", t$title), ref = rng(t$c2, t$r1, t$r2), colour = STATE_COL[if (t$src == "VIC") "VIC" else t$st])),
            y_fmt = t$fmt, dir = "bar", legend = FALSE, top_down = TRUE)
}
GRP <- c(RN0, RN0 + NG - 1)
sections <- list(
  list("1. Raw numbers — how many BEVs", list(
    chart_bar("NSW — new registrations per month: BEV vs other fuels", "Raw_Numbers", rng(1, RN0, RNN),
              list(list(name = "BEV", ref = rng(3, RN0, RNN), colour = STATE_COL[["NSW"]]), list(name = "Other fuels", ref = rng(4, RN0, RNN), colour = OTHER)),
              grouping = "stacked", date = TRUE),
    chart_bar("QLD — new registrations per month: BEV vs other fuels", "Raw_Numbers", rng(1, RN0, RNN),
              list(list(name = "BEV", ref = rng(6, RN0, RNN), colour = STATE_COL[["QLD"]]), list(name = "Other fuels", ref = rng(7, RN0, RNN), colour = OTHER)),
              grouping = "stacked", date = TRUE),
    chart_bar(paste0("New BEVs registered in ", WIN$recent, ", by income group (count)"), "Raw_Numbers", rng(GC, GRP[1], GRP[2]),
              list(list(name = "NSW", ref = rng(GC + 1, GRP[1], GRP[2]), colour = STATE_COL[["NSW"]]),
                   list(name = "QLD", ref = rng(GC + 2, GRP[1], GRP[2]), colour = STATE_COL[["QLD"]]),
                   list(name = "VIC (recent-model BEVs in fleet)", ref = rng(GC + 3, GRP[1], GRP[2]), colour = STATE_COL[["VIC"]]))),
    chart_bar("BEVs in the registered fleet, latest, by income group (count)", "Raw_Numbers", rng(GC, GRP[1], GRP[2]),
              list(list(name = "NSW", ref = rng(GC + 4, GRP[1], GRP[2]), colour = STATE_COL[["NSW"]]),
                   list(name = "QLD (BEVs seen since 2022)", ref = rng(GC + 5, GRP[1], GRP[2]), colour = STATE_COL[["QLD"]]),
                   list(name = "VIC", ref = rng(GC + 6, GRP[1], GRP[2]), colour = STATE_COL[["VIC"]]))),
    chart_bar("NSW — BEV fleet by LGA income group (count, quarter ends)", "Stock_Group", rng(1, STK$NSW[1], nl),
              grp_series(2 + NG, STK$NSW[1], nl), grouping = "stacked", date = TRUE),
    chart_bar("VIC — BEV fleet by postcode income group (count, quarters)", "Stock_Group", rng(1, STK$VIC[1], vl_),
              grp_series(2 + NG, STK$VIC[1], vl_), grouping = "stacked", date = TRUE))),
  list("2. Income — BEV share and fleet rates", list(
    chart_line("NSW — BEV share of new registrations, by LGA income group", "Flow_Group_Month", rng(1, M0, MN), grp_series(SH$NSW, M0, MN)),
    chart_bar(paste0("BEV share of new vehicles, ", WIN$recent, ", by income group"), "Flow_Group_Summary", rng(2, grow$NSW, grow$NSW + NG - 1),
              list(list(name = "NSW (new regos)", ref = rng(8, grow$NSW, grow$NSW + NG - 1), colour = STATE_COL[["NSW"]]),
                   list(name = "QLD (new regos)", ref = rng(8, grow$QLD, grow$QLD + NG - 1), colour = STATE_COL[["QLD"]]),
                   list(name = "VIC (recent-model vehicles in fleet)", ref = rng(8, grow$VIC, grow$VIC + NG - 1), colour = STATE_COL[["VIC"]])), y_fmt = PCT),
    chart_line("NSW fleet — BEVs per 1,000 light vehicles, by LGA income group", "Stock_Group", rng(1, STK$NSW[1], nl),
               grp_series(2 + 2 * NG, STK$NSW[1], nl), y_fmt = "0", y_title = "per 1,000"),
    chart_line("VIC fleet — BEVs per 1,000 vehicles, by postcode income group", "Stock_Group", rng(1, STK$VIC[1], vl_),
               grp_series(2 + 4 * NG, STK$VIC[1], vl_), y_fmt = "0", y_title = "per 1,000"),
    chart_scatter(paste0("NSW LGAs — median income vs BEV share of new regos (", WIN$recent, ")"), "LGA_Summary",
                  rng(4, scat$NSW[1], scat$NSW[2]), rng(10, scat$NSW[1], scat$NSW[2]), STATE_COL[["NSW"]],
                  "LGA median total income ($, 2022-23)", "BEV share"),
    chart_scatter("VIC postcodes — median taxable income vs BEVs per 1,000 vehicles", "VIC_Postcode",
                  rng(2, scat$VIC[1], scat$VIC[2]), rng(7, scat$VIC[1], scat$VIC[2]), STATE_COL[["VIC"]],
                  "Postcode median taxable income ($, 2023-24)", "BEV per 1,000", y_fmt = "0"))),
  list("3. The 2026 fuel crisis", list(
    chart_line("Pump prices — the 2026 fuel crisis (retail, c/L)", "Fuel_Crisis", rng(1, X0, XN),
               list(list(name = "NSW ULP", ref = rng(2, X0, XN), colour = STATE_COL[["NSW"]]),
                    list(name = "NSW diesel", ref = rng(3, X0, XN), colour = "9AA5B1", dash = TRUE),
                    list(name = "QLD ULP", ref = rng(4, X0, XN), colour = STATE_COL[["QLD"]])), y_fmt = "0", y_title = "c/L"),
    chart_bar("New BEVs by income group — crisis months vs same months a year earlier (count)", "Raw_Numbers", rng(GC, GRP[1], GRP[2]),
              list(list(name = "NSW year earlier", ref = rng(GC + 7, GRP[1], GRP[2]), colour = "B3CCE8"),
                   list(name = "NSW crisis", ref = rng(GC + 8, GRP[1], GRP[2]), colour = STATE_COL[["NSW"]]),
                   list(name = "QLD year earlier", ref = rng(GC + 9, GRP[1], GRP[2]), colour = "F8D2A8"),
                   list(name = "QLD crisis", ref = rng(GC + 10, GRP[1], GRP[2]), colour = STATE_COL[["QLD"]]))),
    chart_bar("NSW — BEV share by income group: crisis months vs same months a year earlier", "Fuel_Crisis", rng(2, xgrow$NSW, xgrow$NSW + NG - 1),
              list(list(name = "Same months a year earlier", ref = rng(8, xgrow$NSW, xgrow$NSW + NG - 1), colour = OTHER),
                   list(name = "Fuel-crisis months", ref = rng(5, xgrow$NSW, xgrow$NSW + NG - 1), colour = STATE_COL[["NSW"]])), y_fmt = PCT),
    chart_bar("QLD — BEV share by income group: crisis months vs same months a year earlier", "Fuel_Crisis", rng(2, xgrow$QLD, xgrow$QLD + NG - 1),
              list(list(name = "Same months a year earlier", ref = rng(8, xgrow$QLD, xgrow$QLD + NG - 1), colour = OTHER),
                   list(name = "Fuel-crisis months", ref = rng(5, xgrow$QLD, xgrow$QLD + NG - 1), colour = STATE_COL[["QLD"]])), y_fmt = PCT))),
  list("4. Customer types (tables on the Customer_Types sheet)", list(
    chart_bar(paste0("NSW — BEV share of new regos by customer type, ", WIN$recent), "Customer_Types", rng(2, ctrow$NSW[1], ctrow$NSW[2]),
              list(list(name = "BEV share", ref = rng(5, ctrow$NSW[1], ctrow$NSW[2]), colour = STATE_COL[["NSW"]])), y_fmt = PCT, legend = FALSE),
    chart_bar("NSW — share of new BEVs by customer type", "Customer_Types", rng(2, ctrow$NSW[1], ctrow$NSW[2]),
              list(list(name = "Share of state's BEVs", ref = rng(7, ctrow$NSW[1], ctrow$NSW[2]), colour = STATE_COL[["NSW"]]),
                   list(name = "Share of state's new regos", ref = rng(6, ctrow$NSW[1], ctrow$NSW[2]), colour = OTHER)), y_fmt = PCT),
    chart_bar(paste0("NSW — BEV share by income group: all customers vs private buyers, ", WIN$recent), "Customer_Types",
              sprintf("$C$%d:$%s$%d", ctgrp$NSW - 1, L(2 + NG), ctgrp$NSW - 1),
              list(list(name = "All customers", ref = sprintf("$C$%d:$%s$%d", ctgrp$NSW, L(2 + NG), ctgrp$NSW), colour = STATE_COL[["NSW"]]),
                   list(name = "Private buyers", ref = sprintf("$C$%d:$%s$%d", ctgrp$NSW + 1, L(2 + NG), ctgrp$NSW + 1), colour = OTHER)), y_fmt = PCT),
    chart_bar(paste0("QLD — BEV share by income group: all customers vs individuals, ", WIN$recent), "Customer_Types",
              sprintf("$C$%d:$%s$%d", ctgrp$NSW - 1, L(2 + NG), ctgrp$NSW - 1),
              list(list(name = "All customers", ref = sprintf("$C$%d:$%s$%d", ctgrp$QLD, L(2 + NG), ctgrp$QLD), colour = STATE_COL[["QLD"]]),
                   list(name = "Private buyers", ref = sprintf("$C$%d:$%s$%d", ctgrp$QLD + 1, L(2 + NG), ctgrp$QLD + 1), colour = OTHER)), y_fmt = PCT))),
  list("5. Top areas (full tables on the Top10 sheet)", list(top_chart(1), top_chart(3), top_chart(9), top_chart(5)))
)
ROWS_PER_CHART <- 21; row <- 4
for (sec in sections) {
  put(s, sec[[1]], row); bold(s, dims(row), col = PRIMARY, size = 13); row <- row + 1
  chs <- sec[[2]]
  for (i in seq_along(chs)) {
    r1 <- row + ((i - 1) %/% 2) * ROWS_PER_CHART
    c1 <- if (i %% 2 == 1) 1 else 12
    wb$add_chart_xml(sheet = s, dims = dims(r1, c1, r1 + ROWS_PER_CHART - 2, c1 + 9), xml = chs[[i]])
  }
  row <- row + ((length(chs) + 1) %/% 2) * ROWS_PER_CHART + 1
}

# ================================================================ Summary ===
s <- "Summary"
title(s, "EV take-up by regional income — key metrics",
      "NSW, QLD and VIC. Q1 = the poorest fifth of areas (by earners), Q5 = the richest. All numbers are live formulas.")
head(s, 4, c("Key metric", "NSW", "QLD", "VIC", "Note"))
fg <- "Flow_Group_Summary"; rn <- "Raw_Numbers"; fc <- "Fuel_Crisis"
gN <- grow$NSW; gQ <- grow$QLD; gV <- grow$VIC; top <- NG - 1; xN <- xgrow$NSW; xQ <- xgrow$QLD; xV <- xgrow$VIC
T0 <- function(ref, f) sprintf('TEXT(%s,"%s")', ref, f)
arrow <- function(a, b, f) sprintf('%s&" → "&%s', T0(a, f), T0(b, f))
xs <- function(ref) sprintf('TEXT(%s,"0.0")&"×"', ref)                    # 2.1×
xarrow <- function(a, b) sprintf('%s&" → "&%s', xs(a), xs(b))
rows <- list(
  list(paste("New cars —", WIN$recent), NULL),
  list("New BEVs registered", c(sprintf("%s!%s%d", rn, L(GC + 1), RG_ALL), sprintf("%s!%s%d", rn, L(GC + 2), RG_ALL), NA),
       "NSW and QLD new registrations", NUM),
  list("BEV share of new cars", c(sprintf("%s!H%d", fg, gN + NG), sprintf("%s!H%d", fg, gQ + NG), sprintf("%s!H%d", fg, gV + NG)),
       "VIC: share of recent-model vehicles in the fleet", PCT),
  list("BEV share, poorest → richest fifth of areas",
       c(arrow(sprintf("%s!H%d", fg, gN), sprintf("%s!H%d", fg, gN + top), "0.0%"), arrow(sprintf("%s!H%d", fg, gQ), sprintf("%s!H%d", fg, gQ + top), "0.0%"),
         arrow(sprintf("%s!H%d", fg, gV), sprintf("%s!H%d", fg, gV + top), "0.0%")), NA, NA),
  list(sprintf("Richest ÷ poorest: %s → %s", WIN$base, WIN$recent), c(xarrow(sprintf("%s!N%d", fg, gN + NG + 1), sprintf("%s!H%d", fg, gN + NG + 1)),
                                          xarrow(sprintf("%s!N%d", fg, gQ + NG + 1), sprintf("%s!H%d", fg, gQ + NG + 1)), NA),
       "Falling = the income gap is narrowing as BEVs go mainstream", NA),
  list("Registered fleet — latest snapshot", NULL),
  list("BEVs in the fleet", c(sprintf("%s!%s%d", rn, L(GC + 4), RG_ALL), sprintf("%s!%s%d", rn, L(GC + 5), RG_ALL), sprintf("%s!%s%d", rn, L(GC + 6), RG_ALL)),
       "QLD: BEVs seen in registration records since 2022 (no public fleet data by region)", NUM),
  list("BEVs per 1,000 vehicles, poorest → richest fifth",
       c(arrow(sprintf("Stock_Group!%s%d", L(2 + 2 * NG), nl), sprintf("Stock_Group!%s%d", L(2 + 2 * NG + NG - 1), nl), "0"), NA,
         arrow(sprintf("Stock_Group!%s%d", L(2 + 4 * NG), vl_), sprintf("Stock_Group!%s%d", L(2 + 4 * NG + NG - 1), vl_), "0")), NA, NA),
  list("2026 fuel crisis — crisis months vs same months a year earlier", NULL),
  list("New BEVs registered", c(arrow(sprintf("%s!G%d", fc, xN + NG), sprintf("%s!D%d", fc, xN + NG), "#,##0"),
                                arrow(sprintf("%s!G%d", fc, xQ + NG), sprintf("%s!D%d", fc, xQ + NG), "#,##0"),
                                arrow(sprintf("%s!H%d", fc, xV + NG), sprintf("%s!E%d", fc, xV + NG), "#,##0")),
       "VIC: BEVs added to the fleet in the crisis quarter vs a year earlier", NA),
  list("BEV share of new cars", c(arrow(sprintf("%s!H%d", fc, xN + NG), sprintf("%s!E%d", fc, xN + NG), "0.0%"),
                                          arrow(sprintf("%s!H%d", fc, xQ + NG), sprintf("%s!E%d", fc, xQ + NG), "0.0%"), NA), NA, NA),
  list("Growth in BEV share, poorest vs richest fifth", c(sprintf('%s&" vs "&%s', xs(sprintf("%s!J%d", fc, xN)), xs(sprintf("%s!J%d", fc, xN + top))),
                                                        sprintf('%s&" vs "&%s', xs(sprintf("%s!J%d", fc, xQ)), xs(sprintf("%s!J%d", fc, xQ + top))),
                                                        sprintf('%s&" vs "&%s', xs(sprintf("%s!I%d", fc, xV)), xs(sprintf("%s!I%d", fc, xV + top)))),
       "Poorer areas grew faster in relative terms; richer areas still added more percentage points", NA),
  list("Petrol, diesel and hybrid new cars", c(sprintf("%s!N%d", fc, xN + NG), sprintf("%s!N%d", fc, xQ + NG), NA), "Change vs a year earlier", "+0%;-0%"))
i <- 5
for (x in rows) {
  put(s, x[[1]], i, 1)
  if (is.null(x[[2]])) {
    bold(s, dims(i, 1), col = PRIMARY); wb$add_fill(sheet = s, dims = dims(i, 1, i, 5), color = wb_color(BAND))
  } else {
    for (j in 1:3) if (!is.na(x[[2]][j])) { put(s, FML(x[[2]][j]), i, j + 1); if (!is.na(x[[4]])) nf(s, dims(i, j + 1), x[[4]]) }
    else put(s, "—", i, j + 1)
    if (!is.na(x[[3]])) put(s, x[[3]], i, 5)
    link(s, dims(i, 2, i, 4)); wb$add_cell_style(sheet = s, dims = dims(i, 2, i, 4), horizontal = "right")
  }
  i <- i + 1
}
i <- i + 1
put(s, "How to read this", i); bold(s, dims(i))
guide <- c(
  "Area-level comparison: it shows where BEVs are registered, not the income of individual buyers. Retiree and student areas have low median taxable income but not necessarily low wealth.",
  "Income groups are ranked within each state (NSW/QLD: council areas; VIC: postcodes). QLD council areas are large, so QLD groups are lumpy.",
  "2026 fuel crisis: the US-Israel war with Iran from late February 2026 closed the Strait of Hormuz and pushed pump prices up about 60 c/L in March. BEV share jumped from March in both NSW and QLD while petrol/diesel/hybrid registrations fell.",
  "More: Charts (first sheet), Top10, Fuel_Crisis, Raw_Numbers. Sources and every data adjustment: Sources and Data_Adjustments sheets.")
putv(s, paste("•", guide), i + 1, 1)
for (k in seq_along(guide)) wb$merge_cells(sheet = s, dims = dims(i + k, 1, i + k, 5))
wb$add_cell_style(sheet = s, dims = dims(i + 1, 1, i + length(guide)), wrap_text = TRUE, vertical = "top")
wb$set_row_heights(sheet = s, rows = i + seq_along(guide), heights = 34)
widths(s, c(46, 22, 22, 22, 70))

# ================================================================ Sources ===
s <- "Sources"
title(s, "Data sources — dataset pages and every file used", "All public, open data. File lists are read from raw_data/**/urls.txt.")
head(s, 4, c("Dataset", "Publisher", "Dataset page", "Coverage used", "Licence / terms"))
datasets <- fread(file.path(HERE, "R", "sources.csv"))
put(s, datasets, 5)
r0 <- 5 + nrow(datasets) + 1
put(s, "Individual files downloaded", r0); bold(s, dims(r0)); head(s, r0 + 1, c("Dataset", "File", "URL"))
url_files <- c("NSW transactions" = "nsw/urls.txt", "NSW snapshot" = "nsw/snapshot_urls.txt", "NSW age snapshot" = "nsw/age/urls.txt",
               "QLD transactions" = "qld/urls.txt", "VIC fleet snapshot" = "vic/urls.txt", "Income" = "income/urls.txt")
files <- rbindlist(lapply(names(url_files), function(lab) {
  u <- trimws(readLines(file.path(RAW, url_files[[lab]]), warn = FALSE)); u <- u[nzchar(u)]
  u <- vapply(strsplit(u, "\\s+"), function(p) tail(p, 1), "")
  data.table(dataset = lab, file = URLdecode(sub("\\?.*", "", basename(u))), url = u)
}))
files <- rbind(files, data.table(dataset = c("Fuel prices", "Boundaries", "Boundaries", "Boundaries", "Suburb names"),
                                 file = c("retail_monthly.csv, tgp_monthly.csv", "lga_boundaries.geojson", "vic_postcode_boundaries.geojson",
                                          "australia_states.geojson", "vic_postcode_suburbs.csv"),
                                 url = c(sprintf("%s, %s (copies of the au_fuel_prices project output)", CFG$fuel$retail_file, CFG$fuel$tgp_file), CFG$map$lga_service,
                                         CFG$map$poa_service, CFG$map$ste_service, CFG$map$sal_point_service)))
put(s, files, r0 + 2)
all_urls <- c(datasets$page, files$url)
for (i in which(startsWith(datasets$page, "http"))) wb$add_hyperlink(sheet = s, dims = dims(4 + i, 3), target = datasets$page[i])
for (i in which(startsWith(files$url, "http"))) wb$add_hyperlink(sheet = s, dims = dims(r0 + 1 + i, 3), target = files$url[i])
wb$add_cell_style(sheet = s, dims = dims(5, 1, r0 + 1 + nrow(files), 5), wrap_text = TRUE, vertical = "top")
widths(s, c(44, 44, 90, 50, 22))

# ======================================================= Data adjustments ===
s <- "Data_Adjustments"
title(s, "Summary of adjustments to the data",
      "Every filter, relabelling, imputation and proxy applied between the raw files and the tables. Counts are written by build_data.R on each run.")
head(s, 4, c("Dataset", "Adjustment", "Detail", "Records affected"))
extra <- data.table(dataset = c("NSW new registrations", "NSW fleet", "All", "All", "Fuel crisis"),
                    adjustment = c("Suppressed-cell value (flow)", "Suppressed-cell value (stock)", "Income groups within each state",
                                   "Area-level matching", "Comparison windows"),
                    detail = c("Mean '<=5' cell estimated from QLD unit records re-cut at NSW grain with a synthetic gender x age split — see Suppression sheet",
                               "BEV: calibrated so fleet growth matches new BEV registrations; other fuels: calibrated so the detailed snapshot adds to the age-snapshot total",
                               "Q1–Q5 are ranked within NSW, QLD and VIC separately, so 'Q5' is relative to its own state",
                               "Registrations are matched to income by the owner's address area (LGA or postcode), not by the buyer's own income",
                               "Crisis = onset month to the latest month in both states; compared with the same calendar months a year earlier"),
                    affected = "")
ad <- rbind(adjust[, .(dataset, adjustment, detail, affected = fifelse(is.na(affected), "", as.character(affected)))], extra)
put(s, ad, 5)
wb$add_cell_style(sheet = s, dims = dims(5, 1, 4 + nrow(ad), 4), wrap_text = TRUE, vertical = "top")
widths(s, c(24, 38, 110, 30)); wb$freeze_pane(sheet = s, first_active_row = 5)

# ================================================================== Notes ===
s <- "Notes"
title(s, "Definitions and caveats")
head(s, 3, c("Type", "Item", "Detail"))
put(s, fread(file.path(HERE, "R", "notes.csv")), 4)
wb$add_cell_style(sheet = s, dims = "A4:C40", wrap_text = TRUE, vertical = "top")
widths(s, c(11, 34, 130))

# ---- finish -------------------------------------------------------------------
wb$set_active_sheet("Charts")
wb$workbook$calcPr <- '<calcPr calcId="191029" fullCalcOnLoad="1"/>'
out <- file.path(HERE, Sys.getenv("EV_WORKBOOK", CFG$paths$workbook))
wb$save(out)
logf("wrote %s", out)
