# Pre-compute everything the Shiny dashboard shows and save it as
# processed/dashboard.rds, so the app starts instantly.
#
# Run:  Rscript R/dashboard_data.R   (after build_data.R and fetch_boundaries.R)
# common.R sits next to this script: found via Rscript's --file, or source()'s ofile (RStudio's Source button)
source(file.path(local({
  f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  if (!length(f)) f <- sys.frames()[[1]]$ofile
  if (length(f)) dirname(normalizePath(f)) else "R"
}), "common.R"))
suppressPackageStartupMessages(library(sf))

NG <- CFG$income$n_groups
WIN <- CFG$period$recent_window_months
A <- CFG$analysis
rd <- function(f) fread(file.path(OUT, f))
k <- rd("suppression.csv")
kf <- function(t, f, cl = "all") k[table == t & fuel_group == f & customers == cl, k]
lga <- rd("lga_income.csv")
flow <- rd("flow_lga_month.csv")
snsw <- rd("stock_nsw_lga.csv")
sqld <- rd("stock_qld_proxy_lga.csv")
vic <- rd("vic_postcode_quarter.csv")
pc <- rd("postcode_income.csv")
fuel <- rd("fuel_prices.csv")
onset <- rd("fuel_crisis_onset.csv")
subs <- rd("vic_postcode_suburbs.csv")
for (x in names(CFG$map$postcode_name_overrides)) {
  if (!as.integer(x) %in% subs$postcode) subs <- rbind(subs, data.table(postcode = as.integer(x), suburbs = "")) # override may name a postcode with no ABS suburb point
  subs[postcode == as.integer(x), suburbs := CFG$map$postcode_name_overrides[[x]]]
}
gaps <- rd("nsw_unmapped_months.csv")$month

flow[, private := customer == PRIVATE]
for (f in c("bev", "phev", "other")) {
  flow[, (f) := get(paste0(f, "_exact")) + fifelse(private, kf("flow", f, "private"), kf("flow", f, "other")) * get(paste0(f, "_supp"))]
  snsw[, (f) := get(paste0(f, "_exact")) + kf("stock", f) * get(paste0(f, "_supp"))]
}
flow[, new := bev + phev + other]
snsw[, veh := bev + phev + other]

last <- flow[, .(last = max(month)), by = state]
lastm <- setNames(last$last, last$state)
cr0 <- onset$onset_month
cr1 <- min(lastm)
py0 <- int_to_ym(ym_to_int(cr0) - 12L)
py1 <- int_to_ym(ym_to_int(cr1) - 12L)
months <- ym_seq(min(flow$month), max(flow$month))
win_sum <- function(d, a, b, by = c("state", "lga_name")) d[month >= a & month <= b, .(new = sum(new), bev = sum(bev)), by = by]
recent_sum <- function(d, by = c("state", "lga_name")) {
  rbindlist(lapply(names(lastm), function(s) win_sum(d[state == s], int_to_ym(ym_to_int(lastm[[s]]) - WIN + 1L), lastm[[s]], by)))
}
fleet_nsw <- snsw[month == max(month), .(lga_name, fleet_bev = bev, fleet_veh = veh)]
fleet_qld <- sqld[month == max(month), .(lga_name, fleet_bev = bev_seen)]

gsum <- function(d, st) {
  g <- d[state == st, .(
    share = sum(bev) / sum(new), share_c = sum(c_bev) / sum(c_new), share_p = sum(p_bev) / sum(p_new),
    per1000veh = sum(fleet_bev, na.rm = TRUE) / sum(fleet_veh, na.rm = TRUE) * 1000,
    per1000pop = sum(fleet_bev, na.rm = TRUE) / sum(pop) * 1000, bev = sum(bev), c_bev = sum(c_bev),
    p_bev = sum(p_bev), fleet = sum(fleet_bev, na.rm = TRUE), inc_lo = min(income), inc_hi = max(income)
  ), keyby = group]
  g[, `:=`(chg = (share_c - share_p) * 100, mult = share_c / share_p, state = st)]
  g[]
}

# ---- LGA areas (built for all customers and for private buyers only) -------------
lga_view <- function(fl) {
  rec <- recent_sum(fl)
  cri <- win_sum(fl, cr0, cr1)
  pyr <- win_sum(fl, py0, py1)
  L <- copy(lga)[, id := as.character(lga_code)]
  L <- merge(L, rec[, .(state, lga_name, new, bev)], by = c("state", "lga_name"), all.x = TRUE)
  L <- merge(L, cri[, .(state, lga_name, c_new = new, c_bev = bev)], by = c("state", "lga_name"), all.x = TRUE)
  L <- merge(L, pyr[, .(state, lga_name, p_new = new, p_bev = bev)], by = c("state", "lga_name"), all.x = TRUE)
  L <- merge(L, rbind(fleet_nsw[, state := "NSW"], fleet_qld[, `:=`(state = "QLD", fleet_veh = NA_real_)]), by = c("state", "lga_name"), all.x = TRUE)
  for (c in c("new", "bev", "c_new", "c_bev", "p_new", "p_bev")) set(L, which(is.na(L[[c]])), c, 0)
  L[, `:=`(
    share = fifelse(new > 0, bev / new, NA_real_), share_c = fifelse(c_new > 0, c_bev / c_new, NA_real_),
    share_p = fifelse(p_new > 0, p_bev / p_new, NA_real_)
  )]
  L[, `:=`(
    chg = (share_c - share_p) * 100, mult = fifelse(share_p > 0, share_c / share_p, NA_real_),
    per1000veh = fifelse(fleet_veh > 0, fleet_bev / fleet_veh * 1000, NA_real_), per1000pop = fleet_bev / earners * 1000,
    elig = new >= A$min_new_regs_scatter, name = lga_name, income = median_income, pop = earners, group = income_group
  )]
  # monthly BEV share per LGA (for the selection chart)
  ms <- fl[, .(share = fifelse(sum(new) > 0, sum(bev) / sum(new), NA_real_)), by = .(state, lga_name, month)]
  L_series <- dcast(ms, state + lga_name ~ factor(month, levels = months), value.var = "share")
  G <- rbind(gsum(L, "NSW"), gsum(L, "QLD"), fill = TRUE)
  G[state == "QLD", per1000veh := NA]
  state_month <- fl[, .(new = sum(new), bev = sum(bev)), by = .(state, month)][, `:=`(share = bev / new, other = new - bev)][]
  kst <- function(st) {
    L[state == st, .(
      bev = sum(bev), new = sum(new), share = sum(bev) / sum(new), c_bev = sum(c_bev), p_bev = sum(p_bev),
      share_c = sum(c_bev) / sum(c_new), share_p = sum(p_bev) / sum(p_new), fleet = sum(fleet_bev, na.rm = TRUE)
    )]
  }
  list(lga = L, lga_series = L_series, groups = G, state_month = state_month, kpi = list(NSW = kst("NSW"), QLD = kst("QLD")))
}

# ---- VIC postcodes --------------------------------------------------------------
quarters <- sort(unique(vic$quarter))
qi <- function(q, off) quarters[match(q, quarters) + off]
ql <- max(quarters)
V <- copy(pc)[, id := as.character(postcode)]
get_q <- function(qq, cols) vic[quarter == qq, c("postcode", cols), with = FALSE]
V <- merge(V, get_q(ql, c("vehicles", "bev", "recent_vehicles", "recent_bev")), by = "postcode", all.x = TRUE)
V <- merge(V, setnames(get_q(qi(ql, -1), "bev"), "bev", "b1"), by = "postcode", all.x = TRUE)
V <- merge(V, setnames(get_q(qi(ql, -4), c("bev", "vehicles")), c("bev", "vehicles"), c("b4", "v4")), by = "postcode", all.x = TRUE)
V <- merge(V, setnames(get_q(qi(ql, -5), "bev"), "bev", "b5"), by = "postcode", all.x = TRUE)
V[, `:=`(
  name = fifelse(is.na(subs$suburbs[match(postcode, subs$postcode)]), paste("Postcode", postcode), subs$suburbs[match(postcode, subs$postcode)]),
  state = "VIC", income = median_income, pop = individuals, group = income_group,
  per1000veh = bev / vehicles * 1000, rshare = recent_bev / recent_vehicles, add_c = bev - b1, add_p = b4 - b5
)]
V[, `:=`(
  add_c1000 = add_c / vehicles * 1000, add_p1000 = add_p / v4 * 1000,
  elig = !is.na(recent_vehicles) & recent_vehicles >= A$min_recent_vehicles_vic & individuals >= A$min_individuals_vic_postcode
)]
vs <- vic[, .(per1000 = sum(bev) / sum(vehicles) * 1000), by = .(postcode, quarter)]
V_series <- dcast(vs, postcode ~ factor(quarter, levels = quarters), value.var = "per1000")

# ---- group aggregates (weighted, matching the workbook) --------------------------
GV <- V[!is.na(vehicles), .(
  per1000veh = sum(bev) / sum(vehicles) * 1000, rshare = sum(recent_bev) / sum(recent_vehicles),
  add_c1000 = sum(add_c, na.rm = TRUE) / sum(vehicles) * 1000, mult = sum(add_c, na.rm = TRUE) / sum(add_p, na.rm = TRUE),
  fleet = sum(bev), inc_lo = min(income), inc_hi = max(income)
), keyby = group][, state := "VIC"][]
views <- list(all = lga_view(flow), private = lga_view(flow[private == TRUE]))
for (v in names(views)) views[[v]]$groups <- rbind(views[[v]]$groups, GV, fill = TRUE)

# ---- customer types (NSW, QLD new registrations) ----------------------------------
CT_ORDER <- unique(c(PRIVATE, unlist(CFG$nsw$customer_types), unlist(CFG$qld$customer_types)))
ct_rec <- recent_sum(flow, c("state", "customer"))
ct_cri <- win_sum(flow, cr0, cr1, c("state", "customer"))
ct_pyr <- win_sum(flow, py0, py1, c("state", "customer"))
cust <- merge(ct_rec, ct_cri[, .(state, customer, c_new = new, c_bev = bev)], by = c("state", "customer"))
cust <- merge(cust, ct_pyr[, .(state, customer, p_new = new, p_bev = bev)], by = c("state", "customer"))
cust[, `:=`(
  share = bev / new, share_c = c_bev / c_new, share_p = p_bev / p_new,
  of_new = new / sum(new), of_bev = bev / sum(bev)
), by = state]
cust[, `:=`(chg = (share_c - share_p) * 100, mult = share_c / share_p)]
# where each type's BEVs are registered: share held by its top 5 LGAs, and those LGAs' share of earners
ct_lga <- recent_sum(flow, c("state", "lga_name", "customer"))
earn <- lga[, .(state, lga_name, earners)]
conc <- ct_lga[bev > 0][order(-bev)][, .(
  top5 = paste(head(lga_name, 5), collapse = ", "), top5_bev = sum(head(bev, 5)) / sum(bev),
  top5_lgas = list(head(lga_name, 5))
), by = .(state, customer)]
conc[, top5_earners := mapply(function(st, l) earn[state == st & lga_name %chin% l, sum(earners)] / earn[state == st, sum(earners)], state, top5_lgas)]
conc[, top5_lgas := NULL]
cust <- merge(cust, conc, by = c("state", "customer"), all.x = TRUE)
cust[, customer := factor(customer, levels = CT_ORDER)]
setorder(cust, state, customer)
cust[, customer := as.character(customer)]
ct_lga[, group := lga$income_group[match(paste(state, lga_name), paste(lga$state, lga$lga_name))]]
ct_group <- ct_lga[!is.na(group), .(new = sum(new), bev = sum(bev)), keyby = .(state, customer, group)]
ct_group[, `:=`(share = bev / new, of_bev = bev / sum(bev)), by = .(state, group)]
ct_month <- flow[, .(new = sum(new), bev = sum(bev)), keyby = .(state, customer, month)][, share := bev / new][]
ct_top <- ct_lga[customer != PRIVATE & bev > 0][order(-bev), head(.SD, 15), by = state]
ct_top[, income := lga$median_income[match(paste(state, lga_name), paste(lga$state, lga$lga_name))]]

# ---- how the groups are built (method text + make-up of each group) -------------
pn <- copy(pc)[, nm := postcode_label(postcode, subs)]
group_method <- list(
  steps = income_group_method(),
  comp = rbind(
    income_group_composition(lga[state == "NSW"], "lga_name", "median_income", "earners")[, state := "NSW"],
    income_group_composition(lga[state == "QLD"], "lga_name", "median_income", "earners")[, state := "QLD"],
    income_group_composition(pn, "nm", "median_income", "individuals")[, state := "VIC"]
  ),
  lumpy = c(
    NSW = income_group_lumpiness(lga[state == "NSW"], "NSW", "lga_name", "earners"),
    QLD = income_group_lumpiness(lga[state == "QLD"], "QLD", "lga_name", "earners"),
    VIC = income_group_lumpiness(pn, "VIC", "nm", "individuals")
  )
)

# ---- state series and headline numbers ---------------------------------------------
vic_q <- vic[, .(per1000 = sum(bev) / sum(vehicles) * 1000), by = quarter]
kpi_vic <- V[, .(fleet = sum(bev, na.rm = TRUE), per1000veh = sum(bev, na.rm = TRUE) / sum(vehicles, na.rm = TRUE) * 1000)]
for (v in names(views)) views[[v]]$kpi$VIC <- kpi_vic

# ---- all states: BEV fleet (BITRE), solar, batteries, charging stations ---------------
X <- CFG$context
ctx <- fread(file.path(OUT, "lga_context.csv"), colClasses = list(character = "lga_code"))
cdates <- setNames(rd("context_dates.csv")$value, rd("context_dates.csv")$item)
BY <- as.integer(strsplit(cdates[["bitre_years"]], ",")[[1]])
y1 <- max(BY)
y0 <- y1 - 1L
ratio <- function(a, b, k = 1) fifelse(b > 0, a / b * k, NA_real_)
aus_measures <- function(d) {
  d[, `:=`(
    bitre_per1000 = ratio(bev1, lv1, 1000), bitre_add1000 = ratio(bev1 - bev0, lv0, 1000),
    solar_per100 = ratio(solar_n, dwellings, 100), solar_kw_dw = ratio(solar_kw, dwellings),
    bat_per1000 = ratio(battery_n, dwellings, 1000), bat_kwh_dw = ratio(battery_kwh, dwellings),
    chg_per10k = ratio(chg_sites, persons, 1e4), fast_per10k = ratio(chg_fast, persons, 1e4), bev_per_site = ratio(bev1, chg_sites)
  )]
}
AUS <- ctx[, .(
  id = lga_code, state, name = lga_name, income = median_income, earners, pop = persons, persons, dwellings, group = income_group,
  lv1 = get(paste0("lv_", y1)), lv0 = get(paste0("lv_", y0)), bev1 = get(paste0("bev_", y1)), bev0 = get(paste0("bev_", y0)),
  solar_n, solar_kw, battery_n, battery_kwh, chg_sites, chg_fast
)]
aus_measures(AUS)
AUS[, `:=`(elig_fleet = lv1 >= X$min_light_vehicles, elig_dw = dwellings >= X$min_dwellings, elig = lv1 >= X$min_light_vehicles)]
AUS[, `:=`(elig_site = elig_fleet & chg_sites > 0)]
AUS_STATES <- unname(unlist(X$states))
# yearly BEVs per 1,000 light vehicles (BITRE), per area and per state, for the detail chart
aus_series <- ctx[, c("lga_code", paste0("bev_", BY), paste0("lv_", BY)), with = FALSE]
aus_series <- aus_series[, c(list(id = lga_code), setNames(lapply(BY, function(y) ratio(get(paste0("bev_", y)), get(paste0("lv_", y)), 1000)), BY))]
aus_state_year <- rbindlist(lapply(BY, function(y) ctx[, .(year = y, per1000 = sum(get(paste0("bev_", y))) / sum(get(paste0("lv_", y))) * 1000), by = state]))

# group totals (pooled, like the registration groups), each state plus all of Australia
aus_group <- function(d) {
  g <- d[, .(
    lv1 = sum(lv1), lv0 = sum(lv0), bev1 = sum(bev1), bev0 = sum(bev0), solar_n = sum(solar_n), solar_kw = sum(solar_kw), battery_n = sum(battery_n),
    battery_kwh = sum(battery_kwh), chg_sites = sum(chg_sites), chg_fast = sum(chg_fast), persons = sum(persons), dwellings = sum(dwellings),
    inc_lo = min(income), inc_hi = max(income), areas = .N
  ), keyby = group]
  aus_measures(g)
  g[]
}
aus_groups <- rbind(
  rbindlist(lapply(AUS_STATES, function(s) aus_group(AUS[state == s])[, state := s])),
  aus_group(AUS)[, state := "AUS"]
)
# monthly solar and battery installations per 1,000 dwellings, by income group (Australia)
sbm <- fread(file.path(OUT, "solar_battery_lga_month.csv"), colClasses = list(character = "lga_code"))
sbm[AUS, `:=`(group = i.group, dwellings = i.dwellings), on = c(lga_code = "id")]
aus_month <- sbm[!is.na(group), .(solar_n = sum(solar_n), battery_n = sum(battery_n), dwellings = sum(dwellings)), keyby = .(group, month)]
aus_month[, `:=`(solar_1000 = solar_n / dwellings * 1000, battery_1000 = battery_n / dwellings * 1000)]
aus_month_total <- sbm[, .(solar_n = sum(solar_n), battery_n = sum(battery_n)), keyby = month]

# how each measure moves with the BEV fleet across council areas: Spearman rank correlation,
# raw and after taking out area income (ranks residualised on income rank), per state
spear <- function(x, y) suppressWarnings(cor(rank(x), rank(y)))
partial <- function(x, y, z) {
  rx <- resid(lm(rank(x) ~ rank(z)))
  ry <- resid(lm(rank(y) ~ rank(z)))
  suppressWarnings(cor(rx, ry))
}
CORR_VARS <- c(income = "Median income", solar_per100 = "Solar per 100 dwellings", bat_per1000 = "Batteries per 1,000 dwellings", chg_per10k = "Charging sites per 10,000 people")
aus_corr <- rbindlist(lapply(c("AUS", AUS_STATES), function(s) {
  d <- AUS[elig_fleet == TRUE & (s == "AUS" | state == s)]
  if (nrow(d) < 8) {
    return(NULL)
  }
  rbindlist(lapply(names(CORR_VARS), function(v) {
    data.table(state = s, var = v, label = CORR_VARS[[v]], n = nrow(d), r = spear(d$bitre_per1000, d[[v]]), r_inc = if (v == "income") NA_real_ else partial(d$bitre_per1000, d[[v]], d$income))
  }))
}))
# the same solar, battery, charger (and BITRE fleet) measures on the NSW + QLD and VIC postcode views
CTX_COLS <- c("bitre_per1000", "bitre_add1000", "solar_per100", "solar_kw_dw", "bat_per1000", "bat_kwh_dw", "chg_sites", "chg_fast", "chg_per10k", "bev_per_site", "elig_fleet", "elig_dw", "elig_site")
GRP_COLS <- setdiff(CTX_COLS, c("elig_fleet", "elig_dw", "elig_site"))
for (v in names(views)) {
  views[[v]]$lga <- merge(views[[v]]$lga, AUS[, c("id", CTX_COLS), with = FALSE], by = "id", all.x = TRUE)
  views[[v]]$groups <- merge(views[[v]]$groups, aus_groups[state %chin% c("NSW", "QLD"), c("state", "group", GRP_COLS), with = FALSE], by = c("state", "group"), all.x = TRUE)
}
pcx <- rd("vic_postcode_context.csv")
V[pcx, `:=`(
  persons = i.persons, dwellings = i.dwellings, solar_n = i.solar_n, solar_kw = i.solar_kw, battery_n = i.battery_n, battery_kwh = i.battery_kwh,
  chg_sites = i.chg_sites, chg_fast = i.chg_fast
), on = "postcode"]
vic_measures <- function(d) {
  d[, `:=`(
    solar_per100 = ratio(solar_n, dwellings, 100), solar_kw_dw = ratio(solar_kw, dwellings), bat_per1000 = ratio(battery_n, dwellings, 1000),
    bat_kwh_dw = ratio(battery_kwh, dwellings), chg_per10k = ratio(chg_sites, persons, 1e4), bev_per_site = ratio(bev, chg_sites)
  )]
}
vic_measures(V)
V[, `:=`(elig_dw = !is.na(dwellings) & dwellings >= X$min_dwellings, elig_site = !is.na(chg_sites) & chg_sites > 0 & !is.na(bev))]
gv_ctx <- V[!is.na(dwellings), .(
  solar_n = sum(solar_n), solar_kw = sum(solar_kw), battery_n = sum(battery_n), battery_kwh = sum(battery_kwh), chg_sites = sum(chg_sites),
  chg_fast = sum(chg_fast), persons = sum(persons), dwellings = sum(dwellings), bev = sum(bev, na.rm = TRUE)
), keyby = group]
vic_measures(gv_ctx)
gv_ctx[, state := "VIC"]
VG <- c("solar_per100", "solar_kw_dw", "bat_per1000", "bat_kwh_dw", "chg_sites", "chg_fast", "chg_per10k", "bev_per_site")
for (v in names(views)) views[[v]]$groups[gv_ctx, (VG) := mget(paste0("i.", VG)), on = c("state", "group")]
chargers <- fread(file.path(OUT, "chargers.csv"))[, .(lat, lon, name, operator, fast, capacity, state, lga_code = as.character(lga_code))]
# make-up of the council-area income groups in every state (the all-states view)
group_method$comp_lga <- rbindlist(lapply(AUS_STATES, function(s) income_group_composition(AUS[state == s, .(name, income, earners, income_group = group)], "name", "income", "earners")[, state := s]))
group_method$lumpy_lga <- vapply(setNames(AUS_STATES, AUS_STATES), function(s) income_group_lumpiness(AUS[state == s, .(name, earners, income_group = group)], s, "name", "earners"), "")
context <- list(
  areas = AUS, series = aus_series, state_year = aus_state_year, groups = aus_groups, month = aus_month, month_total = aus_month_total, corr = aus_corr,
  chargers = chargers, states = AUS_STATES, bitre_years = BY, y1 = y1, y0 = y0, cer_last = cdates[["cer_last_month"]],
  battery_first = cdates[["battery_first_month"]], osm_date = cdates[["osm_date"]]
)

# ---- geometry -------------------------------------------------------------------
sf_use_s2(FALSE)
geo_lga <- st_read(file.path(OUT, "lga_boundaries.geojson"), quiet = TRUE)[, c("lga_code_2023")]
names(geo_lga)[1] <- "id"
geo_poa <- st_read(file.path(OUT, "vic_postcode_boundaries.geojson"), quiet = TRUE)[, c("poa_code_2021")]
names(geo_poa)[1] <- "id"
geo_poa <- geo_poa[geo_poa$id %in% V$id, ]
geo_aus <- st_read(file.path(OUT, "australia_states.geojson"), quiet = TRUE)

# views$all / views$private: lga, lga_series, groups, state_month, kpi (the default, all customers, is also at the top level)
saveRDS(
  c(views$all, list(
    views = views, private_label = PRIVATE,
    cust = list(recent = cust, group = ct_group, month = ct_month, top = ct_top, order = CT_ORDER),
    vic = V, vic_series = V_series, vic_q = vic_q, aus = AUS, context = context,
    months = months, quarters = quarters, fuel = fuel[month >= months[1]], gaps = gaps[gaps >= months[1]],
    crisis = list(start = cr0, end = cr1, py_start = py0, py_end = py1, vic_quarter = ql),
    recent = list(start = int_to_ym(ym_to_int(lastm[["NSW"]]) - WIN + 1L), end = lastm[["NSW"]]),
    fleet_dates = list(NSW = max(snsw$month), QLD = max(sqld$month), VIC = ql), n_groups = NG, group_method = group_method, windows = analysis_windows(flow),
    geo_lga = geo_lga, geo_poa = geo_poa, geo_aus = geo_aus, workbook = CFG$paths$workbook
  )),
  file.path(OUT, "dashboard.rds")
)
logf("wrote %s", file.path(OUT, "dashboard.rds"))
