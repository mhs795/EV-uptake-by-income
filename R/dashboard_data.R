# Pre-compute everything the Shiny dashboard shows and save it as
# processed/dashboard.rds, so the app starts instantly.
#
# Run:  Rscript R/dashboard_data.R   (after build_data.R and fetch_boundaries.R)
# common.R sits next to this script: found via Rscript's --file, or source()'s ofile (RStudio's Source button)
source(file.path(local({ f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  if (!length(f)) f <- sys.frames()[[1]]$ofile
  if (length(f)) dirname(normalizePath(f)) else "R" }), "common.R"))
suppressPackageStartupMessages(library(sf))

NG <- CFG$income$n_groups
WIN <- CFG$period$recent_window_months
A <- CFG$analysis
rd <- function(f) fread(file.path(OUT, f))
k <- rd("suppression.csv"); kf <- function(t, f, cl = "all") k[table == t & fuel_group == f & customers == cl, k]
lga <- rd("lga_income.csv"); flow <- rd("flow_lga_month.csv"); snsw <- rd("stock_nsw_lga.csv"); sqld <- rd("stock_qld_proxy_lga.csv")
vic <- rd("vic_postcode_quarter.csv"); pc <- rd("postcode_income.csv"); fuel <- rd("fuel_prices.csv"); onset <- rd("fuel_crisis_onset.csv")
subs <- rd("vic_postcode_suburbs.csv")
for (x in names(CFG$map$postcode_name_overrides)) subs[postcode == as.integer(x), suburbs := CFG$map$postcode_name_overrides[[x]]]
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
cr0 <- onset$onset_month; cr1 <- min(lastm)
py0 <- int_to_ym(ym_to_int(cr0) - 12L); py1 <- int_to_ym(ym_to_int(cr1) - 12L)
months <- ym_seq(min(flow$month), max(flow$month))
win_sum <- function(d, a, b, by = c("state", "lga_name")) d[month >= a & month <= b, .(new = sum(new), bev = sum(bev)), by = by]
recent_sum <- function(d, by = c("state", "lga_name"))
  rbindlist(lapply(names(lastm), function(s) win_sum(d[state == s], int_to_ym(ym_to_int(lastm[[s]]) - WIN + 1L), lastm[[s]], by)))
fleet_nsw <- snsw[month == max(month), .(lga_name, fleet_bev = bev, fleet_veh = veh)]
fleet_qld <- sqld[month == max(month), .(lga_name, fleet_bev = bev_seen)]

gsum <- function(d, st) d[state == st, .(share = sum(bev) / sum(new), share_c = sum(c_bev) / sum(c_new), share_p = sum(p_bev) / sum(p_new),
                                        per1000veh = sum(fleet_bev, na.rm = TRUE) / sum(fleet_veh, na.rm = TRUE) * 1000,
                                        per1000pop = sum(fleet_bev, na.rm = TRUE) / sum(pop) * 1000, bev = sum(bev), c_bev = sum(c_bev),
                                        p_bev = sum(p_bev), fleet = sum(fleet_bev, na.rm = TRUE), inc_lo = min(income), inc_hi = max(income)),
                          keyby = group][, `:=`(chg = (share_c - share_p) * 100, mult = share_c / share_p)][, state := st][]

# ---- LGA areas (built for all customers and for private buyers only) -------------
lga_view <- function(fl) {
  rec <- recent_sum(fl); cri <- win_sum(fl, cr0, cr1); pyr <- win_sum(fl, py0, py1)
  L <- copy(lga)[, id := as.character(lga_code)]
  L <- merge(L, rec[, .(state, lga_name, new, bev)], by = c("state", "lga_name"), all.x = TRUE)
  L <- merge(L, cri[, .(state, lga_name, c_new = new, c_bev = bev)], by = c("state", "lga_name"), all.x = TRUE)
  L <- merge(L, pyr[, .(state, lga_name, p_new = new, p_bev = bev)], by = c("state", "lga_name"), all.x = TRUE)
  L <- merge(L, rbind(fleet_nsw[, state := "NSW"], fleet_qld[, `:=`(state = "QLD", fleet_veh = NA_real_)]), by = c("state", "lga_name"), all.x = TRUE)
  for (c in c("new", "bev", "c_new", "c_bev", "p_new", "p_bev")) set(L, which(is.na(L[[c]])), c, 0)
  L[, `:=`(share = fifelse(new > 0, bev / new, NA_real_), share_c = fifelse(c_new > 0, c_bev / c_new, NA_real_),
           share_p = fifelse(p_new > 0, p_bev / p_new, NA_real_))]
  L[, `:=`(chg = (share_c - share_p) * 100, mult = fifelse(share_p > 0, share_c / share_p, NA_real_),
           per1000veh = fifelse(fleet_veh > 0, fleet_bev / fleet_veh * 1000, NA_real_), per1000pop = fleet_bev / earners * 1000,
           elig = new >= A$min_new_regs_scatter, name = lga_name, income = median_income, pop = earners, group = income_group)]
  # monthly BEV share per LGA (for the selection chart)
  ms <- fl[, .(share = fifelse(sum(new) > 0, sum(bev) / sum(new), NA_real_)), by = .(state, lga_name, month)]
  L_series <- dcast(ms, state + lga_name ~ factor(month, levels = months), value.var = "share")
  G <- rbind(gsum(L, "NSW"), gsum(L, "QLD"), fill = TRUE)
  G[state == "QLD", per1000veh := NA]
  state_month <- fl[, .(new = sum(new), bev = sum(bev)), by = .(state, month)][, `:=`(share = bev / new, other = new - bev)][]
  kst <- function(st) L[state == st, .(bev = sum(bev), new = sum(new), share = sum(bev) / sum(new), c_bev = sum(c_bev), p_bev = sum(p_bev),
                                       share_c = sum(c_bev) / sum(c_new), share_p = sum(p_bev) / sum(p_new), fleet = sum(fleet_bev, na.rm = TRUE))]
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
V[, `:=`(name = fifelse(is.na(subs$suburbs[match(postcode, subs$postcode)]), paste("Postcode", postcode), subs$suburbs[match(postcode, subs$postcode)]),
         state = "VIC", income = median_income, pop = individuals, group = income_group,
         per1000veh = bev / vehicles * 1000, rshare = recent_bev / recent_vehicles, add_c = bev - b1, add_p = b4 - b5)]
V[, `:=`(add_c1000 = add_c / vehicles * 1000, add_p1000 = add_p / v4 * 1000,
         elig = !is.na(recent_vehicles) & recent_vehicles >= A$min_recent_vehicles_vic & individuals >= A$min_individuals_vic_postcode)]
vs <- vic[, .(per1000 = sum(bev) / sum(vehicles) * 1000), by = .(postcode, quarter)]
V_series <- dcast(vs, postcode ~ factor(quarter, levels = quarters), value.var = "per1000")

# ---- group aggregates (weighted, matching the workbook) --------------------------
GV <- V[!is.na(vehicles), .(per1000veh = sum(bev) / sum(vehicles) * 1000, rshare = sum(recent_bev) / sum(recent_vehicles),
                            add_c1000 = sum(add_c, na.rm = TRUE) / sum(vehicles) * 1000, mult = sum(add_c, na.rm = TRUE) / sum(add_p, na.rm = TRUE),
                            fleet = sum(bev), inc_lo = min(income), inc_hi = max(income)), keyby = group][, state := "VIC"][]
views <- list(all = lga_view(flow), private = lga_view(flow[private == TRUE]))
for (v in names(views)) views[[v]]$groups <- rbind(views[[v]]$groups, GV, fill = TRUE)

# ---- customer types (NSW, QLD new registrations) ----------------------------------
CT_ORDER <- unique(c(PRIVATE, unlist(CFG$nsw$customer_types), unlist(CFG$qld$customer_types)))
ct_rec <- recent_sum(flow, c("state", "customer"))
ct_cri <- win_sum(flow, cr0, cr1, c("state", "customer")); ct_pyr <- win_sum(flow, py0, py1, c("state", "customer"))
cust <- merge(ct_rec, ct_cri[, .(state, customer, c_new = new, c_bev = bev)], by = c("state", "customer"))
cust <- merge(cust, ct_pyr[, .(state, customer, p_new = new, p_bev = bev)], by = c("state", "customer"))
cust[, `:=`(share = bev / new, share_c = c_bev / c_new, share_p = p_bev / p_new,
            of_new = new / sum(new), of_bev = bev / sum(bev)), by = state]
cust[, `:=`(chg = (share_c - share_p) * 100, mult = share_c / share_p)]
# where each type's BEVs are registered: share held by its top 5 LGAs, and those LGAs' share of earners
ct_lga <- recent_sum(flow, c("state", "lga_name", "customer"))
earn <- lga[, .(state, lga_name, earners)]
conc <- ct_lga[bev > 0][order(-bev)][, .(top5 = paste(head(lga_name, 5), collapse = ", "), top5_bev = sum(head(bev, 5)) / sum(bev),
                                         top5_lgas = list(head(lga_name, 5))), by = .(state, customer)]
conc[, top5_earners := mapply(function(st, l) earn[state == st & lga_name %chin% l, sum(earners)] / earn[state == st, sum(earners)], state, top5_lgas)]
conc[, top5_lgas := NULL]
cust <- merge(cust, conc, by = c("state", "customer"), all.x = TRUE)
cust[, customer := factor(customer, levels = CT_ORDER)]; setorder(cust, state, customer); cust[, customer := as.character(customer)]
ct_group <- ct_lga[, group := lga$income_group[match(paste(state, lga_name), paste(lga$state, lga$lga_name))]][
  !is.na(group), .(new = sum(new), bev = sum(bev)), keyby = .(state, customer, group)]
ct_group[, `:=`(share = bev / new, of_bev = bev / sum(bev)), by = .(state, group)]
ct_month <- flow[, .(new = sum(new), bev = sum(bev)), keyby = .(state, customer, month)][, share := bev / new][]
ct_top <- ct_lga[customer != PRIVATE & bev > 0][order(-bev), head(.SD, 15), by = state][
  , income := lga$median_income[match(paste(state, lga_name), paste(lga$state, lga$lga_name))]][]

# ---- how the groups are built (method text + make-up of each group) -------------
pn <- copy(pc)[, nm := postcode_label(postcode, subs)]
group_method <- list(
  steps = income_group_method(),
  comp = rbind(income_group_composition(lga[state == "NSW"], "lga_name", "median_income", "earners")[, state := "NSW"],
               income_group_composition(lga[state == "QLD"], "lga_name", "median_income", "earners")[, state := "QLD"],
               income_group_composition(pn, "nm", "median_income", "individuals")[, state := "VIC"]),
  lumpy = c(NSW = income_group_lumpiness(lga[state == "NSW"], "NSW", "lga_name", "earners"),
            QLD = income_group_lumpiness(lga[state == "QLD"], "QLD", "lga_name", "earners"),
            VIC = income_group_lumpiness(pn, "VIC", "nm", "individuals")))

# ---- state series and headline numbers ---------------------------------------------
vic_q <- vic[, .(per1000 = sum(bev) / sum(vehicles) * 1000), by = quarter]
kpi_vic <- V[, .(fleet = sum(bev, na.rm = TRUE), per1000veh = sum(bev, na.rm = TRUE) / sum(vehicles, na.rm = TRUE) * 1000)]
for (v in names(views)) views[[v]]$kpi$VIC <- kpi_vic

# ---- geometry -------------------------------------------------------------------
sf_use_s2(FALSE)
geo_lga <- st_read(file.path(OUT, "lga_boundaries.geojson"), quiet = TRUE)[, c("lga_code_2023")]
names(geo_lga)[1] <- "id"
geo_poa <- st_read(file.path(OUT, "vic_postcode_boundaries.geojson"), quiet = TRUE)[, c("poa_code_2021")]
names(geo_poa)[1] <- "id"
geo_poa <- geo_poa[geo_poa$id %in% V$id, ]
geo_aus <- st_read(file.path(OUT, "australia_states.geojson"), quiet = TRUE)

# views$all / views$private: lga, lga_series, groups, state_month, kpi (the default, all customers, is also at the top level)
saveRDS(c(views$all, list(views = views, private_label = PRIVATE,
             cust = list(recent = cust, group = ct_group, month = ct_month, top = ct_top, order = CT_ORDER),
             vic = V, vic_series = V_series, vic_q = vic_q,
             months = months, quarters = quarters, fuel = fuel[month >= months[1]], gaps = gaps[gaps >= months[1]],
             crisis = list(start = cr0, end = cr1, py_start = py0, py_end = py1, vic_quarter = ql),
             recent = list(start = int_to_ym(ym_to_int(lastm[["NSW"]]) - WIN + 1L), end = lastm[["NSW"]]),
             fleet_dates = list(NSW = max(snsw$month), QLD = max(sqld$month), VIC = ql), n_groups = NG, group_method = group_method, windows = analysis_windows(flow),
             geo_lga = geo_lga, geo_poa = geo_poa, geo_aus = geo_aus, workbook = CFG$paths$workbook)),
        file.path(OUT, "dashboard.rds"))
logf("wrote %s", file.path(OUT, "dashboard.rds"))
