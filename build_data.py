"""Build the processed tables for the EV-uptake-by-income analysis.

Reads the raw state registration files and the ATO/ABS income tables listed in
config.yaml and writes tidy CSVs to processed/:

  lga_income.csv          NSW + QLD LGAs, ABS Personal Income 2022-23 + income group
  postcode_income.csv     VIC postcodes, ATO Taxation Statistics 2023-24 + income group
  flow_lga_month.csv      new private registrations by LGA and month (NSW, QLD),
                          exact counts plus number of suppressed "<=5" cells
  stock_nsw_lga.csv       NSW light-vehicle fleet by LGA at quarter ends
  stock_qld_proxy_lga.csv QLD BEVs seen in transactions since Jan 2022, at last known LGA
  vic_postcode_quarter.csv VIC fleet by postcode and quarter (stock + recent model years)
  suppression.csv         imputed value for NSW "<=5" cells, by fuel group

Run:  python3 build_data.py
"""
import glob
import re
import zipfile
from pathlib import Path

import numpy as np
import pandas as pd
import yaml

HERE = Path(__file__).resolve().parent
CFG = yaml.safe_load(open(HERE / "config.yaml"))
RAW = HERE / CFG["paths"]["raw"]
OUT = HERE / CFG["paths"]["processed"]
OUT.mkdir(exist_ok=True)

FUEL_GROUPS = ["bev", "phev", "other"]


def log(*a):
    print(*a, flush=True)


ADJ = []


def adj(dataset, step, detail, affected=""):
    """Record one data adjustment for the workbook's Data_Adjustments sheet."""
    ADJ.append({"dataset": dataset, "adjustment": step, "detail": detail, "affected": affected})


# ---------------------------------------------------------------- income ----
def income_groups(df, value_col, weight_col, n):
    """Earner-weighted income groups: sort by income, cut the cumulative
    earner share into n equal slices. 1 = lowest income."""
    df = df.sort_values(value_col).copy()
    cum = df[weight_col].cumsum() / df[weight_col].sum()
    # midpoint of each area's slice decides its group, so no group is empty
    mid = cum - df[weight_col] / df[weight_col].sum() / 2
    df["income_group"] = np.minimum((mid * n).astype(int) + 1, n)
    return df


def load_lga_income():
    c = CFG["income"]
    d = pd.read_excel(RAW / c["lga_file"], sheet_name=c["lga_sheet"], header=c["lga_header_rows"])
    d.columns = [b if str(a).startswith("Unnamed") else f"{a}|{b}" for a, b in d.columns]
    d = d[pd.to_numeric(d["LGA"], errors="coerce").notna()].copy()
    d["lga_code"] = d["LGA"].astype(int).astype(str)
    d["state"] = d["lga_code"].str[0].map({"1": "NSW", "3": "QLD"})
    d = d[d["state"].notna()]
    out = pd.DataFrame({
        "state": d["state"],
        "lga_code": d["lga_code"],
        "lga_name": d["LGA NAME"],
        "median_income": pd.to_numeric(d[f"{c['lga_measure']}|{c['lga_year']}"], errors="coerce"),
        "earners": pd.to_numeric(d[f"{c['lga_weight']}|{c['lga_year']}"], errors="coerce"),
    }).dropna()
    unin = out["lga_name"].str.startswith("Unincorporated")
    adj("ABS LGA income", "Dropped unincorporated areas", "No council area to match registrations to",
        f"{int(unin.sum())} areas")
    out = out[~unin]
    adj("ABS LGA income", "Earner-weighted income groups",
        f"LGAs ranked by median total income {c['lga_year']} and cut into {c['n_groups']} groups each holding ~1/{c['n_groups']} "
        "of the state's earners (within state)", f"{len(out)} LGAs")
    parts = [income_groups(g, "median_income", "earners", c["n_groups"]) for _, g in out.groupby("state")]
    return pd.concat(parts).sort_values(["state", "lga_name"])


def load_postcode_income():
    c = CFG["income"]
    d = pd.read_excel(RAW / c["postcode_file"], sheet_name=c["postcode_sheet"], header=c["postcode_header_row"])
    d.columns = [re.sub(r"\s+", " ", str(x)).strip() for x in d.columns]
    yr = c["postcode_year"]
    med = next(x for x in d.columns if x.startswith("Median") and yr in x)
    ind = next(x for x in d.columns if x.startswith("Individuals") and yr in x)
    st = next(x for x in d.columns if x.startswith("State"))
    pc = next(x for x in d.columns if x.startswith("Postcode"))
    d = d[d[st] == "VIC"]
    out = pd.DataFrame({
        "postcode": pd.to_numeric(d[pc], errors="coerce"),
        "median_income": pd.to_numeric(d[med], errors="coerce"),
        "individuals": pd.to_numeric(d[ind], errors="coerce"),
    }).dropna()
    out["postcode"] = out["postcode"].astype(int)
    v = CFG["vic"]
    out = out[out["postcode"].between(v["postcode_min"], v["postcode_max"])]
    adj("ATO postcode income", "Kept VIC postcodes only",
        f"State = VIC and postcode {v['postcode_min']}-{v['postcode_max']} (drops PO-box-only codes outside the range); "
        f"ATO omits postcodes with too few individuals", f"{len(out)} postcodes")
    adj("ATO postcode income", "Earner-weighted income groups",
        f"Postcodes ranked by median taxable income {yr} and cut into {c['n_groups']} groups weighted by individuals", f"{len(out)} postcodes")
    return income_groups(out, "median_income", "individuals", c["n_groups"]).sort_values("postcode")


# ------------------------------------------------------------ NSW flows ----
def fuel_group(labels, bev, phev):
    return np.select([labels.isin(bev), labels.isin(phev)], ["bev", "phev"], "other")


def load_nsw_transactions():
    n = CFG["nsw"]
    frames = []
    for z in sorted(glob.glob(str(RAW / n["transactions_glob"]))):
        zf = zipfile.ZipFile(z)
        for m in sorted(zf.namelist()):
            d = pd.read_csv(zf.open(m), sep=n["sep"], dtype=str)
            d["month"] = pd.Period(m.split("_")[-2], "M")
            frames.append(d)
    d = pd.concat(frames, ignore_index=True)
    log(f"NSW transactions: {len(d):,} rows, {d.month.min()} to {d.month.max()}")
    return d


def nsw_flows(tx, lga_lookup):
    n = CFG["nsw"]
    is_new = tx["VEHICLE REGISTRATION TRANSACTION TYPE"] == n["new_transaction"]
    adj("NSW new registrations", "Kept new-vehicle registrations only",
        f"Transaction type '{n['new_transaction']}'; transfers and second-hand re-registrations dropped",
        f"{int(is_new.sum()):,} of {len(tx):,} rows kept")
    priv = is_new & tx["CUSTOMER TYPE"].isin(n["private_customer_types"])
    adj("NSW new registrations", "Private customers only",
        f"Customer type in {n['private_customer_types']}; Business, Dealer (demonstrators) and Government dropped",
        f"{int((is_new & ~priv).sum()):,} rows dropped")
    bad = priv & tx["FUEL TYPE"].isin(n["exclude_fuel_labels"])
    unm = tx[bad & (tx["FUEL TYPE"] == "Other/Unmapped")].groupby("month").size()
    min_block = CFG["nsw"]["unmapped_block_min_rows"]
    pd.DataFrame({"month": [str(m) for m in unm[unm > min_block].index]}).to_csv(OUT / "nsw_unmapped_months.csv", index=False)
    adj("NSW new registrations", "Dropped rows with no usable fuel type",
        f"Fuel type in {n['exclude_fuel_labels']} (trailers/caravans and unmapped rows). A block of 'Other/Unmapped' rows "
        f"(manufacturer also unmapped) appears in {', '.join(str(m) for m in unm[unm > min_block].index)}; assumed spread across fuels like mapped rows",
        f"{int(bad.sum()):,} rows dropped")
    adj("NSW new registrations", "Harmonised fuel labels",
        f"TfNSW changed labels twice. BEV = {n['bev_labels']}; PHEV = {n['phev_labels']}; everything else = other", "")
    d = tx[priv & ~bad].copy()
    d["fuel"] = fuel_group(d["FUEL TYPE"], n["bev_labels"], n["phev_labels"])
    d["supp"] = (d["COUNT"] == n["suppressed_token"]).astype(int)
    d["exact"] = pd.to_numeric(d["COUNT"], errors="coerce").fillna(0)
    d["lga_name"] = d["CUSTOMER ADDRESS LGA"].replace(n["lga_aliases"])
    alias = d["CUSTOMER ADDRESS LGA"].isin(n["lga_aliases"].keys())
    adj("NSW new registrations", "LGA names mapped to ABS",
        f"Aliases {n['lga_aliases']}", f"{int(alias.sum()):,} rows relabelled")
    miss = ~d["lga_name"].isin(lga_lookup)
    adj("NSW new registrations", "Dropped rows with no matchable LGA",
        f"Labels: {sorted(d.loc[miss, 'lga_name'].unique())}", f"{int(miss.sum()):,} rows dropped")
    d = d[~miss]
    adj("NSW new registrations", "Suppressed counts imputed",
        f"Counts of 5 or fewer are published as '{n['suppressed_token']}'; each such cell is valued at an estimated mean "
        "(see Suppression sheet; editable on Inputs)",
        f"{int(d['supp'].sum()):,} of {len(d):,} rows ({d['supp'].mean():.1%}) suppressed")
    g = d.groupby(["lga_name", "month", "fuel"])[["exact", "supp"]].sum().unstack("fuel", fill_value=0)
    g.columns = [f"{b}_{a}" for a, b in g.columns]
    g = g.reindex(columns=[f"{f}_{m}" for f in FUEL_GROUPS for m in ("exact", "supp")], fill_value=0)
    g = g.reset_index()
    g.insert(0, "state", "NSW")
    return g, d


# -------------------------------------------------------------- QLD ----
def load_qld():
    q = CFG["qld"]
    cols = ["RECORD_DATE", "OPEN_DATA_VEHICLE_IDENTIFIER", "TRANSACTION_TYPE", "CUSTOMER_TYPE",
            "LGA_NAME", "MAKE", "COLOUR", "FUEL_TYPE", "YEAR_OF_MANUFACTURE"]
    d = pd.concat([pd.read_csv(f, usecols=cols, dtype=str) for f in sorted(glob.glob(str(RAW / q["glob"])))],
                  ignore_index=True)
    before = len(d)
    d = d.drop_duplicates().reset_index(drop=True)
    log(f"QLD transactions: {before:,} rows, {before - len(d):,} exact duplicates dropped")
    adj("QLD registrations", "Dropped exact duplicate records", "Identical on every field read", f"{before - len(d):,} of {before:,} rows")
    d["date"] = pd.to_datetime(d["RECORD_DATE"])
    d["month"] = d["date"].dt.to_period("M")
    d["lga_name"] = d["LGA_NAME"].str.replace(q["lga_suffix_regex"], "", regex=True).str.strip()
    adj("QLD registrations", "LGA names mapped to ABS", "Stripped council-type suffix, e.g. 'Brisbane (C)' -> 'Brisbane'", "all rows")
    d["fuel"] = fuel_group(d["FUEL_TYPE"], q["bev_labels"], q["phev_labels"])
    adj("QLD registrations", "Fuel groups", f"BEV = {q['bev_labels']}. 'Petrol And Electric' mixes HEV and PHEV so it stays in 'other'", "")
    return d


def qld_new_private(d):
    q = CFG["qld"]
    yom = pd.to_numeric(d["YEAR_OF_MANUFACTURE"], errors="coerce")
    new = d["TRANSACTION_TYPE"] == q["new_transaction"]
    priv = new & d["CUSTOMER_TYPE"].isin(q["private_customer_types"])
    young = priv & (yom >= d["date"].dt.year - q["max_new_vehicle_age_years"])
    adj("QLD registrations", "Kept 'Registration New' only", "Transfers dropped", f"{int(new.sum()):,} of {len(d):,} rows kept")
    adj("QLD registrations", "Private customers only", f"Customer type {q['private_customer_types']}; Organisation dropped",
        f"{int((new & ~priv).sum()):,} rows dropped")
    adj("QLD registrations", "Dropped re-registrations of older vehicles",
        f"'Registration New' also covers used vehicles coming back on the register; kept only year of manufacture >= "
        f"registration year - {q['max_new_vehicle_age_years']}", f"{int((priv & ~young).sum()):,} rows dropped")
    return d[young]


def qld_last_complete_month(d):
    last = d["date"].max()
    tol = pd.Timedelta(days=CFG["qld"]["month_complete_tolerance_days"])
    p = last.to_period("M")
    return p if last >= p.to_timestamp(how="end").normalize() - tol else p - 1


def qld_flows(new, lga_lookup, last_month):
    part = new["month"] > last_month
    adj("QLD registrations", "Dropped incomplete latest month",
        f"Extract ends {new['date'].max():%d %b %Y}; months after {last_month} are partial", f"{int(part.sum()):,} rows")
    new = new[new["lga_name"].isin(lga_lookup) & ~part]
    g = new.groupby(["lga_name", "month", "fuel"]).size().unstack("fuel", fill_value=0)
    out = pd.DataFrame(index=g.index)
    for f in FUEL_GROUPS:
        out[f"{f}_exact"] = g[f] if f in g else 0
        out[f"{f}_supp"] = 0
    out = out.reset_index()
    out.insert(0, "state", "QLD")
    return out


def qld_stock_proxy(d, lga_lookup):
    """BEVs seen in any QLD transaction since the data start, placed at the LGA
    of their latest transaction up to each month end."""
    b = d[d["fuel"] == "bev"].sort_values("date")
    months = pd.period_range(b["month"].min(), b["month"].max(), freq="M")
    rows = []
    for m in months:
        last = b[b["month"] <= m].groupby("OPEN_DATA_VEHICLE_IDENTIFIER").tail(1)
        c = last.groupby("lga_name").size()
        rows.append(pd.DataFrame({"lga_name": c.index, "month": m, "bev_seen": c.values}))
    out = pd.concat(rows)
    adj("QLD fleet (proxy)", "Built a BEV stock proxy from transactions",
        "QLD publishes no current fleet-by-fuel data by region. Each BEV appearing in any new or transfer record since "
        f"{b['month'].min()} is placed at the LGA of its latest record up to each month. Misses pre-2022 BEVs never transferred; "
        "keeps BEVs later written off or moved interstate", f"{b['OPEN_DATA_VEHICLE_IDENTIFIER'].nunique():,} distinct BEVs")
    return out[out["lga_name"].isin(lga_lookup)]


# ------------------------------------------------ suppression estimate ----
def estimate_suppression(nsw_new_rows, qld_new):
    """Mean size of a '<=5' cell at NSW's grain, by fuel group.

    NSW cross-classifies each new registration by LGA x make x fuel x colour x
    gender x age group x transfer type before suppressing counts <=5. QLD
    publishes unit records with LGA, make, fuel and colour but no gender/age.
    We give each QLD vehicle a synthetic gender x age group drawn from the NSW
    private new-vehicle mix for its fuel group, aggregate to NSW's grain, and
    take the mean of the cells that NSW would have suppressed."""
    n = CFG["nsw"]
    rng = np.random.default_rng(n["suppression_seed"])
    mix = (nsw_new_rows.groupby(["fuel", "GENDER", "AGE GROUP"]).size()
           .groupby(level="fuel").transform(lambda s: s / s.sum()))
    q = qld_new.copy()
    q["GENDER"] = ""
    q["AGE GROUP"] = ""
    for f in FUEL_GROUPS:
        idx = q.index[q["fuel"] == f]
        p = mix.loc[f]
        pick = rng.choice(len(p), size=len(idx), p=p.values)
        q.loc[idx, "GENDER"] = p.index.get_level_values("GENDER")[pick]
        q.loc[idx, "AGE GROUP"] = p.index.get_level_values("AGE GROUP")[pick]
    token_max = int(re.sub(r"\D", "", n["suppressed_token"]))
    cells = q.groupby(["month", "lga_name", "MAKE", "fuel", "COLOUR", "GENDER", "AGE GROUP"]).size()
    small = cells[cells <= token_max]
    est = small.groupby(level="fuel").mean()
    share = (cells <= token_max).groupby(level="fuel").mean()
    out = pd.DataFrame({"fuel_group": est.index, "mean_suppressed_cell": est.values.round(3),
                        "share_of_cells_suppressed": share.reindex(est.index).values.round(3)})
    log("Suppressed-cell estimate:\n" + out.to_string(index=False))
    return out


def calibrate_stock_suppression(tx, statewide, register, k_flow_bev):
    """Imputed value for '<=5' cells in the NSW fleet snapshot (see config)."""
    n = CFG["nsw"]
    first, last = statewide.index.min(), statewide.index.max()
    b = tx[(tx["VEHICLE REGISTRATION TRANSACTION TYPE"] == n["new_transaction"])
           & tx["FUEL TYPE"].isin(n["bev_labels"])
           & (tx["month"] > first) & (tx["month"] <= last)]
    new_bev = (pd.to_numeric(b["COUNT"], errors="coerce").sum()
               + k_flow_bev * (b["COUNT"] == n["suppressed_token"]).sum())
    d_exact = statewide.loc[last, "bev_exact"] - statewide.loc[first, "bev_exact"]
    d_supp = statewide.loc[last, "bev_supp"] - statewide.loc[first, "bev_supp"]
    k_bev = (new_bev - d_exact) / d_supp

    z = zipfile.ZipFile(RAW / n["age_snapshot_file"])
    tag = n["age_calibration_month"].replace("-", "")
    a = pd.concat([pd.read_csv(z.open(m), sep=n["sep"], dtype=str) for m in z.namelist() if f"_{tag}_" in m])
    age_total = (pd.to_numeric(a["COUNT"], errors="coerce").sum()
                 + n["age_file_suppressed_value"] * (a["COUNT"] == n["suppressed_token"]).sum())
    k_other = (age_total - register["exact"]) / register["supp"]
    log(f"Stock calibration: new BEV {first}..{last} = {new_bev:,.0f}; BEV exact growth {d_exact:,.0f}; "
        f"suppressed BEV cells growth {d_supp:,} -> k_bev {k_bev:.3f}")
    log(f"  register {tag}: age-file total {age_total:,.0f}, detailed exact {register['exact']:,.0f}, "
        f"suppressed cells {register['supp']:,} -> k_other {k_other:.3f}")
    return k_bev, k_other


# ------------------------------------------------------------ NSW stock ----
def nsw_stock(lga_lookup):
    n = CFG["nsw"]
    files = []
    for z in sorted(glob.glob(str(RAW / n["snapshot_glob"]))):
        zf = zipfile.ZipFile(z)
        for m in zf.namelist():
            files.append((z, m, pd.Period(m.split("_")[-2], "M")))
    latest = max(p for _, _, p in files)
    calib = pd.Period(n["age_calibration_month"], "M")
    keep = {p for _, _, p in files if p.month in n["snapshot_months_of_year"]} | {latest, calib}
    rows = []
    register = {"exact": 0.0, "supp": 0}
    for z, m, p in sorted(files, key=lambda x: (x[2], x[1])):
        if p not in keep:
            continue
        d = pd.read_csv(zipfile.ZipFile(z).open(m), sep=n["sep"], dtype=str,
                        usecols=["VEHICLE TYPE", "MOTIVE POWER", "CUSTOMER ADDRESS LGA", "COUNT"])
        if p == calib:
            register["exact"] += pd.to_numeric(d["COUNT"], errors="coerce").sum()
            register["supp"] += int((d["COUNT"] == n["suppressed_token"]).sum())
        d = d[d["VEHICLE TYPE"].isin(n["snapshot_vehicle_types"])
              & ~d["MOTIVE POWER"].isin(n["snapshot_exclude_power"])]
        if d.empty:
            continue
        d["fuel"] = fuel_group(d["MOTIVE POWER"], n["snapshot_bev_labels"], n["snapshot_phev_labels"])
        d["supp"] = (d["COUNT"] == n["suppressed_token"]).astype(int)
        d["exact"] = pd.to_numeric(d["COUNT"], errors="coerce").fillna(0)
        d["lga_name"] = d["CUSTOMER ADDRESS LGA"].replace(n["lga_aliases"])
        g = d.groupby(["lga_name", "fuel"])[["exact", "supp"]].sum()
        g["month"] = p
        rows.append(g.reset_index())
        log(f"  NSW snapshot {m}: {len(d):,} light-vehicle rows")
    adj("NSW fleet", "Light vehicles only",
        f"Vehicle types {n['snapshot_vehicle_types']}; trailers, motorcycles, plant, buses and medium/heavy trucks excluded; "
        f"motive power {n['snapshot_exclude_power']} excluded", "")
    adj("NSW fleet", "Quarter-end snapshots", f"Months {n['snapshot_months_of_year']} plus the latest; {len(rows)} snapshot files read",
        f"{len(keep)} snapshot months")
    adj("NSW fleet", "Suppressed counts imputed", "BEV and other-fuel cells valued by calibration (see Suppression sheet)", "")
    s = pd.concat(rows).groupby(["lga_name", "month", "fuel"])[["exact", "supp"]].sum().unstack("fuel", fill_value=0)
    s.columns = [f"{b}_{a}" for a, b in s.columns]
    s = s.reindex(columns=[f"{f}_{m}" for f in FUEL_GROUPS for m in ("exact", "supp")], fill_value=0).reset_index()
    statewide = s.groupby("month")[["bev_exact", "bev_supp"]].sum()
    return s[s["lga_name"].isin(lga_lookup)], statewide, register


# -------------------------------------------------------------- VIC ----
def vic_quarters(pc_lookup):
    v = CFG["vic"]
    rows = []
    for f in sorted(glob.glob(str(RAW / v["glob"]))):
        q, y = re.search(r"_q(\d)_(\d{4})\.csv$", f).groups()
        period = pd.Period(f"{y}Q{q}", "Q")
        d = pd.read_csv(f, dtype=str, encoding="utf-8-sig")
        d.columns = [c.strip() for c in d.columns]
        d = d[d["CD_CLASS_VEH"].str.strip().isin(v["vehicle_classes"])]
        d["fuel_code"] = d["CD_CL_FUEL_ENG"].fillna("").str.strip()
        d = d[~d["fuel_code"].isin(v["exclude_fuel_codes"])]
        d["n"] = pd.to_numeric(d["TOTAL1"], errors="coerce").fillna(0)
        d["postcode"] = pd.to_numeric(d["POSTCODE"], errors="coerce")
        d["recent"] = pd.to_numeric(d["NB_YEAR_MFC_VEH"], errors="coerce") >= period.year - v["recent_model_year_lag"]
        d["bev"] = d["fuel_code"] == v["bev_code"]
        d["hyb"] = d["fuel_code"] == v["hybrid_code"]
        g = d.assign(
            vehicles=d["n"],
            bev=d["n"] * d["bev"],
            hybrid=d["n"] * d["hyb"],
            recent_vehicles=d["n"] * d["recent"],
            recent_bev=d["n"] * (d["recent"] & d["bev"]),
        ).groupby("postcode")[["vehicles", "bev", "hybrid", "recent_vehicles", "recent_bev"]].sum().reset_index()
        g["quarter"] = str(period)
        rows.append(g)
        log(f"  VIC snapshot {period}: {int(g.vehicles.sum()):,} vehicles, {int(g.bev.sum()):,} BEV")
    out = pd.concat(rows)
    out["postcode"] = out["postcode"].astype(int)
    keep = out["postcode"].isin(pc_lookup)
    adj("VIC fleet", "Vehicle class and fuel filter",
        f"Class {v['vehicle_classes']} (motor vehicles; motorcycles excluded); blank fuel code excluded. BEV = code "
        f"'{v['bev_code']}'; code '{v['hybrid_code']}' = hybrids (HEV+PHEV, not separable)", "")
    adj("VIC fleet", "Recent-model proxy for new take-up",
        f"VIC's monthly new-registration file has no location, so 'recent-model' vehicles (year of manufacture >= snapshot year - "
        f"{v['recent_model_year_lag']}) in each quarterly snapshot stand in for recent new-vehicle take-up", "")
    lost = out.loc[~keep & (out["quarter"] == out["quarter"].max()), "vehicles"].sum()
    adj("VIC fleet", "Dropped postcodes with no ATO income",
        "Postcodes absent from ATO Table 8 (too few taxpayers) or outside the VIC range",
        f"{int((~keep).sum()):,} postcode-quarters; {int(lost):,} vehicles in latest quarter")
    return out[keep].sort_values(["postcode", "quarter"])


# ------------------------------------------------------------- fuel ----
def fuel_prices():
    f = CFG["fuel"]
    r = pd.read_csv(f["retail_file"], usecols=["month"] + f["retail_series"])
    t = pd.read_csv(f["tgp_file"], usecols=["month"] + f["tgp_series"])
    d = r.merge(t, on="month", how="outer").sort_values("month")
    d = d[d[f["retail_series"]].notna().all(axis=1)]
    s = d.set_index("month")[f["onset_series"]]
    trail = s.shift(1).rolling(f["onset_trailing_months"]).mean()
    rise = s / trail - 1
    cand = rise[(rise >= f["onset_threshold"]) & (rise.index >= f["onset_search_from"])]
    onset = cand.index[0]
    adj("Fuel prices", "Crisis onset detected from prices",
        f"First month from {f['onset_search_from']} where {f['onset_series']} is >= {f['onset_threshold']:.0%} above its trailing "
        f"{f['onset_trailing_months']}-month mean", f"onset {onset}")
    log(f"Fuel crisis onset: {onset} ({f['onset_series']} {s[onset]:.1f} c/L, "
        f"{rise[onset]:.0%} above trailing {f['onset_trailing_months']}-month mean {trail[onset]:.1f})")
    return d, onset, float(s[onset]), float(trail[onset])


# ------------------------------------------------------------------ main ----
def main():
    lga = load_lga_income()
    lga.to_csv(OUT / "lga_income.csv", index=False)
    log(f"LGA income: {lga.groupby('state').size().to_dict()}")
    pc = load_postcode_income()
    pc.to_csv(OUT / "postcode_income.csv", index=False)
    log(f"VIC postcodes with ATO income: {len(pc)}")

    start = pd.Period(CFG["period"]["start_month"], "M")

    fuel, onset, onset_px, trail_px = fuel_prices()
    fuel[fuel["month"] >= str(start)].to_csv(OUT / "fuel_prices.csv", index=False)
    pd.DataFrame([{"onset_month": onset, "onset_series": CFG["fuel"]["onset_series"], "onset_price": round(onset_px, 1),
                   "trailing_mean": round(trail_px, 1), "threshold": CFG["fuel"]["onset_threshold"]}]
                 ).to_csv(OUT / "fuel_crisis_onset.csv", index=False)

    tx = load_nsw_transactions()
    nsw_lgas = set(lga.loc[lga.state == "NSW", "lga_name"])
    nsw_f, nsw_rows = nsw_flows(tx, nsw_lgas)
    unmatched = set(tx["CUSTOMER ADDRESS LGA"].replace(CFG["nsw"]["lga_aliases"])) - nsw_lgas
    log(f"NSW LGA labels not matched to ABS (dropped): {sorted(unmatched)}")

    q = load_qld()
    qld_lgas = set(lga.loc[lga.state == "QLD", "lga_name"])
    log(f"QLD LGA labels not matched to ABS (dropped): {sorted(set(q.lga_name.dropna()) - qld_lgas)}")
    qnew = qld_new_private(q)
    q_last = qld_last_complete_month(q)
    log(f"QLD last complete month: {q_last}")
    qld_f = qld_flows(qnew, qld_lgas, q_last)

    supp = estimate_suppression(nsw_rows, qnew[qnew["lga_name"].isin(qld_lgas)])
    k_flow = dict(zip(supp.fuel_group, supp.mean_suppressed_cell))
    k_flow["phev"] = k_flow[CFG["nsw"]["phev_suppression_from"]]

    flows = pd.concat([nsw_f, qld_f])
    adj("Both states", "Analysis window", f"New-registration analysis starts {start} (NSW data begin Jul 2022, QLD Jan 2022)", "")
    flows = flows[flows["month"] >= start]
    flows["month"] = flows["month"].astype(str)
    flows.to_csv(OUT / "flow_lga_month.csv", index=False)
    log(f"Flows: {len(flows):,} LGA-months, {flows.month.min()} to {flows.month.max()}")

    qs = qld_stock_proxy(q[q["month"] <= q_last], qld_lgas)
    qs["month"] = qs["month"].astype(str)
    qs.to_csv(OUT / "stock_qld_proxy_lga.csv", index=False)
    del q

    ns, statewide, register = nsw_stock(nsw_lgas)
    ns["month"] = ns["month"].astype(str)
    ns.to_csv(OUT / "stock_nsw_lga.csv", index=False)
    k_bev, k_other = calibrate_stock_suppression(tx, statewide, register, k_flow["bev"])
    del tx
    k_stock = {"bev": k_bev, "other": k_other}
    k_stock["phev"] = k_stock[CFG["nsw"]["phev_suppression_from"]]
    method = {
        ("flow", "bev"): "QLD unit records re-cut at NSW grain (synthetic gender x age from NSW mix)",
        ("flow", "other"): "QLD unit records re-cut at NSW grain (synthetic gender x age from NSW mix)",
        ("flow", "phev"): "Borrowed from flow BEV (QLD has no PHEV label)",
        ("stock", "bev"): "NSW BEV fleet growth = new BEV registrations over the same period",
        ("stock", "other"): "Detailed snapshot total = coarse age-of-vehicles snapshot total",
        ("stock", "phev"): "Borrowed from stock BEV",
    }
    pd.DataFrame([{"table": t, "fuel_group": f, "k": round(float(k[f]), 3), "method": method[(t, f)]}
                  for t, k in (("flow", k_flow), ("stock", k_stock)) for f in FUEL_GROUPS]
                 ).to_csv(OUT / "suppression.csv", index=False)

    vq = vic_quarters(set(pc.postcode))
    vq.to_csv(OUT / "vic_postcode_quarter.csv", index=False)
    pd.DataFrame(ADJ).to_csv(OUT / "adjustments.csv", index=False)
    log("done")


if __name__ == "__main__":
    main()
