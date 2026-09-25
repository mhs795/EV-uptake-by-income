"""Write EV_income_map.html — a self-contained interactive map dashboard.

All data and boundaries are embedded in the page; it opens straight from disk.
Leaflet (cdnjs) and CARTO basemap tiles are fetched when online; without them
the choropleth still draws on a plain background.

Run:  python3 build_dashboard.py   (after build_data.py and fetch_boundaries.py)
"""
import json
from pathlib import Path

import numpy as np
import pandas as pd
import yaml

HERE = Path(__file__).resolve().parent
CFG = yaml.safe_load(open(HERE / "config.yaml"))
P = HERE / CFG["paths"]["processed"]
NG = CFG["income"]["n_groups"]
WIN = CFG["period"]["recent_window_months"]
A = CFG["analysis"]


def r(x, d=4):
    return None if x is None or (isinstance(x, float) and not np.isfinite(x)) else round(float(x), d)


# ---------------------------------------------------------------- inputs ----
k = pd.read_csv(P / "suppression.csv").set_index(["table", "fuel_group"])["k"]
lga = pd.read_csv(P / "lga_income.csv")
flow = pd.read_csv(P / "flow_lga_month.csv")
snsw = pd.read_csv(P / "stock_nsw_lga.csv")
sqld = pd.read_csv(P / "stock_qld_proxy_lga.csv")
vic = pd.read_csv(P / "vic_postcode_quarter.csv")
pc = pd.read_csv(P / "postcode_income.csv")
fuel = pd.read_csv(P / "fuel_prices.csv")
onset = pd.read_csv(P / "fuel_crisis_onset.csv").iloc[0]
subs = dict(pd.read_csv(P / "vic_postcode_suburbs.csv").values)
subs.update({int(a): b for a, b in CFG["map"]["postcode_name_overrides"].items()})

for f in ("bev", "phev", "other"):
    flow[f] = flow[f"{f}_exact"] + k[("flow", f)] * flow[f"{f}_supp"]
    snsw[f] = snsw[f"{f}_exact"] + k[("stock", f)] * snsw[f"{f}_supp"]
flow["new"] = flow.bev + flow.phev + flow.other
snsw["veh"] = snsw.bev + snsw.phev + snsw.other
flow["month"] = pd.PeriodIndex(flow.month, freq="M")

last = {s: flow.loc[flow.state == s, "month"].max() for s in ("NSW", "QLD")}
cr0 = pd.Period(onset.onset_month, "M")
cr1 = min(last.values())
py0, py1 = cr0 - 12, cr1 - 12
months = pd.period_range(flow.month.min(), flow.month.max(), freq="M")


def window(df, a, b):
    return df[(df.month >= a) & (df.month <= b)]


def sums(df, keys):
    return df.groupby(keys)[["new", "bev"]].sum()


rec = pd.concat([sums(window(flow[flow.state == s], last[s] - (WIN - 1), last[s]), ["state", "lga_name"]) for s in last])
cri = sums(window(flow, cr0, cr1), ["state", "lga_name"])
pyr = sums(window(flow, py0, py1), ["state", "lga_name"])
nsw_last = snsw.month.max()
qld_last = sqld.month.max()
fleet_nsw = snsw[snsw.month == nsw_last].set_index("lga_name")
fleet_qld = sqld[sqld.month == qld_last].set_index("lga_name")
monthly = flow.groupby(["state", "lga_name", "month"])[["new", "bev"]].sum()

areas = []
for x in lga.itertuples(index=False):
    key = (x.state, x.lga_name)
    n, b = rec.new.get(key, 0), rec.bev.get(key, 0)
    cn, cb = cri.new.get(key, 0), cri.bev.get(key, 0)
    pn, pb = pyr.new.get(key, 0), pyr.bev.get(key, 0)
    sh_c = cb / cn if cn else None
    sh_p = pb / pn if pn else None
    if x.state == "NSW":
        fb = fleet_nsw.bev.get(x.lga_name)
        fv = fleet_nsw.veh.get(x.lga_name)
    else:
        fb, fv = fleet_qld.bev_seen.get(x.lga_name), None
    m = monthly.loc[key] if key in monthly.index.droplevel(2) else None
    series = []
    for mo in months:
        if m is not None and mo in m.index and m.loc[mo, "new"] > 0:
            series.append(r(m.loc[mo, "bev"] / m.loc[mo, "new"]))
        else:
            series.append(None)
    areas.append({
        "id": str(x.lga_code), "name": x.lga_name, "state": x.state, "income": int(x.median_income), "pop": int(x.earners),
        "group": int(x.income_group), "elig": bool(n >= A["min_new_regs_scatter"]),
        "new": r(n, 0), "bev": r(b, 0), "share": r(b / n) if n else None,
        "c_new": r(cn, 0), "c_bev": r(cb, 0), "p_new": r(pn, 0), "p_bev": r(pb, 0),
        "share_c": r(sh_c), "share_p": r(sh_p),
        "chg": r((sh_c - sh_p) * 100, 2) if sh_c is not None and sh_p is not None else None,
        "mult": r(sh_c / sh_p, 3) if sh_c is not None and sh_p else None,
        "fleet_bev": r(fb, 0), "fleet_veh": r(fv, 0),
        "per1000veh": r(fb / fv * 1000, 2) if fb is not None and fv else None,
        "per1000pop": r(fb / x.earners * 1000, 2) if fb is not None else None,
        "series": series,
    })

# ---- VIC postcodes
vic["qdate"] = [pd.Period(q, "Q") for q in vic.quarter]
vq = vic.set_index(["postcode", "qdate"])
q_last = vic.qdate.max()
quarters = sorted(vic.qdate.unique())
vareas = []
for x in pc.itertuples(index=False):
    p = x.postcode

    def g(col, q):
        try:
            return float(vq.loc[(p, q), col])
        except KeyError:
            return None
    veh, bev, rv, rb = g("vehicles", q_last), g("bev", q_last), g("recent_vehicles", q_last), g("recent_bev", q_last)
    b1, b4, b5, v4 = g("bev", q_last - 1), g("bev", q_last - 4), g("bev", q_last - 5), g("vehicles", q_last - 4)
    add_c = bev - b1 if bev is not None and b1 is not None else None
    add_p = b4 - b5 if b4 is not None and b5 is not None else None
    series = []
    for q in quarters:
        v_, b_ = g("vehicles", q), g("bev", q)
        series.append(r(b_ / v_ * 1000, 2) if v_ else None)
    vareas.append({
        "id": str(p), "name": subs.get(p, f"Postcode {p}"), "state": "VIC", "income": int(x.median_income), "pop": int(x.individuals),
        "group": int(x.income_group),
        "elig": bool((rv or 0) >= A["min_recent_vehicles_vic"] and x.individuals >= A["min_individuals_vic_postcode"]),
        "veh": r(veh, 0), "bev": r(bev, 0), "per1000veh": r(bev / veh * 1000, 2) if veh else None,
        "r_veh": r(rv, 0), "r_bev": r(rb, 0), "rshare": r(rb / rv) if rv else None,
        "add_c": r(add_c, 0), "add_p": r(add_p, 0),
        "add_c1000": r(add_c / veh * 1000, 2) if add_c is not None and veh else None,
        "add_p1000": r(add_p / v4 * 1000, 2) if add_p is not None and v4 else None,
        "series": series,
    })

# ---- group aggregates (weighted, matching the workbook definitions)
lg = pd.DataFrame(areas)
groups = {}
for s in ("NSW", "QLD"):
    d = lg[lg.state == s]
    rows = []
    for gnum in range(1, NG + 1):
        x = d[d.group == gnum]
        rows.append({
            "share": r(x.bev.sum() / x.new.sum()), "share_c": r(x.c_bev.sum() / x.c_new.sum()),
            "share_p": r(x.p_bev.sum() / x.p_new.sum()),
            "chg": r((x.c_bev.sum() / x.c_new.sum() - x.p_bev.sum() / x.p_new.sum()) * 100, 2),
            "mult": r((x.c_bev.sum() / x.c_new.sum()) / (x.p_bev.sum() / x.p_new.sum()), 3),
            "per1000veh": r(x.fleet_bev.sum() / x.fleet_veh.sum() * 1000, 2) if s == "NSW" else None,
            "per1000pop": r(x.fleet_bev.sum() / x["pop"].sum() * 1000, 2),
            "income": [int(x.income.min()), int(x.income.max())],
            "bev": r(x.bev.sum(), 0), "c_bev": r(x.c_bev.sum(), 0), "p_bev": r(x.p_bev.sum(), 0), "fleet": r(x.fleet_bev.sum(), 0),
        })
    groups[s] = rows
va = pd.DataFrame(vareas)
rows = []
for gnum in range(1, NG + 1):
    x = va[va.group == gnum]
    rows.append({
        "per1000veh": r(x.bev.sum() / x.veh.sum() * 1000, 2), "rshare": r(x.r_bev.sum() / x.r_veh.sum()),
        "add_c1000": r(x.add_c.sum() / x.veh.sum() * 1000, 2),
        "income": [int(x.income.min()), int(x.income.max())],
    })
groups["VIC"] = rows

# state monthly BEV share (all areas) for the reference line
state_series = {}
for s in ("NSW", "QLD"):
    m = flow[flow.state == s].groupby("month")[["new", "bev"]].sum()
    state_series[s] = [r(m.bev.get(mo, np.nan) / m.new.get(mo, np.nan)) if mo in m.index else None for mo in months]
vs = vic.groupby("qdate")[["vehicles", "bev"]].sum()
state_series["VIC"] = [r(vs.bev[q] / vs.vehicles[q] * 1000, 2) for q in quarters]

# headline KPIs and monthly counts (raw numbers)
kpi = {}
for s_ in ("NSW", "QLD"):
    d = lg[lg.state == s_]
    kpi[s_] = {"share": r(d.bev.sum() / d.new.sum()), "bev": r(d.bev.sum(), 0), "new": r(d.new.sum(), 0),
               "share_c": r(d.c_bev.sum() / d.c_new.sum()), "share_p": r(d.p_bev.sum() / d.p_new.sum()),
               "c_bev": r(d.c_bev.sum(), 0), "p_bev": r(d.p_bev.sum(), 0), "fleet": r(d.fleet_bev.sum(), 0)}
kpi["VIC"] = {"fleet": r(va.bev.sum(), 0), "per1000veh": r(va.bev.sum() / va.veh.sum() * 1000, 2)}
counts = {}
for s_ in ("NSW", "QLD"):
    m = flow[flow.state == s_].groupby("month")[["new", "bev"]].sum()
    counts[s_] = {"bev": [r(m.bev.get(mo), 0) if mo in m.index else None for mo in months],
                  "other": [r(m.new.get(mo) - m.bev.get(mo), 0) if mo in m.index else None for mo in months]}
fuel = fuel[fuel.month >= str(months[0])]
data = {
    "lga": areas, "vic": vareas, "groups": groups, "stateSeries": state_series,
    "months": [str(m) for m in months], "quarters": [f"{q.year}-{q.quarter * 3:02d}" for q in quarters],
    "fuel": {"months": fuel.month.tolist(), "NSW_ULP": fuel.NSW_ULP.round(1).tolist(), "QLD_ULP": fuel.QLD_ULP.round(1).tolist(),
             "NSW_Diesel": fuel.NSW_Diesel.round(1).tolist()},
    "crisis": {"start": str(cr0), "end": str(cr1), "pyStart": str(py0), "pyEnd": str(py1),
               "vicQuarter": f"{q_last.year}-{q_last.quarter * 3:02d}"},
    "recent": {s: [str(last[s] - (WIN - 1)), str(last[s])] for s in last},
    "fleetDates": {"NSW": str(nsw_last), "QLD": str(qld_last), "VIC": f"{q_last.year}-{q_last.quarter * 3:02d}"},
    "nGroups": NG, "kpi": kpi, "counts": counts,
    "gapMonths": pd.read_csv(P / "nsw_unmapped_months.csv")["month"].tolist(),
}
geo_lga = json.loads((P / "lga_boundaries.geojson").read_text())
geo_poa = json.loads((P / "vic_postcode_boundaries.geojson").read_text())
keep_pc = {a["id"] for a in vareas}
geo_poa["features"] = [f for f in geo_poa["features"] if f["properties"]["poa_code_2021"] in keep_pc]
for f in geo_lga["features"]:
    f["properties"] = {"id": f["properties"]["lga_code_2023"]}
for f in geo_poa["features"]:
    f["properties"] = {"id": f["properties"]["poa_code_2021"]}

tpl = (HERE / "dashboard_template.html").read_text()
html = (tpl.replace("/*__DATA__*/null", json.dumps(data, separators=(",", ":")))
           .replace("/*__GEO_LGA__*/null", json.dumps(geo_lga, separators=(",", ":")))
           .replace("/*__GEO_POA__*/null", json.dumps(geo_poa, separators=(",", ":")))
           .replace("/*__GEO_AUS__*/null", (P / "australia_states.geojson").read_text()))
out = HERE / CFG["paths"]["dashboard"]
out.write_text(html)
print("wrote", out, f"{len(html) / 1e6:.1f} MB")
