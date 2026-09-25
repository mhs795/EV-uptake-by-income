# EV_income

How battery-electric vehicle (BEV) take-up in NSW, QLD and VIC is distributed by area income,
covering both new registrations and the registered fleet. It also looks at the effect of the
2026 Middle East (Strait of Hormuz) fuel crisis. Every input is public open data.

## Outputs

| File | What it is |
|---|---|
| `EV_uptake_by_income.xlsx` | Workbook. The Charts sheet comes first, in four sections: raw numbers, income, fuel crisis, top areas. Then Summary (counts and shares); Raw_Numbers; Top10; Fuel_Crisis; Inputs (editable assumptions); group and area tables; Suppression; Sources (every dataset page and file URL); Data_Adjustments (every filter, relabel and imputation, with counts); Notes. All shares and totals are live formulas. |
| `EV_income_map.html` | Interactive dashboard in the GARY / NELLY flat Material style: sidebar controls, KPI cards, and tabs for Map, Income groups, Fuel crisis, Raw numbers, Top 10 and Findings. The map covers all of Australia (states in outline). Data is embedded, so you can open it straight from disk. It has download-to-Excel buttons for each view; the full-workbook link expects the .xlsx in the same folder. |

## Rebuild

```bash
cd ~/models/EV_income
CLAUDE_CODE_DISABLE_BG_SHELL_PRESSURE_REAP=1 python3 build_data.py   # ~3 min; raw_data -> processed/
python3 fetch_boundaries.py                                          # ABS boundaries + VIC suburb names
python3 build_workbook.py                                            # -> EV_uptake_by_income.xlsx
python3 build_dashboard.py                                           # -> EV_income_map.html
```

Parameters (windows, thresholds, fuel-label mappings, crisis-onset rule) live in `config.yaml`.
`raw_data/` is not in git (~1.8 GB). Download the files listed in `raw_data/**/urls.txt` into the
same folders, then run the steps above. `processed/` is committed, so the workbook and dashboard can
be rebuilt without the raw data. The fuel prices come from `~/models/au_fuel_prices/work/*.csv`
(path set in `config.yaml`), so run `PRICES` first.

## Data

- **NSW** — TfNSW monthly registration transactions and monthly fleet snapshots, by LGA. Counts
  of 5 or fewer are suppressed as `<=5`. The value used for those cells is estimated and
  calibrated (Suppression sheet) and can be edited on Inputs.
- **QLD** — TMR unit records of new and transfer registrations, by LGA. There is no public
  fleet-by-fuel data by region, so fleet is a proxy: BEVs seen since 2022, at their last known LGA.
- **VIC** — DTP quarterly whole-fleet snapshot by postcode. The monthly VIC file has no
  location, so recent-model vehicles in the fleet stand in for new sales.
- **Income** — ABS Personal Income in Australia 2022-23 (ATO-based) by LGA, and ATO Taxation
  Statistics 2023-24 Table 8 by postcode.
- **Suburbs** — no state publishes EV data by suburb. VIC postcodes are named by the ABS
  suburbs/localities whose points fall inside them.

## Caveats

This compares areas, not people. Income groups are ranked within each state. QLD LGAs are
large. The fuel-crisis comparison is before/after, so other 2026 changes also fall inside it.
More detail is on the workbook's Notes sheet.
