# EV uptake by regional income

This project asks how battery-electric vehicle (BEV) take-up in **NSW, QLD and VIC** is spread across areas of
different income. It covers new registrations (flow) and the registered fleet (stock), and looks at the effect
of the **2026 Middle East / Strait of Hormuz fuel crisis**. Every input is public open data.

Written in **R**: the data pipeline, an Excel workbook with native charts, and a **Shiny** dashboard.

## Quick start

```bash
git clone https://github.com/mhs795/EV-uptake-by-income.git
cd EV-uptake-by-income
Rscript R/install_packages.R   # first time only
Rscript run_dashboard.R        # open the dashboard (uses the committed processed data)
```

The processed tables, the dashboard data and the workbook are committed. You can open the dashboard or
`EV_uptake_by_income.xlsx` straight away, without downloading any raw data.

## Rebuild everything

```bash
Rscript run_all.R                  # install packages, download raw data, build everything
Rscript run_all.R --from data      # skip install + download
Rscript run_all.R --only workbook  # one step
```

| Step | Script | What it does |
|---|---|---|
| `install` | `R/install_packages.R` | Installs the R packages that are missing (binaries from Posit Package Manager on Linux) |
| `download` | `R/download_raw.R` | Fetches ~1.8 GB of public raw data into `raw_data/`, from the URLs in `raw_data/**/urls.txt`; files you already have are skipped |
| `data` | `R/build_data.R` | `raw_data/` → `processed/*.csv` (~5 min). Also writes `processed/adjustments.csv`, the log of every data adjustment |
| `boundaries` | `R/fetch_boundaries.R` | ABS boundaries (LGAs, VIC postcodes, states) and suburb names for each VIC postcode |
| `workbook` | `R/build_workbook.R` | `EV_uptake_by_income.xlsx`: charts first, then Summary, raw numbers, top 10s, fuel crisis, inputs, detailed tables, sources, data adjustments, notes. Shares and totals are live Excel formulas |
| `dashboard` | `R/dashboard_data.R` | `processed/dashboard.rds` for the Shiny app |

Then run `Rscript run_dashboard.R`. It opens the dashboard at http://127.0.0.1:8050.

All settings (analysis windows, thresholds, fuel-label mappings, the crisis-onset rule, file paths) are
in **`config.yaml`**. No numbers are hard-coded in the scripts.

**Requirements:** R ≥ 4.3. On Linux, `sf` needs `libgdal-dev libgeos-dev libproj-dev libudunits2-dev`.

## Folder layout

```
run_all.R            one command to run the whole project
run_dashboard.R      open the Shiny dashboard
config.yaml          every setting
R/                   pipeline scripts (+ sources.csv, notes.csv used in the workbook)
app/app.R            the Shiny dashboard (GARY / NELLY flat Material design)
raw_data/            downloads (git-ignored except urls.txt lists and raw_data/fuel/*.csv)
processed/           tidy tables, boundaries and dashboard.rds (committed)
EV_uptake_by_income.xlsx
```

## Data

- **NSW**: TfNSW monthly registration transactions and fleet snapshots, by council area (LGA).
  Counts of 5 or fewer are published as `<=5`. The value used for those cells is estimated from QLD
  unit records and calibrated against fleet totals (Suppression sheet), and can be edited on the
  workbook's Inputs sheet.
- **QLD**: TMR unit records of new and transferred registrations, by LGA. There is no public regional
  fleet-by-fuel data, so the fleet figure is a proxy: BEVs seen since 2022, placed at their last known LGA.
- **VIC**: DTP quarterly whole-fleet snapshot by postcode. The monthly VIC file has no location, so
  recent-model vehicles in the fleet stand in for new sales.
- **Income**: ABS Personal Income in Australia 2022-23 (ATO-based) by LGA; ATO Taxation Statistics 2023-24
  (Table 8) by postcode.
- **Fuel prices**: NSW FuelCheck, QLD Fuel Price Reporting and AIP terminal gate prices. Monthly averages are
  kept in `raw_data/fuel/`, copied from the author's `au_fuel_prices` project.
- **Suburbs**: no state publishes EV data by suburb. VIC postcodes are named by the ABS suburbs whose points
  fall inside them.

Every dataset page and file URL is on the workbook's **Sources** sheet. Every filter, relabel, imputation
and proxy is on its **Data_Adjustments** sheet, with the number of records it affected.

## Caveats

- This compares areas, not people: it shows where BEVs are registered, not the income of the person who bought each one.
- Income groups are ranked within each state. QLD council areas are large (Brisbane alone is about a quarter of QLD earners).
- The fuel-crisis comparison is before/after (crisis months vs the same months a year earlier), so other
  changes in 2026 fall inside it too.

More on the workbook's **Notes** sheet.
