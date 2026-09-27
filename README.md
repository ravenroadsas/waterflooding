# FloodPulse — waterflood surveillance pilot (R Shiny)

Surveillance of a complex, multi-sand waterflood at **pattern x sand** level, organised in two sections:

| Section | Question it answers | Views |
|---|---|---|
| **Maturity** | *Where is each pattern in its flood life and what should we do with it?* | KPI ribbon · flood maturity map (WF RF vs HCPVI against Buckley-Leverett iso-sweep curves, with trajectories) · life-cycle stage bars · reservoir map (pattern polygons coloured by any metric) · opportunity quadrant (HCPVI x Ev → Accelerate / Harvest / Conformance / Investigate) · pattern x sand matrix · oil left on the table (produced / reserves to economic WOR / movable oil beyond) · action register |
| **Process** | *How is the flood being run this month?* | Rates & water cut · voidage balance (rb in vs out, VRR since flood start) · pulse board heatmap (every pattern x month, any metric) · automated exception feed with recommended actions · water-utilisation ranking |

Plus a **drill-down** for any pattern / block (click a bubble, polygon, bar, heatmap cell or table row):
performance, Chan plot, log(WOR) vs Np with EUR, recovery vs Buckley-Leverett, wells & allocation coefficients,
per-sand breakdown, rel-perm / fractional flow, exceptions.

The **As-of** slider in the sidebar re-evaluates everything at any past month; press ▶ to replay the flood history.

## Run

```bash
Rscript install.R                     # once: shiny bslib plotly DT readxl data.table writexl testthat
Rscript -e 'shiny::runApp(port = 3838)'
# own data: WF_DATA_DIR=/path/to/folder Rscript -e 'shiny::runApp(port = 3838)'
```

or `docker build -t floodpulse . && docker run -p 3838:3838 floodpulse`.

Tests: `Rscript tests/testthat.R`. Regenerate the demo field / Excel template: `Rscript scripts/make_demo.R`.

## Input tables

Load an Excel workbook (one sheet per table) or six CSVs from the **Data** page, or point `WF_DATA_DIR` at a folder.
Sheet/file and column names are matched flexibly (English and Spanish aliases, e.g. `pozo`, `fecha`, `POES`,
`arena`). Uploading a single table (e.g. a new allocation sheet) replaces only that table.
Download the template from the Data page (`data/wf_template.xlsx`); the in-app dictionary lists every alias.

| Table | Key columns | Notes |
|---|---|---|
| production | well, date, bopd, bwpd, bwipd | monthly calendar-day rates; any date in the month |
| hierarchy | field, block, pattern, well, well_type, x, y | one row per pattern-well; x/y optional (enables the map) |
| stooip | pattern, sand, stooip (stb) + area, net_pay, porosity, swi, permeability, boi | per pattern and sand |
| fluids | sand, bo, bw, mu_o, mu_w, swc, sor, krw_or, kro_wc, nw, no | Corey; `sand = *` is a default |
| petrophysics | well, sand, net_pay, porosity, sw, permeability | kh split of each well between sands |
| allocation | pattern, well, coefficient, [date] | no date = constant; with dates each value holds until the next → **historical allocation works without code changes** |

## Calculations (`R/engine.R`, `R/fluids.R`, `R/diagnostics.R`)

1. Monthly volumes = rate × days in month.
2. Pattern volume = Σ well volume × areal coefficient(pattern, well, month).
3. Each well's share is split between the pattern's sands by kh (STOOIP share if no petrophysics).
4. Reservoir barrels with Bo/Bw per sand; all additive quantities kept at pattern x sand level and aggregated to pattern / block / field for any sand selection, then ratios are derived.
5. Dimensionless variables: RF, HCPVI = WiBw/(N·Boi), PVI, VRR (month, cumulative, since flood start), WC, WOR, maturity index = Np / movable oil, water utilisation = Wi / WF oil, injection efficiency.
6. Waterflood RF = oil since first injection minus the extrapolated primary decline.
7. Ideal recovery E_D(HCPVI) from Buckley-Leverett/Welge per pattern x sand; apparent volumetric sweep Ev = WF oil / ideal WF oil.
8. EUR from log(WOR) vs Np extrapolated to the economic WOR (capped at movable oil).
9. Stage, quadrant and exception rules use the thresholds editable on the Data page.

## Layout

```
app.R               UI shell, sidebar, shared reactives
R/schema.R          table specs + aliases
R/io.R              readers, date parsing, validation, template
R/fluids.R          Corey kr, fractional flow, Welge E_D(HCPVI)
R/engine.R          allocation → pattern x sand volumes → metrics
R/diagnostics.R     stages, quadrants, exceptions, WOR forecast, Chan
R/mod_*.R           Maturity, Process, Data, drill-down
R/demo_data.R       synthetic 16-pattern, 3-sand field
www/styles.css      theme
```

## Pilot notes / next steps

- Demo data is synthetic (with planted anomalies: stopped injector P13, over/under-injected and poorly swept patterns, one pattern on primary).
- Time-varying allocation: just add a `date` column to the allocation sheet.
- Candidates after the pilot: pressure data (Hall plot, pattern pressure maps), well-level injector-producer connectivity (CRM), gas / GOR, forecast scenarios.
