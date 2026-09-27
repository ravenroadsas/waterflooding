# FloodPulse v2 — waterflood surveillance (R Shiny)

Pattern-level waterflood surveillance built on the La Cira-Infantas dimensionless methodology
(SPE-190314) and the Simmons & Falls throughput scaling (SPE-96469). The app follows the
macroprocess **Pattern maturity → Process velocity → Opportunities**, and it keeps what engineers
decide, so that interventions and outcomes become evidence for the next review.

| Step | Question | Main views |
|---|---|---|
| **1 Maturity** | Where is each pattern / unit in its flood life, and is it recovering what the prototype expects at that DWI? | Sec RF vs DWI (prototype band; colour, size = IWR, shape = start of flood; trajectories) · OPR vs WPR · Utilization vs DWI · log WOR vs DWI · DWI by unit · maturity map by unit · heterogeneity index trajectories · stage · register |
| **2 Process velocity** | Is injection / extraction at the right speed for that maturity, and is the pattern balanced? | Utilization vs Inj TP (target line) · TP now vs 12 months ago · IWR vs utilization · Inj TP vs Prod TP / DWI vs DTP · DWI + TP map by unit · rate gap to target TP or Cobb · unit TP history · pulse board · screening signals |
| **3 Opportunities** | Which interventions are supported by several families of evidence? | Rules A–F (injection control, stimulation, extraction, ADPERF support, conformance, rate change) · decision records (evidence, mechanism, alternatives, data gaps, validation, action, expected response) · status funnel and board · conformance ranking methods 1 and 2 · interventions with before/after and waterflood-fit baseline |

**Pattern 360** (click any point, polygon, bar or row) walks Field › Area › Pattern › Well › Unit: evidence,
maturity, velocity, units (VRF, Cobb, actual rate), wells (allocation, HI, fluid levels), waterflood-fit forecast at any TP,
interventions, performance / Chan, rock & fluid.

**Data & reference**: table status and validation, reconciliation against your derived tables, prototype
library (uploaded, analog built in the app, Buckley-Leverett; versioned), **Analytics (ML)** (k-means / Ward clustering,
PCA, outlier scores, nearest analogs; choose *Cluster* under "Colour points by" to use the groups on every plot),
settings (LCI reference values labelled), method.

## Run

```bash
Rscript install.R                                   # shiny bslib plotly DT readxl data.table writexl RSQLite DBI cluster httr2 jsonlite testthat
Rscript -e 'shiny::runApp(port = 3838)'
WF_DATA_DIR=/path/to/tables Rscript -e 'shiny::runApp(port = 3838)'   # your data
```

| Variable | Meaning |
|---|---|
| `WF_DATA_DIR` | folder with the input tables (CSV or one Excel workbook), default `data/demo` |
| `WF_DB` | SQLite file for decisions, interventions, outcomes, analog prototypes, AI drafts (default `data/floodpulse.sqlite`) |
| `ANTHROPIC_API_KEY` | optional: enables "Draft with AI" on decision records |
| `WF_AI_MODEL` | optional model id for drafting (default `claude-opus-5`) |

Tests: `Rscript tests/testthat.R`. Regenerate the demo field and the template: `Rscript scripts/make_demo.R`.

## Input tables (names as in the schema; aliases accepted)

| Table | Role | Columns |
|---|---|---|
| Wells | source | Well, Date, BOPD, BWPD, BWIPD (calendar-day rates) |
| Alloc | source | Well, Pattern, Date (valid-from, optional), Coeff |
| Vol | source | Pattern, Reservoir, Sand, STOIIP (stb), HCPV (rb), H, Phi, Sw, [K] |
| Fluids | source | Reservoir, API, Rs, Bo, Bw, visco, viscw, [Swc, Sor, Krw, Kro, Nw, No] |
| InjSand | source | Well, Sand, Date, BWIPD (profile; shares held until the next profile) |
| InjSand_status | source | Well, Sand, Date, VRF (valve size), Cobb (design rate, bbl/d) |
| Hierarchy | optional | Field, Area, Pattern, Well, Well_Type, X, Y |
| Baseline | optional | Pattern, WF_Start, [Np_Primary] |
| Prototypes / Prototype_Assign | reference | Prototype, Version, DWI, Sec_RF, DWP, Util, WOR / Pattern, Prototype, Version, [Valid_From] |
| Interventions | optional | Well, Date, Type (STIM, ISOLATION, ADPERF, RATE, LIFT, CONFORMANCE), Sand, Status, Notes |
| WellStatus | optional | Well, Date, Status, Lift, DFL (ft) |
| Patterns, Patterns_Vel, Patterns_Mat, InjSand_calc | derived | used only for reconciliation |

## Calculations (all in reservoir volumes)

- Pattern volumes = Σ well volume × Coeff (valid at the month). Pattern Bo / Bw = HCPV-weighted over its sands.
- DWI = ΣWi·Bw/HCPV · RF = ΣNp·Bo/HCPV · Sec RF = (ΣNp − Np at flood start)·Bo/HCPV · DWP = ΣWp·Bw/HCPV · DTP = Σ(Np·Bo+Wp·Bw)/HCPV · Loss = DWI − DTP since flood start.
- Inj TP = monthly Wi·Bw/HCPV annualized (%/yr); TP 12 m = mean of the last 12 months; Prod TP likewise; IWR 12 m = 12-month injection / withdrawals.
- Utilization = Wi·Bw / Np·Bo over 3, 6 or 12 months (setting), plus cumulative since flood start.
- OPR = Sec RF / expected Sec RF, WPR = DWP / expected DWP at the same DWI, from the prototype valid at the month; not judged below DWI 0.15.
- Unit metrics: InjSand shares × Wells.BWIPD × Coeff → unit DWI and TP on the unit HCPV; VRF and Cobb carried to each month.
- Heterogeneity index = cumulative well volume / area average − 1. Waterflood fit: Sec RF = A(1 − e^(−C·DWI)).
- Method 2: Evol(MB)/Evol(FF) = Sec RF / Welge displacement implied by the current water cut, against Loss.

## Opportunity evidence

Families: **M** maturity, **V** velocity, **U** unit/vertical, **S** spatial, **O** operations. One family =
`screening_only`; two or more including M or V = `candidate`. `validated_candidate` requires every validation item
ticked; logging an executed intervention sets `executed`; recording the outcome sets `outcome_evaluated`. All changes
are stored with history in SQLite. Priority score = weighted evidence count, indicative oil gain (waterflood fit) and
remaining waterflood oil (weights in Settings).

## Layout

```
app.R                 shell, sidebar, reactive graph
R/schema.R io.R       table specs, aliases, loaders, validation, template
R/engine.R            allocation, pattern maturity & velocity, unit injection, HI
R/prototypes.R sf.R   prototypes (versioned, analog, Buckley-Leverett), Simmons & Falls fit
R/opportunities.R     rules A–F, evidence records, conformance ranking, intervention evaluation
R/store.R             SQLite decisions / interventions / outcomes / prototypes / AI drafts
R/ml.R                clustering, PCA, outliers, analogs
R/ai.R                optional AI draft of decision records (Messages API)
R/reconcile.R         comparison with uploaded derived tables
R/mod_*.R             Maturity, Process velocity, Opportunities, Pattern 360, Data & reference
R/demo_data.R         synthetic field in the v2 layout
docs/v2-blueprint.html  the approved design
```
