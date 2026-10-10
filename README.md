# FloodPulse v3 — reservoir surveillance and well opportunities (R Shiny)

Pattern-level waterflood surveillance built on the La Cira-Infantas dimensionless methodology
(SPE-190314) and the Simmons & Falls throughput scaling (SPE-96469). The app follows the
macroprocess **Pattern maturity → Process velocity → Opportunities**, and it keeps what engineers
decide, so that interventions and outcomes become evidence for the next review.

| Step | Question | Main views |
|---|---|---|
| **1 Maturity** | Where is each pattern / unit in its flood life, and is it recovering what the prototype expects at that DWI? | Sec RF vs DWI (prototype band; colour, size = IWR, shape = start of flood; trajectories) · OPR vs WPR · Utilization vs DWI · log WOR vs DWI · DWI by unit · maturity map by unit · heterogeneity index trajectories · stage · register |
| **2 Process velocity** | Is injection / extraction at the right speed for that maturity, and is the pattern balanced? | Utilization vs Inj TP (target line) · TP now vs 12 months ago · IWR vs utilization · Inj TP vs Prod TP / DWI vs DTP · DWI + TP map by unit · rate gap to target TP or Cobb · unit TP history · pulse board · screening signals |
| **3 Opportunities** | Which well interventions are supported by several families of evidence? | Every opportunity is a job on a well (action · well · unit · interval). Lenses: pattern rules A–F projected onto wells, single-well analysis (new intervals → ADPERF, water shut-off, decline anomaly, shut-in wells; any drive), other analyses (Findings table) · portfolio grouped by well with lens / drive / action / hierarchy filters · decision records with Bajo/Base/Alto forecast, evidence by lens, automatic and manual checks · portfolio map · board · job packages · post-job evaluation against the forecast frozen at validation · conformance ranking |

**Pattern 360** (click any point, polygon, bar or row) walks Field › Area › Pattern › Well › Unit: evidence,
maturity, velocity, units (VRF, Cobb, actual rate), wells (allocation, HI, fluid levels), waterflood-fit forecast at any TP,
interventions, performance / Chan, rock & fluid.

**Well 360** (sidebar, any record or Pattern 360 › Wells): opportunities, rate history with jobs, interval strip (status, kh, Sw, BSW), Bajo/Base/Alto forecasts, pattern memberships and unit metrics (or "primary"), jobs.

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
| `WF_ECON_FILE` | optional R file defining `wf_econ(summary, forecasts)`; its columns are joined to the opportunities (ranking uses volumes otherwise) |

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
| Hierarchy | optional | Orgunit, Contract, Field, Structure, Substructure, Area, Pattern (empty = primary well), Well, Well_Type, X, Y |
| Baseline | optional | Pattern, WF_Start, [Np_Primary], [Method: waterflood (default), polymer, gas ...] |
| Prototypes / Prototype_Assign | reference | Prototype, Version, DWI, Sec_RF, DWP, Util, WOR / Pattern, Prototype, Version, [Valid_From] |
| Interventions | optional | Well, Date, Type (STIM, ISOLATION, ADPERF, RATE, LIFT, CONFORMANCE), Sand, Status, Notes |
| WellStatus | optional | Well, Date, Status, Lift, DFL (ft) |
| Intervalos (INTERVALOS) | optional | ORGUNIT, FIELD, WELL, UNIT, intervalo_id, top_ft, base_ft, estado_apertura, h_net_ft, kabs_md, phi, sw_las, kh_md_ft, area_ac, ooip_stb, rf, eur_stb, np_total_pozo_stb, np_ooip_ratio, sw_actual, bsw_inicial_pct, qo/qw/qf_inicial, qa_resultado |
| Perfiles_Mensuales | optional | ORGUNIT, FIELD, WELL, UNIT, intervalo_id, escenario (Bajo/Base/Alto), mes, qoi_bopd, qwi_bwpd (initial water *production*), qo/qw/qf_perfil, b, di_por_mes |
| Findings | optional | Source, Well, [Unit, Interval_ID], Action, [Family M/V/U/S/O, Metric, Value, Reference, Units, Comment, Gain_bopd, Date]: evidence from any other analysis |
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

## Opportunities

- **Target**: every opportunity is a well job, key `action|well|unit|interval`. Actions: ADPERF, WSO, STIM_PROD, LIFT, REACTIVATE (well lens); ISOLATE, STIM_INJ, CONFORMANCE, RATE, SUPPORT_INJ (pattern rules on the dominant injector / producer); any action named by another analysis.
- **Drive**: a well (unit) allocated to a pattern under injection follows that pattern's process; everything else is primary. Pattern rules run on waterflood patterns only; well rules run everywhere.
- **Single-well analysis**: closed or partly open intervals → ADPERF (M: Np/OOIP or Sw actual; U: kh vs unit median and BSW; S: Voronoi area; V: injection support of the unit in the well's patterns). Open intervals with very high BSW → WSO. A QA-corrected estimate is shown and adds a review item. Interval rates add up.
- **Well history**: oil 25 % below the well's own Arps decline → STIM_PROD (LIFT when the fluid level is high); no production for 3 months → REACTIVATE.
- **Status**: one family = `screening_only`; two or more including M or V = `candidate`; validation requires every item ticked and **freezes the Bajo/Base/Alto forecast**; logging a job (one or several opportunities of the same well) sets `executed`; the post-job comparison (actual minus pre-job decline vs the frozen profile: above Alto / within range / below Bajo) records `outcome_evaluated`. Decisions stored under v2 pattern keys are moved to the well keys automatically.
- **Score** = weighted evidence count + gain rate + EUR / remaining oil − Bajo–Alto spread (weights in Settings). Volumes only until an economic function is plugged in.

## Layout

```
app.R                 shell, sidebar, reactive graph
R/schema.R io.R       table specs, aliases, loaders, validation, template
R/engine.R            allocation, pattern maturity & velocity, unit injection, HI
R/prototypes.R sf.R   prototypes (versioned, analog, Buckley-Leverett), Simmons & Falls fit
R/opportunities.R     lenses (pattern rules A–F, well rules W1–W4, other analyses), merge on the well target, score
R/forecast.R          profiles, Arps fits, post-job evaluation against the frozen forecast
R/econ.R              hook for the private economic function (WF_ECON_FILE)
R/store.R             SQLite decisions / interventions / outcomes / prototypes / AI drafts
R/ml.R                clustering, PCA, outliers, analogs
R/ai.R                optional AI draft of decision records (Messages API)
R/reconcile.R         comparison with uploaded derived tables
R/mod_*.R ui_wells.R  Maturity, Process velocity, Opportunities, Pattern 360, Well 360, Data & reference
R/demo_data.R         synthetic field (waterflood + primary satellite, well analysis)
docs/v2-blueprint.html  the approved v2 design
docs/v3-opportunities-design.html  the well-centric opportunity design and decisions
```
