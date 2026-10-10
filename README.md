# FloodPulse v3 — reservoir surveillance and well opportunities (R Shiny)

Pattern-level waterflood surveillance built on the La Cira-Infantas dimensionless methodology
(SPE-190314) and the Simmons & Falls throughput scaling (SPE-96469). The app follows the
macroprocess **Pattern maturity → Process velocity → Opportunities**, and it keeps what engineers
decide, so that interventions and outcomes become evidence for the next review.

The app follows five steps: two of pattern analysis, then the opportunity process.

| Step | Who | What happens | Main views |
|---|---|---|---|
| **1 Maturity** | app | Where is each pattern / unit in its flood life, against its prototype? | Sec RF vs DWI · OPR vs WPR · utilization · WOR · unit maps · heterogeneity · register |
| **2 Process velocity** | app | Is injection / extraction at the right speed, is the pattern balanced? | utilization vs TP · IWR · TP history · rate gap to target or Cobb · screening signals |
| **3 Identified opportunities** | app (automatic) | On every data load the app runs the pattern rules, the single-well analysis, the vertical analysis (log algorithms vs wellbore, offenders, potential gaps) and other analyses. Each finding is an opportunity: action · well · unit · interval. | summary by rule · Vertical analysis · Conformance ranking · Rules & weights |
| **4 Manage opportunities** | engineer | screening (one kind of evidence) → candidate (two or more incl. maturity or velocity, or promoted with a reason) → in a job → executed; dismissed aside. Only candidates go into a job; the rest stay as identified. Triggered wells (lift run life, repeated failures, well down) are flagged ⚑ so their opportunities can be done in one visit. | Portfolio list + decision record · Portfolio map · Board · Triggers |
| **5 Manage jobs** | engineer, lead | A job is one rig visit on one well. Engineers propose, a lead approves (forecast frozen), the job is executed and evaluated as a whole: the well's response against the sum of its items' forecasts; every opportunity of the job inherits the verdict. | value per status · Portfolio · Board · job detail |

"In a job" and "executed" are read from the jobs; a rejected or cancelled job releases its opportunities back to candidate.

**Pattern 360** (click any point, polygon, bar or row) walks Field › Area › Pattern › Well › Unit: evidence,
maturity, velocity, units (VRF, Cobb, actual rate), wells (allocation, HI, fluid levels), waterflood-fit forecast at any TP,
interventions, performance / Chan, rock & fluid.

**Vertical analysis** (step 3): per well, the log algorithms' intervals against the wellbore (open, squeezed, below plug), merged candidates with their potential, water offenders, open intervals below their theoretical potential, and a job composer (Bajo/Base/Alto of the job, oil and water bridges, cost, lift check) that proposes the job.

**5 Manage jobs**: value per status (risked oil of open jobs, oil delivered by executed ones, cost), portfolio of proposed and approved jobs (risked oil vs uncertainty, cost, opportunity triggers, score), approval board, job detail (approve / reject by a lead, mark executed, evaluate against the frozen forecast).

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
| `WF_LEADS` | comma-separated user names who can approve jobs (everyone when empty) |
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
| Log_Intervals | optional | Algorithm, Well, Top_ft, Base_ft, [Unit, Score]: intervals with potential from each log algorithm |
| Completions | optional | Well, Top_ft, Base_ft, Type (PERFORATION, SQUEEZE, PLUG, SLEEVE), Date, Status |
| Interval_Rates | optional | Well, [Interval_ID], Top_ft, Base_ft, [Unit, Date], Qo, Qw, [Method]: oil and water per open interval (offenders; later synthetic PLT) |
| Interval_Potential | optional | Well, [Interval_ID], Top_ft, Base_ft, [Unit], Qo_theo, [Qw_theo]: theoretical rate of open intervals |
| Job_Costs | reference | Job_Type (RIG, ADPERF, ISOLATION, STIM, REPERF, ALS_CHANGE ...), Depth_min_ft, Depth_max_ft, Cost_USD |
| Lift_Status | optional | Well, Lift_Type, Install_Date, Expected_Runlife_Days, Capacity_bfpd, Failures_12m (export from CDF) |
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
- **Status**: `screening_only` (one family) → `candidate` (two or more including M or V, or promoted by an engineer with a reason) → `in_job` (in a proposed or approved job) → `executed` (job executed or evaluated); `dismissed` aside. A **job** is the combination of the candidates the engineer ticks on one well (e.g. open 2 of 10 intervals with potential); the others stay as identified. Evaluation is per job, because production is measured per well: the well's incremental oil over its pre-job decline against the sum of the items' frozen forecasts (above Alto / within range / below Bajo); each opportunity of the job inherits the verdict, which calibrates P(success). Per-interval evaluation becomes possible with interval rates after the job (synthetic PLT). Decisions stored under earlier keys or statuses are mapped automatically.
- **Wellbore**: log algorithms' intervals merge into candidates (agreement n of m = evidence U); candidates on a squeeze or below a plug are blocked; open intervals with ≥ 40 % of the well's water and ≥ 90 % water cut → isolation (WSO); open intervals 20 bopd and 30 % below their theoretical potential → REPERF.
- **Jobs**: one rig visit on one well. Potential = sum of the items (ADPERF profiles, isolation removing its oil and water, REPERF gap); cost = rig + items from the cost lookup by depth; P(success) per job type (Settings, then outcomes); trigger from the lift (run life ≥ 85 %, repeated failures) or a well down. Engineers propose, a lead approves (forecast frozen), then executed and evaluated. Portfolio score = risked oil per cost × (1 + opportunity bonus) − uncertainty penalty.
- **Score** = weighted evidence count + gain rate + EUR / remaining oil − Bajo–Alto spread (weights in Settings). Volumes only until an economic function is plugged in.

## Layout

```
app.R                 shell, sidebar, reactive graph
R/schema.R io.R       table specs, aliases, loaders, validation, template
R/engine.R            allocation, pattern maturity & velocity, unit injection, HI
R/prototypes.R sf.R   prototypes (versioned, analog, Buckley-Leverett), Simmons & Falls fit
R/opportunities.R     lenses (pattern rules A–F, well rules W1–W4, other analyses), merge on the well target, score
R/forecast.R          profiles, Arps fits, post-job evaluation against the frozen forecast
R/wellbore.R          log candidates, completion state and conflicts, water offenders, potential gaps
R/jobs.R              job potential, cost lookup, lift triggers, P(success), scoring, approval flow
R/econ.R              hook for the private economic function (WF_ECON_FILE)
R/store.R             SQLite decisions / interventions / outcomes / prototypes / AI drafts
R/ml.R                clustering, PCA, outliers, analogs
R/ai.R                optional AI draft of decision records (Messages API)
R/reconcile.R         comparison with uploaded derived tables
R/mod_*.R ui_wells.R  Maturity, Process velocity, Opportunities, Pattern 360, Well 360, Data & reference
R/demo_data.R         synthetic field (waterflood + primary satellite, well analysis)
docs/v2-blueprint.html  the approved v2 design
docs/v3-opportunities-design.html  the well-centric opportunity design and decisions
docs/v3-adperf-workbench.html      the ADPERF workbench, jobs and approval design
```
