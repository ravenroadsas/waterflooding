# Input table specifications (v2) ----------------------------------------------
#
# Source tables follow the surveillance data model: Wells, Alloc, Vol, Fluids,
# InjSand, InjSand_status (+ optional Hierarchy, Baseline, Prototypes,
# PrototypeAssign, Interventions, WellStatus). Derived tables (Patterns,
# Patterns_Vel, Patterns_Mat, InjSand_calc) are optional and only used to
# reconcile the app's results. Column and sheet names are matched through
# aliases (English / Spanish / the original .txt names).

col_spec <- function(name, type = "num", required = TRUE, aliases = character(),
                     unit = "", doc = "") {
  list(name = name, type = type, required = required, aliases = aliases,
       unit = unit, doc = doc)
}

wf_schema <- list(
  wells = list(
    title = "Wells: monthly rates", role = "source", grain = "Well x month",
    doc = "One row per well and month. Rates are calendar-day averages (stb/d, bbl/d).",
    sheet_aliases = c("wells", "wellrates", "production", "prod", "produccion", "pozos"),
    cols = list(
      col_spec("well", "chr", aliases = c("well_name", "pozo", "uwi")),
      col_spec("date", "date", aliases = c("month", "fecha", "period", "mes")),
      col_spec("bopd", aliases = c("oil_rate", "qo"), unit = "stb/d"),
      col_spec("bwpd", aliases = c("water_rate", "qw"), unit = "bbl/d"),
      col_spec("bwipd", aliases = c("winj", "qwi", "inj_rate"), unit = "bbl/d")
    )
  ),
  alloc = list(
    title = "Alloc: well-pattern coefficients", role = "source", grain = "Well x pattern x valid-from",
    doc = "Areal allocation. Without a date the coefficient is constant; with dates each value holds until the next date.",
    sheet_aliases = c("alloc", "allocation", "wellpatternallocation", "asignacion", "coeficientes"),
    cols = list(
      col_spec("well", "chr", aliases = c("well_name", "pozo", "uwi")),
      col_spec("pattern", "chr", aliases = c("patron", "pattern_name")),
      col_spec("date", "date", FALSE, c("valid_from", "fecha", "month", "effective_date")),
      col_spec("coeff", aliases = c("coefficient", "coef", "factor", "af"), unit = "fraction")
    )
  ),
  vol = list(
    title = "Vol: volumetrics per pattern and sand", role = "source", grain = "Pattern x sand",
    doc = "HCPV is the denominator of every dimensionless variable. Reservoir links to Fluids.",
    sheet_aliases = c("vol", "volumetrics", "patternsandvolumetrics", "stooip", "poes", "volumetria"),
    cols = list(
      col_spec("pattern", "chr", aliases = c("patron")),
      col_spec("reservoir", "chr", FALSE, c("yacimiento", "formation")),
      col_spec("sand", "chr", aliases = c("unit", "unidad", "arena", "layer", "zone")),
      col_spec("stoiip", aliases = c("stooip", "ooip", "poes"), unit = "stb"),
      col_spec("hcpv", required = FALSE, aliases = c("hcpv_rb", "vph"), unit = "rb"),
      col_spec("h", required = FALSE, aliases = c("net_pay", "hn", "thickness"), unit = "ft"),
      col_spec("phi", required = FALSE, aliases = c("porosity", "por"), unit = "fraction"),
      col_spec("sw", required = FALSE, aliases = c("swi", "sw_init"), unit = "fraction"),
      col_spec("k", required = FALSE, aliases = c("perm", "permeability", "k_md"), unit = "mD")
    )
  ),
  fluids = list(
    title = "Fluids: PVT per reservoir", role = "source", grain = "Reservoir",
    doc = "Bo and Bw convert to reservoir barrels. Relative-permeability columns are optional (default Corey set).",
    sheet_aliases = c("fluids", "fluid", "pvt", "fluidproperties", "fluidos"),
    cols = list(
      col_spec("reservoir", "chr", aliases = c("yacimiento", "sand", "region")),
      col_spec("api", required = FALSE, unit = "API"),
      col_spec("rs", required = FALSE, unit = "scf/stb"),
      col_spec("bo", aliases = c("bo_rb_stb"), unit = "rb/stb"),
      col_spec("bw", required = FALSE, unit = "rb/stb"),
      col_spec("visco", required = FALSE, aliases = c("mu_o", "muo"), unit = "cp"),
      col_spec("viscw", required = FALSE, aliases = c("mu_w", "muw"), unit = "cp"),
      col_spec("swc", required = FALSE, aliases = c("swir"), unit = "fraction"),
      col_spec("sor", required = FALSE, aliases = c("sorw"), unit = "fraction"),
      col_spec("krw", required = FALSE, aliases = c("krw_or", "krw_max"), unit = "fraction"),
      col_spec("kro", required = FALSE, aliases = c("kro_wc", "kro_max"), unit = "fraction"),
      col_spec("nw", required = FALSE, aliases = c("corey_w")),
      col_spec("no", required = FALSE, aliases = c("corey_o"))
    )
  ),
  injsand = list(
    title = "InjSand: injection by sand", role = "source", grain = "Well x sand x date",
    doc = "Injection profile per injector and sand. Used as shares of Wells.BWIPD, held until the next profile date.",
    sheet_aliases = c("injsand", "injectionsandrates", "inj_sand", "perfiles", "profiles"),
    cols = list(
      col_spec("well", "chr", aliases = c("pozo")),
      col_spec("sand", "chr", aliases = c("unit", "unidad", "arena")),
      col_spec("date", "date", aliases = c("fecha", "month")),
      col_spec("bwipd", aliases = c("winj", "rate", "qwi"), unit = "bbl/d")
    )
  ),
  injsand_status = list(
    title = "InjSand_status: valves and design rates", role = "source", grain = "Well x sand x date",
    doc = "VRF = flow-regulating valve size per interval. Cobb = designed optimum injection rate (input, bbl/d).",
    sheet_aliases = c("injsand_status", "injectionsandstatus", "injsandstatus", "mandrels", "valvulas"),
    cols = list(
      col_spec("well", "chr", aliases = c("pozo")),
      col_spec("sand", "chr", aliases = c("unit", "unidad", "arena")),
      col_spec("date", "date", aliases = c("fecha")),
      col_spec("vrf", required = FALSE, aliases = c("valve", "valvula", "valve_size")),
      col_spec("cobb", required = FALSE, aliases = c("design_rate", "optimum_rate", "caudal_optimo"), unit = "bbl/d")
    )
  ),
  hierarchy = list(
    title = "Hierarchy: orgunit, contract, field, structure, pattern, well", role = "optional", grain = "Pattern x well",
    doc = paste("ORGUNIT > CONTRACT > FIELD > STRUCTURE > SUBSTRUCTURE (lower levels optional), area grouping, well type and coordinates.",
                "Wells without a pattern are listed with an empty pattern: they are produced on primary."),
    sheet_aliases = c("hierarchy", "jerarquia", "patterns_master", "wellmaster", "tree"),
    cols = list(
      col_spec("orgunit", "chr", FALSE, c("org_unit", "unidad_organizacional", "business_unit", "asset")),
      col_spec("contract", "chr", FALSE, c("contrato", "block_contract")),
      col_spec("field", "chr", FALSE, c("campo")),
      col_spec("structure", "chr", FALSE, c("estructura")),
      col_spec("substructure", "chr", FALSE, c("subestructura", "sub_structure")),
      col_spec("area", "chr", FALSE, c("block", "sector", "bloque")),
      col_spec("pattern", "chr", FALSE, c("patron")),
      col_spec("well", "chr", aliases = c("pozo")),
      col_spec("well_type", "chr", FALSE, c("type", "tipo", "role")),
      col_spec("x", required = FALSE, aliases = c("x_coord", "easting")),
      col_spec("y", required = FALSE, aliases = c("y_coord", "northing"))
    )
  ),
  baseline = list(
    title = "Baseline: waterflood start per pattern", role = "optional", grain = "Pattern",
    doc = "Secondary-recovery baseline per pattern. Without it, the first injection month is used. Method: waterflood (default), polymer, gas ...",
    sheet_aliases = c("baseline", "patternwaterfloodbaseline", "wf_start", "linea_base"),
    cols = list(
      col_spec("pattern", "chr", aliases = c("patron")),
      col_spec("wf_start", "date", aliases = c("start", "fecha_inicio", "waterflood_start")),
      col_spec("np_primary", required = FALSE, aliases = c("np_base", "primary_np"), unit = "stb"),
      col_spec("method", "chr", FALSE, c("mechanism", "metodo", "process", "drive"))
    )
  ),
  prototypes = list(
    title = "Prototype curves", role = "reference", grain = "Prototype x version x DWI",
    doc = "Expected performance vs DWI (simulation or analog). Sec RF and DWP as fractions of HCPV.",
    sheet_aliases = c("prototypes", "prototype", "prototypecurve", "prototipos"),
    cols = list(
      col_spec("prototype", "chr", aliases = c("name", "prototipo")),
      col_spec("version", "chr", FALSE, c("ver")),
      col_spec("dwi"),
      col_spec("sec_rf", aliases = c("secrf", "rf")),
      col_spec("dwp", required = FALSE),
      col_spec("util", required = FALSE, aliases = c("utilization")),
      col_spec("wor", required = FALSE)
    )
  ),
  prototype_assign = list(
    title = "Prototype assignment", role = "reference", grain = "Pattern x valid-from",
    doc = "Which prototype (and version) each pattern is compared with.",
    sheet_aliases = c("prototype_assign", "patternprototypeassignment", "assignment", "asignacion_prototipo"),
    cols = list(
      col_spec("pattern", "chr", aliases = c("patron")),
      col_spec("prototype", "chr", aliases = c("prototipo")),
      col_spec("version", "chr", FALSE, c("ver")),
      col_spec("valid_from", "date", FALSE, c("date", "fecha"))
    )
  ),
  interventions = list(
    title = "Interventions", role = "optional", grain = "Well x date",
    doc = "Executed or planned jobs: STIM, ISOLATION, ADPERF, RATE, LIFT, CONFORMANCE.",
    sheet_aliases = c("interventions", "wellintervention", "workovers", "trabajos"),
    cols = list(
      col_spec("well", "chr", aliases = c("pozo")),
      col_spec("date", "date", aliases = c("fecha")),
      col_spec("type", "chr", aliases = c("job", "tipo")),
      col_spec("sand", "chr", FALSE, c("unit", "arena")),
      col_spec("status", "chr", FALSE, c("estado")),
      col_spec("notes", "chr", FALSE, c("comment", "comentario"))
    )
  ),
  well_status = list(
    title = "Well status and fluid levels", role = "optional", grain = "Well x date",
    doc = "Dynamic fluid level (ft above pump) supports extraction / lift opportunities.",
    sheet_aliases = c("well_status", "wellstatus", "estado_pozos", "fluid_levels"),
    cols = list(
      col_spec("well", "chr", aliases = c("pozo")),
      col_spec("date", "date", aliases = c("fecha")),
      col_spec("status", "chr", FALSE, c("estado")),
      col_spec("lift", "chr", FALSE, c("lift_type", "sla")),
      col_spec("dfl", required = FALSE, aliases = c("fluid_level", "nivel", "submergence"), unit = "ft")
    )
  ),
  # ---- single-well analysis and other studies (opportunity sources) ----
  intervals = list(
    title = "Intervals: single-well analysis (INTERVALOS)", role = "optional", grain = "Well x unit x interval",
    doc = paste("One row per well, unit and interval from the producer analysis. Closed or partly open intervals are",
                "potential additional perforations (ADPERF). Rates are the interval on its own; intervals add up."),
    sheet_aliases = c("intervals", "intervalos", "new_opportunities", "nuevas_oportunidades", "well_intervals"),
    cols = list(
      col_spec("orgunit", "chr", FALSE, c("org_unit")),
      col_spec("field", "chr", FALSE, c("campo")),
      col_spec("well", "chr", aliases = c("pozo")),
      col_spec("sand", "chr", aliases = c("unit", "unidad", "arena")),
      col_spec("interval_id", "chr", aliases = c("intervalo_id", "intervalo", "interval")),
      col_spec("top_ft", required = FALSE, aliases = c("top", "tope"), unit = "ft"),
      col_spec("base_ft", required = FALSE, aliases = c("base", "bottom"), unit = "ft"),
      col_spec("estado", "chr", FALSE, c("estado_apertura", "status", "opening_status")),
      col_spec("h_net_ft", required = FALSE, aliases = c("h_net", "hnet", "net_pay"), unit = "ft"),
      col_spec("kabs_md", required = FALSE, aliases = c("kabs", "k_md", "k"), unit = "mD"),
      col_spec("phi", required = FALSE, aliases = c("porosity"), unit = "fraction"),
      col_spec("sw_las", required = FALSE, aliases = c("sw_log", "sw"), unit = "fraction"),
      col_spec("kh_md_ft", required = FALSE, aliases = c("kh"), unit = "mD.ft"),
      col_spec("area_ac", required = FALSE, aliases = c("area", "voronoi_area"), unit = "acre"),
      col_spec("ooip_stb", required = FALSE, aliases = c("ooip", "stoiip", "poes"), unit = "stb"),
      col_spec("rf", required = FALSE, aliases = c("recovery_factor", "fr"), unit = "fraction"),
      col_spec("eur_stb", required = FALSE, aliases = c("eur"), unit = "stb"),
      col_spec("np_well_stb", required = FALSE, aliases = c("np_total_pozo_stb", "np_total_pozo", "np_well"), unit = "stb"),
      col_spec("np_ooip", required = FALSE, aliases = c("np_ooip_ratio"), unit = "fraction"),
      col_spec("sw_act", required = FALSE, aliases = c("sw_actual", "sw_current"), unit = "fraction"),
      col_spec("bsw0_pct", required = FALSE, aliases = c("bsw_inicial_pct", "bsw_inicial", "bsw"), unit = "%"),
      col_spec("qo0", required = FALSE, aliases = c("qo_inicial_bopd", "qo_inicial"), unit = "bopd"),
      col_spec("qw0", required = FALSE, aliases = c("qw_inicial_bwpd", "qw_inicial"), unit = "bwpd"),
      col_spec("qf0", required = FALSE, aliases = c("qf_inicial_bfpd", "qf_inicial"), unit = "bfpd"),
      col_spec("qa", "chr", FALSE, c("qa_resultado", "qa_result"))
    )
  ),
  profiles = list(
    title = "Profiles: monthly forecast by scenario (PERFILES_MENSUALES)", role = "optional", grain = "Well x unit x interval x scenario x month",
    doc = paste("Bajo / Base / Alto monthly profiles (hyperbolic decline, constant liquid per scenario). qw0 is the initial water",
                "PRODUCTION rate of the scenario (qwi_bwpd in the source), not injection. Any source can deliver profiles with the same keys."),
    sheet_aliases = c("profiles_monthly", "perfiles_mensuales", "monthly_profiles", "well_profiles", "forecast_profiles"),
    cols = list(
      col_spec("orgunit", "chr", FALSE, c("org_unit")),
      col_spec("field", "chr", FALSE, c("campo")),
      col_spec("well", "chr", aliases = c("pozo")),
      col_spec("sand", "chr", FALSE, c("unit", "unidad", "arena")),
      col_spec("interval_id", "chr", FALSE, c("intervalo_id", "intervalo", "interval")),
      col_spec("scenario", "chr", aliases = c("escenario", "case")),
      col_spec("month", aliases = c("mes", "t")),
      col_spec("qoi", required = FALSE, aliases = c("qoi_bopd"), unit = "bopd"),
      col_spec("qw0", required = FALSE, aliases = c("qwi_bwpd", "qwi"), unit = "bwpd"),
      col_spec("qo", required = FALSE, aliases = c("qo_perfil_bopd", "qo_perfil"), unit = "bopd"),
      col_spec("qw", required = FALSE, aliases = c("qw_perfil_bwpd", "qw_perfil"), unit = "bwpd"),
      col_spec("qf", required = FALSE, aliases = c("qf_perfil_bfpd", "qf_perfil"), unit = "bfpd"),
      col_spec("b", required = FALSE, aliases = c("b_exp", "b_hyp")),
      col_spec("di", required = FALSE, aliases = c("di_por_mes", "di_month"), unit = "1/month")
    )
  ),
  findings = list(
    title = "Findings: other analyses (injectors, field studies ...)", role = "optional", grain = "Source x well x action x evidence row",
    doc = paste("Evidence delivered by any other analysis. Rows with the same action, well, unit and interval become one",
                "opportunity (merged with the app's own findings). Family: M maturity, V velocity, U unit, S spatial, O operations."),
    sheet_aliases = c("findings", "hallazgos", "external_findings", "other_analyses", "estudios"),
    cols = list(
      col_spec("source", "chr", aliases = c("fuente", "analysis", "analisis", "study")),
      col_spec("well", "chr", aliases = c("pozo")),
      col_spec("sand", "chr", FALSE, c("unit", "unidad", "arena")),
      col_spec("interval_id", "chr", FALSE, c("intervalo_id", "intervalo", "interval")),
      col_spec("action", "chr", aliases = c("accion", "intervention", "job", "type")),
      col_spec("family", "chr", FALSE, c("familia", "evidence_family")),
      col_spec("metric", "chr", FALSE, c("variable", "metrica")),
      col_spec("value", required = FALSE, aliases = c("valor")),
      col_spec("reference", required = FALSE, aliases = c("referencia", "threshold")),
      col_spec("unit_label", "chr", FALSE, c("units", "unidades", "uom")),
      col_spec("comment", "chr", FALSE, c("comentario", "text", "note")),
      col_spec("gain_bopd", required = FALSE, aliases = c("gain", "qo_gain", "incremental_bopd"), unit = "bopd"),
      col_spec("date", "date", FALSE, c("fecha", "as_of"))
    )
  ),
  # ---- derived tables: optional, reconciliation only ----
  patterns = list(
    title = "Patterns (derived)", role = "derived", grain = "Pattern x month", doc = "Reconciliation only.",
    sheet_aliases = c("patterns", "patternrates"),
    cols = list(col_spec("pattern", "chr"), col_spec("date", "date"),
                col_spec("bopd", required = FALSE), col_spec("bwpd", required = FALSE), col_spec("bwipd", required = FALSE))
  ),
  patterns_vel = list(
    title = "Patterns_Vel (derived)", role = "derived", grain = "Pattern x month", doc = "Reconciliation only.",
    sheet_aliases = c("patterns_vel", "patternvelocity"),
    cols = list(col_spec("pattern", "chr"), col_spec("date", "date"),
                col_spec("wor", required = FALSE), col_spec("util", required = FALSE), col_spec("tp", required = FALSE), col_spec("iwr", required = FALSE))
  ),
  patterns_mat = list(
    title = "Patterns_Mat (derived)", role = "derived", grain = "Pattern x month", doc = "Reconciliation only.",
    sheet_aliases = c("patterns_mat", "patternmaturity"),
    cols = list(col_spec("pattern", "chr"), col_spec("date", "date"),
                col_spec("np", required = FALSE), col_spec("nw", required = FALSE), col_spec("nwi", required = FALSE),
                col_spec("iwrcum", required = FALSE), col_spec("opr", required = FALSE), col_spec("wpr", required = FALSE))
  ),
  injsand_calc = list(
    title = "InjSand_calc (derived)", role = "derived", grain = "Pattern x sand x month", doc = "Reconciliation only.",
    sheet_aliases = c("injsand_calc", "patternsandmetrics"),
    cols = list(col_spec("pattern", "chr"), col_spec("sand", "chr"), col_spec("date", "date"),
                col_spec("tp", required = FALSE), col_spec("nwi", required = FALSE))
  )
)

required_tables <- c("wells", "alloc", "vol", "fluids")

fluid_defaults <- list(bw = 1.02, visco = 5, viscw = 0.5, swc = 0.2, sor = 0.3,
                       krw = 0.3, kro = 0.9, nw = 2, no = 2)
