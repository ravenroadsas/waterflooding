# Input table specifications ---------------------------------------------------
#
# The six input tables are described once here: canonical column names, the
# aliases accepted when reading user files (English / Spanish variants), which
# columns are required and their type. Loaders, validation and the Excel
# template are all driven by these specs.

col_spec <- function(name, type = "num", required = TRUE, aliases = character(),
                     unit = "", doc = "") {
  list(name = name, type = type, required = required, aliases = aliases,
       unit = unit, doc = doc)
}

wf_schema <- list(
  production = list(
    title = "Monthly production / injection",
    doc = "One row per well and month. Rates are calendar-day averages for the month.",
    sheet_aliases = c("production", "prod", "produccion", "monthly", "rates"),
    cols = list(
      col_spec("well", "chr", aliases = c("well_name", "pozo", "uwi", "wellname")),
      col_spec("date", "date", aliases = c("month", "fecha", "period", "mes")),
      col_spec("bopd", aliases = c("oil_rate", "qo", "oil_bpd"), unit = "stb/d", doc = "Oil rate"),
      col_spec("bwpd", aliases = c("water_rate", "qw", "water_bpd"), unit = "stb/d", doc = "Water production rate"),
      col_spec("bwipd", aliases = c("winj", "water_inj", "qwi", "inj_rate", "bwpid"), unit = "stb/d", doc = "Water injection rate")
    )
  ),
  hierarchy = list(
    title = "Hierarchy",
    doc = paste("Field > block > pattern > well membership. A producer shared by",
                "several patterns appears once per pattern. Optional x / y enable the reservoir map."),
    sheet_aliases = c("hierarchy", "jerarquia", "tree", "patterns"),
    cols = list(
      col_spec("field", "chr", FALSE, c("campo")),
      col_spec("block", "chr", FALSE, c("sector", "area", "bloque", "segment")),
      col_spec("pattern", "chr", aliases = c("patron", "pattern_name", "pattern_id")),
      col_spec("well", "chr", aliases = c("well_name", "pozo", "uwi")),
      col_spec("well_type", "chr", FALSE, c("type", "tipo", "role"), doc = "PRODUCER or INJECTOR"),
      col_spec("x", "num", FALSE, c("x_coord", "easting", "coord_x"), unit = "m / ft"),
      col_spec("y", "num", FALSE, c("y_coord", "northing", "coord_y"), unit = "m / ft")
    )
  ),
  stooip = list(
    title = "STOOIP & average properties per pattern / sand",
    doc = "One row per pattern and sand unit.",
    sheet_aliases = c("stooip", "ooip", "poes", "volumetrics"),
    cols = list(
      col_spec("pattern", "chr", aliases = c("patron", "pattern_name")),
      col_spec("sand", "chr", aliases = c("sand_index", "sandindex", "sandxind", "zone", "unit", "arena", "layer")),
      col_spec("stooip", aliases = c("stooip_stb", "ooip", "poes", "n"), unit = "stb"),
      col_spec("area", required = FALSE, aliases = c("area_acres"), unit = "acres"),
      col_spec("net_pay", required = FALSE, aliases = c("h", "net_pay_ft", "hn", "thickness"), unit = "ft"),
      col_spec("porosity", required = FALSE, aliases = c("phi", "por"), unit = "frac"),
      col_spec("swi", required = FALSE, aliases = c("sw", "sw_init", "swo"), unit = "frac"),
      col_spec("permeability", required = FALSE, aliases = c("k", "perm", "k_md"), unit = "mD"),
      col_spec("boi", required = FALSE, aliases = c("bo_i", "bo_init"), unit = "rb/stb")
    )
  ),
  fluids = list(
    title = "Fluid & relative permeability",
    doc = "One row per sand (sand = * is a default for every sand). Corey model.",
    sheet_aliases = c("fluids", "fluid", "pvt", "relperm", "kr", "fluidos"),
    cols = list(
      col_spec("sand", "chr", aliases = c("sand_index", "zone", "unit", "arena", "layer", "region")),
      col_spec("bo", aliases = c("bo_rb_stb"), unit = "rb/stb"),
      col_spec("bw", required = FALSE, aliases = c("bw_rb_stb"), unit = "rb/stb"),
      col_spec("mu_o", aliases = c("muo", "visc_oil", "uo"), unit = "cp"),
      col_spec("mu_w", required = FALSE, aliases = c("muw", "visc_water", "uw"), unit = "cp"),
      col_spec("swc", aliases = c("swir", "swcr"), unit = "frac"),
      col_spec("sor", aliases = c("sorw"), unit = "frac"),
      col_spec("krw_or", required = FALSE, aliases = c("krw_max", "krw_end", "krwor"), unit = "frac", doc = "krw at Sor"),
      col_spec("kro_wc", required = FALSE, aliases = c("kro_max", "kro_end", "krowc"), unit = "frac", doc = "kro at Swc"),
      col_spec("nw", required = FALSE, aliases = c("corey_w", "n_w"), doc = "Corey exponent, water"),
      col_spec("no", required = FALSE, aliases = c("corey_o", "n_o"), doc = "Corey exponent, oil")
    )
  ),
  petrophysics = list(
    title = "Petrophysics per well / sand",
    doc = "Splits each well's volumes between sands by kh (falls back to STOOIP share).",
    sheet_aliases = c("petrophysics", "petro", "petrofisica", "logs"),
    cols = list(
      col_spec("well", "chr", aliases = c("well_name", "pozo", "uwi")),
      col_spec("sand", "chr", aliases = c("sand_index", "zone", "unit", "arena", "layer")),
      col_spec("net_pay", aliases = c("h", "net_pay_ft", "hn", "thickness"), unit = "ft"),
      col_spec("porosity", required = FALSE, aliases = c("phi", "por"), unit = "frac"),
      col_spec("sw", required = FALSE, aliases = c("swi"), unit = "frac"),
      col_spec("permeability", required = FALSE, aliases = c("k", "perm", "k_md"), unit = "mD")
    )
  ),
  allocation = list(
    title = "Areal allocation coefficients",
    doc = paste("Fraction of each well's volumes assigned to a pattern. Without a date the",
                "coefficient is constant; with dates each value holds until the next date."),
    sheet_aliases = c("allocation", "alloc", "coefficients", "asignacion", "factores"),
    cols = list(
      col_spec("pattern", "chr", aliases = c("patron", "pattern_name")),
      col_spec("well", "chr", aliases = c("well_name", "pozo", "uwi")),
      col_spec("coefficient", aliases = c("coef", "factor", "alloc", "allocation", "fraction", "af"), unit = "frac"),
      col_spec("date", "date", FALSE, c("month", "fecha", "effective_date", "from"))
    )
  )
)

fluid_defaults <- list(bw = 1.02, mu_w = 0.5, krw_or = 0.3, kro_wc = 0.9, nw = 2, no = 2)
