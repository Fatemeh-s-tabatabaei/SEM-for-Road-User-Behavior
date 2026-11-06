# common.R -- shared settings and helpers, sourced by CFA.R, Discriminant.R,
# Alternatives.R and SEM.R so every script uses the same sample and items.
#
# Settings use "set only if not already set", so a script can override any of
# them by assigning the variable BEFORE source("common.R").

suppressPackageStartupMessages(library(lavaan))

set_default <- function(name, value) {
  if (!exists(name, envir = globalenv(), inherits = FALSE))
    assign(name, value, envir = globalenv())
}

# ---- Settings ---------------------------------------------------------------
set_default("data_dir", "ItemPrep")
set_default("groups",   c("Cyclist", "Pedestrian", "Driver", "Motorcyclist"))

# Sample definition
#   "all"          every row in the ready file
#   "no_dups"      drop rows with dup_of set
#   "clean"        no_dups and drop straightline == 1   (= Correlations.py "clean")
#   "adults"       no_dups and age >= 18
#   "clean_adults" clean and age >= 18
set_default("sample_def", "clean")

# Extra straightlining rule for sensitivity runs: also drop respondents with
# sl_blocks >= this number. NA = off. Pick the value from StraightlineCheck.py.
set_default("sl_max_blocks", NA)

# Use reverse-coded items (columns ending in _r, already recoded) as indicators.
# The original CFA.R regex silently excluded them.
set_default("include_reversed", TRUE)

# Approved item drops: CSV with columns group,item. Missing file = no drops.
set_default("drop_file", file.path("Model Test", "drop_items.csv"))

set_default("missing_method", "listwise")
set_default("min_items", 3)

set_default("factor_prefix", c(ATT = "att", SN = "sn", PBC = "pbc", HAB = "hab",
                               INT = "int", SA = "sa", PA = "pa", RBF = "rbf",
                               NBV = "nbv", INFRA = "infra", MR = "mr",
                               CAUSE = "cause", PENF = "penf"))
set_default("tpb",   c("ATT", "SN", "PBC", "HAB", "INT"))
set_default("tier1", c("ATT", "SN", "PBC", "HAB", "INT", "SA", "PA", "RBF", "NBV"))

# Short tag for output folders, so runs with different samples don't overwrite
sample_tag <- function() {
  paste0(sample_def,
         if (!is.na(sl_max_blocks)) paste0("_sl", sl_max_blocks) else "",
         if (!include_reversed) "_norev" else "")
}

# ---- Data -------------------------------------------------------------------
read_group <- function(g, def = sample_def, sl_max = sl_max_blocks) {
  f <- list.files(data_dir, pattern = paste0("^", g, "_items_ready.*\\.csv$"),
                  full.names = TRUE)
  if (!length(f)) stop("No ready file for ", g, " in ",
                       normalizePath(data_dir, mustWork = FALSE))
  d  <- read.csv(f[1], na.strings = c("", "NA"), fileEncoding = "UTF-8-BOM")
  n0 <- nrow(d)
  is1  <- function(col) if (col %in% names(d)) !is.na(d[[col]]) & d[[col]] == 1
                        else rep(FALSE, nrow(d))
  drop <- rep(FALSE, nrow(d))
  if (def != "all" && "dup_of" %in% names(d)) drop <- drop | !is.na(d$dup_of)
  if (def %in% c("clean", "clean_adults"))  drop <- drop | is1("straightline")
  if (def %in% c("adults", "clean_adults")) drop <- drop | is.na(d$age) | d$age < 18
  if (!is.na(sl_max)) {
    if ("sl_blocks" %in% names(d)) {
      drop <- drop | (!is.na(d$sl_blocks) & d$sl_blocks >= sl_max)
    } else warning(g, ": sl_blocks column not found; sl_max_blocks ignored")
  }
  d <- d[!drop, , drop = FALSE]
  message(sprintf("%-13s %4d -> %4d rows  (sample = %s)", g, n0, nrow(d), sample_tag()))
  d
}

read_drops <- function(path = drop_file) {
  if (is.null(path) || !file.exists(path))
    return(data.frame(group = character(0), item = character(0)))
  x <- read.csv(path, stringsAsFactors = FALSE, strip.white = TRUE)
  stopifnot(all(c("group", "item") %in% names(x)))
  x$group <- tolower(x$group)
  x
}

# list(FACTOR = c(items)), items sorted by number; applies drops and _r rule
find_factors <- function(dat, g, reversed = include_reversed, drops = read_drops()) {
  gone <- drops$item[drops$group == tolower(g)]
  out  <- list()
  for (nm in names(factor_prefix)) {
    p  <- factor_prefix[[nm]]
    rx <- if (reversed) paste0("^", p, "[0-9]+(_r)?$") else paste0("^", p, "[0-9]+$")
    its <- grep(rx, names(dat), value = TRUE)
    # if both raw (sn4) and recoded (sn4_r) exist, keep only the recoded one
    base <- sub("_r$", "", its)
    dup  <- base %in% base[grepl("_r$", its)] & !grepl("_r$", its)
    if (any(dup)) message(g, ": using recoded version instead of ",
                          paste(its[dup], collapse = ", "))
    its <- setdiff(its[!dup], gone)
    its <- its[order(as.numeric(gsub("\\D", "", its)))]
    if (length(its) >= min_items) out[[nm]] <- its
    else if (length(its) > 0) message(g, ": ", nm, " has only ", length(its),
                                      " item(s) left -> not modelled")
  }
  out
}

# ---- Models -----------------------------------------------------------------
make_syntax <- function(fs) {
  paste(sprintf("%s =~ %s", names(fs),
                vapply(fs, paste, "", collapse = " + ")), collapse = "\n")
}

safe <- function(expr, default = NA) tryCatch(expr, error = function(e) default)

# Fit any lavaan model on ordered items; never stops the run.
fit_lav <- function(model, dat, items, fun = lavaan::cfa, ...) {
  warns <- character(0)
  fit <- tryCatch(
    withCallingHandlers(
      fun(model, data = dat[, items, drop = FALSE], ordered = items,
          estimator = "WLSMV", std.lv = TRUE, missing = missing_method, ...),
      warning = function(w) { warns <<- c(warns, conditionMessage(w))
                              invokeRestart("muffleWarning") }),
    error = function(e) { warns <<- c(warns, paste("ERROR:", conditionMessage(e)))
                          NULL })
  list(fit = fit, warns = unique(warns))
}

# One row of fit information (scaled statistics, as appropriate for WLSMV)
fit_summary <- function(r, g, label) {
  cols <- c("chisq.scaled", "df.scaled", "pvalue.scaled", "cfi.scaled",
            "tli.scaled", "rmsea.scaled", "srmr")
  base <- data.frame(group = g, model = label, stringsAsFactors = FALSE)
  if (is.null(r$fit)) {
    fm <- setNames(as.list(rep(NA_real_, length(cols))), cols)
    return(cbind(base, converged = FALSE, admissible = NA, n_used = NA,
                 as.data.frame(fm), warnings = paste(r$warns, collapse = " | ")))
  }
  fm <- safe(fitMeasures(r$fit, cols), setNames(rep(NA_real_, length(cols)), cols))
  cbind(base,
        converged  = isTRUE(lavInspect(r$fit, "converged")),
        admissible = safe(suppressWarnings(lavInspect(r$fit, "post.check")), NA),
        n_used     = lavInspect(r$fit, "nobs"),
        as.data.frame(as.list(round(fm, 4))),
        warnings   = paste(r$warns, collapse = " | "))
}

write_csv <- function(x, dir, name) {
  if (is.null(x) || !NROW(x)) return(invisible(NULL))
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  write.csv(x, file.path(dir, paste0(name, ".csv")), row.names = FALSE)
}
bind_rows <- function(x) { x <- Filter(Negate(is.null), x)
                           if (length(x)) do.call(rbind, x) else NULL }