# Common.R -- shared settings and helpers, sourced by CFA.R, Discriminant.R,
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
# Constructs screened in Correlations.py / Pruningcandidates.py (rule R4) and
# excluded from all later analysis: binary items, item-rest -.14 to .19 in every group.
set_default("excluded_prefixes", "aenf")

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
set_default("drop_file", file.path("Screening", "drop_items_CONFIRMED.csv"))

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
# Row filter for one sample definition. Also used by CFA.R (apply_sample) for
# the raw files behind rbf_mean, so both use the same respondents.
filter_sample <- function(d, def = sample_def, sl_max = sl_max_blocks, label = "") {
  is1  <- function(col) if (col %in% names(d)) !is.na(d[[col]]) & d[[col]] == 1
                        else rep(FALSE, nrow(d))
  drop <- rep(FALSE, nrow(d))
  if (def != "all" && "dup_of" %in% names(d))
    drop <- drop | (!is.na(d$dup_of) & d$dup_of != "")
  if (def %in% c("clean", "clean_adults"))  drop <- drop | is1("straightline")
  if (def %in% c("adults", "clean_adults")) drop <- drop | is.na(d$age) | d$age < 18
  if (!is.na(sl_max)) {
    if ("sl_blocks" %in% names(d)) {
      drop <- drop | (!is.na(d$sl_blocks) & d$sl_blocks >= sl_max)
    } else warning(label, ": sl_blocks column not found; sl_max_blocks ignored")
  }
  d[!drop, , drop = FALSE]
}
apply_sample <- function(d) filter_sample(d)

read_group <- function(g, def = sample_def, sl_max = sl_max_blocks) {
  f <- list.files(data_dir, pattern = paste0("^", g, "_items_ready.*\\.csv$"),
                  full.names = TRUE)
  if (!length(f)) stop("No ready file for ", g, " in ",
                       normalizePath(data_dir, mustWork = FALSE))
  d  <- read.csv(f[1], na.strings = c("", "NA"), fileEncoding = "UTF-8-BOM")
  n0 <- nrow(d)
  d  <- filter_sample(d, def, sl_max, g)
  message(sprintf("%-13s %4d -> %4d rows  (sample = %s)", g, n0, nrow(d), sample_tag()))
  d
}

read_drops <- function(path = drop_file) {
  if (is.null(path))
    return(data.frame(group = character(0), item = character(0)))
  if (!file.exists(path)) {
    warning("drop file not found: ", normalizePath(path, mustWork = FALSE),
            " -- no item drops applied from file", call. = FALSE)
    return(data.frame(group = character(0), item = character(0)))
  }
  x <- read.csv(path, stringsAsFactors = FALSE, strip.white = TRUE)
  stopifnot(all(c("group", "item") %in% names(x)))
  x$group <- tolower(x$group)
  x
}

# In-script respecification lists, if the calling script defines them:
#   add_items  <- list(Cyclist = list(SN = "sn4_r"))   reversed items to add
#   drop_items <- list(Cyclist = c("sn5", "pbc7"))      items to remove
get_spec <- function(name, g) {
  if (!exists(name, envir = globalenv(), inherits = FALSE)) return(NULL)
  x <- get(name, envir = globalenv())
  if (is.list(x)) x[[g]] else NULL
}

# list(FACTOR = c(items)), items sorted by number.
#   use_items = FALSE : original item set (no _r items, no drops) = "M0"
#   use_items = TRUE  : drops from drop_file and drop_items[[g]]; _r items from
#                       add_items[[g]] if given, otherwise all of them when
#                       include_reversed is TRUE. A recoded item replaces its raw one.
find_factors <- function(dat, g, use_items = TRUE, reversed = include_reversed,
                         drops = read_drops()) {
  spec_add  <- get_spec("add_items", g)
  spec_drop <- unlist(get_spec("drop_items", g), use.names = FALSE)
  gone <- if (use_items) unique(c(drops$item[drops$group == tolower(g)], spec_drop))
          else character(0)
  if (use_items && length(gone))
    message(g, ": dropping ", paste(gone, collapse = ", "))
  out <- list()
  for (nm in names(factor_prefix)) {
    p   <- factor_prefix[[nm]]
    if (p %in% excluded_prefixes) next
    its <- grep(paste0("^", p, "[0-9]+$"), names(dat), value = TRUE)
    if (use_items) {
      rev <- grep(paste0("^", p, "[0-9]+_r$"), names(dat), value = TRUE)
      if (!is.null(spec_add)) rev <- intersect(rev, spec_add[[nm]])
      else if (!reversed)     rev <- character(0)
      its <- setdiff(c(setdiff(its, sub("_r$", "", rev)), rev), gone)
    }
    its <- its[order(as.numeric(gsub("\\D", "", its)))]
    if (length(its) >= min_items) out[[nm]] <- its
    else if (length(its) > 0) message(g, ": ", nm, " has only ", length(its),
                                      " item(s) left -> not modelled")
  }
  out
}

# ---- Behaviour map (the questionnaire grid) -----------------------------------
# behaviour_map.csv tags each item with the behaviour its wording names
# (speed, phone, ...). Shared by Alternatives.R and SEM.R.
set_default("behaviour_file", "behaviour_map.csv")
set_default("belief", c("ATT", "SN", "PBC", "HAB"))   # where the overlap problem is

read_behaviours <- function(path = behaviour_file) {
  if (!file.exists(path)) return(NULL)
  b <- read.csv(path, stringsAsFactors = FALSE, strip.white = TRUE)
  if (!"group" %in% names(b) && "mode" %in% names(b)) names(b)[names(b) == "mode"] <- "group"
  b$group <- tolower(b$group)
  b[!is.na(b$behaviour) & nzchar(b$behaviour), ]
}

# Residual correlations between items of DIFFERENT factors in `scope` that ask
# about the same behaviour. Fixed by wording in advance, not chosen from the data.
behaviour_residuals <- function(fs, bmap, g, scope = names(fs)) {
  if (is.null(bmap)) return(character(0))
  fs <- fs[names(fs) %in% scope]
  if (!length(fs)) return(character(0))
  fac_of <- setNames(rep(names(fs), lengths(fs)), unlist(fs, use.names = FALSE))
  b <- bmap[bmap$group == tolower(g) & bmap$item %in% names(fac_of), ]
  out <- character(0)
  for (beh in unique(b$behaviour)) {
    its <- unique(b$item[b$behaviour == beh])
    if (length(its) < 2) next
    pr <- combn(its, 2)
    for (j in seq_len(ncol(pr)))
      if (fac_of[[pr[1, j]]] != fac_of[[pr[2, j]]])
        out <- c(out, paste(pr[1, j], "~~", pr[2, j]))
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