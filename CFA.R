library(lavaan)

# 1. Settings
data_dir <- "ItemPrep"            # folder with the four ready files
out_dir  <- "cfa_results"
groups   <- c("Cyclist", "Pedestrian", "Driver", "Motorcyclist")
# groups <- "Cyclist"             # <- one group while debugging

sample_def     <- "no_dups"       # "all"     every row
                                  # "no_dups" drop rows with dup_of set
                                  # "adults"  no_dups and age >= 18
missing_method <- "listwise"      # or "pairwise" (keeps more rows, but the
                                  # correlation matrix can fail to be PD)
min_items   <- 3                  # fewer items -> not a factor
min_loading <- 0.50               # standardized loading flag
min_omega   <- 0.70               # reliability flag
mi_min      <- 10                 # smallest modification index reported
run_all_factors <- FALSE          # also fit ALL factors in one model
                                  # (large; N = 206 motorcyclists is risky)

# factor name -> column prefix
factor_prefix <- c(ATT = "att", SN = "sn", PBC = "pbc", HAB = "hab",
                   INT = "int", SA = "sa", PA = "pa", RBF = "rbf",
                   NBV = "nbv", INFRA = "infra", MR = "mr",
                   CAUSE = "cause", PENF = "penf")

# A block is fitted only if >= 2 of its factors exist.
blocks <- list(
  tpb        = c("ATT", "SN", "PBC", "HAB", "INT"),
  acceptance = c("SA", "PA"),
  context    = c("MR", "PENF", "CAUSE", "INFRA")
)
# Tier 1 structural model (H1-H7): everything that predicts INT and RBF.
# NBV (parking violations) exists for drivers only.
tier1 <- c("ATT", "SN", "PBC", "HAB", "INT", "SA", "PA", "RBF", "NBV")

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# 2. Helpers
read_group <- function(g) { # read the ready file for one group, filter by sample_def
  f <- list.files(data_dir, pattern = paste0("^", g, "_items_ready.*\\.csv$"),
                  full.names = TRUE)
  if (length(f) == 0) stop("No ready file for ", g, " in '", data_dir, "'")
  d <- read.csv(f[1], na.strings = c("", "NA"), fileEncoding = "UTF-8-BOM")
  n0 <- nrow(d)
  if (sample_def %in% c("no_dups", "adults")) d <- d[is.na(d$dup_of), ]
  if (sample_def == "adults")                 d <- d[!is.na(d$age) & d$age >= 18, ]
  cat(sprintf("\n==== %s: %d of %d rows (sample = %s) ====\n",
              g, nrow(d), n0, sample_def))
  d
}

# list of factors -> list(NAME = c(items)), items sorted by their number
find_factors <- function(dat) {
  fs <- list()
  for (nm in names(factor_prefix)) {
    p   <- factor_prefix[[nm]]
    its <- grep(paste0("^", p, "[0-9]+$"), names(dat), value = TRUE)
    if (length(its) >= min_items) {
      its <- its[order(as.numeric(sub(p, "", its)))]
      fs[[nm]] <- its
    }
  }
  fs
}

make_syntax <- function(fs) {
  paste(sapply(names(fs), function(f)
    paste0(f, " =~ ", paste(fs[[f]], collapse = " + "))), collapse = "\n")
}

# fit one model; never stops the whole run
fit_cfa <- function(fs, dat) {
  items <- unlist(fs, use.names = FALSE)
  warns <- character(0)
  fit <- tryCatch(
    withCallingHandlers(
      cfa(make_syntax(fs), data = dat[, items, drop = FALSE], ordered = items,
          estimator = "WLSMV", std.lv = TRUE, missing = missing_method),
      warning = function(w) { warns <<- c(warns, conditionMessage(w))
                              invokeRestart("muffleWarning") }),
    error = function(e) { warns <<- c(warns, paste("ERROR:", conditionMessage(e)))
                          NULL })
  list(fit = fit, warns = unique(warns))
}

safe <- function(expr, default = NA) tryCatch(expr, error = function(e) default)

fit_row <- function(res, g, label, fs) {
  fit <- res$fit
  base <- data.frame(group = g, model = label, n_factors = length(fs),
                     n_items = length(unlist(fs)), stringsAsFactors = FALSE)
  if (is.null(fit)) {
    return(cbind(base, n_used = NA, converged = FALSE, admissible = FALSE,
                 chisq = NA, df = NA, p = NA, cfi = NA, tli = NA, rmsea = NA,
                 rmsea_lo = NA, rmsea_hi = NA, srmr = NA,
                 warnings = paste(res$warns, collapse = " | ")))
  }
  conv <- lavInspect(fit, "converged")
  fm <- safe(fitMeasures(fit, c("chisq.scaled", "df.scaled", "pvalue.scaled",
                                "cfi.scaled", "tli.scaled", "rmsea.scaled",
                                "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled",
                                "srmr")), rep(NA_real_, 9))
  # admissible = converged, no negative variances, no |std loading| > 1
  post <- safe(suppressWarnings(lavInspect(fit, "post.check")), FALSE)
  ss   <- safe(standardizedSolution(fit), NULL)
  over <- !is.null(ss) && any(abs(ss$est.std[ss$op == "=~"]) > 1, na.rm = TRUE)
  cbind(base, n_used = lavInspect(fit, "nobs"), converged = conv,
        admissible = isTRUE(conv) && isTRUE(post) && !over,
        chisq = fm[1], df = fm[2], p = fm[3], cfi = fm[4], tli = fm[5],
        rmsea = fm[6], rmsea_lo = fm[7], rmsea_hi = fm[8], srmr = fm[9],
        warnings = paste(res$warns, collapse = " | "))
}

loading_rows <- function(fit, g, label) {
  ss <- standardizedSolution(fit)
  ss <- ss[ss$op == "=~", c("lhs", "rhs", "est.std", "se", "pvalue")]
  names(ss) <- c("factor", "item", "std_loading", "se", "p")
  ss$flag <- ifelse(ss$std_loading < min_loading, paste("loading <", min_loading), "")
  cbind(group = g, model = label, ss, row.names = NULL)
}

# composite reliability (omega from standardized loadings) and AVE
reliab_rows <- function(fit, g, label) {
  ss <- standardizedSolution(fit)
  ss <- ss[ss$op == "=~", ]
  out <- lapply(split(ss, factor(ss$lhs, levels = unique(ss$lhs))), function(x) {
    l <- x$est.std
    data.frame(factor = x$lhs[1], n_items = length(l),
               omega = sum(l)^2 / (sum(l)^2 + sum(1 - l^2)),
               ave = mean(l^2), min_loading = min(l),
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, out)
  out$flag <- paste0(ifelse(out$omega < min_omega, paste("omega <", min_omega, ";"), ""),
                     ifelse(out$ave < 0.5, " AVE < .50", ""))
  cbind(group = g, model = label, out, row.names = NULL)
}

# HTMT from the sample (polychoric) item correlations
htmt_rows <- function(fit, fs, g, label) {
  R <- as.matrix(lavInspect(fit, "sampstat")$cov)   # polychoric correlations
  nm <- names(fs)
  within <- sapply(fs, function(it) {
    m <- abs(R[it, it]); mean(m[lower.tri(m)])
  })
  rows <- list()
  for (i in seq_along(nm)) for (j in seq_along(nm)) if (i < j) {
    between <- mean(abs(R[fs[[i]], fs[[j]]]))
    rows[[length(rows) + 1]] <- data.frame(
      factor1 = nm[i], factor2 = nm[j],
      htmt = between / sqrt(within[i] * within[j]))
  }
  if (length(rows) == 0) return(NULL)
  out <- do.call(rbind, rows)
  out$flag <- ifelse(out$htmt > 0.85, "HTMT > .85", "")
  cbind(group = g, model = label, out, row.names = NULL)
}

factor_cor_rows <- function(fit, rel, g, label) {
  R <- lavInspect(fit, "cor.lv")
  nm <- rownames(R)
  rows <- list()
  for (i in seq_along(nm)) for (j in seq_along(nm)) if (i < j) {
    sq <- min(sqrt(rel$ave[rel$factor == nm[i]]), sqrt(rel$ave[rel$factor == nm[j]]))
    rows[[length(rows) + 1]] <- data.frame(
      factor1 = nm[i], factor2 = nm[j], r = R[i, j],
      sqrt_ave_min = sq,
      flag = ifelse(abs(R[i, j]) > sq, "|r| > sqrt(AVE)", ""))
  }
  if (length(rows) == 0) return(NULL)
  cbind(group = g, model = label, do.call(rbind, rows), row.names = NULL)
}

mi_rows <- function(fit, g, label, top = 20) {
  mi <- safe(modindices(fit, sort. = TRUE, minimum.value = mi_min), NULL)
  if (is.null(mi) || nrow(mi) == 0) return(NULL)
  mi <- mi[mi$op %in% c("=~", "~~"), c("lhs", "op", "rhs", "mi", "epc", "sepc.all")]
  mi <- head(mi, top)
  if (nrow(mi) == 0) return(NULL)
  mi$type <- ifelse(mi$op == "=~", "cross-loading", "residual correlation")
  cbind(group = g, model = label, mi, row.names = NULL)
}

# ---- 3. Run -----------------------------------------------------------
all_fit <- list()

for (g in groups) {
  dat <- read_group(g)
  fs  <- find_factors(dat)
  cat("Factors:", paste(sprintf("%s(%d)", names(fs), lengths(fs)), collapse = "  "), "\n")

  res <- list(fit = list(), load = list(), rel = list(), fcor = list(),
              htmt = list(), mi = list())
  record <- function(label, fsub) {
    r <- fit_cfa(fsub, dat)
    res$fit[[label]] <<- fit_row(r, g, label, fsub)
    cat(sprintf("  %-14s items=%2d  %s\n", label, length(unlist(fsub)),
                if (is.null(r$fit)) "FAILED"
                else if (!isTRUE(res$fit[[label]]$admissible)) "fitted, NOT admissible"
                else "ok"))
    if (is.null(r$fit)) return(invisible(NULL))
    res$load[[label]] <<- loading_rows(r$fit, g, label)
    rel <- reliab_rows(r$fit, g, label)
    res$rel[[label]]  <<- rel
    if (length(fsub) >= 2) {
      res$fcor[[label]] <<- factor_cor_rows(r$fit, rel, g, label)
      res$htmt[[label]] <<- htmt_rows(r$fit, fsub, g, label)
      res$mi[[label]]   <<- mi_rows(r$fit, g, label)
    }
  }

  # Stage 1: one factor at a time
  for (f in names(fs)) record(paste0("one_", f), fs[f])

  # Stage 2a: blocks
  for (b in names(blocks)) {
    fb <- fs[names(fs) %in% blocks[[b]]]
    if (length(fb) >= 2) record(paste0("block_", b), fb)
  }

  # Stage 2b: Tier 1 model
  ft <- fs[names(fs) %in% tier1]
  if (length(ft) >= 2) record("tier1", ft)

  # optional: everything at once
  if (run_all_factors) record("all_factors", fs)

  bind <- function(x) if (length(x)) do.call(rbind, x) else NULL
  write_out <- function(x, name)
    if (!is.null(x)) write.csv(x, file.path(out_dir, paste0(g, "_", name, ".csv")),
                               row.names = FALSE)
  fit_tab <- bind(res$fit)
  write_out(fit_tab,         "fit")
  write_out(bind(res$load),  "loadings")
  write_out(bind(res$rel),   "reliability")
  write_out(bind(res$fcor),  "factor_cor")
  write_out(bind(res$htmt),  "htmt")
  write_out(bind(res$mi),    "modindices")
  all_fit[[g]] <- fit_tab

  # console summary
  s <- bind(res$load)
  if (!is.null(s)) {
    weak <- s[s$model %in% paste0("one_", names(fs)) & s$flag != "", ]
    cat(sprintf("\n  Items with std loading < %.2f in their one-factor model: %d\n",
                min_loading, nrow(weak)))
    if (nrow(weak)) print(weak[, c("factor", "item", "std_loading")], row.names = FALSE)
  }
  r <- bind(res$rel)
  if (!is.null(r)) {
    lowr <- r[r$model %in% paste0("one_", names(fs)) & r$omega < min_omega,
              c("factor", "omega", "ave")]
    if (nrow(lowr)) { cat("\n  Scales with omega <", min_omega, ":\n")
                      print(lowr, row.names = FALSE) }
  }
  cat("\n  Fit of multi-factor models:\n")
  mf <- fit_tab[fit_tab$n_factors >= 2,
                c("model", "n_items", "n_used", "converged", "admissible",
                  "cfi", "tli", "rmsea", "srmr")]
  if (nrow(mf)) print(format(mf, digits = 3), row.names = FALSE)
}

write.csv(do.call(rbind, all_fit), file.path(out_dir, "all_groups_fit.csv"),
          row.names = FALSE)
cat("\nResults written to", normalizePath(out_dir), "\n")