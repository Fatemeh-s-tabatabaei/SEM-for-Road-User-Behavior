library(lavaan)

# 1. Settings -----------------------------------------------------------
data_dir <- "ItemPrep"            # *_items_ready.csv (merged categories)
raw_dir  <- "CleaningResults"     # *_model_data.csv (original 1-5 codes, for rbf_mean)
out_dir  <- "cfa_results"
groups   <- c("Cyclist", "Pedestrian", "Driver", "Motorcyclist")
id_col   <- "resp_id"             # respondent id, written next to rbf_mean

sl_max_blocks  <- NA              # e.g. 5: also drop sl_blocks >= 5 (sensitivity)
include_reversed <- TRUE          # use *_r items (the old regex skipped them)
missing_method <- "listwise"
min_items   <- 3
min_loading <- 0.50
min_omega   <- 0.70
min_ave     <- 0.50
mi_min      <- 10

factor_prefix <- c(ATT = "att", SN = "sn", PBC = "pbc", HAB = "hab",
                   INT = "int", SA = "sa", PA = "pa", RBF = "rbf",
                   NBV = "nbv", INFRA = "infra", MR = "mr",
                   CAUSE = "cause", PENF = "penf")

blocks <- list(
  tpb        = c("ATT", "SN", "PBC", "HAB", "INT"),
  acceptance = c("SA", "PA"),
  context    = c("MR", "PENF", "CAUSE", "INFRA")
)
tier1 <- c("ATT", "SN", "PBC", "HAB", "INT", "SA", "PA", "RBF", "NBV")

# 2. Respecification (numbers refer to the CFA notes) -------------------

# Residual covariances (used only if both items are in the model)
res_cov <- list(
  Cyclist = c("hab5 ~~ hab6",
              "att4 ~~ int4",
              paste0("sa", 1:6, " ~~ pa", 1:6)),
  Pedestrian = "rbf1 ~~ rbf2"
)
# If PENF still has AVE below min_ave without penf1, PENF is dropped (#9)

# M0 = previous run, M1 = item changes, M2 = item changes + residual covariances
steps <- list(M0 = c(items = FALSE, cov = FALSE),
              M1 = c(items = TRUE,  cov = FALSE),
              M2 = c(items = TRUE,  cov = TRUE))

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# 3. Helpers -------------------------------------------------------------
apply_sample <- function(d) {
  if (sample_def %in% c("no_dups", "adults") && "dup_of" %in% names(d)) d <- d[is.na(d$dup_of), ]
  if (sample_def == "adults")                 d <- d[!is.na(d$age) & d$age >= 18, ]
  d
}

source("Model Test/Common.R")                # read_group(), find_factors(), sample_tag()
out_dir <- file.path(out_dir, sample_tag())   # one folder per sample setting
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

make_syntax <- function(fs, cov_lines = NULL) {
  items <- unlist(fs)
  lines <- sapply(names(fs), function(f)
    paste0(f, " =~ ", paste(fs[[f]], collapse = " + ")))
  for (cl in cov_lines)
    if (all(strsplit(cl, " ~~ ")[[1]] %in% items)) lines <- c(lines, cl)
  paste(lines, collapse = "\n")
}

fit_cfa <- function(fs, dat, cov_lines = NULL) {
  items <- unlist(fs, use.names = FALSE)
  warns <- character(0)
  fit <- tryCatch(
    withCallingHandlers(
      cfa(make_syntax(fs, cov_lines), data = dat[, items, drop = FALSE], ordered = items,
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
                     ifelse(out$ave < min_ave, " AVE < .50", ""))
  cbind(group = g, model = label, out, row.names = NULL)
}

htmt_rows <- function(fit, fs, g, label) {
  R <- as.matrix(lavInspect(fit, "sampstat")$cov)
  nm <- names(fs)
  within <- sapply(fs, function(it) { m <- abs(R[it, it]); mean(m[lower.tri(m)]) })
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

# rbf_mean from the ORIGINAL 1-5 codes, all RBF items including rbf1 (#5)
write_rbf_mean <- function(g, g_dir) {
  f <- file.path(raw_dir, paste0(g, "_model_data.csv"))
  if (!file.exists(f)) { cat("  rbf_mean skipped, file not found:", f, "\n"); return(invisible()) }
  d <- apply_sample(read.csv(f, na.strings = c("", "NA"), fileEncoding = "UTF-8-BOM"))
  its <- grep("^rbf[0-9]+$", names(d), value = TRUE)
  x <- d[, its]
  ok <- rowSums(!is.na(x)) >= length(its) / 2          # at least half answered
  id <- if (id_col %in% names(d)) d[[id_col]] else seq_len(nrow(d))
  out <- data.frame(id = id, rbf_mean = ifelse(ok, rowMeans(x, na.rm = TRUE), NA))
  names(out)[1] <- id_col
  write.csv(out, file.path(g_dir, "rbf_mean.csv"), row.names = FALSE)
  cat(sprintf("  rbf_mean: items %s, n = %d, mean = %.2f, sd = %.2f\n",
              paste(its, collapse = " "), sum(ok), mean(out$rbf_mean, na.rm = TRUE),
              sd(out$rbf_mean, na.rm = TRUE)))
}

# 4. Run -----------------------------------------------------------------
for (g in groups) {
  cat("\n=====", g, "=====\n")
  dat   <- read_group(g)
  g_dir <- file.path(out_dir, g)
  dir.create(g_dir, showWarnings = FALSE, recursive = TRUE)

  res <- list(fit = list(), load = list(), rel = list(), fcor = list(),
              htmt = list(), mi = list())
  record <- function(label, fsub, cov_lines) {
    r <- fit_cfa(fsub, dat, cov_lines)
    res$fit[[label]] <<- fit_row(r, g, label, fsub)
    cat(sprintf("  %-18s items=%2d  %s\n", label, length(unlist(fsub)),
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
    }
    res$mi[[label]] <<- mi_rows(r$fit, g, label)
  }

  for (s in names(steps)) {
    if (s == "M2" && is.null(res_cov[[g]])) next
    sp <- steps[[s]]
    fs <- find_factors(dat, g, sp[["items"]])
    cl <- if (sp[["cov"]]) res_cov[[g]] else NULL
    cat("\n", s, " factors: ", paste(sprintf("%s(%d)", names(fs), lengths(fs)), collapse = "  "),
        "\n", sep = "")

    # direction check of added reversed items: should correlate positively
    if (s == "M1") for (nm in names(add_items[[g]])) for (it in intersect(add_items[[g]][[nm]], fs[[nm]])) {
      rho <- cor(dat[[it]], rowMeans(dat[setdiff(fs[[nm]], it)], na.rm = TRUE),
                 use = "complete.obs", method = "spearman")
      cat(sprintf("  direction check: %s vs other %s items: rho = %.2f%s\n", it, nm, rho,
                  if (rho < 0) "  <- NEGATIVE, check the reversal" else ""))
    }

    # # Stage 1: one factor at a time
    # for (f in names(fs)) record(paste0(s, "_one_", f), fs[f], cl)

    # # #9: PENF still weak without penf1 -> drop the whole scale
    # if (sp[["items"]] && "PENF" %in% names(fs) && "penf1" %in% drop_items[[g]]) {
    #   ave <- res$rel[[paste0(s, "_one_PENF")]]$ave
    #   if (!is.null(ave) && ave < min_ave) {
    #     cat(sprintf("  PENF AVE = %.2f < %.2f without penf1 -> PENF dropped\n", ave, min_ave))
    #     fs$PENF <- NULL
    #   }
    # }

    # Stage 2: blocks, Tier 1, Tier 1 without the RBF factor
    for (b in names(blocks)) {
      fb <- fs[names(fs) %in% blocks[[b]]]
      if (length(fb) >= 2) record(paste0(s, "_block_", b), fb, cl)
    }
    ft <- fs[names(fs) %in% tier1]
    if (length(ft) >= 2) record(paste0(s, "_tier1"), ft, cl)
    ft <- ft[names(ft) != "RBF"]
    if (length(ft) >= 2) record(paste0(s, "_tier1_noRBF"), ft, cl)
  }

  # 5. Output ------------------------------------------------------------
  bind <- function(x) if (length(x)) do.call(rbind, x) else NULL
  write_out <- function(x, name)
    if (!is.null(x)) write.csv(x, file.path(g_dir, paste0(name, ".csv")), row.names = FALSE)
  fit_tab <- bind(res$fit)
  write_out(fit_tab,         "fit")
  write_out(bind(res$load),  "loadings")
  write_out(bind(res$rel),   "reliability")
  write_out(bind(res$fcor),  "factor_cor")
  write_out(bind(res$htmt),  "htmt")
  write_out(bind(res$mi),    "modindices")
  write_rbf_mean(g, g_dir)

  # step comparison for the multi-factor models
  tpb <- c("ATT", "SN", "PBC", "HAB")
  cmp <- list()
  for (lab in names(res$fit)) {
    if (!grepl("_(block_tpb|tier1|tier1_noRBF)$", lab)) next
    f  <- res$fit[[lab]]; fc <- res$fcor[[lab]]; ht <- res$htmt[[lab]]
    sel <- if (is.null(fc)) FALSE else fc$factor1 %in% tpb & fc$factor2 %in% tpb
    cmp[[lab]] <- data.frame(
      model = lab, n_items = f$n_items, n_used = f$n_used, admissible = f$admissible,
      cfi = f$cfi, tli = f$tli, rmsea = f$rmsea, srmr = f$srmr,
      max_r_tpb      = if (any(sel)) max(abs(fc$r[sel])) else NA,
      n_r_gt_sqrtave = if (is.null(fc)) NA else sum(fc$flag != ""),
      n_htmt_gt_85   = if (is.null(ht)) NA else sum(ht$flag != ""))
  }
  cmp <- bind(cmp)
  write_out(cmp, "step_comparison")
  cat("\n  Step comparison:\n")
  print(format(cmp, digits = 3), row.names = FALSE)

  # weak items and scales in the final step (int1 check, #6)
  s <- bind(res$load)
  weak <- s[s$model == "M2_tier1" & s$flag != "", c("factor", "item", "std_loading")]
  cat(sprintf("\n  M2 tier1, items with std loading < %.2f: %d\n", min_loading, nrow(weak)))
  if (nrow(weak)) print(weak, row.names = FALSE)
  r <- bind(res$rel)
  lowr <- r[grepl("^M2_one_", r$model) & r$flag != "", c("factor", "omega", "ave", "flag")]
  if (nrow(lowr)) { cat("\n  M2 one-factor models with flags:\n"); print(lowr, row.names = FALSE) }
}

cat("\nResults written to", normalizePath(out_dir), "\n")