# SEM.R -- structural model for H1-H7.
#
# Measurement (choose with `measurement`, following Alternatives.R):
#   "second_order"  ATT, PBC and HAB are facets of one higher-order factor CORE
#                   (risk-favourable attitude, control and habit). Default.
#   "as_is"         every factor separate; a CORE term in a hypothesis is
#                   replaced by ATT + PBC + HAB
# plus, if `behaviour_res` is TRUE, residual correlations between belief items
# (ATT, SN, PBC, HAB) about the same behaviour, from behaviour_map.csv, and
# sa_k ~~ pa_k residuals for the parallel acceptance items.
#
# INT (next month) and RBF/NBV (past month) are parallel outcomes linked only by
# residual correlations, because a cross-sectional survey cannot order them.
#
# diagnose = TRUE fits, per group, four steps and reports which one breaks:
#   measurement  the same factors, CORE and residuals, all latent covariances free
#   INT_part     only hypotheses with outcome INT (and the SA -> PA step)
#   RBF_part     only hypotheses with outcome RBF / NBV (and SA -> PA)
#   full         everything
#
# Every setting can be overridden from the command line, for example
#   Rscript -e 'measurement <- "as_is"; source("Model Test/SEM.R")'
#   Rscript -e 'diagnose <- TRUE; source("Model Test/SEM.R")'
#
# Output: sem_results/<sample>_<measurement>[_res]/...

source("Model Test/Common.R")

set_default("measurement",    "second_order")
set_default("behaviour_res",  TRUE)
set_default("parallel_sa_pa", TRUE)
set_default("diagnose",       FALSE)
set_default("core_facets",    c("ATT", "PBC", "HAB"))
# saturate = TRUE: every outcome (INT, RBF, NBV and the mediator PA) is also
# regressed on all other predictors in the model, as control paths labelled
# C_<outcome>_<predictor>. The hypotheses are then tested holding the other
# predictors constant, and no relationship is forced to zero just because no
# hypothesis names it. Core facets (ATT, PBC, HAB) act through CORE only,
# except where a hypothesis names one (H9).
set_default("saturate",  TRUE)
# lv_bounds = TRUE keeps latent (residual) variances positive (lavaan bounds =
# "pos.var"). Use it only for a Heywood case whose CI includes admissible
# values, and report that you did.
set_default("lv_bounds", FALSE)
# equal_loadings: factors whose items get one common loading (tau-equivalence),
# per group. A standard remedy when a short scale produces a loading above 1
# (Heywood case) in a small sample. Report it as a constraint you imposed.
set_default("equal_loadings", list(Motorcyclist = c("MR", "PENF")))

set_default("hypotheses", c(
  # intention (next month)
  H1  = "INT ~ CORE",    # risk-favourable attitude, control and habit -> weaker safe intention (-)
  H2  = "INT ~ SN",      # friends' risky behaviour -> weaker intention (-)
  H3  = "PA ~ SA",       # city acceptance -> personal acceptance (+)
  H4  = "INT ~ PA",      # personal acceptance -> weaker intention (-)
                         # H5 = SA -> INT through PA: see `indirect`
  H6  = "INT ~ MR",      # willingness to act against others' violations -> stronger intention (+)
  H7  = "INT ~ PENF",    # perceived police enforcement -> stronger intention (+)
  H14 = "INT ~ PRISK",   # perceived risk of the behaviours -> stronger intention (+)
  # risky behaviour (past month)
  H8  = "RBF ~ CORE",    # (+)
  H9  = "RBF ~ HAB",     # habit beyond CORE (+)
  H10 = "RBF ~ SN",      # (+)
  H11 = "RBF ~ PA",      # (+)
  H12 = "RBF ~ PENF",    # (-)
  H13 = "NBV ~ PA"       # drivers only (+)
))
set_default("indirect",    c(H5 = "H3*H4"))
# Total effect of CORE on RBF: its direct path (H8) plus the route through the
# HAB facet (CORE -> HAB loading x H9). lc_<facet> are the CORE loadings.
set_default("totals",      c(H8_total = "H8 + lc_HAB*H9"))
# Joint Wald tests: are these paths together different from zero? Use them
# when collinear predictors make the single paths unstable.
set_default("joint_tests", list(H8_H9 = c("H8", "H9"), H8_H9_H10 = c("H8", "H9", "H10")))
set_default("outcome_cov", c("INT ~~ RBF", "INT ~~ NBV", "RBF ~~ NBV"))
# PENF and CAUSE items ask about cyclists and drivers (police checks; causes of
# crashes "while driving or cycling") in every form, so they do not describe
# pedestrians' own behaviour. Hypotheses listed here are skipped in those groups.
set_default("skip_groups", list(H7 = "Pedestrian", H12 = "Pedestrian", H14 = "Pedestrian"))

# Factors defined from a subset of items, added to those find_factors() builds.
# PRISK = perceived risk: the six CAUSE items that rate human-factor causes
# (unalert, speeding, phone, fatigue, hands-free, distraction); cause4 (vehicle
# malfunction) and cause8 (road conditions) are external causes and stay out.
set_default("custom_factors", list(PRISK = c("cause1", "cause2", "cause3",
                                             "cause5", "cause6", "cause7")))
# Residual correlations fixed in advance from near-identical wording
set_default("extra_res", c("cause1 ~~ cause5",    # unalert / fatigued
                           "cause3 ~~ cause6"))   # mobile phone / hands-free

stopifnot(measurement %in% c("second_order", "as_is"))
out_dir <- file.path("sem_results", paste0(sample_tag(), "_", measurement,
                                           if (behaviour_res) "_res" else "",
                                           if (saturate) "_sat" else "",
                                           if (lv_bounds) "_bounded" else "",
                                           if (length(unlist(equal_loadings))) "_eq" else "",
                                           if (diagnose) "_diagnose" else ""))

# ---- model building -----------------------------------------------------------
parse_h <- function(h) {
  parts <- strsplit(hypotheses[[h]], "~", fixed = TRUE)[[1]]
  list(y = trimws(parts[1]), xs = trimws(strsplit(parts[2], "+", fixed = TRUE)[[1]]))
}

build_paths <- function(lv, g, hyp, so, quiet = FALSE) {
  lines <- character(0); key <- list(); seen <- character(0)
  for (h in hyp) {
    if (g %in% skip_groups[[h]]) { if (!quiet) message(g, ": ", h, " not tested in this group"); next }
    p  <- parse_h(h)
    xs <- unlist(lapply(p$xs, function(x) if (x == "CORE" && !so) core_facets else x))
    miss <- setdiff(c(p$y, xs), lv)
    if (length(miss) && !quiet) message(g, ": ", h, " skips ", paste(miss, collapse = ", "),
                                        " (not in this group)")
    if (!p$y %in% lv) next
    xs <- setdiff(intersect(unique(xs), lv), p$y)
    dup <- paste(p$y, xs) %in% seen          # same path already claimed by an earlier hypothesis
    if (any(dup) && !quiet) message(g, ": ", h, " drops ", paste(xs[dup], collapse = ", "),
                                    " (path already in an earlier hypothesis)")
    xs <- xs[!dup]
    if (!length(xs)) next
    seen <- c(seen, paste(p$y, xs))
    labs <- if (length(xs) == 1) h else paste0(h, "_", seq_along(xs))
    lines <- c(lines, paste0(p$y, " ~ ", paste0(labs, "*", xs, collapse = " + ")))
    key[[h]] <- data.frame(hypothesis = h, label = labs, outcome = p$y, predictor = xs,
                           stringsAsFactors = FALSE)
  }
  list(lines = lines, key = do.call(rbind, key))
}

add_controls <- function(pb, core) {
  preds <- setdiff(unique(pb$key$predictor), core)
  outs  <- unique(pb$key$outcome)
  extra <- list()
  for (y in outs) {
    have <- pb$key$predictor[pb$key$outcome == y]
    # never let a variable predict something that predicts it (no loops)
    up   <- pb$key$outcome[pb$key$predictor == y]
    xs   <- setdiff(preds, c(y, have, up))
    if (!length(xs)) next
    labs <- paste0("C_", y, "_", xs)
    pb$lines <- c(pb$lines, paste0(y, " ~ ", paste0(labs, "*", xs, collapse = " + ")))
    extra[[y]] <- data.frame(hypothesis = "control", label = labs, outcome = y,
                             predictor = xs, stringsAsFactors = FALSE)
  }
  pb$key <- do.call(rbind, c(list(pb$key), extra))
  pb
}

prepare <- function(g, fs_all, bmap, hyp = names(hypotheses), quiet = FALSE) {
  so <- measurement == "second_order" && all(core_facets %in% names(fs_all))
  lv <- c(names(fs_all), if (so) "CORE")
  pb <- build_paths(lv, g, hyp, so, quiet)
  if (!length(pb$lines)) return(NULL)
  if (saturate) pb <- add_controls(pb, core = if (so) core_facets else character(0))
  used <- unique(c(pb$key$outcome, pb$key$predictor))
  if (so && "CORE" %in% used) used <- unique(c(setdiff(used, "CORE"), core_facets))
  fs <- fs_all[names(fs_all) %in% used]
  so <- so && all(core_facets %in% names(fs))
  eq   <- intersect(equal_loadings[[g]], names(fs))
  meas <- vapply(names(fs), function(f)
            if (f %in% eq) paste0(f, " =~ ", paste0("eq_", f, "*", fs[[f]], collapse = " + "))
            else paste(f, "=~", paste(fs[[f]], collapse = " + ")), "")
  meas <- c(unname(meas), if (so) paste("CORE =~", paste0("lc_", core_facets, "*", core_facets,
                                                          collapse = " + ")))
  if (length(eq) && !quiet) message(g, ": equal loadings within ", paste(eq, collapse = ", "))
  res  <- if (behaviour_res) behaviour_residuals(fs, bmap, g, scope = belief) else character(0)
  if (parallel_sa_pa && all(c("SA", "PA") %in% names(fs))) {
    k   <- intersect(sub("^sa", "", fs$SA), sub("^pa", "", fs$PA))
    res <- c(res, paste0("sa", k, " ~~ pa", k))
  }
  items_used <- unlist(fs, use.names = FALSE)
  res  <- c(res, extra_res[vapply(strsplit(extra_res, " ~~ "),
                                  function(v) all(v %in% items_used), TRUE)])
  ocov <- outcome_cov[vapply(strsplit(outcome_cov, " ~~ "), function(v) all(v %in% names(fs)), TRUE)]
  ind  <- indirect[vapply(indirect, function(e)
            all(strsplit(gsub("\\s", "", e), "*", fixed = TRUE)[[1]] %in% pb$key$label), TRUE)]
  avail <- c(pb$key$label, if (so) paste0("lc_", core_facets))
  tot   <- totals[vapply(totals, function(e)
             all(regmatches(e, gregexpr("[A-Za-z][A-Za-z0-9_]*", e))[[1]] %in% avail), TRUE)]
  list(fs = fs, items = unlist(fs, use.names = FALSE), so = so, key = pb$key, eq = eq,
       meas = meas, res = res,
       struct = c(pb$lines, ocov, if (length(ind)) paste(names(ind), ":=", ind),
                  if (length(tot)) paste(names(tot), ":=", tot)))
}

add_custom <- function(fs, dat, g) {
  gone <- read_drops()$item[read_drops()$group == tolower(g)]
  for (nm in names(custom_factors)) {
    its <- setdiff(intersect(custom_factors[[nm]], names(dat)), gone)
    if (length(its) >= min_items) fs[[nm]] <- its
    else message(g, ": ", nm, " has only ", length(its), " item(s) -> not modelled")
  }
  fs
}

fit_lav2 <- function(syn, dat, items, fun = lavaan::sem) {
  if (lv_bounds) fit_lav(syn, dat, items, fun = fun, bounds = "pos.var")
  else           fit_lav(syn, dat, items, fun = fun)
}

# ---- checks -------------------------------------------------------------------
health <- function(fit) {
  if (is.null(fit)) return(list(ok = FALSE, se_ok = FALSE, n_neg = NA, max_beta = NA))
  pe    <- parameterEstimates(fit)
  free  <- pe$se != 0 | is.na(pe$se)
  se_ok <- !any(is.na(pe$se[free & pe$op %in% c("=~", "~", "~~")]))
  est   <- safe(lavInspect(fit, "est"), NULL)
  neg   <- if (is.null(est)) character(0) else
             c(names(which(diag(est$psi) < 0)),                # latent (residual) variances
               names(which(diag(est$theta) < -1e-6)))          # item residual variances
  n_neg <- if (is.null(est)) NA else length(neg)
  st    <- standardizedSolution(fit)
  b     <- abs(st$est.std[st$op == "~"])
  adm   <- isTRUE(safe(suppressWarnings(lavInspect(fit, "post.check")), FALSE))
  big   <- st[st$op %in% c("=~", "~") & abs(st$est.std) > 1, ]
  list(ok = isTRUE(lavInspect(fit, "converged")) && adm && se_ok,
       se_ok = se_ok, n_neg = n_neg, max_beta = if (length(b)) round(max(b), 3) else NA,
       neg = neg, big = if (nrow(big)) sprintf("%s %s %s = %.2f", big$lhs, big$op, big$rhs,
                                                big$est.std) else character(0))
}

latent_vif <- function(fit, key, g) {
  R <- safe(lavInspect(fit, "cor.lv"), NULL)
  if (is.null(R)) return(NULL)
  out <- list()
  for (y in unique(key$outcome)) {
    xs <- intersect(unique(key$predictor[key$outcome == y]), rownames(R))
    if (length(xs) < 2) next
    v <- safe(diag(solve(R[xs, xs])), rep(NA, length(xs)))
    out[[y]] <- data.frame(group = g, outcome = y, predictor = xs, vif = round(v, 2))
  }
  bind_rows(out)
}

# ---- run ----------------------------------------------------------------------
bmap <- if (behaviour_res) read_behaviours() else NULL
if (behaviour_res && is.null(bmap)) warning("behaviour_map.csv not found: no behaviour residuals")

fits <- paths <- r2s <- inds <- vifs <- loads <- diag_rows <- joints <- list()
for (g in groups) {
  dat    <- read_group(g)
  fs_all <- add_custom(find_factors(dat, g), dat, g)

  if (diagnose) {
    outs <- vapply(names(hypotheses), function(h) parse_h(h)$y, "")
    steps <- list(
      measurement = names(hypotheses),
      INT_part    = names(hypotheses)[outs %in% c("INT", "PA")],
      RBF_part    = names(hypotheses)[outs %in% c("RBF", "NBV", "PA")],
      full        = names(hypotheses))
    cat("\n==", g, "== diagnosis\n")
    for (s in names(steps)) {
      m <- prepare(g, fs_all, bmap, steps[[s]], quiet = s != "measurement")
      if (is.null(m)) next
      if (s == "measurement") {
        r <- fit_lav2(paste(c(m$meas, m$res), collapse = "\n"), dat, m$items, lavaan::cfa)
      } else {
        r <- fit_lav2(paste(c(m$meas, m$struct, m$res), collapse = "\n"), dat, m$items)
      }
      h  <- health(r$fit)
      fr <- fit_summary(r, g, s)
      diag_rows[[paste(g, s)]] <- cbind(fr[, c("group", "model", "n_used", "converged",
                                               "admissible", "cfi.scaled", "rmsea.scaled", "srmr")],
                                        se_ok = h$se_ok, n_negative_var = h$n_neg,
                                        max_abs_beta = h$max_beta)
      cat(sprintf("  %-12s %-4s items=%2d  cfi=%s  neg.var=%s  max|beta|=%s  %s\n", s,
                  if (h$ok) "ok" else "FAIL", length(m$items),
                  format(fr$cfi.scaled), format(h$n_neg), format(h$max_beta),
                  if (length(r$warns)) substr(paste(r$warns, collapse = " | "), 1, 90) else ""))
      if (length(h$neg)) cat("    negative variances:", paste(h$neg, collapse = ", "), "\n")
      if (length(h$big)) cat("    standardized > 1:", paste(h$big, collapse = ", "), "\n")
      if (s == "measurement" && !is.null(r$fit)) {
        R <- safe(lavInspect(r$fit, "cor.lv"), NULL)
        if (!is.null(R)) { ix <- which(upper.tri(R) & abs(R) > .80, arr.ind = TRUE)
          if (nrow(ix)) cat("    latent |r| > .80:", paste(sprintf("%s-%s %.2f",
              rownames(R)[ix[, 1]], colnames(R)[ix[, 2]], R[ix]), collapse = ", "), "\n") }
      }
    }
    next
  }

  m <- prepare(g, fs_all, bmap)
  if (is.null(m)) { message(g, ": no hypotheses apply -- skipped"); next }
  cat(sprintf("\n== %s ==  %d factors%s, %d items, %d residual correlations\n",
              g, length(m$fs), if (m$so) " + CORE" else "", length(m$items), length(m$res)))
  r <- fit_lav2(paste(c(m$meas, m$struct, m$res), collapse = "\n"), dat, m$items)
  fits[[g]] <- fit_summary(r, g, basename(out_dir))
  h <- health(r$fit)
  if (is.null(r$fit)) { message(g, ": model failed: ", paste(r$warns, collapse = " | ")); next }
  if (!h$ok) cat("  !! NOT USABLE: converged/admissible/SE check failed. Estimates below are not interpretable.\n",
                 if (length(h$neg)) paste("     negative variances:", paste(h$neg, collapse = ", "), "\n"),
                 if (length(h$big)) paste("     standardized > 1:", paste(h$big, collapse = ", "), "\n"))

  st <- standardizedSolution(r$fit)
  p  <- st[st$op == "~", c("label", "est.std", "se", "pvalue", "ci.lower", "ci.upper")]
  p  <- merge(m$key, p, by = "label")
  paths[[g]] <- cbind(group = g, usable = h$ok,
                      p[order(p$hypothesis, p$label),
                        c("hypothesis", "label", "outcome", "predictor",
                          "est.std", "se", "pvalue", "ci.lower", "ci.upper")])
  r2 <- safe(lavInspect(r$fit, "r2"), NULL)
  if (!is.null(r2)) {
    r2 <- r2[names(r2) %in% unique(m$key$outcome)]
    if (length(r2)) r2s[[g]] <- data.frame(group = g, outcome = names(r2), r2 = round(r2, 3))
  }
  d <- st[st$op == ":=", c("lhs", "est.std", "se", "pvalue", "ci.lower", "ci.upper")]
  if (nrow(d)) inds[[g]] <- cbind(group = g, d)
  vifs[[g]] <- latent_vif(r$fit, m$key, g)
  for (jt in names(joint_tests)) {
    labs <- joint_tests[[jt]]
    if (!all(labs %in% m$key$label)) next
    w <- safe(lavTestWald(r$fit, constraints = paste(paste(labs, "== 0"), collapse = "; ")), NULL)
    if (!is.null(w)) joints[[paste(g, jt)]] <- data.frame(group = g, test = jt,
      paths = paste(labs, collapse = "+"), wald = round(w$stat, 3), df = w$df,
      p = signif(w$p.value, 3))
  }
  if (m$so) {
    cl <- st[st$op == "=~" & st$lhs == "CORE", c("rhs", "est.std")]
    loads[[g]] <- data.frame(group = g, facet = cl$rhs, loading = round(cl$est.std, 3))
  }
  pr <- paths[[g]]
  pr <- pr[pr$hypothesis != "control", ]
  cat(sprintf("  %-4s %-4s <- %-5s beta = %6.3f  p = %s\n", pr$hypothesis, pr$outcome,
              pr$predictor, pr$est.std, ifelse(is.na(pr$pvalue), "NA", signif(pr$pvalue, 3))), sep = "")
}

write_csv(bind_rows(diag_rows), out_dir, "diagnosis")
write_csv(bind_rows(fits),  out_dir, "fit")
write_csv(bind_rows(paths), out_dir, "paths")
write_csv(bind_rows(r2s),   out_dir, "r2")
write_csv(bind_rows(inds),  out_dir, "indirect")
write_csv(bind_rows(vifs),  out_dir, "vif")
write_csv(bind_rows(joints), out_dir, "joint_tests")
write_csv(bind_rows(loads), out_dir, "core_loadings")

dg <- bind_rows(diag_rows)
if (!is.null(dg)) { cat("\nDiagnosis:\n"); print(dg, row.names = FALSE) }
f <- bind_rows(fits)
if (!is.null(f)) { cat("\nFit:\n"); print(f[, c("group", "n_used", "cfi.scaled", "rmsea.scaled",
                                                  "srmr", "admissible")], row.names = FALSE) }
j <- bind_rows(joints)
if (!is.null(j)) { cat("\nJoint Wald tests (all listed paths = 0):\n"); print(j, row.names = FALSE) }
dd <- bind_rows(inds)
if (!is.null(dd)) { cat("\nIndirect and total effects (standardized):\n")
                    print(dd[, c("group", "lhs", "est.std", "pvalue", "ci.lower", "ci.upper")],
                          row.names = FALSE) }
v <- bind_rows(vifs)
if (!is.null(v) && any(v$vif > 5, na.rm = TRUE)) {
  cat("\nLatent collinearity (VIF > 5): do not interpret these paths one by one:\n")
  print(v[!is.na(v$vif) & v$vif > 5, ], row.names = FALSE)
}
cat("\nSaved to", normalizePath(out_dir, mustWork = FALSE), "\n")