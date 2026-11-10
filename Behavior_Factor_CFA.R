#  The questionnaire crosses constructs (ATT, SN, PBC, HAB, INT, SA, PA, RBF) 
#  with behaviours (alertness, speed, phone, helmet/seatbelt, ...).
#  Part of the shared item variance detected in main CFA model is behaviour-specific;
#  once it is modelled, ATT, PBC and HAB should become distinct (latent r < .85).
#
#  Model family: correlated constructs + uncorrelated behaviour factors (CT-UM). 
#  Each item loads on its construct factor and, if the wording names a behaviour, on that behaviour factor. 
#  Behaviour factors are orthogonal to the constructs and to each other. 
#  The behaviour of each item is fixed in advance from the wording, not from modification indices.
#  The mapping of items to behaviours is in behaviour_map.csv, built from the questionnaire wording.
#
#  Models per mode
#   base : Tier 1 constructs only (as in the first CFA run, with the agreed item drops)
#   A    : base + behaviour factors on the belief items (ATT, SN, PBC, HAB) - where the discriminant-validity problem is
#   B    : base + behaviour factors on all Tier 1 items (adds INT, SA, PA, RBF, NBV) - stronger, but absorbs behaviour-specific links between predictors and outcomes
#
#  A behaviour needs >= 3 items from >= 2 constructs to become a factor;
#  with exactly 2 items (2 constructs) it becomes a residual covariance.
#
#  Estimator: WLSMV, ordered items, std.lv = TRUE, pairwise missing.


library(lavaan)
 
data_dir <- "ItemPrep"              # folder with <Mode>_items_ready*.csv
map_file <- "behaviour_map.csv"     # from 08_build_behaviour_map.py
out_dir  <- "behaviour_results"
dir.create(out_dir, showWarnings = FALSE)
 
modes    <- c("Cyclist", "Pedestrian", "Driver", "Motorcyclist")
# modes  <- "Driver"                # run one mode while debugging
variants <- c("base", "A", "B")
 
prefixes  <- c(att = "ATT", sn = "SN", pbc = "PBC", hab = "HAB", int = "INT",
               sa = "SA", pa = "PA", rbf = "RBF", nbv = "NBV")
belief    <- c("ATT", "SN", "PBC", "HAB")          # variant A scope
key_facs  <- c("ATT", "PBC", "HAB", "SN", "INT")   # correlations to watch
dv_cut    <- 0.85                                  # discriminant-validity cut
 
# Agreed drops after the first CFA run (protective gear out of RBF;
# night clothing habit for pedestrians). penf1 is not in Tier 1.
drop_items <- list(Cyclist = "rbf1", Pedestrian = "hab5",
                   Driver = "rbf3", Motorcyclist = "rbf4")
 
bmap <- read.csv(map_file, stringsAsFactors = FALSE)
 
# ---- helpers ----------------------------------------------------------
source("Model Test/Common.R")
read_mode <- function(m) read_group(m)
construct_items <- function(d, m) { fs <- find_factors(d, m); fs[names(fs) %in% prefixes] }

behaviour_terms <- function(sp, m, scope) {
  item_con <- setNames(rep(names(sp), lengths(sp)), unlist(sp))
  mm <- bmap[bmap$mode == m & bmap$item %in% names(item_con), ]
  mm <- mm[item_con[mm$item] %in% scope, ]
  facs <- list(); rescov <- character(0)
  for (b in unique(mm$behaviour)) {
    its  <- mm$item[mm$behaviour == b]
    ncon <- length(unique(item_con[its]))
    if (length(its) >= 3 && ncon >= 2) facs[[paste0("B_", b)]] <- its
    else if (length(its) == 2 && ncon == 2) rescov <- c(rescov, paste(its, collapse = " ~~ "))
  }
  list(facs = facs, rescov = rescov)
}
 
build_model <- function(sp, bt) {
  lines <- sprintf("%s =~ %s", names(sp), sapply(sp, paste, collapse = " + "))
  if (length(bt$facs)) {
    lines <- c(lines, sprintf("%s =~ %s", names(bt$facs),
                              sapply(bt$facs, paste, collapse = " + ")))
    bf <- names(bt$facs); allf <- c(names(sp), bf)
    for (b in bf) {                       # orthogonal behaviour factors
      others <- setdiff(allf, b)
      others <- others[!(others %in% bf & match(others, allf) < match(b, allf))]
      if (length(others)) lines <- c(lines, sprintf("%s ~~ %s", b,
                                     paste0("0*", others, collapse = " + ")))
    }
  }
  c(lines, bt$rescov)
}
 
fit_one <- function(model, d, items) {
  tryCatch(
    cfa(paste(model, collapse = "\n"), data = d, ordered = items,
        estimator = "WLSMV", std.lv = TRUE, missing = "listwise"),
    error = function(e) { message("  fit error: ", conditionMessage(e)); NULL })
}
 
admissible <- function(fit) {
  ok <- tryCatch(suppressWarnings(lavInspect(fit, "post.check")),
                 error = function(e) FALSE)
  ss <- standardizedSolution(fit)
  isTRUE(ok) && all(abs(ss$est.std[ss$op == "=~"]) < 1, na.rm = TRUE)
}
 
fit_row <- function(fit, m, v, bt) {
  base <- data.frame(mode = m, variant = v, n_beh_factors = length(bt$facs),
                     n_rescov = length(bt$rescov))
  idx  <- c("npar", "chisq.scaled", "df.scaled", "pvalue.scaled",
            "cfi.scaled", "tli.scaled", "rmsea.scaled",
            "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "srmr")
  if (is.null(fit) || !lavInspect(fit, "converged")) {
    na <- as.data.frame(t(setNames(rep(NA_real_, length(idx)), idx)))
    return(cbind(base, converged = FALSE, admissible = NA, na))
  }
  fm   <- tryCatch(fitMeasures(fit), error = function(e) NULL)
  vals <- setNames(rep(NA_real_, length(idx)), idx)
  if (!is.null(fm)) {
    fm <- unclass(fm)                       # drop lavaan.vector class
    hit <- intersect(idx, names(fm))
    vals[hit] <- round(as.numeric(fm[hit]), 3)
  }
  cbind(base, converged = TRUE, admissible = admissible(fit),
        as.data.frame(as.list(vals)))
}
 
# omega of each construct on its own factor, with behaviour variance in the
# denominator (hierarchical-omega analogue), plus omega_total
omega_tab <- function(fit, sp, m, v) {
  ss  <- standardizedSolution(fit)
  imp <- lavInspect(fit, "implied")$cov
  do.call(rbind, lapply(names(sp), function(f) {
    its <- sp[[f]]
    lc  <- ss$est.std[ss$op == "=~" & ss$lhs == f & ss$rhs %in% its]
    tot <- sum(imp[its, its])
    lb  <- ss[ss$op == "=~" & ss$lhs != f & ss$rhs %in% its & grepl("^B_", ss$lhs), ]
    beh <- if (nrow(lb)) sum(tapply(lb$est.std, lb$lhs, function(x) sum(x)^2)) else 0
    data.frame(mode = m, variant = v, factor = f, k = length(its),
               omega_construct = round(sum(lc)^2 / tot, 3),
               omega_total = round((sum(lc)^2 + beh) / tot, 3),
               share_behaviour = round(beh / (sum(lc)^2 + beh), 3))
  }))
}
 
cor_tab <- function(fit, m, v) {
  ss <- standardizedSolution(fit)
  r  <- ss[ss$op == "~~" & ss$lhs != ss$rhs & ss$lhs %in% key_facs &
             ss$rhs %in% key_facs, ]
  data.frame(mode = m, variant = v, pair = paste(r$lhs, r$rhs, sep = "-"),
             r = round(r$est.std, 3), ci_low = round(r$ci.lower, 3),
             ci_up = round(r$ci.upper, 3), above_cut = abs(r$est.std) >= dv_cut)
}
 
load_tab <- function(fit, m, v) {
  ss <- standardizedSolution(fit); l <- ss[ss$op == "=~", ]
  con <- l[!grepl("^B_", l$lhs), c("rhs", "lhs", "est.std")]
  beh <- l[grepl("^B_", l$lhs), c("rhs", "lhs", "est.std")]
  names(con) <- c("item", "construct", "lambda_construct")
  names(beh) <- c("item", "behaviour", "lambda_behaviour")
  out <- merge(con, beh, by = "item", all.x = TRUE)
  out$lambda_construct <- round(out$lambda_construct, 3)
  out$lambda_behaviour <- round(out$lambda_behaviour, 3)
  cbind(mode = m, variant = v, out)
}
 
# ---- main loop --------------------------------------------------------
res <- list(fit = list(), cor = list(), omega = list(), load = list(),
            mi = list(), lrt = list())
 
for (m in modes) {
  cat("\n=====================", m, "=====================\n")
  d  <- read_mode(m)
  sp <- construct_items(d, m)
  items <- unlist(sp)
  cat(sprintf("N = %d | constructs: %s\n", nrow(d),
              paste(sprintf("%s(%d)", names(sp), lengths(sp)), collapse = " ")))
  fits <- list()
  for (v in variants) {
    scope <- switch(v, base = character(0), A = belief, B = names(sp))
    bt    <- behaviour_terms(sp, m, scope)
    model <- build_model(sp, bt)
    # writeLines(model, file.path(out_dir, sprintf("%s_%s_model.lav", m, v)))
    # cat(sprintf("\n-- %s: %d behaviour factors%s%s\n", v, length(bt$facs),
    #             if (length(bt$facs)) paste0(" (", paste(sprintf("%s:%d", names(bt$facs),
    #               lengths(bt$facs)), collapse = ", "), ")") else "",
    #             if (length(bt$rescov)) paste0("; rescov: ", paste(bt$rescov, collapse = ", ")) else ""))
    fit <- fit_one(model, d, items)
    fits[[v]] <- fit
    res$fit[[paste(m, v)]] <- fit_row(fit, m, v, bt)
    if (is.null(fit) || !lavInspect(fit, "converged")) { cat("   not converged\n"); next }
    fr <- res$fit[[paste(m, v)]]
    print(fr[, intersect(c("admissible", "cfi.scaled", "tli.scaled",
                           "rmsea.scaled", "srmr"), names(fr)), drop = FALSE],
          row.names = FALSE)
    ct <- cor_tab(fit, m, v); res$cor[[paste(m, v)]] <- ct
    cat("   ATT/PBC/HAB correlations:\n")
    print(ct[ct$pair %in% c("ATT-PBC", "ATT-HAB", "PBC-HAB", "PBC-ATT", "HAB-ATT", "HAB-PBC"),
             c("pair", "r", "ci_low", "ci_up")], row.names = FALSE)
    res$omega[[paste(m, v)]] <- omega_tab(fit, sp, m, v)
    res$load[[paste(m, v)]]  <- load_tab(fit, m, v)
    mi <- tryCatch(modindices(fit, sort. = TRUE, maximum.number = 15),
                   error = function(e) NULL)
    if (!is.null(mi)) res$mi[[paste(m, v)]] <-
      cbind(mode = m, variant = v, mi[, c("lhs", "op", "rhs", "mi", "sepc.all")])
  }
  # scaled difference tests (base nested in A nested in B)
  for (pr in list(c("base", "A"), c("A", "B"))) {
    f0 <- fits[[pr[1]]]; f1 <- fits[[pr[2]]]
    if (is.null(f0) || is.null(f1)) next
    t <- tryCatch(lavTestLRT(f1, f0), error = function(e) NULL)
    if (!is.null(t)) res$lrt[[paste(m, pr[2])]] <- data.frame(
      mode = m, comparison = paste(pr, collapse = " vs "),
      chisq_diff = round(t[2, "Chisq diff"], 2), df_diff = t[2, "Df diff"],
      p = signif(t[2, "Pr(>Chisq)"], 3))
  }
}
 
# ---- save -------------------------------------------------------------
save_all <- function(x, name) if (length(x))
  write.csv(do.call(rbind, x), file.path(out_dir, paste0(name, ".csv")), row.names = FALSE)
save_all(res$fit, "fit_comparison")
save_all(res$cor, "factor_correlations")
save_all(res$omega, "omega")
save_all(res$load, "loadings")
save_all(res$mi, "top_modification_indices")
save_all(res$lrt, "difference_tests")
 
# ---- MH7 decision table ------------------------------------------------
if (length(res$cor)) {
  ct <- do.call(rbind, res$cor)
  ct <- ct[ct$pair %in% c("ATT-PBC", "ATT-HAB", "PBC-HAB", "PBC-ATT", "HAB-ATT", "HAB-PBC"), ]
  dec <- aggregate(r ~ mode + variant, ct, function(x) max(abs(x)))
  names(dec)[3] <- "max_r_ATT_PBC_HAB"
  up  <- aggregate(ci_up ~ mode + variant, ct, max); names(up)[3] <- "max_ci_up"
  dec <- merge(dec, up)
  dec$distinct <- dec$max_r_ATT_PBC_HAB < dv_cut
  ft  <- do.call(rbind, lapply(res$fit, function(x)
           x[, intersect(c("mode", "variant", "admissible", "cfi.scaled", "srmr"), names(x))]))
  dec <- merge(dec, ft, all.x = TRUE)
  write.csv(dec, file.path(out_dir, "mh7_decision.csv"), row.names = FALSE)
  cat("\n================ MH7 decision ================\n")
  print(dec[order(dec$mode, dec$variant), ], row.names = FALSE)
  cat("\nMH7 supported for a mode if variant A is admissible, fits at least as\n",
      "well as base, and max |r| among ATT, PBC, HAB is below ", dv_cut, ".\n", sep = "")
}
