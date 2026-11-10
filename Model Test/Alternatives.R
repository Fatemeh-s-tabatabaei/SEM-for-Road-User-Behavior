# Alternatives.R -- competing measurement structures for the TPB block, to
# decide how to handle the ATT / PBC / HAB overlap before the structural model.
#
#   M0 five_factor     ATT, SN, PBC, HAB, INT as separate correlated factors
#   M1 merge_PBC_HAB   PBC and HAB items load on one factor (CTRLHAB)
#   M2 merge_core      ATT, PBC and HAB items load on one factor (CORE)
#   M3 second_order    M0 + CORE =~ ATT + PBC + HAB (SN, INT correlate with CORE)
#   M4 behaviour_res   M0 + residual correlations between items of DIFFERENT
#                      factors that ask about the SAME behaviour (needs
#                      behaviour_map.csv; this is the README's design explanation)
#   M5 esem            exploratory SEM over the same items (geomin rotation);
#                      off by default, needs lavaan >= 0.6-13
#
# M1, M2, M3 are nested in M0, and M0 is nested in M4, so each comparison
# gets a scaled chi-square difference test. Read the tests together with
# CFI/RMSEA/SRMR changes and with theory: with N of 200-400, small
# differences are often significant.
#
# Output: alternatives_results/<sample>/{fit,lrt,factor_cor,esem_loadings}.csv
# If behaviour_map.csv is missing, a template is written for you to fill in.

source("Model Test/Common.R")

alt_scope <- tpb            # factors in the block
run_esem  <- FALSE
behaviour_file <- "behaviour_map.csv"   # same map as Behavior_Factor_CFA.R (mode or group, item, behaviour)
out_dir   <- file.path("alternatives_results", sample_tag())

# ---- behaviour map -----------------------------------------------------------
read_behaviours <- function() {
  if (!file.exists(behaviour_file)) return(NULL)
  b <- read.csv(behaviour_file, stringsAsFactors = FALSE, strip.white = TRUE)
  if (!"group" %in% names(b) && "mode" %in% names(b)) names(b)[names(b) == "mode"] <- "group"
  b$group <- tolower(b$group)
  b[!is.na(b$behaviour) & nzchar(b$behaviour), ]
}

write_behaviour_template <- function(all_fs) {
  rows <- lapply(names(all_fs), function(g) {
    fs <- all_fs[[g]]
    data.frame(group = g, factor = rep(names(fs), lengths(fs)),
               item = unlist(fs, use.names = FALSE), behaviour = "",
               stringsAsFactors = FALSE)
  })
  write.csv(do.call(rbind, rows), file.path("Model Test", "behaviour_map_TEMPLATE.csv"),
            row.names = FALSE)
  cat("\nWrote Model Test/behaviour_map_TEMPLATE.csv. Fill in 'behaviour' (e.g. speed, phone,",
      "\nhelmet, alertness) using the questionnaire, save as", behaviour_file,
      "and rerun to get M4.\n")
}

behaviour_residuals <- function(fs, bmap, g) {
  if (is.null(bmap)) return(character(0))
  b <- bmap[bmap$group == tolower(g), ]
  fac_of <- setNames(rep(names(fs), lengths(fs)), unlist(fs, use.names = FALSE))
  b <- b[b$item %in% names(fac_of), ]
  out <- character(0)
  for (beh in unique(b$behaviour)) {
    its <- b$item[b$behaviour == beh]
    if (length(its) < 2) next
    pr <- combn(its, 2)
    for (j in seq_len(ncol(pr)))
      if (fac_of[[pr[1, j]]] != fac_of[[pr[2, j]]])
        out <- c(out, paste(pr[1, j], "~~", pr[2, j]))
  }
  out
}

# ---- model syntax ------------------------------------------------------------
merge_factors <- function(fs, which, new) {
  keep <- fs[!names(fs) %in% which]
  c(setNames(list(unlist(fs[which], use.names = FALSE)), new), keep)
}

esem_syntax <- function(fs) {
  lhs <- paste(sprintf('efa("tpb")*%s', names(fs)), collapse = " + ")
  paste(lhs, "=~", paste(unlist(fs, use.names = FALSE), collapse = " + "))
}

# ---- run ---------------------------------------------------------------------
bmap <- read_behaviours()
fits <- lrts <- cors <- esem_load <- list()
all_fs <- list()

for (g in groups) {
  dat <- read_group(g)
  fs  <- find_factors(dat, g)
  fs  <- fs[names(fs) %in% alt_scope]
  all_fs[[g]] <- fs
  if (length(fs) < 2) next
  items <- unlist(fs, use.names = FALSE)
  core  <- intersect(c("ATT", "PBC", "HAB"), names(fs))
  cat("\n", g, ": ", paste(sprintf("%s(%d)", names(fs), lengths(fs)), collapse = " "), "\n", sep = "")

  models <- list(five_factor = make_syntax(fs))
  if (all(c("PBC", "HAB") %in% names(fs)))
    models$merge_PBC_HAB <- make_syntax(merge_factors(fs, c("PBC", "HAB"), "CTRLHAB"))
  if (length(core) == 3) {
    models$merge_core   <- make_syntax(merge_factors(fs, core, "CORE"))
    models$second_order <- paste0(make_syntax(fs), "\nCORE =~ ",
                                  paste(core, collapse = " + "))
  }
  res <- behaviour_residuals(fs, bmap, g)
  if (length(res)) {
    models$behaviour_res <- paste(c(make_syntax(fs), res), collapse = "\n")
    cat("  M4 adds", length(res), "behaviour-based residual correlations\n")
  }

  fitted <- list()
  for (m in names(models)) {
    r <- fit_lav(models[[m]], dat, items)
    fitted[[m]] <- r$fit
    fits[[paste(g, m)]] <- fit_summary(r, g, m)
    cat(sprintf("  %-14s %s\n", m, if (is.null(r$fit)) "FAILED" else "ok"))
    if (!is.null(r$fit)) {
      R <- safe(lavInspect(r$fit, "cor.lv"), NULL)
      if (!is.null(R) && nrow(R) > 1) {
        ix <- which(upper.tri(R), arr.ind = TRUE)
        cors[[paste(g, m)]] <- data.frame(group = g, model = m,
          factor1 = rownames(R)[ix[, 1]], factor2 = colnames(R)[ix[, 2]],
          r = round(R[ix], 3), stringsAsFactors = FALSE)
      }
    }
  }

  # nested comparisons: (restricted, general)
  pairs <- list(c("merge_PBC_HAB", "five_factor"), c("merge_core", "five_factor"),
                c("second_order", "five_factor"), c("five_factor", "behaviour_res"))
  for (p in pairs) {
    if (is.null(fitted[[p[1]]]) || is.null(fitted[[p[2]]])) next
    t <- safe(lavTestLRT(fitted[[p[2]]], fitted[[p[1]]]), NULL)
    if (is.null(t) || nrow(t) < 2) next
    lrts[[paste(g, p[1])]] <- data.frame(group = g, restricted = p[1], general = p[2],
      chisq_diff = round(t[2, "Chisq diff"], 3), df_diff = t[2, "Df diff"],
      p = signif(t[2, "Pr(>Chisq)"], 3), stringsAsFactors = FALSE)
  }

  if (run_esem) {
    r <- fit_lav(esem_syntax(fs), dat, items, rotation = "geomin")
    fits[[paste(g, "esem")]] <- fit_summary(r, g, "esem")
    cat(sprintf("  %-14s %s\n", "esem", if (is.null(r$fit)) "FAILED" else "ok"))
    if (!is.null(r$fit)) {
      st <- standardizedSolution(r$fit)
      st <- st[st$op == "=~", c("lhs", "rhs", "est.std")]
      esem_load[[g]] <- cbind(group = g, st)
      R <- lavInspect(r$fit, "cor.lv"); ix <- which(upper.tri(R), arr.ind = TRUE)
      cors[[paste(g, "esem")]] <- data.frame(group = g, model = "esem",
        factor1 = rownames(R)[ix[, 1]], factor2 = colnames(R)[ix[, 2]],
        r = round(R[ix], 3), stringsAsFactors = FALSE)
    }
  }
}

if (is.null(bmap)) write_behaviour_template(all_fs)

write_csv(bind_rows(fits),      out_dir, "fit")
write_csv(bind_rows(lrts),      out_dir, "lrt")
write_csv(bind_rows(cors),      out_dir, "factor_cor")
write_csv(bind_rows(esem_load), out_dir, "esem_loadings")

f <- bind_rows(fits)
if (!is.null(f)) {
  cat("\nFit comparison:\n")
  print(f[, c("group", "model", "n_used", "cfi.scaled", "rmsea.scaled", "srmr",
              "admissible")], row.names = FALSE)
}
l <- bind_rows(lrts)
if (!is.null(l)) { cat("\nNested tests (significant = restriction worsens fit):\n")
                   print(l, row.names = FALSE) }
cat("\nSaved to", normalizePath(out_dir, mustWork = FALSE), "\n")