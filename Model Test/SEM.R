# SEM.R -- structural model for H1-H7 (DRAFT).
#
# !! The paths in `hypotheses` are PLACEHOLDERS. H1-H7 are not written down in
# !! the project, so replace them with the real hypotheses before running.
#
# Each hypothesis is "outcome ~ predictor" (several predictors allowed with +).
# Coefficients are labelled with the hypothesis id (H3, or H7_1, H7_2 when a
# hypothesis has several predictors), so they are easy to find in the output.
#
# The measurement part comes from common.R (same sample, same item drops) plus
# the choice in `measurement`, which should follow what Alternatives.R and
# Discriminant.R showed:
#   "as_is"         every factor separate
#   "merge_PBC_HAB" PBC and HAB items form one factor CTRLHAB (paths renamed)
#   "merge_core"    ATT, PBC, HAB items form one factor CORE (paths renamed)
#
# Output: sem_results/<sample>_<measurement>/{fit,paths,r2,indirect,vif}.csv

source("Model Test/Common.R")

measurement <- "as_is"

hypotheses <- c(            # PLACEHOLDERS -- replace with the real H1-H7
  H1 = "INT ~ ATT",
  H2 = "INT ~ SN",
  H3 = "INT ~ PBC",
  H4 = "INT ~ HAB",
  H5 = "RBF ~ INT",
  H6 = "RBF ~ HAB + PBC",
  H7 = "INT ~ SA + PA"
)
# Indirect effects to estimate (products of hypothesis labels), optional
indirect <- c(ATT_via_INT = "H1*H5", HAB_via_INT = "H4*H5")

out_dir <- file.path("sem_results", paste0(sample_tag(), "_", measurement))

rename <- switch(measurement,
  as_is         = character(0),
  merge_PBC_HAB = c(PBC = "CTRLHAB", HAB = "CTRLHAB"),
  merge_core    = c(ATT = "CORE", PBC = "CORE", HAB = "CORE"),
  stop("unknown measurement: ", measurement))

build_measurement <- function(fs) {
  if (!length(rename)) return(fs)
  for (new in unique(rename)) {
    old <- intersect(names(rename)[rename == new], names(fs))
    if (length(old)) fs <- c(setNames(list(unlist(fs[old], use.names = FALSE)), new),
                             fs[!names(fs) %in% old])
  }
  fs
}

# Turn the hypothesis list into labelled lavaan regressions for this group.
build_paths <- function(lv, g) {
  lines <- character(0); key <- list()
  for (h in names(hypotheses)) {
    parts <- strsplit(hypotheses[[h]], "~", fixed = TRUE)[[1]]
    y  <- trimws(parts[1]); y <- if (y %in% names(rename)) rename[[y]] else y
    xs <- trimws(strsplit(parts[2], "+", fixed = TRUE)[[1]])
    xs <- unique(ifelse(xs %in% names(rename), rename[xs], xs))
    xs <- setdiff(xs, y)
    miss <- setdiff(c(y, xs), lv)
    if (length(miss)) message(g, ": ", h, " uses ", paste(miss, collapse = ", "),
                              " (not in this group) -> those terms dropped")
    if (!y %in% lv) next
    xs <- intersect(xs, lv)
    if (!length(xs)) next
    labs <- if (length(xs) == 1) h else paste0(h, "_", seq_along(xs))
    lines <- c(lines, paste0(y, " ~ ", paste0(labs, "*", xs, collapse = " + ")))
    key[[h]] <- data.frame(hypothesis = h, label = labs, outcome = y, predictor = xs,
                           stringsAsFactors = FALSE)
  }
  list(lines = lines, key = do.call(rbind, key))
}

# VIF of each outcome's latent predictors, from the model-implied latent correlations
latent_vif <- function(fit, key, g) {
  R <- lavInspect(fit, "cor.lv")
  out <- list()
  for (y in unique(key$outcome)) {
    xs <- unique(key$predictor[key$outcome == y])
    if (length(xs) < 2) next
    v <- safe(diag(solve(R[xs, xs])), rep(NA, length(xs)))
    out[[y]] <- data.frame(group = g, outcome = y, predictor = xs, vif = round(v, 2))
  }
  bind_rows(out)
}

fits <- paths <- r2s <- inds <- vifs <- list()
for (g in groups) {
  dat <- read_group(g)
  fs  <- build_measurement(find_factors(dat, g))
  pb  <- build_paths(names(fs), g)
  if (!length(pb$lines)) { message(g, ": no hypotheses apply -- skipped"); next }
  used <- unique(c(pb$key$outcome, pb$key$predictor))
  fs   <- fs[names(fs) %in% used]
  items <- unlist(fs, use.names = FALSE)
  labs  <- pb$key$label
  ind   <- indirect[vapply(indirect, function(e)
             all(strsplit(gsub("\\s", "", e), "*", fixed = TRUE)[[1]] %in% labs), TRUE)]
  syn <- paste(c(make_syntax(fs), pb$lines,
                 if (length(ind)) paste(names(ind), ":=", ind)), collapse = "\n")
  cat("\n==", g, "==\n", syn, "\n", sep = "")

  r <- fit_lav(syn, dat, items, fun = lavaan::sem)
  fits[[g]] <- fit_summary(r, g, measurement)
  if (is.null(r$fit)) { message(g, ": model failed: ", paste(r$warns, collapse = " | ")); next }

  st <- standardizedSolution(r$fit)
  p  <- st[st$op == "~", c("lhs", "rhs", "label", "est.std", "se", "pvalue",
                           "ci.lower", "ci.upper")]
  p  <- merge(pb$key, p, by.x = "label", by.y = "label")
  paths[[g]] <- cbind(group = g, p[order(p$hypothesis, p$label),
                                   c("hypothesis", "label", "outcome", "predictor",
                                     "est.std", "se", "pvalue", "ci.lower", "ci.upper")])
  r2 <- safe(lavInspect(r$fit, "r2"), NULL)
  if (!is.null(r2)) {
    r2 <- r2[names(r2) %in% names(fs)]
    if (length(r2)) r2s[[g]] <- data.frame(group = g, outcome = names(r2), r2 = round(r2, 3))
  }
  d <- st[st$op == ":=", c("lhs", "est.std", "se", "pvalue", "ci.lower", "ci.upper")]
  if (nrow(d)) inds[[g]] <- cbind(group = g, d)
  vifs[[g]] <- latent_vif(r$fit, pb$key, g)
}

write_csv(bind_rows(fits),  out_dir, "fit")
write_csv(bind_rows(paths), out_dir, "paths")
write_csv(bind_rows(r2s),   out_dir, "r2")
write_csv(bind_rows(inds),  out_dir, "indirect")
write_csv(bind_rows(vifs),  out_dir, "vif")

v <- bind_rows(vifs)
if (!is.null(v) && any(v$vif > 5, na.rm = TRUE)) {
  cat("\nLatent collinearity (VIF > 5): standardized paths for these predictors are",
      "\nunstable and should not be interpreted one by one:\n")
  print(v[!is.na(v$vif) & v$vif > 5, ], row.names = FALSE)
}
cat("\nSaved to", normalizePath(out_dir, mustWork = FALSE), "\n")