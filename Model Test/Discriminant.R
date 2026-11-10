# Discriminant.R -- formal discriminant-validity tests for the factors that go
# into the structural model.
#
# Two pieces of evidence per factor pair, following Ronkko & Cho (2022,
# Organizational Research Methods, "An updated guideline for assessing
# discriminant validity"):
#   1. CI_CFA: the 95% CI of the factor correlation from one CFA.
#      Upper bound of |r|:  < .80 no problem | .80-.90 marginal |
#                           .90-1  moderate  | >= 1 severe
#   2. chi-square(cut): refit with the correlation FIXED at each value in
#      `cuts` and compare with the free model (scaled difference test).
#      A significant result with |r| below the cut means the two factors are
#      distinguishable at that level. Only run for pairs whose CI reaches
#      `test_from`, because every test is a full refit.
#
# Output: discriminant_results/<sample>/{fit,factor_cor_ci,chisq_cut_tests}.csv

source("Model Test/Common.R")

scope     <- tier1          # factors in the tested model; set to tpb for a faster run
cuts      <- c(0.90, 1.00)  # correlation values to test against
test_from <- 0.80           # run chi-square(cut) tests when CI upper bound >= this
out_dir   <- file.path("discriminant_results", sample_tag())

classify <- function(ub) as.character(cut(ub, c(-Inf, .80, .90, 1, Inf), right = FALSE,
                         labels = c("no problem", "marginal", "moderate", "severe")))

lrt_row <- function(free, con) {
  t <- safe(lavTestLRT(free, con), NULL)
  if (is.null(t) || nrow(t) < 2) return(c(chisq_diff = NA, df_diff = NA, p = NA))
  c(chisq_diff = t[2, "Chisq diff"], df_diff = t[2, "Df diff"], p = t[2, "Pr(>Chisq)"])
}

fits <- cors <- tests <- list()
for (g in groups) {
  dat <- read_group(g)
  fs  <- find_factors(dat, g)
  fs  <- fs[names(fs) %in% scope]
  if (length(fs) < 2) next
  items    <- unlist(fs, use.names = FALSE)
  base_syn <- make_syntax(fs)
  cat("\n", g, ": fitting free model with ", length(fs), " factors, ",
      length(items), " items\n", sep = "")
  free <- fit_lav(base_syn, dat, items)
  fits[[g]] <- fit_summary(free, g, "free")
  if (is.null(free$fit)) { message(g, ": free model failed -- skipped"); next }

  # 1. CI_CFA
  lv <- names(fs)
  pe <- parameterEstimates(free$fit, ci = TRUE, level = 0.95)
  pe <- pe[pe$op == "~~" & pe$lhs != pe$rhs & pe$lhs %in% lv & pe$rhs %in% lv, ]
  ub <- pmax(abs(pe$ci.lower), abs(pe$ci.upper))
  cr <- data.frame(group = g, factor1 = pe$lhs, factor2 = pe$rhs,
                   r = round(pe$est, 3), se = round(pe$se, 3),
                   ci_lower = round(pe$ci.lower, 3), ci_upper = round(pe$ci.upper, 3),
                   ub_abs = round(ub, 3), level = classify(ub),
                   stringsAsFactors = FALSE)
  cors[[g]] <- cr

  # 2. chi-square(cut) for pairs that reach test_from
  todo <- cr[cr$ub_abs >= test_from, ]
  cat("  ", nrow(todo), " pair(s) need chi-square(cut) tests\n", sep = "")
  for (i in seq_len(nrow(todo))) for (k in cuts) {
    a <- todo$factor1[i]; b <- todo$factor2[i]
    v <- sign(todo$r[i]) * k
    con <- fit_lav(paste0(base_syn, "\n", a, " ~~ ", sprintf("%.3f", v), "*", b),
                   dat, items)
    res <- if (is.null(con$fit)) c(chisq_diff = NA, df_diff = NA, p = NA)
           else lrt_row(free$fit, con$fit)
    concl <- if (is.na(res[["p"]])) "test failed"
             else if (res[["p"]] < .05 && abs(todo$r[i]) < k)
               paste0("distinct: |r| reliably below ", k)
             else paste0("cannot reject |r| = ", k)
    tests[[length(tests) + 1]] <- data.frame(
      group = g, factor1 = a, factor2 = b, r = todo$r[i], cut = k,
      chisq_diff = round(res[["chisq_diff"]], 3), df_diff = res[["df_diff"]],
      p = signif(res[["p"]], 3),
      constrained_admissible = if (is.null(con$fit)) NA
                               else safe(suppressWarnings(lavInspect(con$fit, "post.check")), NA),
      conclusion = concl, stringsAsFactors = FALSE)
    cat(sprintf("    %-5s %-5s r = %6.3f  cut %.2f  -> %s\n", a, b, todo$r[i], k, concl))
  }
}

write_csv(bind_rows(fits),  out_dir, "fit")
write_csv(bind_rows(cors),  out_dir, "factor_cor_ci")
write_csv(bind_rows(tests), out_dir, "chisq_cut_tests")

cr <- bind_rows(cors)
if (!is.null(cr)) {
  cat("\nPairs with a discriminant-validity problem (CI upper bound >= .80):\n")
  print(cr[cr$level != "no problem", c("group", "factor1", "factor2", "r",
                                        "ci_upper", "ub_abs", "level")], row.names = FALSE)
}
cat("\nSaved to", normalizePath(out_dir, mustWork = FALSE), "\n")