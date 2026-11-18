###############################################################################
# Changes in ANC utilisation (four or more visits) in Bangladesh, 2014-2022:
# survey-weighted Blinder-Oaxaca decomposition (PLOS ONE revision 6)
#
# Reproduces every number in the revised manuscript:
#   * analytic sample sizes and exclusions (Methods, Abstract)
#   * Table 1: weighted % with four or more ANC visits by subgroup, with the
#     2017-18 to 2022 change and its 95% CI
#   * Table 2: aggregate decomposition (explained / unexplained), both periods
#   * Fig 1 and Fig 2 (TIFF, PLOS specification)
#   * S1 Table: weighted covariate distribution by survey (composition)
#   * S2 Table: survey-specific weighted LPM coefficients and diagnostics
#   * S3 Table: detailed decomposition by category (explained and unexplained)
#   * S4 Table: sensitivity analyses
#
# Usage (from a terminal):
#   Rscript ANC4_decomposition_R6.R <IR_2014.dta> <IR_2017-18.dta> <IR_2022.dta> [outdir] [B]
# e.g. Rscript ANC4_decomposition_R6.R BDIR72FL.DTA BDIR7RFL.DTA BDIR81FL.DTA outputs 1000
#
# Inputs are the standard DHS Individual (women's) Recode Stata files as
# downloaded from dhsprogram.com. No pre-processing is needed.
# Required packages: haven, survey, ggplot2
###############################################################################

suppressPackageStartupMessages({
  library(haven)
  library(survey)
  library(ggplot2)
})
options(survey.lonely.psu = "adjust", stringsAsFactors = FALSE)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 3) stop("Supply the three IR .dta files (2014, 2017-18, 2022).")
files  <- setNames(args[1:3], c("2014", "2017", "2022"))
outdir <- if (length(args) >= 4) args[4] else "outputs"
B      <- if (length(args) >= 5) as.integer(args[5]) else 1000L
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
set.seed(20261117)

# ANC module: in all three surveys the ANC questions refer to the woman's most
# recent live birth in the 3 years (36 months) preceding the interview.
# In 2022 the full Woman's Questionnaire (which contains the ANC, spouse and
# autonomy questions) was administered in a two-thirds subsample of households;
# women in the remaining households answered a short questionnaire.
WINDOW <- c(`2014` = 36, `2017` = 36, `2022` = 36)
YEARLAB <- c(`2014` = "2014", `2017` = "2017--18", `2022` = "2022")
# Mid-point of fieldwork (decimal years) for annualised change:
# 2014: Jun-Nov 2014; 2017-18: Oct 2017-Mar 2018; 2022: Jun-Dec 2022
FIELD_MID <- c(`2014` = 2014 + 8.5/12, `2017` = 2017 + 12/12, `2022` = 2022 + 9/12)

# ---------------------------------------------------------------------------
# 1. Variable construction from raw IR recode variables
#    >>> Every analytic definition is in this function. <<<
# ---------------------------------------------------------------------------
num <- function(x) as.numeric(zap_labels(x))
NEEDED <- c("v005","v008","v011","v021","v022","v024","v025","v106","v136","v151","v190",
            "v501","v632","v632a","v701","v705","v714","v743a","v744a","v744d",
            "b3_01","b19_01","bord_01","midx_1","m14_1")
REQUIRED <- setdiff(NEEDED, c("b19_01","v632a"))
# BDHS 2014 v024 codes, used only if the file carries no value labels
DIV2014 <- c("Barishal","Chattogram","Dhaka","Khulna","Rajshahi","Rangpur","Sylhet")

read_ir <- function(file) {
  nm <- names(read_dta(file, n_max = 1))
  keep <- nm[tolower(nm) %in% NEEDED]
  d <- read_dta(file, col_select = all_of(keep)); names(d) <- tolower(names(d))
  miss <- setdiff(REQUIRED, names(d))
  if (length(miss)) stop(sprintf("%s lacks required variables: %s. Is the file truncated? Use the complete Individual Recode.",
                                 file, paste(miss, collapse = ", ")), call. = FALSE)
  d
}

division_names <- function(v024, year) {
  labs <- attr(v024, "labels")
  if (is.null(labs)) {
    if (year == 2014) return(DIV2014[num(v024)])
    stop("v024 has no value labels")
  }
  l <- tolower(names(labs)[match(num(v024), labs)])
  ifelse(grepl("baris", l), "Barishal", ifelse(grepl("chitt|chatt", l), "Chattogram",
  ifelse(grepl("dhaka", l), "Dhaka", ifelse(grepl("khul", l), "Khulna",
  ifelse(grepl("myme", l), "Mymensingh", ifelse(grepl("rajs", l), "Rajshahi",
  ifelse(grepl("rang", l), "Rangpur", ifelse(grepl("sylh", l), "Sylhet", NA))))))))
}

build <- function(file, year, window, d = read_ir(file)) {
  flow <- c(interviewed = nrow(d))
  full <- !is.na(d$v744a)                               # completed the full questionnaire
  flow["full_questionnaire"] <- sum(full)

  age_m <- if ("b19_01" %in% names(d)) num(d$b19_01) else num(d$v008) - num(d$b3_01)
  age_m[is.na(age_m)] <- (num(d$v008) - num(d$b3_01))[is.na(age_m)]
  visits <- num(d$m14_1)
  # ANC module answered for the most recent live birth within the window
  elig <- full & !is.na(visits) & num(d$midx_1) %in% 1 & !is.na(age_m) & age_m < window
  flow["recent_birth_with_anc_module"] <- sum(elig)
  married <- elig & num(d$v501) %in% c(1, 2)            # currently married / in union
  flow["not_currently_married"] <- sum(elig & !married)
  anc_ok <- married & visits <= 90                      # 98 = don't know
  flow["anc_dont_know"] <- sum(married & !anc_ok)

  division8 <- division_names(d$v024, year)
  division7 <- ifelse(division8 == "Mymensingh", "Dhaka", division8)   # pre-2015 boundaries

  mage <- floor((num(d$b3_01) - num(d$v011)) / 12)     # mother's age at most recent birth
  edu  <- num(d$v106); hedu <- num(d$v701); hedu[hedu > 3] <- NA

  # Women's decision-making autonomy: three indicators, 1 = response reflecting autonomy
  h <- num(d$v743a)                                     # own health care: alone or jointly
  i_health <- ifelse(h %in% c(1, 2), 1, ifelse(h %in% c(4, 5, 6), 0, NA))
  wb <- num(d$v744d)                                    # wife-beating justified if she refuses sex
  i_beat <- ifelse(wb == 0, 1, ifelse(wb %in% c(1, 8), 0, NA))
  cu <- num(d$v632); cn <- if ("v632a" %in% names(d)) num(d$v632a) else rep(NA, nrow(d))
  cc <- ifelse(!is.na(cu), cu, cn)                      # users: v632; non-users: v632a
  i_contra <- ifelse(cc %in% c(1, 3), 1, ifelse(cc %in% c(2, 6), 0, NA))   # 1 mainly woman, 3 joint
  items <- cbind(i_health, i_beat, i_contra)
  n_ans <- rowSums(!is.na(items)); n_fav <- rowSums(items, na.rm = TRUE)
  # higher autonomy: favourable on every item answered (at least two answered)
  autonomy <- ifelse(n_ans >= 2, ifelse(n_fav == n_ans, "Higher", "Lower"), NA)

  rwork <- num(d$v714); rwork[rwork > 1] <- NA         # currently working
  hocc  <- num(d$v705); hocc[hocc >= 98] <- NA         # husband's occupation (0 = did not work)

  out <- data.frame(
    year      = year,
    psu       = num(d$v021),
    strata    = paste(year, num(d$v022)),
    w         = num(d$v005) / 1e6,
    visits    = visits,
    anc4      = as.numeric(visits >= 4),
    mage      = cut(mage, c(-Inf, 19, 34, Inf), labels = c("Below 20 years", "20--34 years", "35 years or older")),
    residence = factor(ifelse(num(d$v025) == 1, "Urban", "Rural"), levels = c("Urban", "Rural")),
    division7 = factor(division7, levels = c("Barishal","Chattogram","Dhaka","Khulna","Rajshahi","Rangpur","Sylhet")),
    division8 = factor(division8, levels = c("Barishal","Chattogram","Dhaka","Khulna","Mymensingh","Rajshahi","Rangpur","Sylhet")),
    wealth    = factor(num(d$v190), 1:5, c("Poorest","Poorer","Middle","Richer","Richest")),
    edu       = factor(edu, 0:3, c("No education","Primary","Secondary","Higher")),
    hedu      = factor(hedu, 0:3, c("No education","Primary","Secondary","Higher")),
    hhsize    = factor(ifelse(num(d$v136) <= 4, "Four or fewer", "Five or more"), levels = c("Four or fewer","Five or more")),
    border    = factor(ifelse(num(d$bord_01) <= 2, "First or second", "Third or higher"), levels = c("First or second","Third or higher")),
    autonomy  = factor(autonomy, levels = c("Lower","Higher")),
    hhsex     = factor(ifelse(num(d$v151) == 1, "Male", "Female"), levels = c("Male","Female")),
    rwork     = factor(ifelse(rwork == 1, "Working", "Not working"), levels = c("Not working","Working")),
    hwork     = factor(ifelse(hocc == 0, "Not working", "Working"), levels = c("Working","Not working"))
  )[anc_ok, ]
  cc_ok <- complete.cases(out[, COVS_ALL])
  flow["covariate_missing"] <- sum(!cc_ok)
  flow["analytic"] <- sum(cc_ok)
  list(data = out[cc_ok, ], flow = flow, raw = d,
       missing_by_var = colSums(is.na(out[, COVS_ALL])))
}

COVS_ALL <- c("mage","residence","division8","wealth","edu","hedu","hhsize","border",
              "autonomy","hhsex","rwork","hwork")
VARLAB <- c(mage = "Maternal age at birth", residence = "Place of residence",
            division = "Division", wealth = "Wealth quintile", edu = "Woman's education",
            hedu = "Husband's education", hhsize = "Household size", border = "Birth order",
            autonomy = "Decision-making autonomy", hhsex = "Sex of household head",
            rwork = "Woman's employment", hwork = "Husband's employment")

S <- list()
for (y in names(files)) {
  # 2014 has no Mymensingh cases (division created in 2015); level kept empty
  S[[y]] <- build(files[[y]], as.integer(y), WINDOW[[y]])
}

# ---------------------------------------------------------------------------
# 2. Design-based descriptive estimates (Table 1, S1 Table)
# ---------------------------------------------------------------------------
des <- function(df) svydesign(ids = ~psu, strata = ~strata, weights = ~w, data = df, nest = TRUE)

prop_by <- function(df, v) {
  ds <- des(df)
  r <- svyby(~anc4, as.formula(paste0("~", v)), ds, svymean, na.rm = TRUE, keep.var = TRUE)
  data.frame(level = as.character(r[[v]]), est = 100 * r$anc4, se = 100 * SE(r))
}
overall <- function(df) { m <- svymean(~anc4, des(df)); c(est = 100 * coef(m)[[1]], se = 100 * SE(m)[[1]]) }

T1rows <- list()
tab_vars <- c("mage","residence","division8","wealth","edu","hedu","hhsize","border",
              "autonomy","hhsex","rwork","hwork")
for (v in tab_vars) {
  r <- lapply(names(S), function(y) { p <- prop_by(S[[y]]$data, v); p$year <- y; p })
  T1rows[[v]] <- do.call(rbind, r)
}
OV <- sapply(names(S), function(y) overall(S[[y]]$data))

# composition (S1 Table)
comp <- do.call(rbind, lapply(tab_vars, function(v) do.call(rbind, lapply(names(S), function(y) {
  ds <- des(S[[y]]$data)
  m <- svymean(as.formula(paste0("~", v)), ds)
  data.frame(variable = VARLAB[[sub("8$", "", v)]], level = sub(paste0("^", v), "", names(coef(m))),
             year = YEARLAB[[y]], pct = round(100 * coef(m), 1), se = round(100 * SE(m), 2))
}))))
write.csv(comp, file.path(outdir, "S1_Table_composition.csv"), row.names = FALSE)

# ---------------------------------------------------------------------------
# 3. Decomposition engine (weighted LPM, base-period reference coefficients,
#    Yun normalisation of categorical covariates)
# ---------------------------------------------------------------------------
design_matrix <- function(df, covs) {
  # full set of indicators for every category (including the reference)
  M <- do.call(cbind, lapply(covs, function(v) {
    f <- df[[v]]; L <- levels(f)
    X <- sapply(L, function(l) as.numeric(f == l)); if (is.null(dim(X))) X <- matrix(X, nrow = 1)
    colnames(X) <- paste0(v, "::", L); X
  }))
  M
}
fit_norm <- function(df, covs) {
  # treatment-coded weighted LPM, then Yun (2005) normalisation
  Xf <- design_matrix(df, covs)
  refs <- sapply(covs, function(v) paste0(v, "::", levels(df[[v]])[1]))
  Xt <- cbind(`(Intercept)` = 1, Xf[, setdiff(colnames(Xf), refs), drop = FALSE])
  f <- lm.wfit(Xt, df$anc4, df$w)
  b <- f$coefficients; b[is.na(b)] <- 0
  full <- setNames(rep(0, ncol(Xf)), colnames(Xf)); full[names(b)[-1]] <- b[-1]
  a <- b[1]
  for (v in covs) {
    idx <- startsWith(names(full), paste0(v, "::"))
    mu <- mean(full[idx]); full[idx] <- full[idx] - mu; a <- a + mu
  }
  xbar <- colSums(Xf * df$w) / sum(df$w)
  ybar <- sum(df$anc4 * df$w) / sum(df$w)
  fitted <- drop(Xt %*% b)
  r2 <- 1 - sum(df$w * (df$anc4 - fitted)^2) / sum(df$w * (df$anc4 - ybar)^2)
  list(a = unname(a), b = full, b_treat = b, xbar = xbar, ybar = ybar, r2 = r2,
       out01 = 100 * sum(df$w * (fitted < 0 | fitted > 1)) / sum(df$w),
       fitted_range = range(fitted))
}
decompose <- function(dA, dB, covs) {
  A <- fit_norm(dA, covs); Bf <- fit_norm(dB, covs)
  dx <- Bf$xbar - A$xbar
  expl_k <- dx * A$b                              # (xB - xA) bA
  unex_k <- Bf$xbar * (Bf$b - A$b)                # xB (bB - bA)
  list(yA = A$ybar, yB = Bf$ybar, total = Bf$ybar - A$ybar,
       explained = sum(expl_k), unexplained = sum(unex_k) + (Bf$a - A$a),
       intercept = Bf$a - A$a, expl_k = expl_k, unex_k = unex_k, A = A, B = Bf)
}
by_variable <- function(v_k) {
  g <- sub("::.*$", "", names(v_k)); tapply(v_k, g, sum)
}
# pooled (Neumark/Jann) reference coefficients: sensitivity
decompose_pooled <- function(dA, dB, covs) {
  P <- rbind(transform(dA, grp = 0), transform(dB, grp = 1))
  Xf <- design_matrix(P, covs)
  refs <- sapply(covs, function(v) paste0(v, "::", levels(P[[v]])[1]))
  Xt <- cbind(1, Xf[, setdiff(colnames(Xf), refs), drop = FALSE], grp = P$grp)
  # equalise total weight of the two surveys in the pooled fit
  wp <- P$w / ave(P$w, P$grp, FUN = sum)
  bp <- lm.wfit(Xt, P$anc4, wp)$coefficients; bp[is.na(bp)] <- 0
  bpx <- bp[-c(1, length(bp))]
  A <- fit_norm(dA, covs); Bf <- fit_norm(dB, covs)
  Xa <- design_matrix(dA, covs)[, names(bpx), drop = FALSE]
  Xb <- design_matrix(dB, covs)[, names(bpx), drop = FALSE]
  xa <- colSums(Xa * dA$w) / sum(dA$w); xb <- colSums(Xb * dB$w) / sum(dB$w)
  expl <- sum((xb - xa) * bpx)
  c(explained = expl, unexplained = (Bf$ybar - A$ybar) - expl)
}
# nonlinear (logit) aggregate decomposition: sensitivity for the LPM
decompose_logit <- function(dA, dB, covs) {
  fm <- as.formula(paste("anc4 ~", paste(covs, collapse = " + ")))
  gA <- suppressWarnings(glm(fm, data = dA, weights = w / mean(w), family = quasibinomial()))
  pBA <- predict(gA, newdata = dB, type = "response")
  pAA <- predict(gA, newdata = dA, type = "response")
  yA <- sum(dA$anc4 * dA$w) / sum(dA$w); yB <- sum(dB$anc4 * dB$w) / sum(dB$w)
  expl <- sum(pBA * dB$w) / sum(dB$w) - sum(pAA * dA$w) / sum(dA$w)
  c(explained = expl, unexplained = (yB - yA) - expl)
}

# cluster (PSU) bootstrap within strata, independently for each survey
boot_psu <- function(df) {
  ids <- unique(df[, c("strata", "psu")])
  pick <- do.call(rbind, lapply(split(ids, ids$strata), function(s) s[sample(nrow(s), replace = TRUE), , drop = FALSE]))
  rows <- split(seq_len(nrow(df)), paste(df$strata, df$psu))
  idx <- unlist(rows[paste(pick$strata, pick$psu)], use.names = FALSE)
  df[idx, ]
}

COVS_P1 <- c("mage","residence","division7","wealth","edu","hedu","hhsize","border","autonomy","hhsex","rwork","hwork")
COVS_P2 <- sub("division7", "division8", COVS_P1)
PERIODS <- list(P1 = list(a = "2014", b = "2017", covs = COVS_P1),
                P2 = list(a = "2017", b = "2022", covs = COVS_P2))
# Both surveys in a period share identical factor levels for every covariate
# used (division7 in period 1, division8 in period 2), so design matrices align.
for (p in names(PERIODS)) {
  PERIODS[[p]]$da <- S[[PERIODS[[p]]$a]]$data
  PERIODS[[p]]$db <- S[[PERIODS[[p]]$b]]$data
}

RES <- list()
for (p in names(PERIODS)) {
  P <- PERIODS[[p]]
  est <- decompose(P$da, P$db, P$covs)
  cat(sprintf("\n%s: bootstrapping (%d replications) ...\n", p, B))
  bt <- replicate(B, {
    e <- decompose(boot_psu(P$da), boot_psu(P$db), P$covs)
    c(total = e$total, explained = e$explained, unexplained = e$unexplained,
      setNames(by_variable(e$expl_k), paste0("EV_", names(by_variable(e$expl_k)))),
      setNames(by_variable(e$unex_k), paste0("UV_", names(by_variable(e$unex_k)))),
      setNames(e$expl_k, paste0("EK_", names(e$expl_k))),
      setNames(e$unex_k, paste0("UK_", names(e$unex_k))))
  })
  RES[[p]] <- list(est = est, boot = bt,
                   pooled = decompose_pooled(P$da, P$db, P$covs),
                   logit = decompose_logit(P$da, P$db, P$covs))
}
ci <- function(x) quantile(x, c(.025, .975), na.rm = TRUE)

# ---------------------------------------------------------------------------
# 4. Sensitivity: common 24-month recall window for all three surveys
# ---------------------------------------------------------------------------
win24 <- list()
for (y in c("2014", "2017")) {
  win24[[y]] <- build(files[[y]], as.integer(y), 24, d = S[[y]]$raw)$data
}
win24[["2022"]] <- build(files[["2022"]], 2022L, 24, d = S[["2022"]]$raw)$data
SENS24 <- list(
  P1 = decompose(win24[["2014"]], win24[["2017"]], COVS_P1),
  P2 = decompose(win24[["2017"]], win24[["2022"]], COVS_P2))

# ---------------------------------------------------------------------------
# 5. Outputs
# ---------------------------------------------------------------------------
neg <- function(z) sub("^-", "$-$", z)
f1 <- function(x) neg(formatC(x, format = "f", digits = 1))
f2 <- function(x) neg(formatC(x, format = "f", digits = 2))
fn <- function(x) formatC(x, format = "d", big.mark = ",")
pp <- function(x) f2(100 * x)
ppc <- function(x) formatC(100 * x, format = "f", digits = 2)   # plain, for CSV

## Sample flow
flow <- sapply(S, `[[`, "flow")
colnames(flow) <- YEARLAB[colnames(flow)]
write.csv(flow, file.path(outdir, "sample_flow.csv"))
write.csv(sapply(S, `[[`, "missing_by_var"), file.path(outdir, "missing_by_variable.csv"))

## Table 1 body (LaTeX)
t1 <- c()
t1 <- c(t1, sprintf("\\textbf{Overall} & %s & %s & %s & %s \\\\",
                    f1(OV["est","2014"]), f1(OV["est","2017"]), f1(OV["est","2022"]),
                    {d <- OV["est","2022"] - OV["est","2017"]; s <- sqrt(OV["se","2022"]^2 + OV["se","2017"]^2)
                     z <- sprintf("%s (%s, %s)", f1(d), f1(d - 1.96*s), f1(d + 1.96*s)); if (abs(d/s) > 1.96) paste0("\\textbf{", z, "}") else z}))
t1 <- c(t1, sprintf("\\quad Unweighted $n$ & %s & %s & %s & \\\\",
                    fn(nrow(S[["2014"]]$data)), fn(nrow(S[["2017"]]$data)), fn(nrow(S[["2022"]]$data))))
for (v in tab_vars) {
  t1 <- c(t1, sprintf("\\textbf{%s} & & & & \\\\", VARLAB[[sub("8$", "", v)]]))
  R <- T1rows[[v]]
  for (l in intersect(levels(S[["2022"]]$data[[v]]), unique(R$level))) {
    g <- function(y) R[R$level == l & R$year == y, , drop = FALSE]
    cell <- function(y) { r <- g(y); if (nrow(r) == 0) "NA" else f1(r$est) }
    a <- g("2017"); b <- g("2022")
    chg <- if (nrow(a) && nrow(b)) {
      d <- b$est - a$est; s <- sqrt(a$se^2 + b$se^2)
      z <- sprintf("%s (%s, %s)", f1(d), f1(d - 1.96*s), f1(d + 1.96*s))
      if (abs(d/s) > 1.96) paste0("\\textbf{", z, "}") else z
    } else ""
    t1 <- c(t1, sprintf("\\quad %s & %s & %s & %s & %s \\\\", l, cell("2014"), cell("2017"), cell("2022"), chg))
  }
}
writeLines(t1, file.path(outdir, "table1_body.tex"))
write.csv(do.call(rbind, lapply(names(T1rows), function(v) cbind(variable = v, T1rows[[v]]))),
          file.path(outdir, "table1_estimates.csv"), row.names = FALSE)

## Table 2 body (aggregate decomposition)
row2 <- function(lbl, p, key, share = TRUE) {
  e <- RES[[p]]$est; v <- switch(key, total = e$total, explained = e$explained, unexplained = e$unexplained)
  c_ <- ci(RES[[p]]$boot[key, ])
  sprintf("%s & %s (%s, %s) & %s \\\\", lbl, pp(v), pp(c_[1]), pp(c_[2]),
          if (share) paste0(f1(100 * v / e$total), "\\%") else "100.0\\%")
}
t2 <- c()
for (p in c("P1", "P2")) {
  e <- RES[[p]]$est
  hdr <- if (p == "P1") "2014 to 2017--18" else "2017--18 to 2022"
  yrs <- c(PERIODS[[p]]$a, PERIODS[[p]]$b)
  ann <- 100 * e$total / (FIELD_MID[[yrs[2]]] - FIELD_MID[[yrs[1]]])
  t2 <- c(t2, sprintf("\\multicolumn{3}{l}{\\textbf{%s}} \\\\", hdr),
          sprintf("\\quad ANC4+ at start / end of period (\\%%) & %s / %s & \\\\", f1(100 * e$yA), f1(100 * e$yB)),
          row2("\\quad Total change (pp)", p, "total", share = FALSE),
          sprintf("\\quad Annualised change (pp per year) & %s & \\\\", f2(ann)),
          row2("\\quad Explained (composition)", p, "explained"),
          row2("\\quad Unexplained (coefficients)", p, "unexplained"))
  if (p == "P1") t2 <- c(t2, "\\midrule")
}
writeLines(t2, file.path(outdir, "table2_body.tex"))

## S2 Table: survey-specific LPM coefficients (treatment coding) + diagnostics
s2 <- do.call(rbind, lapply(names(S), function(y) {
  df <- S[[y]]$data; ds <- des(df)
  covs <- if (y == "2014") COVS_P1 else COVS_P2
  m <- svyglm(as.formula(paste("anc4 ~", paste(covs, collapse = " + "))), design = ds)
  cf <- summary(m)$coefficients
  fitd <- fit_norm(df, covs)
  rbind(data.frame(survey = YEARLAB[[y]], term = rownames(cf), estimate = round(cf[,1], 4),
                   ci_low = round(cf[,1] - 1.96*cf[,2], 4), ci_high = round(cf[,1] + 1.96*cf[,2], 4),
                   p_value = signif(cf[,4], 3)),
        data.frame(survey = YEARLAB[[y]], term = c("Weighted R-squared", "% fitted outside [0,1]",
                                                   "Min fitted", "Max fitted"),
                   estimate = round(c(fitd$r2, fitd$out01, fitd$fitted_range), 4),
                   ci_low = NA, ci_high = NA, p_value = NA))
}))
write.csv(s2, file.path(outdir, "S2_Table_LPM_coefficients.csv"), row.names = FALSE)

## S3 Table: detailed decomposition by category
s3 <- do.call(rbind, lapply(names(RES), function(p) {
  e <- RES[[p]]$est; bt <- RES[[p]]$boot
  k <- names(e$expl_k)
  data.frame(period = if (p == "P1") "2014 to 2017-18" else "2017-18 to 2022",
             variable = VARLAB[sub("[78]$", "", sub("::.*$", "", k))], category = sub("^.*::", "", k),
             explained_pp = round(100 * e$expl_k, 3),
             explained_ci = sprintf("(%s, %s)", ppc(apply(bt[paste0("EK_", k), , drop = FALSE], 1, ci)[1, ]),
                                    ppc(apply(bt[paste0("EK_", k), , drop = FALSE], 1, ci)[2, ])),
             unexplained_pp = round(100 * e$unex_k, 3),
             unexplained_ci = sprintf("(%s, %s)", ppc(apply(bt[paste0("UK_", k), , drop = FALSE], 1, ci)[1, ]),
                                      ppc(apply(bt[paste0("UK_", k), , drop = FALSE], 1, ci)[2, ])))
}))
s3 <- rbind(s3, do.call(rbind, lapply(names(RES), function(p) {
  e <- RES[[p]]$est; bt <- RES[[p]]$boot
  ib <- bt["unexplained", ] - colSums(bt[grep("^UK_", rownames(bt)), , drop = FALSE])
  q <- ci(ib)
  data.frame(period = if (p == "P1") "2014 to 2017-18" else "2017-18 to 2022",
             variable = "Intercept", category = "Shift common to all groups",
             explained_pp = NA, explained_ci = "",
             unexplained_pp = round(100 * e$intercept, 3),
             unexplained_ci = sprintf("(%s, %s)", ppc(q[1]), ppc(q[2])))
})))
s3 <- s3[order(s3$period), ]
write.csv(s3, file.path(outdir, "S3_Table_detailed_decomposition.csv"), row.names = FALSE)

## S4 Table: sensitivity
s4 <- do.call(rbind, lapply(c("P1","P2"), function(p) {
  e <- RES[[p]]$est; s24 <- SENS24[[p]]
  data.frame(period = if (p == "P1") "2014 to 2017-18" else "2017-18 to 2022",
             analysis = c("Main: LPM, base-period coefficients",
                          "Pooled reference coefficients (Jann/Neumark)",
                          "Logit model (nonlinear aggregate decomposition)",
                          "Births in the 24 months before the survey, LPM"),
             total_pp = round(100 * c(e$total, e$total, e$total, s24$total), 2),
             explained_pp = round(100 * c(e$explained, RES[[p]]$pooled["explained"], RES[[p]]$logit["explained"], s24$explained), 2),
             unexplained_pp = round(100 * c(e$unexplained, RES[[p]]$pooled["unexplained"], RES[[p]]$logit["unexplained"], s24$unexplained), 2))
}))
s4$explained_share_pct <- round(100 * s4$explained_pp / s4$total_pp, 1)
write.csv(s4, file.path(outdir, "S4_Table_sensitivity.csv"), row.names = FALSE)

## LaTeX macros for numbers quoted in the text
mac <- function(name, val) sprintf("\\newcommand{\\%s}{%s}", name, val)
e1 <- RES$P1$est; e2 <- RES$P2$est
c1e <- ci(RES$P1$boot["explained", ]); c1u <- ci(RES$P1$boot["unexplained", ])
c2e <- ci(RES$P2$boot["explained", ]); c2u <- ci(RES$P2$boot["unexplained", ])
oc <- function(y) sprintf("%s\\%% (95\\%% CI: %s, %s)", f1(OV["est", y]), f1(OV["est", y] - 1.96 * OV["se", y]), f1(OV["est", y] + 1.96 * OV["se", y]))
macros <- c(
  "%% ---- generated by ANC4_decomposition_R6.R ----",
  mac("nIntA", fn(S[["2014"]]$flow["interviewed"])), mac("nIntB", fn(S[["2017"]]$flow["interviewed"])), mac("nIntC", fn(S[["2022"]]$flow["interviewed"])),
  mac("nFullC", fn(S[["2022"]]$flow["full_questionnaire"])),
  mac("nBirthA", fn(S[["2014"]]$flow["recent_birth_with_anc_module"])), mac("nBirthB", fn(S[["2017"]]$flow["recent_birth_with_anc_module"])), mac("nBirthC", fn(S[["2022"]]$flow["recent_birth_with_anc_module"])),
  mac("nNotMarA", fn(S[["2014"]]$flow["not_currently_married"])), mac("nNotMarB", fn(S[["2017"]]$flow["not_currently_married"])), mac("nNotMarC", fn(S[["2022"]]$flow["not_currently_married"])),
  mac("nAncMissA", fn(S[["2014"]]$flow["anc_dont_know"])), mac("nAncMissB", fn(S[["2017"]]$flow["anc_dont_know"])), mac("nAncMissC", fn(S[["2022"]]$flow["anc_dont_know"])),
  mac("nCovMissA", fn(S[["2014"]]$flow["covariate_missing"])), mac("nCovMissB", fn(S[["2017"]]$flow["covariate_missing"])), mac("nCovMissC", fn(S[["2022"]]$flow["covariate_missing"])),
  mac("nA", fn(S[["2014"]]$flow["analytic"])), mac("nB", fn(S[["2017"]]$flow["analytic"])), mac("nC", fn(S[["2022"]]$flow["analytic"])),
  mac("ancA", oc("2014")), mac("ancB", oc("2017")), mac("ancC", oc("2022")),
  mac("ancAp", f1(OV["est","2014"])), mac("ancBp", f1(OV["est","2017"])), mac("ancCp", f1(OV["est","2022"])),
  mac("dTotA", pp(e1$total)), mac("dTotB", pp(e2$total)),
  mac("dExpA", sprintf("%s pp (95\\%% CI: %s, %s)", pp(e1$explained), pp(c1e[1]), pp(c1e[2]))),
  mac("dUneA", sprintf("%s pp (95\\%% CI: %s, %s)", pp(e1$unexplained), pp(c1u[1]), pp(c1u[2]))),
  mac("dExpB", sprintf("%s pp (95\\%% CI: %s, %s)", pp(e2$explained), pp(c2e[1]), pp(c2e[2]))),
  mac("dUneB", sprintf("%s pp (95\\%% CI: %s, %s)", pp(e2$unexplained), pp(c2u[1]), pp(c2u[2]))),
  mac("dExpShA", f1(100 * e1$explained / e1$total)), mac("dUneShA", f1(100 * e1$unexplained / e1$total)),
  mac("dExpShB", f1(100 * e2$explained / e2$total)), mac("dUneShB", f1(100 * e2$unexplained / e2$total)),
  mac("annA", f2(100 * e1$total / (FIELD_MID[["2017"]] - FIELD_MID[["2014"]]))),
  mac("annB", f2(100 * e2$total / (FIELD_MID[["2022"]] - FIELD_MID[["2017"]]))),
  mac("rsqA", f2(RES$P1$est$A$r2)), mac("rsqB", f2(RES$P1$est$B$r2)), mac("rsqC", f2(RES$P2$est$B$r2)),
  mac("outA", f1(RES$P1$est$A$out01)), mac("outB", f1(RES$P1$est$B$out01)), mac("outC", f1(RES$P2$est$B$out01)),
  mac("logitShA", f1(100 * RES$P1$logit["explained"] / e1$total)), mac("logitShB", f1(100 * RES$P2$logit["explained"] / e2$total)),
  mac("pooledShA", f1(100 * RES$P1$pooled["explained"] / e1$total)), mac("pooledShB", f1(100 * RES$P2$pooled["explained"] / e2$total)),
  mac("winShA", f1(100 * SENS24$P1$explained / SENS24$P1$total)), mac("winShB", f1(100 * SENS24$P2$explained / SENS24$P2$total)),
  mac("Bboot", fn(B)))
writeLines(macros, file.path(outdir, "macros.tex"))

## Fig 1: long-run trend. 1993-94 to 2011 from published BDHS reports; 2014-2022 from this analysis
trend <- data.frame(round = c("1993--94","1996--97","1999--2000","2004","2007","2011","2014","2017--18","2022"),
                    pct = c(6, 7, 11, 17, 22, 26, OV["est","2014"], OV["est","2017"], OV["est","2022"]),
                    se = c(rep(NA, 6), OV["se","2014"], OV["se","2017"], OV["se","2022"]))
trend$round <- factor(gsub("--", "-", trend$round), levels = gsub("--", "-", trend$round))
g1 <- ggplot(trend, aes(round, pct, group = 1)) +
  geom_line(colour = "grey35", linewidth = 0.6) +
  geom_errorbar(aes(ymin = pct - 1.96 * se, ymax = pct + 1.96 * se), width = 0.15, colour = "grey20", na.rm = TRUE) +
  geom_point(size = 2.2, colour = "#1f4e79") +
  geom_text(aes(label = paste0(round(pct, 1), "%")), hjust = 1, nudge_x = -0.12, vjust = -0.35, size = 3.2) +
  scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, 10), expand = c(0, 0)) +
  labs(x = "BDHS survey round", y = "Percentage with 4+ ANC visits") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 0))
ggsave(file.path(outdir, "Fig1.tif"), g1, device = "tiff", width = 6.5, height = 3.8, dpi = 400, compression = "lzw", bg = "white")

## Fig 2: explained (composition) contributions by covariate, both periods
fig2 <- do.call(rbind, lapply(c("P1","P2"), function(p) {
  e <- RES[[p]]$est; bt <- RES[[p]]$boot
  v <- by_variable(e$expl_k)
  q <- sapply(names(v), function(n) ci(bt[paste0("EV_", n), ]))
  data.frame(period = if (p == "P1") "A. 2014 to 2017-18" else "B. 2017-18 to 2022",
             var = VARLAB[sub("[78]$", "", names(v))], est = 100 * v, lo = 100 * q[1, ], hi = 100 * q[2, ])
}))
ord <- with(fig2[fig2$period == unique(fig2$period)[1], ], var[order(est)])
fig2$var <- factor(fig2$var, levels = ord)
g2 <- ggplot(fig2, aes(est, var)) +
  geom_vline(xintercept = 0, colour = "grey50", linewidth = 0.4) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.25, colour = "grey25") +
  geom_point(size = 2, colour = "#1f4e79") +
  facet_wrap(~period, nrow = 1) +
  labs(x = "Contribution to change in ANC4+ (percentage points), with 95% bootstrap CI", y = NULL) +
  theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank(), strip.text = element_text(face = "bold", hjust = 0))
ggsave(file.path(outdir, "Fig2.tif"), g2, device = "tiff", width = 7.5, height = 4.2, dpi = 400, compression = "lzw", bg = "white")

## Human-readable summary (safe to share: aggregate results only, no microdata)
f1 <- function(x) formatC(x, format = "f", digits = 1); f2 <- function(x) formatC(x, format = "f", digits = 2)
sink(file.path(outdir, "results_summary.txt"))
cat("Sample flow\n"); print(flow)
cat("\nOverall ANC4+ (%), est and SE\n"); print(round(OV, 2))
cat("\nAggregate decomposition (pp)\n")
for (p in c("P1","P2")) {
  e <- RES[[p]]$est
  cat(p, ": total", pp(e$total), " explained", pp(e$explained), sprintf("[%s, %s]", pp(ci(RES[[p]]$boot["explained", ])[1]), pp(ci(RES[[p]]$boot["explained", ])[2])),
      " unexplained", pp(e$unexplained), sprintf("[%s, %s]", pp(ci(RES[[p]]$boot["unexplained", ])[1]), pp(ci(RES[[p]]$boot["unexplained", ])[2])), "\n")
}
cat("\nExplained by covariate (pp)\n"); print(fig2[, c("period","var","est","lo","hi")], digits = 3)
cat("\nSensitivity\n"); print(s4)
cat("\nDiagnostics: R2 (2014, 2017-18, 2022):", f2(RES$P1$est$A$r2), f2(RES$P1$est$B$r2), f2(RES$P2$est$B$r2), "\n")
cat("% fitted outside [0,1]:", f1(RES$P1$est$A$out01), f1(RES$P1$est$B$out01), f1(RES$P2$est$B$out01), "\n")
sink()
cat("\nDone. Outputs written to", normalizePath(outdir), "\n")
