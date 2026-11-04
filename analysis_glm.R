# ============================================================
# analysis_glm.R
#
# INPUT:  data/main.xlsx (sheet "main") — the source of truth.
# OUTPUT: data/main.csv (rebuilt each run), results/*.csv, results/analysis_log.txt
#
# Studies / conditions analysed (as reported in the manuscript):
#   Study 1a  False belief, Transparency, Spatial-relation, Testimony         (LLM only)
#   Study 1b  Clarified transparency, Clarified spatial-relation              (Human vs LLM)
#   Study 2   Multimodal transparency  (primary: Human vs Gemini3;
#                                       GPT-5.2 reported descriptively, own image set)
# ============================================================

pkgs <- c("readxl", "MASS")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)) {
  message("Installing: ", paste(to_install, collapse = ", "))
  install.packages(to_install, repos = "https://cloud.r-project.org")
}
suppressPackageStartupMessages({
  library(readxl); library(MASS)
})

dir.create("results", showWarnings = FALSE)
LOG <- file("results/analysis_log.txt", open = "wt")
sink(LOG, split = TRUE); sink(LOG, type = "message")
cat("analysis_glm.R run:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("R", as.character(getRversion()), "\n\n")

saved <- character(0)
save_csv <- function(x, path) {
  write.csv(x, path, row.names = FALSE)
  saved <<- c(saved, path)
  cat("  [saved]", path, "\n")
}

# ============================================================
# SECTION 1: Build data/main.csv from data/main.xlsx
# ============================================================
XLSX_PATH <- "data/main.xlsx"; CSV_PATH <- "data/main.csv"

Q_MAP <- c(study1a_q1_correct="study1a_q1_correct", study1b_q1_correct="study1b_q1_correct",
  study1b_q2_correct="study1b_q2_correct", study1b_q3_correct="study1b_q3_correct (visibility)",
  study1b_q4_correct="study1b_q4_correct (belief question)", study2_q1A_correct="study2_q1A_correct",
  study2_q1B_correct="study2_q1B_correct", study2_q1_both_correct="study2_q1_both_correct",
  study2_q2_correct="study2_q2_correct (visibility)", study2_q3_correct="study2_q3_correct (belief question)")
DV_MAP <- c("1a"="study1a_q1_correct","1b"="study1b_q4_correct","2"="study2_q3_correct")
STUDY_ORDER <- c("1a","1b","2")
COND_ORDER  <- c("False belief","Transparency","Spatial-relation","Testimony",
                 "Clarified transparency","Clarified spatial-relation","Multimodal transparency")

build_long <- function(xlsx_path = XLSX_PATH, csv_path = CSV_PATH) {
  if (!file.exists(xlsx_path)) stop("Not found: ", xlsx_path)
  raw <- as.data.frame(read_excel(xlsx_path, sheet = "main", col_types = "text"))
  blank_to_na <- function(x) { x <- trimws(as.character(x)); x[x %in% c("","NA","N/A","na")] <- NA; x }
  raw[] <- lapply(raw, blank_to_na)
  story_col  <- grep("^Story", names(raw), value = TRUE)[1]
  raw$Study  <- trimws(raw$Study)
  if (!all(raw$Study %in% STUDY_ORDER & raw$Condition %in% COND_ORDER)) stop("unknown Study/Condition value")
  if (length(setdiff(unname(Q_MAP), names(raw)))) stop("missing question column(s)")
  as_int <- function(x) suppressWarnings(as.integer(x))
  long <- data.frame(
    Study = raw$Study, Condition = raw$Condition, Story = raw[[story_col]],
    Source = raw$Source, Model = raw$Model,
    participant_id = ifelse(raw$Source=="Human" & !is.na(raw$participant_id),
                            paste0("human-", raw$participant_id), NA_character_),
    session_id = ifelse(raw$Source=="Model", raw$session_id, NA_character_),
    stringsAsFactors = FALSE)
  for (o in names(Q_MAP)) long[[o]] <- as_int(raw[[Q_MAP[[o]]]])
  long$score <- NA_integer_
  for (st in names(DV_MAP)) { sel <- long$Study == st; long$score[sel] <- long[[DV_MAP[[st]]]][sel] }
  long$included_in_analysis <- as_int(raw$included_in_analysis)
  unit   <- ifelse(long$Source=="Human", long$participant_id, long$session_id)
  id_num <- suppressWarnings(as.integer(sub(".*-(\\d+)$","\\1",unit)))
  long <- long[order(match(long$Study,STUDY_ORDER), match(long$Condition,COND_ORDER),
                     as.integer(long$Story), long$Source, long$Model, id_num), ]
  if (anyDuplicated(paste(long$Condition, unit))) warning("duplicate participant/session in a condition")
  long <- long[, c("Study","Condition","Story","Source","Model","participant_id","session_id",
                   names(Q_MAP), "score","included_in_analysis")]
  write.csv(long, csv_path, row.names = FALSE, na = "NA")
  long
}

cat("Building", CSV_PATH, "from", XLSX_PATH, "...\n")
df_all <- build_long()
df <- df_all[!is.na(df_all$included_in_analysis) & df_all$included_in_analysis == 1, ]
df$score  <- as.numeric(df$score)
df$Story  <- factor(as.character(df$Story), levels = c("1","2","3"))
df$Source <- relevel(factor(df$Source), ref = "Human")
df$agent  <- ifelse(df$Source == "Human", "Human", df$Model)
story_labels <- c("1"="John","2"="Lucy","3"="Grace")
df$story_label <- unname(story_labels[as.character(df$Story)])
cat("Rows in main.csv:", nrow(df_all), " | analysed:", nrow(df), "\n\n")

STUDY1A <- c("False belief","Transparency","Spatial-relation","Testimony")
STUDY1B <- c("Clarified transparency","Clarified spatial-relation")
STUDY2  <- "Multimodal transparency"

# ============================================================
# SECTION 2: Helpers — LRT, profile CI, Fisher, binomial, prerequisites
# ============================================================
fit <- function(f, d) suppressWarnings(glm(as.formula(f), family = binomial, data = d))

lrt <- function(m0, m1, label) {
  a <- suppressWarnings(anova(m0, m1, test = "LRT"))
  data.frame(test = label,
             df       = a$Df[2],
             LR_chisq = round(a$Deviance[2], 3),
             p_LRT    = a$`Pr(>Chi)`[2],
             stringsAsFactors = FALSE)
}

# Odds ratio + 95% profile-likelihood CI for one term
or_profile <- function(m, term) {
  co <- coef(m)
  if (!term %in% names(co)) return(c(OR=NA, lo=NA, hi=NA))
  ci <- tryCatch(suppressMessages(suppressWarnings(confint(m, parm = term, level = .95))),
                 error = function(e) c(NA, NA))
  c(OR = unname(exp(co[term])), lo = unname(exp(ci[1])), hi = unname(exp(ci[2])))
}

fmt_p <- function(p) ifelse(is.na(p), "NA", ifelse(p < .001, "< .001", sprintf("%.3f", p)))

fisher_pair <- function(d, a1, a2) {
  s <- d[d$agent %in% c(a1, a2), ]
  tab <- table(factor(s$agent, levels = c(a1, a2)), factor(s$score, levels = c(0, 1)))
  if (!all(dim(tab) == 2)) return(NULL)
  ft <- fisher.test(tab)
  data.frame(n_1 = sum(tab[1,]), acc_1 = round(mean(s$score[s$agent==a1]), 3),
             n_2 = sum(tab[2,]), acc_2 = round(mean(s$score[s$agent==a2]), 3),
             OR_fisher = round(unname(ft$estimate), 4),
             CI_low = round(ft$conf.int[1], 4), CI_high = round(ft$conf.int[2], 4),
             p_fisher = ft$p.value, stringsAsFactors = FALSE)
}

run_binom <- function(x, n, label) {
  bt <- binom.test(x, n, p = .5)
  data.frame(comparison = label, n_total = n, n_correct = x, accuracy = round(x/n, 3),
             CI_low = round(bt$conf.int[1], 3), CI_high = round(bt$conf.int[2], 3),
             p_binom = bt$p.value, stringsAsFactors = FALSE)
}

# Prerequisite pass rates and target accuracy among retained observations
prereq <- function(d, cols) {
  d$agent <- ifelse(d$Source == "Human", "Human", d$Model)
  do.call(rbind, lapply(split(d, list(d$Condition, d$agent), drop = TRUE), function(x) {
    r <- data.frame(Condition = x$Condition[1], agent = x$agent[1], n = nrow(x), stringsAsFactors = FALSE)
    for (cc in names(cols)) r[[cc]] <- sum(x[[cols[[cc]]]] == 1, na.rm = TRUE)
    pass <- Reduce(`&`, lapply(cols, function(v) !is.na(x[[v]]) & x[[v]] == 1))
    r$n_retained <- sum(pass); r$pct_retained <- round(100 * mean(pass), 1)
    r$n_target_correct <- sum(x$score[pass] == 1, na.rm = TRUE)
    r$target_accuracy  <- round(mean(x$score[pass]), 3)
    r }))
}

# ============================================================
# SECTION 3: Descriptives
# ============================================================
agg <- function(g) {
  s <- split(df, lapply(g, function(v) df[[v]]), drop = TRUE)
  do.call(rbind, lapply(s, function(x) {
    if (!nrow(x)) return(NULL)
    r <- x[1, g, drop = FALSE]
    r$n <- nrow(x); r$n_correct <- sum(x$score); r$accuracy <- round(mean(x$score), 3)
    r$sd <- round(sd(x$score), 3); r
  }))
}
desc <- agg(c("Study","Condition","agent","Source"))
desc <- desc[order(match(desc$Study,STUDY_ORDER), match(desc$Condition,COND_ORDER), desc$Source, desc$agent), ]
cat("=== Descriptives ===\n"); print(desc, row.names = FALSE); save_csv(desc, "results/descriptives.csv")

desc_s <- agg(c("Study","Condition","Story","story_label","agent","Source"))
desc_s <- desc_s[order(match(desc_s$Study,STUDY_ORDER), match(desc_s$Condition,COND_ORDER),
                       desc_s$Story, desc_s$Source, desc_s$agent), ]
cat("\n=== Descriptives by vignette ===\n"); print(desc_s, row.names = FALSE)
save_csv(desc_s, "results/descriptives_by_story.csv")

# ============================================================
# SECTION 4: Study 1a — LLM baseline (binomial vs chance)
# ============================================================
cat("\n\n=== STUDY 1a: replication baseline (LLM only) ===\n")
b1a <- do.call(rbind, lapply(STUDY1A, function(cond) {
  d <- df[df$Condition == cond, ]
  r <- do.call(rbind, lapply(sort(unique(d$agent)), function(a) {
    x <- d[d$agent == a, ]
    cbind(Condition = cond, agent = a, run_binom(sum(x$score), nrow(x), paste0(cond, ": ", a)))
  }))
  r$p_holm <- p.adjust(r$p_binom, "holm"); r
}))
b1a$p_binom_fmt <- fmt_p(b1a$p_binom); b1a$p_holm_fmt <- fmt_p(b1a$p_holm)
print(b1a[, c("Condition","agent","n_total","n_correct","accuracy","CI_low","CI_high","p_binom_fmt","p_holm_fmt")],
      row.names = FALSE)
save_csv(b1a, "results/results_1a_binomial.csv")

# ============================================================
# SECTION 5: Study 1b — vignette FIXED, all tests by LRT
# ============================================================
cat("\n\n=== STUDY 1b: clarity manipulation ===\n")

# ---- (0) Prerequisite questions (all observations, before exclusion) ----
cat("\n[0] Prerequisite questions Q1-Q3 (all observations, before exclusion)\n")
pre1b <- prereq(df_all[df_all$Condition %in% STUDY1B, ],
                c(Q1_correct = "study1b_q1_correct", Q2_correct = "study1b_q2_correct",
                  Q3_correct = "study1b_q3_correct"))
pre1b <- pre1b[order(match(pre1b$Condition, COND_ORDER), pre1b$agent != "Human", pre1b$agent), ]
print(pre1b, row.names = FALSE); save_csv(pre1b, "results/prereq_1b.csv")

d1b <- df[df$Condition %in% STUDY1B, ]
d1b$Condition <- relevel(factor(d1b$Condition), ref = "Clarified spatial-relation")
d1b$Story     <- relevel(droplevels(d1b$Story), ref = "1")

# ---- (1) Omnibus LR tests for every term in the full model ----------------
# Full model: score ~ Source * Condition + Story. Factor-level LRTs with
# marginality respected (Type II): each main effect is tested against the
# additive model; the interaction and vignette terms against the full model.
cat("\n[1] Omnibus LR tests for all terms in the full model",
    "(score ~ Source * Condition + Story)\n")
m_full      <- fit("score ~ Source * Condition + Story", d1b)
m_add       <- fit("score ~ Source + Condition + Story", d1b)
m_no_story  <- fit("score ~ Source * Condition", d1b)
m_no_source <- fit("score ~ Condition + Story", d1b)
m_no_cond   <- fit("score ~ Source + Story",    d1b)

omni <- rbind(
  lrt(m_no_source, m_add,  "Source (main effect)"),
  lrt(m_no_cond,   m_add,  "Condition (main effect)"),
  lrt(m_add,       m_full, "Source x Condition (interaction)"),
  lrt(m_no_story,  m_full, "Story / vignette (omnibus)")
)
names(omni)[1] <- "term"
omni$p_LRT_fmt <- fmt_p(omni$p_LRT)
print(omni, row.names = FALSE)
save_csv(omni, "results/lrt_1b_omnibus_effects.csv")

# marginal accuracies that the two main effects summarise
marg_src  <- do.call(rbind, lapply(levels(d1b$Source), function(l) {
  x <- d1b[d1b$Source == l, ]
  data.frame(effect = "Source", level = l, n = nrow(x),
             n_correct = sum(x$score), accuracy = round(mean(x$score), 3)) }))
marg_cond <- do.call(rbind, lapply(levels(d1b$Condition), function(l) {
  x <- d1b[d1b$Condition == l, ]
  data.frame(effect = "Condition", level = l, n = nrow(x),
             n_correct = sum(x$score), accuracy = round(mean(x$score), 3)) }))
marg_cell <- do.call(rbind, lapply(split(d1b, list(d1b$Source, d1b$Condition), drop = TRUE),
  function(x) data.frame(effect = "Source x Condition",
             level = paste(x$Source[1], x$Condition[1], sep = " / "), n = nrow(x),
             n_correct = sum(x$score), accuracy = round(mean(x$score), 3))))
marg <- rbind(marg_src, marg_cond, marg_cell)
cat("  marginal / cell accuracies behind these terms:\n")
print(marg, row.names = FALSE)
save_csv(marg, "results/lrt_1b_omnibus_margins.csv")

# ---- (2) Per condition: LRT humans vs pooled models, vignette controlled ----
cat("\n[2] Per condition: LRT for Source (humans vs pooled models), vignette controlled\n")
t3 <- do.call(rbind, lapply(STUDY1B, function(cond) {
  d <- d1b[d1b$Condition == cond, ]; d$Story <- droplevels(d$Story)
  m1 <- fit("score ~ Source + Story", d); m0 <- fit("score ~ Story", d)
  r  <- lrt(m0, m1, paste0(cond, ": Source | + Story"))
  o  <- or_profile(m1, "SourceModel")
  data.frame(Condition = cond,
             n_human = sum(d$Source=="Human"), acc_human = round(mean(d$score[d$Source=="Human"]),3),
             n_model = sum(d$Source=="Model"), acc_model = round(mean(d$score[d$Source=="Model"]),3),
             df = r$df, LR_chisq = r$LR_chisq, p_LRT = r$p_LRT, p_LRT_fmt = fmt_p(r$p_LRT),
             OR = round(o["OR"],4), CI_low = round(o["lo"],4), CI_high = round(o["hi"],4),
             stringsAsFactors = FALSE)
}))
print(t3, row.names = FALSE); save_csv(t3, "results/lrt_1b_source_by_condition.csv")

# ---- (3) Fisher per individual model vs humans, Holm within condition ----
cat("\n[3] Fisher's exact: each model vs humans, Holm-adjusted within condition\n")
t4 <- do.call(rbind, lapply(STUDY1B, function(cond) {
  d <- d1b[d1b$Condition == cond, ]
  mods <- sort(unique(d$agent[d$agent != "Human"]))
  r <- do.call(rbind, lapply(mods, function(m) {
    f <- fisher_pair(d, "Human", m); if (is.null(f)) return(NULL)
    cbind(Condition = cond, model = m, f)
  }))
  r$p_holm <- p.adjust(r$p_fisher, "holm"); r
}))
names(t4)[names(t4)=="n_1"] <- "n_human"; names(t4)[names(t4)=="acc_1"] <- "acc_human"
names(t4)[names(t4)=="n_2"] <- "n_model"; names(t4)[names(t4)=="acc_2"] <- "acc_model"
t4$p_fisher_fmt <- fmt_p(t4$p_fisher); t4$p_holm_fmt <- fmt_p(t4$p_holm)
print(t4[, c("Condition","model","n_human","acc_human","n_model","acc_model",
             "OR_fisher","CI_low","CI_high","p_fisher_fmt","p_holm_fmt")], row.names = FALSE)
save_csv(t4, "results/fisher_1b_per_model.csv")

# ---- (4) Humans only: LRT for Condition x (John vs other vignettes) ----
cat("\n[4] Humans only: LRT for Condition x John-vs-other interaction\n")
dh <- d1b[d1b$Source == "Human", ]
dh$Vig <- relevel(factor(ifelse(dh$Story == "1", "John", "Other")), ref = "Other")
m5_full <- fit("score ~ Condition * Vig", dh)
m5_add  <- fit("score ~ Condition + Vig", dh)
t5 <- lrt(m5_add, m5_full, "Condition x John-vs-other (humans)")
t5$p_LRT_fmt <- fmt_p(t5$p_LRT)
print(t5, row.names = FALSE)
cat("  cell accuracies (humans):\n")
cells <- do.call(rbind, lapply(split(dh, list(dh$Condition, dh$Vig), drop=TRUE), function(x)
  data.frame(Condition=as.character(x$Condition[1]), Vignette=as.character(x$Vig[1]),
             n=nrow(x), n_correct=sum(x$score), accuracy=round(mean(x$score),3))))
print(cells, row.names = FALSE)
save_csv(t5, "results/lrt_1b_human_vignette.csv"); save_csv(cells, "results/human_vignette_cells.csv")

# ---- (5) Fisher: John vs other two combined, humans, clarified spatial-relation ----
cat("\n[5] Fisher's exact: John vs other two vignettes (humans, clarified spatial-relation)\n")
dpc <- dh[dh$Condition == "Clarified spatial-relation", ]
tabpc <- table(factor(ifelse(dpc$Story=="1","John","Other"), levels=c("John","Other")),
               factor(dpc$score, levels=c(0,1)))
f6 <- fisher.test(tabpc)
t6 <- data.frame(test = "Clarified spatial-relation humans: John vs Lucy+Grace",
                 n_John = sum(tabpc[1,]),  acc_John  = round(tabpc[1,2]/sum(tabpc[1,]), 3),
                 n_Other= sum(tabpc[2,]),  acc_Other = round(tabpc[2,2]/sum(tabpc[2,]), 3),
                 OR_fisher = round(unname(f6$estimate),4),
                 CI_low = round(f6$conf.int[1],4), CI_high = round(f6$conf.int[2],4),
                 p_fisher = f6$p.value, p_fisher_fmt = fmt_p(f6$p.value), stringsAsFactors = FALSE)
print(t6, row.names = FALSE); save_csv(t6, "results/fisher_1b_john_vs_other.csv")

# ============================================================
# SECTION 6: Study 2 — Multimodal transparency 
# ============================================================
cat("\n\n=== STUDY 2: Multimodal transparency ===\n")

# ---- (0) Prerequisites (Q1 = image identification, both pictures; Q2 = perceptual access) ----
cat("\n[0] Prerequisite questions and target accuracy among retained observations (Table 3)\n")
pre2 <- prereq(df_all[df_all$Condition == STUDY2, ],
               c(Q1_correct = "study2_q1_both_correct", Q2_correct = "study2_q2_correct"))
pre2 <- pre2[order(pre2$agent != "Human", pre2$agent), ]
print(pre2, row.names = FALSE); save_csv(pre2, "results/prereq_2.csv")

# ---- (1) Primary: Human vs Gemini3, vignette controlled ----
cat("\n[1] Primary: Human vs Gemini3, vignette controlled\n")
d2 <- df[df$Condition == STUDY2 & df$agent %in% c("Human", "Gemini3"), ]; d2$Story <- droplevels(d2$Story)
m2_1 <- fit("score ~ Source + Story", d2); m2_0 <- fit("score ~ Story", d2)
r2 <- lrt(m2_0, m2_1, "Multimodal transparency: Source | + Story"); o2 <- or_profile(m2_1, "SourceModel")
t7 <- data.frame(Condition = STUDY2,
                 n_human = sum(d2$Source=="Human"), acc_human = round(mean(d2$score[d2$Source=="Human"]),3),
                 n_model = sum(d2$Source=="Model"), acc_model = round(mean(d2$score[d2$Source=="Model"]),3),
                 df = r2$df, LR_chisq = r2$LR_chisq, p_LRT = r2$p_LRT, p_LRT_fmt = fmt_p(r2$p_LRT),
                 OR = round(o2["OR"],4), CI_low = round(o2["lo"],4), CI_high = round(o2["hi"],4),
                 stringsAsFactors = FALSE)
print(t7, row.names = FALSE); save_csv(t7, "results/lrt_2_multimodal.csv")

# ---- (2) Omnibus vignette effect ----
cat("\n[2] Omnibus LRT: vignette (Story), df = 2\n")
t7b <- lrt(fit("score ~ Source", d2), m2_1, "Study 2: vignette omnibus | Source")
t7b$p_LRT_fmt <- fmt_p(t7b$p_LRT); print(t7b, row.names = FALSE)
save_csv(t7b, "results/lrt_2_vignette_omnibus.csv")

# ============================================================
cat("\n\n=== FILES WRITTEN ===\n"); for (f in saved) cat(" ", f, "\n")
cat("  results/analysis_log.txt (this log)\n")
cat("\nDone:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
sink(type = "message"); sink(); close(LOG)
