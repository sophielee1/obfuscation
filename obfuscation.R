

####### Step 1: Prepping the data ########

# 1. Load both files (note the 2025 filename matches your attached CSV)
data_25 <- read.csv("2025_individually_reported_AI_use_cases.csv",
                    fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE)
data_24 <- read.csv("2024_consolidated_ai_inventory_raw_v2.csv",
                    fileEncoding = "windows-1252", stringsAsFactors = FALSE)

# Quick check
#names(data_25)
#names(data_24)


# 2. Standardize 2024 variables by column index and add year tag
library(dplyr)

check_col <- function(df, i, pattern) {
  if (!grepl(pattern, names(df)[i], ignore.case = TRUE))
    stop(sprintf("Column %d is '%s', expected to match '%s'", i, names(df)[i], pattern))
}
check_col(data_24, 8,  "purpose")
check_col(data_24, 9,  "output")
check_col(data_24, 11, "rights")
check_col(data_24, 51, "without.direct.human")
check_col(data_24, 59, "fallback")
check_col(data_25, 13, "problem_solved")
check_col(data_25, 34, "hi_failsafe")
check_col(data_25, 35, "hi_appeal")




clean_24 <- data_24 %>%
  transmute(
    year                    = 2024L,
    use_case_id             = NA_character_,
    agency                  = .[[2]],
    agency_abbr             = .[[3]],
    bureau                  = .[[4]],
    use_case_name           = .[[1]],
    topic_area              = .[[5]],
    topic_area_other        = .[[6]],
    ai_method               = NA_character_,
    cots_category           = .[[7]],
    purpose                 = .[[8]],   # 2024 combines purpose and expected benefits
    benefits                = NA_character_,
    outputs                 = .[[9]],
    stage                   = .[[10]],
    date_initiated          = .[[12]],
    date_acq_dev            = .[[13]],
    date_implemented        = .[[14]],
    date_retired            = .[[15]],
    dev_source              = .[[16]],
    contract_piid           = .[[17]],
    vendor_name             = NA_character_,
    has_ato                 = .[[34]],
    system_name             = .[[35]],
    has_custom_code         = .[[31]],
    agency_code_access      = .[[32]],
    code_url                = .[[33]],
    training_data           = .[[27]],
    # The 2024 and 2025 forms ask different things, so they get
    # separate columns instead of being merged.
    data_docs_2024          = .[[28]],  # yes/no: is training data documented?
    data_link_2025          = NA_character_,
    has_pii                 = .[[23]],
    saop_review_2024        = .[[24]],  # did the privacy official assess risks?
    pia_url_2025            = NA_character_,
    demographics_used       = .[[29]],
    demographics_other      = .[[30]],
    impact_type             = .[[11]],
    impact_justification    = NA_character_,
    impact_assessment       = .[[46]],
    op_testing              = .[[47]],
    key_risks               = .[[48]],  # 2025 analog: potential impacts
    independent_review      = .[[49]],
    post_deploy_monitor     = .[[50]],
    # column 51 asks whether the AI can act WITHOUT a human, which is
    # not the same as the 2025 failsafe question (column 34).
    acts_without_human_2024 = .[[51]],
    acts_without_human_other= .[[52]],
    failsafe_2025           = NA_character_,
    operator_training       = NA_character_,
    appeal_process          = .[[59]],
    appeal_no_explain       = .[[60]],
    opt_out_mechanism       = .[[61]],
    opt_out_other           = .[[62]],
    public_notice           = .[[53]],
    public_notice_other     = .[[54]],
    public_consultation     = .[[57]],
    public_consultation_other = .[[58]],
    is_hisp                 = .[[18]],
    hisp_name               = .[[19]],
    hisp_service            = .[[20]],
    public_dissemination    = .[[21]],
    iqa_compliance          = .[[22]],
    has_data_catalog        = .[[25]],
    data_catalog_other      = .[[26]],
    dev_tools_wait          = .[[36]],
    central_it_intake       = .[[37]],
    central_it_intake_other = .[[38]],
    compute_request_process = .[[39]],
    compute_request_other   = .[[40]],
    timely_resource_comm    = .[[41]],
    timely_resource_other   = .[[42]],
    internal_tool_reuse     = .[[43]],
    internal_review_avail   = .[[44]],
    extension_requested     = .[[45]],
    influences_adverse_decisions = .[[55]],  # renamed: it asks whether AI informs adverse decisions
    bias_mitigation         = .[[56]]
  )

clean_25 <- data_25 %>%
  transmute(
    year                    = 2025L,
    use_case_id             = as.character(.[[3]]),
    agency                  = .[[2]],
    agency_abbr             = .[[1]],
    bureau                  = .[[5]],
    use_case_name           = .[[4]],
    topic_area              = .[[11]],
    ai_method               = .[[12]],
    purpose                 = .[[13]],
    benefits                = .[[14]],
    outputs                 = .[[15]],
    stage                   = .[[8]],
    date_implemented        = .[[16]],
    dev_source              = .[[17]],
    vendor_name             = .[[18]],
    has_ato                 = .[[19]],
    system_name             = .[[20]],
    has_custom_code         = .[[26]],
    code_url                = .[[27]],
    training_data           = .[[21]],
    data_link_2025          = .[[22]],
    has_pii                 = .[[23]],
    pia_url_2025            = .[[24]],
    demographics_used       = .[[25]],
    impact_type             = .[[9]],
    impact_justification    = .[[10]],
    impact_assessment       = .[[29]],
    op_testing              = .[[28]],
    key_risks               = .[[30]],
    independent_review      = .[[31]],
    post_deploy_monitor     = .[[32]],
    operator_training       = .[[33]],
    failsafe_2025           = .[[34]],  # (see above)
    appeal_process          = .[[35]],
    public_consultation     = .[[36]],
    contact_email           = .[[6]],
    withheld_status         = .[[7]]
  )

ai_inventory <- bind_rows(clean_24, clean_25) %>%
  mutate(across(where(is.character), ~ na_if(trimws(.), "")))

# the 2025 file stores empty multi-select answers as the literal "[]".
ai_inventory <- ai_inventory %>%
  mutate(across(where(is.character), ~ na_if(., "[]")))

stopifnot(sum(ai_inventory$year == 2024) == 2133,
          sum(ai_inventory$year == 2025) == 3611)

####### Step 2: Harmonized variables for the Specificity Index#######

paste_na <- function(...) {
  x <- trimws(do.call(paste, c(lapply(list(...), function(v) ifelse(is.na(v), "", v)), sep = " ")))
  ifelse(x == "", NA_character_, x)
}
n_words <- function(x) ifelse(is.na(x), 0L, lengths(regmatches(x, gregexpr("[A-Za-z][A-Za-z'-]*", x))))

si <- ai_inventory %>%
  group_by(year) %>%
  mutate(row_id = if_else(year == 2025L, paste0("2025_", use_case_id),
                          sprintf("2024_%04d", row_number()))) %>%
  ungroup() %>%
  mutate(
    # 1. Stage on one scale. 2024 used five lifecycle labels; 2025 uses four.
    stage_std = case_when(
      year == 2025L                                         ~ stage,
      grepl("retired|action complete", stage, ignore.case = TRUE)          ~ "Retired",
      grepl("operation|production|in mission", stage, ignore.case = TRUE) ~ "Deployed",
      grepl("implementation", stage, ignore.case = TRUE)                  ~ "Pilot",
      grepl("initiat|planned|ideation|acquisition", stage, ignore.case = TRUE) ~ "Pre-deployment",
      TRUE ~ NA_character_),
    
    # 2. Classification label, cleaned. 2024 labels contain stray line breaks
    #    and inconsistent capitalization.
    label_std = case_when(
      year == 2025L & is.na(impact_type)            ~ "Blank",
      year == 2025L                                 ~ impact_type,
      grepl("^both", impact_type, ignore.case = TRUE)             ~ "Rights- and safety-impacting",
      grepl("^rights", impact_type, ignore.case = TRUE)           ~ "Rights-impacting",
      grepl("^safety", impact_type, ignore.case = TRUE)           ~ "Safety-impacting",
      grepl("too new", impact_type, ignore.case = TRUE)           ~ "Too new to assess",
      grepl("^neither", impact_type, ignore.case = TRUE)          ~ "Neither",
      TRUE                                          ~ "Blank/other"),
    flagged = (year == 2025L & label_std == "High-impact") |
      (year == 2024L & label_std %in% c("Rights-impacting", "Safety-impacting",
                                        "Rights- and safety-impacting")),
    
    # 3. Commercial tools listed in 2024's general-products list were exempt
    #    from most detailed questions, so missing answers there are not evasive.
    cots_2024 = year == 2024L & !is.na(cots_category) &
      !grepl("^none of the", cots_category, ignore.case = TRUE),
    
    # 4. Narrative text scored for detail, linkage, hedging and boilerplate.
    #    2024 asked one combined purpose-and-benefits question, so 2025's two
    #    questions are combined to match.
    purpose_benefits = if_else(year == 2025L, paste_na(purpose, benefits), purpose),
    narrative        = paste_na(purpose_benefits, outputs),
    narrative_words  = n_words(narrative),
    data_text        = training_data,   # optional extra text for technical detail
    
    # 5. Which entries to score.
    # A missing stage does not exempt an entry; only retired entries are skipped.
    scored     = (is.na(stage_std) | stage_std != "Retired") & narrative_words > 0,
    # Detailed questions were required in both years only for systems in use
    # (pilot or deployed) that were not on 2024's commercial list.
    detailed_q = stage_std %in% c("Pilot", "Deployed") & !cots_2024
  )

# Evasive completion -----------------------------------------------------------
evasive_rx <- paste0(
  "^\\s*(n/?a|na|none|tbd|tba|unknown|pending|forthcoming|-+)\\s*\\.?\\s*$",
  "|not applicable|in[- ]progress|to be determined|see (above|comments?)|same as above")

# Answers pasted verbatim into many entries (6+ words, 10+ repeats) count as evasive.
template_fields <- c("training_data", "key_risks", "impact_assessment", "op_testing",
                     "independent_review", "post_deploy_monitor", "appeal_process",
                     "public_consultation")
all_answers <- unlist(si[template_fields], use.names = FALSE)
all_answers <- all_answers[!is.na(all_answers) & n_words(all_answers) >= 6]
templates   <- names(which(table(all_answers) >= 10))

is_evasive <- function(x) is.na(x) | grepl(evasive_rx, x, ignore.case = TRUE) | x %in% templates

# Required in both years for pilot/deployed, non-commercial systems.
required_common <- c("dev_source", "has_pii", "has_ato", "has_custom_code", "training_data")
# Risk-practice questions with a counterpart in both years (flagged systems only).
hi_common <- c("impact_assessment", "op_testing", "key_risks", "independent_review",
               "post_deploy_monitor", "appeal_process", "public_consultation")

ev_req <- sapply(required_common, function(f) is_evasive(si[[f]]))
ev_hi  <- sapply(hi_common,       function(f) is_evasive(si[[f]]))

si <- si %>%
  mutate(
    evasive_required = ifelse(detailed_q, rowMeans(ev_req), NA_real_),
    # 2025 vendor-purchased tools should name the vendor.
    vendor_missing = ifelse(year == 2025L & detailed_q &
                              grepl("vendor|contracting", dev_source, ignore.case = TRUE),
                            is.na(vendor_name), NA),
    evasive_hi = ifelse(flagged, rowMeans(ev_hi), NA_real_)
  )

out <- si %>%
  select(row_id, year, agency_abbr, agency, bureau, use_case_name, topic_area,
         stage_std, label_std, flagged, cots_2024, scored, detailed_q,
         narrative, narrative_words, data_text, evasive_required, vendor_missing,
         evasive_hi, impact_justification)

write.csv(out, "si_input.csv", row.names = FALSE, fileEncoding = "UTF-8")

# Quick checks
print(table(out$year, out$scored))
print(table(out$year, out$label_std))
print(out %>% group_by(year) %>%
        summarise(n_scored = sum(scored),
                  n_detailed_q = sum(detailed_q),
                  evasive_required = round(mean(evasive_required, na.rm = TRUE), 2),
                  n_flagged = sum(flagged),
                  evasive_hi = round(mean(evasive_hi, na.rm = TRUE), 2)))







