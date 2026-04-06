# ── STEP 1: Diagnose es_combined ──────────────────────────────────────────
length(es_combined)
lapply(es_combined, function(x) {
  if (is.null(x)) return("NULL")
  paste0("nrow=", nrow(x), " | cols=", paste(names(x), collapse=","))
})

# ── STEP 2: Check window values in first non-null element ─────────────────
first_good <- es_combined[[which(!sapply(es_combined, is.null))[1]]]
unique(first_good$window)
class(first_good$window)

# ── STEP 3: Rebuild es_combined CLEANLY forcing character window ───────────
es_combined <- vector("list", length(es_outcomes))

for (i in seq_along(es_outcomes)) {
  parts <- list()
  
  mc <- es_crisis_list[[i]]
  mr <- es_rbc_list[[i]]
  
  if (!is.null(mc)) {
    coefs_c <- tryCatch(
      tidy(mc, conf.int = TRUE) |>
        dplyr::filter(grepl("event_time::", term)) |>
        dplyr::mutate(
          event_time = as.integer(regmatches(term,
                                             regexpr("-?[0-9]+", term))),
          window     = "Crisis (2008q3)"
        ) |>
        dplyr::filter(event_time >= EVENT_MIN,
                      event_time <= EVENT_MAX),
      error = function(e) NULL
    )
    if (!is.null(coefs_c)) parts[[length(parts)+1]] <- coefs_c
  }
  
  if (!is.null(mr)) {
    coefs_r <- tryCatch(
      tidy(mr, conf.int = TRUE) |>
        dplyr::filter(grepl("event_time::", term)) |>
        dplyr::mutate(
          event_time = as.integer(regmatches(term,
                                             regexpr("-?[0-9]+", term))),
          window     = "RBC Rule (2022q1)"
        ) |>
        dplyr::filter(event_time >= EVENT_MIN,
                      event_time <= EVENT_MAX),
      error = function(e) NULL
    )
    if (!is.null(coefs_r)) parts[[length(parts)+1]] <- coefs_r
  }
  
  if (length(parts) > 0) {
    combined <- dplyr::bind_rows(parts)
    # Add reference rows
    for (win in unique(combined$window)) {
      combined <- dplyr::bind_rows(
        combined,
        data.frame(
          term = "ref", estimate = 0, std.error = 0,
          statistic = NA_real_, p.value = NA_real_,
          conf.low = 0, conf.high = 0,
          event_time = as.integer(EVENT_REF),
          window = win,
          stringsAsFactors = FALSE
        )
      )
    }
    es_combined[[i]] <- combined |>
      dplyr::mutate(window = as.character(window)) |>
      dplyr::arrange(window, event_time)
  }
}

# ── STEP 4: Verify ────────────────────────────────────────────────────────
cat("Non-null elements:", sum(!sapply(es_combined, is.null)), "\n")
if (!is.null(es_combined[[1]])) {
  cat("Window values:", unique(es_combined[[1]]$window), "\n")
  cat("Class:", class(es_combined[[1]]$window), "\n")
}

# ── STEP 5: Re-run shape analysis ─────────────────────────────────────────
shape_rows <- list()
for (i in seq_len(n_outcomes)) {
  coefs <- es_combined[[i]]
  lbl   <- es_outcomes[[i]]$label
  if (is.null(coefs) || nrow(coefs) == 0) next
  
  for (win in c("Crisis (2008q3)", "RBC Rule (2022q1)")) {
    d <- coefs[coefs$window == win & !is.na(coefs$p.value), ]
    if (nrow(d) == 0) next
    
    early <- d[d$event_time >= 1 & d$event_time <= 4, ]
    late  <- d[d$event_time >= 5 & d$event_time <= EVENT_MAX, ]
    pre   <- d[d$event_time >= EVENT_MIN & d$event_time <= -2, ]
    
    if (nrow(early) == 0 | nrow(late) == 0) next
    
    em <- mean(early$estimate, na.rm = TRUE)
    lm_val <- mean(late$estimate, na.rm = TRUE)
    pm <- mean(pre$estimate, na.rm = TRUE)
    pers <- if (!is.na(em) && abs(em) > 0.001) lm_val/em else NA_real_
    
    post_d <- d[d$event_time >= 0, ]
    sl <- tryCatch(
      coef(lm(estimate ~ event_time, data = post_d))[2],
      error = function(e) NA_real_
    )
    
    shape_rows[[length(shape_rows)+1]] <- data.frame(
      Outcome     = lbl,
      Window      = win,
      Pre_mean    = round(pm,    3),
      Early_post  = round(em,    3),
      Late_post   = round(lm_val,3),
      Persistence = round(pers,  2),
      Post_slope  = round(sl,    4),
      Shape = dplyr::case_when(
        is.na(pers)               ~ "Undetermined",
        em > 0 & lm_val < 0      ~ "Full reversal (V-shape)",
        pers < 0.3                ~ "Strong recovery (V-shape)",
        pers >= 0.3 & pers < 0.7 ~ "Partial recovery",
        pers >= 0.7               ~ "Persistent (step-change)",
        TRUE                      ~ "Undetermined"
      ),
      stringsAsFactors = FALSE
    )
  }
}

shape_results <- dplyr::bind_rows(shape_rows)
cat("\n=== SHAPE ANALYSIS ===\n")
print(shape_results[order(shape_results$Outcome, shape_results$Window), ])
write.csv(shape_results,
          file.path(TABLE_PATH, "3B_shape_analysis.csv"),
          row.names = FALSE)