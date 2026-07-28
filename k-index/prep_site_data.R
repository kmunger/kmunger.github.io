# ============================================================================
# prep_site_data.R  —  turn the pipeline outputs into the website's data files.
#
# MVP version: the site shows ONE fixed ranking (no visitor-adjustable
# weights). Authors are shipped pre-sorted by rank, with scores; feat.bin
# carries per-feature percentiles for the profile card.
#
# Reads (from OPENALEX_DIR, default C:/Users/kmunger/openalex_polisci):
#   k_index_features_full.rds  (Stage B-2)  - features incl. full_* columns
#   k_index_weights_full.csv   (Stage D-4)  - THE weights (feature, weight)
# Writes (into this script's own folder = the repo's k-index/):
#   data/meta.json  data/authors.json  data/feat.bin
# Then: git add k-index && git commit && git push
#
# Run from RStudio:  source("path/to/prep_site_data.R")
# Author: Kevin Munger (+ assist) | Built: 2026-07-27 (MVP rework)
# ============================================================================

suppressPackageStartupMessages({ library(dplyr); library(jsonlite) })

data_dir <- Sys.getenv("OPENALEX_DIR", "C:/Users/kmunger/openalex_polisci")
site_dir <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)),
                     error = function(e) getwd())
out_dir  <- file.path(site_dir, "data")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ---- labels for every feature the pipeline can produce ----------------------
LABELS <- tibble::tribble(
  ~key,                    ~label,                      ~blurb,
  "frac_works",            "Fractional output",         "In-scope works, credit split among coauthors",
  "cites_frac",            "Fractional citations",      "In-scope citations split among coauthors",
  "cites_solo",            "Solo citations",            "Citations to solo-authored in-scope work",
  "share_solo",            "Solo share",                "Share of in-scope works that are solo-authored",
  "independence",          "Independence",              "1 / mean team size",
  "first_author_share",    "First-author share",        "Share of in-scope works as first author",
  "cites_per_career_year", "Citations per career year", "Age-normalized in-scope impact",
  "cites_recent_frac",     "Recent citations",          "Recency-discounted fractional citations, in scope",
  "share_recent_works",    "Recent activity",           "Share of in-scope works from the last six years",
  "topic_entropy",         "Topic diversity (in scope)","Shannon entropy of in-scope topics",
  "outside_share",         "Outside reach (in scope)",  "In-scope works touching other subfields",
  "share_nonarticle",      "Format pluralism",          "Books and chapters alongside articles",
  "full_cites",            "Career citations",          "All citations, all fields, whole career",
  "full_recent_cites",     "Career recent citations",   "Citations earned in the last three years, all fields",
  "full_2yr_citedness",    "2-year citedness",          "Mean citations to recent work, all fields",
  "full_topic_entropy",    "Career topic diversity",    "Shannon entropy over the full topic distribution",
  "full_outside_share",    "Career outside share",      "Share of all work outside the core subfields",
  "full_works",            "Career works",              "Total works, all fields",
  "full_h_index",          "h-index",                   "Included for irony"
)

# ---- 1. Weights define the feature set (single source of truth) -------------
w <- read.csv(file.path(data_dir, "k_index_weights_full.csv"))
FEATURES <- w$feature
message("Features (from weights csv): ", length(FEATURES))
stopifnot(all(FEATURES %in% LABELS$key))

# ---- 2. Universe, percentiles, scores, sort by rank --------------------------
target_id <- "https://openalex.org/A5015770363"
PS_CUTOFF <- 0.20   # universe rule: >=20% of career output in the two
                    # political-science subfields (3320 + 3312)
feat <- readRDS(file.path(data_dir, "k_index_features_full.rds")) %>%
  mutate(ps = coalesce(share_3320, 0) + coalesce(share_3312, 0)) %>%
  filter((eligible & ps >= PS_CUTOFF) | author_id == target_id) %>%
  mutate(across(all_of(FEATURES), ~ coalesce(., 0)))

X <- feat %>%
  mutate(across(all_of(FEATURES), ~ rank(., ties.method = "average") / n()))

M <- as.matrix(X[, FEATURES])
score <- as.numeric(M %*% w$weight)             # weights sum to 1 -> [0,1]
ord <- order(-score, X$author_name)             # rank order, name tiebreak
X <- X[ord, ]; M <- M[ord, , drop = FALSE]; score <- score[ord]
message("Universe: ", nrow(X), " authors, sorted. Kevin's rank: ",
        which(X$author_id == target_id))

# ---- 3. feat.bin (uint8 percentiles, row-major, in rank order) ---------------
bytes <- as.raw(pmin(255L, pmax(0L, as.integer(round(t(M) * 255)))))
writeBin(bytes, file.path(out_dir, "feat.bin"))

# ---- 4. authors.json ----------------------------------------------------------
inst <- if ("institution" %in% names(X)) X$institution else {
  ai <- tryCatch(readRDS(file.path(data_dir, "polisci_authors.rds")) %>%
                   select(author_id, institution),
                 error = function(e) NULL)
  if (!is.null(ai)) left_join(X["author_id"], ai, by = "author_id")$institution
  else rep("", nrow(X))
}
write_json(
  list(names  = X$author_name,
       ids    = sub("https://openalex.org/", "", X$author_id),
       inst   = substr(ifelse(is.na(inst), "", inst), 1, 60),
       scores = round(score * 100, 1)),
  file.path(out_dir, "authors.json"), auto_unbox = FALSE, na = "null")

# ---- 5. meta.json --------------------------------------------------------------
fdefs <- LABELS %>% filter(key %in% FEATURES) %>% arrange(match(key, FEATURES))
meta <- list(
  demo     = FALSE,
  vintage  = paste("OpenAlex snapshot,", format(Sys.Date(), "%B %Y")),
  n        = nrow(X),
  universe = sprintf(paste("%s scholars with ≥5 in-scope works, ≥100 in-scope citations",
                           "(2000–2025), and ≥20%% of career output in the political",
                           "science subfields"),
                     format(nrow(X), big.mark = ",")),
  features = purrr::pmap(fdefs, function(key, label, blurb)
               list(key = key, label = label, blurb = blurb)),
  weights  = setNames(as.list(round(w$weight, 4)), w$feature)
)
write_json(meta, file.path(out_dir, "meta.json"), auto_unbox = TRUE, pretty = TRUE)

sz <- function(f) sprintf("%.1f MB", file.size(file.path(out_dir, f)) / 1e6)
message("Wrote: feat.bin ", sz("feat.bin"), " | authors.json ", sz("authors.json"),
        " | meta.json ", sz("meta.json"))
message("Now: git add k-index && git commit -m 'k-index: real data' && git push")
# ============================================================================
