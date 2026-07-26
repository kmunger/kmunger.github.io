# ============================================================================
# prep_site_data.R  —  turn the k-index pipeline outputs into the website's
# data files. Run this AFTER the pipeline finishes (Stages A-D), then git
# add/commit/push the k-index folder. The site needs no other changes:
# index.html reads whatever these files say.
#
# Reads (from OPENALEX_DIR, default C:/Users/kmunger/openalex_polisci):
#   k_index_features.rds   (Stage C)  - features + eligibility
#   polisci_authors.rds    (Stage B)  - institution, for display only
#   k_index_weights.csv    (Stage D)  - the LP-winning "official" weights (optional;
#                                       equal weights are used if it's missing)
# Writes (into this script's own folder, i.e. the repo's k-index/data/):
#   data/meta.json  data/authors.json  data/feat.bin
#
# Run from RStudio (any working directory):  source("path/to/prep_site_data.R")
# Author: Kevin Munger (+ assist) | Built: 2026-07-26
# ============================================================================

suppressPackageStartupMessages({ library(dplyr); library(jsonlite) })

data_dir <- Sys.getenv("OPENALEX_DIR", "C:/Users/kmunger/openalex_polisci")

# site folder = wherever this script lives (works with source() from RStudio)
site_dir <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)),
                     error = function(e) getwd())
out_dir  <- file.path(site_dir, "data")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Must match FEATURES in k_index_optimize.R (order = column order in feat.bin)
FEATURES <- tibble::tribble(
  ~key,                    ~label,                ~blurb,
  "frac_works",            "Fractional output",   "Works credited, split among coauthors",
  "cites_frac",            "Fractional citations","Citations split among coauthors (Waltman & van Eck)",
  "cites_solo",            "Solo citations",      "Citations to solo-authored work: verifiable individual contribution",
  "share_solo",            "Solo share",          "Share of works that are solo-authored",
  "independence",          "Independence",        "1 / mean team size — resists hyperauthorship",
  "first_author_share",    "First-author share",  "Share of works as first author",
  "cites_per_career_year", "Anti-gerontocracy",   "Citations per career year — age-normalized impact (cf. Munger 2022)",
  "cites_recent_frac",     "Temporal validity",   "Recency-discounted fractional citations — knowledge depreciates",
  "share_recent_works",    "Recent activity",     "Share of works from the last six years",
  "topic_entropy",         "Interdisciplinarity", "Shannon entropy of topics published in",
  "outside_share",         "External reach",      "Share of works reaching beyond the core subfields",
  "share_nonarticle",      "Format pluralism",    "Books and chapters alongside articles"
)

# ---- 1. Load + filter to the eligible universe ------------------------------
feat <- readRDS(file.path(data_dir, "k_index_features.rds")) %>%
  filter(eligible | author_id == "https://openalex.org/A5015770363")
message("Eligible universe: ", nrow(feat), " authors")

auth <- tryCatch(
  readRDS(file.path(data_dir, "polisci_authors.rds")) %>%
    select(author_id, institution),
  error = function(e) { message("(no polisci_authors.rds — skipping institutions)");
                        tibble(author_id = character(), institution = character()) })
feat <- feat %>% left_join(auth, by = "author_id")

# ---- 2. Percentile-rank each feature (same normalization as Stage D) --------
X <- feat %>%
  mutate(across(all_of(FEATURES$key), ~ coalesce(., 0))) %>%
  mutate(across(all_of(FEATURES$key),
                ~ rank(., ties.method = "average") / n()))

# ---- 3. Write feat.bin: uint8 percentiles, row-major n x p ------------------
M <- as.matrix(X[, FEATURES$key])                 # n x p, values in (0,1]
stopifnot(!anyNA(M))
bytes <- as.raw(pmin(255L, pmax(0L, as.integer(round(t(M) * 255)))))
# t(M) because R fills column-major: transposing gives row-major on disk
writeBin(bytes, file.path(out_dir, "feat.bin"))

# ---- 4. authors.json --------------------------------------------------------
clean_inst <- function(x) {
  x <- ifelse(is.na(x), "", x)
  substr(x, 1, 60)
}
write_json(
  list(names = X$author_name,
       ids   = sub("https://openalex.org/", "", X$author_id),
       inst  = clean_inst(X$institution)),
  file.path(out_dir, "authors.json"),
  auto_unbox = FALSE, na = "null")

# ---- 5. meta.json -----------------------------------------------------------
wfile <- file.path(data_dir, "k_index_weights.csv")
default_w <- if (file.exists(wfile)) {
  w <- read.csv(wfile)
  setNames(as.list(round(w$weight, 4)), w$feature)
} else {
  message("(no k_index_weights.csv yet — 'official' weights fall back to equal)")
  setNames(as.list(rep(round(1 / nrow(FEATURES), 4), nrow(FEATURES))), FEATURES$key)
}

meta <- list(
  demo     = FALSE,
  vintage  = paste("OpenAlex snapshot,", format(Sys.Date(), "%B %Y")),
  n        = nrow(X),
  universe = sprintf("%s political scientists with ≥5 works & ≥100 citations in scope (2000–2025)",
                     format(nrow(X), big.mark = ",")),
  w_max    = 0.5,
  features = purrr::pmap(FEATURES, function(key, label, blurb)
               list(key = key, label = label, blurb = blurb)),
  default_weights = default_w
)
write_json(meta, file.path(out_dir, "meta.json"), auto_unbox = TRUE, pretty = TRUE)

sz <- function(f) sprintf("%.1f MB", file.size(file.path(out_dir, f)) / 1e6)
message("Wrote to ", out_dir, ":")
message("  feat.bin     ", sz("feat.bin"))
message("  authors.json ", sz("authors.json"))
message("  meta.json    ", sz("meta.json"))
message("\nNow: git add k-index && git commit -m 'k-index: real data' && git push")
# ============================================================================
