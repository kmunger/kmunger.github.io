# The k-index (site)

Static, single-file leaderboard for the k-index. One fixed ranking; visitors
look themselves up. Everything runs in the visitor's browser — no server.

## Files

- `index.html` — the whole site (search, leaderboard, per-author profile card).
- `data/meta.json` — vintage, feature definitions, the fixed weights.
- `data/authors.json` — names, OpenAlex ids, institutions, scores — PRE-SORTED
  by rank (rank = array position + 1).
- `data/feat.bin` — uint8 binary, row-major n×p, same rank order: each author's
  percentile (0–255) on each feature, for the profile card.
- `prep_site_data.R` — regenerates the three data files from
  `k_index_features_full.rds` + `k_index_weights_full.csv`. Run after each new
  OpenAlex vintage, then push.

## Current status

The data currently in `data/` is **synthetic demo data** (4,000 simulated
scholars) so the site works while the real harvest runs. Swap in the real
thing by running `prep_site_data.R` — no HTML changes needed.

## Publishing

This folder lives inside kmunger.github.io, so:

```
git add k-index
git commit -m "k-index"
git push
```

and it's live at `<your domain>/k-index/`.
