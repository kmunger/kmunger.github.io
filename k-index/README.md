# The k-index (site)

Static, single-file leaderboard for the Munger Index / metrics-accelerationism
project. Everything runs in the visitor's browser — no server, no database.

## Files

- `index.html` — the whole site (search, weight sliders, live re-ranking).
- `data/meta.json` — vintage, feature definitions, "official" (LP-winning) weights.
- `data/authors.json` — names, OpenAlex ids, institutions of the eligible universe.
- `data/feat.bin` — uint8 binary, row-major n×12: each author's percentile
  (0–255) on each feature.
- `prep_site_data.R` — regenerates the three data files from the pipeline
  outputs in `openalex_polisci/`. Run after each new OpenAlex vintage, then push.

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
