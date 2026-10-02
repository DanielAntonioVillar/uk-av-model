# data/

This folder holds the app's data files. Nothing here is committed to
git by default (see `../.gitignore`) **except** `baked.rds`, which
ships with the deployed app.

## To build `baked.rds` the first time

1. Drop the raw sources here:
   - `hcl_2024.csv` — from <https://commonslibrary.parliament.uk/research-briefings/cbp-10009/>
     (download "Detailed results by constituency (csv)")
   - `constituencies.geojson` — from the ONS Open Geography Portal,
     "Westminster Parliamentary Constituencies (July 2024) Boundaries
     UK BGC"

2. Run from the project root:
   ```
   Rscript data-raw/bake_data.R
   ```

That produces `data/baked.rds`. Commit it; it's what the deployed app
loads.
