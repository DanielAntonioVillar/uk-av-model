# UK 2024 under AV

Interactive Shiny app that re-counts the UK 2024 General Election under
Australian-style full-preferential Alternative Vote. Set preference
flows per region and per party (with voter sub-blocks for heterogeneity),
tweak regional vote shares to model swing, and see the result as a
constituency choropleth. Click any seat for a round-by-round Sankey of
the count.

Try it live at **<https://YOUR-USERNAME.shinyapps.io/uk-av-model/>**
(replace with your deployment URL once published).

## Running locally

```r
install.packages(c("shiny", "leaflet", "dplyr", "sf", "networkD3"))
shiny::runApp(".")
```

If `data/baked.rds` is present (it is in the committed repo), the app
boots with the real 2024 results and constituency boundaries. If it
isn't, the app falls back to raw `data/hcl_2024.csv` +
`data/constituencies.geojson`, then to a synthetic fixture.

## What you can edit

The sidebar has two editing modes:

**Preferences (sub-blocks):** for each region and origin party, define
one or more sub-blocks of voters with distinct rankings. Captures
within-party heterogeneity in second-preference behaviour.

**1st-preference vote shares:** for each region, edit the target vote
share per party as a percentage. The app rescales every constituency
in that region proportionally to hit your targets, while preserving
within-region variation. Lets you model swing scenarios.

Both edits are stackable. Toggle between "Actual 2024 result (FPTP)"
and "Simulated AV result" at the top of the main panel to compare.

## Constituency detail

After running an AV count, click any polygon on the map to see that
seat's round-by-round Sankey diagram: parties as nodes, transfers as
flows. Needs the `networkD3` package.

## Project layout

```
app.R                       Shiny UI + server
R/irv_engine.R              IRV count: blocks API + back-compat wrapper
R/data_loader.R             Data loading (baked.rds / csv / fixture)
R/flow_helpers.R            Default flow matrices
R/subblock_helpers.R        Sub-block construction / validation
R/vote_share_helpers.R      Regional vote-share rescaling
R/sankey_helpers.R          Sankey node/link construction
tests/testthat/             Unit tests (26 assertions)
data-raw/bake_data.R        Pre-process raw sources into data/baked.rds
data-raw/deploy.R           Publish to shinyapps.io
data/baked.rds              Pre-processed data shipped with the app
```

## Deploying to shinyapps.io

1. Sign up at <https://www.shinyapps.io> (free tier is plenty).
2. In R: `install.packages("rsconnect")`.
3. On the shinyapps dashboard, Account → Tokens → Show, copy the
   `rsconnect::setAccountInfo(...)` call, paste and run in R once.
4. From this directory, `source("data-raw/deploy.R")`. First deploy
   takes a few minutes while shinyapps builds the environment.

The deploy script only uploads the baked data, not the raw CSV or
GeoJSON, so each push is small.

## Rebuilding the baked data

If the HCL updates their dataset (see
<https://commonslibrary.parliament.uk/research-briefings/cbp-10009/>)
or ONS releases new boundaries:

1. Drop the new raw files into `data/`:
   - `data/hcl_2024.csv`
   - `data/constituencies.geojson` (or `.gpkg`)
2. Run `Rscript data-raw/bake_data.R`
3. Commit the updated `data/baked.rds` and push.

## Tests

```r
testthat::test_dir("tests/testthat", reporter = "summary")
```

## Methodology

- Full-preferential AV: unranked parties are appended alphabetically
  so no ballot ever exhausts, matching Australian House of
  Representatives rules.
- Sub-block model: each party's voters in each region are split into
  groups with distinct complete rankings. Captures the real
  heterogeneity in second-preference behaviour.
- Vote-share editing: iterative proportional rescaling to hit regional
  share targets while preserving per-constituency turnout.
- Single-member seats: Westminster constituencies kept as-is; this is
  AV, not STV.

## Data sources and attribution

- **Boundaries:** ONS Open Geography Portal, "Westminster Parliamentary
  Constituencies (July 2024) Boundaries UK BGC". Open Government Licence v3.0.
- **Results:** House of Commons Library, CBP-10009, "General election
  2024: Results and analysis". CC BY-SA 4.0.

## Licence

Code: MIT (see `LICENSE`). Bundled data retains its upstream licences.
