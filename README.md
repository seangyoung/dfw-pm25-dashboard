# DFW PM2.5 Monitor Explorer

An interactive dashboard for exploring hourly fine-particulate-matter (PM2.5)
measurements from Texas Commission on Environmental Quality monitoring sites in
the Dallas-Fort Worth region.

The dashboard includes synchronized map and dropdown station selection, hourly
date-by-hour heatmaps, navigable 7–90 day windows, long-term patterns,
data-quality diagnostics, and exploratory event screening. It runs entirely in
the browser through Shinylive; no server-side R session receives user activity
or data.

## Event definitions

- A **short-term spike** is a maximal run of one to five consecutive hourly
  composite values strictly above 35 µg/m³.
- A **multi-day particulate event** has at least two core days with computed
  daily maximum above 25 µg/m³ and computed daily mean above 20 µg/m³. One
  adequately observed intervening trough day may be included.

These are exploratory screening definitions, not regulatory exceedance
determinations or evidence of a particular emissions source.

## Data and provenance

The versioned deployment cache contains public TCEQ monitoring data for 17
physical monitoring locations. The site-hour composite prefers regulatory
parameter 88101 and uses acceptable parameter 88502 only when no finite 88101
measurement is available. It preserves underlying report-table records,
data-quality flags, POC, report block, QA status, source filenames, station
metadata provenance, and file hashes. Timestamps are presented as local
standard time without daylight-saving shifts.

Source: [TCEQ Monthly Summary Report](https://www.tceq.texas.gov/cgi-bin/compliance/monops/monthly_summary.pl)

## Run locally with R

From the repository root:

```r
install.packages("renv")
renv::restore()
shiny::runApp("tceq_pm25_dashboard")
```

The included cache is used automatically; the original monthly CSV collection
is not required for dashboard use.

## Refresh the source data

The reproducible acquisition and preparation scripts are included. From the
repository root, the default workflow discovers both PM2.5 parameter streams,
downloads through the most recent complete month, imports the reports, and
rebuilds the local cache:

```sh
Rscript --vanilla scripts/08a_acquire_tceq_dfw_pm25_monthly.R --reuse-inventory
Rscript --vanilla scripts/08c_prepare_tceq_dfw_pm25_dashboard.R
```

Parameter 88101 remains primary at each site-hour; parameter 88502 is retained
and used only as a fallback. Raw parameter codes, POCs, report tables, QA
statements, flags, and source filenames remain available for diagnostics.

## Browser compatibility

Current Chrome or Edge is recommended. The dashboard runs R in the browser
through WebAssembly and may not initialize reliably in Safari. A startup screen
remains visible while the browser downloads and initializes the R runtime; the
first load may take 30–60 seconds.

The station map uses Esri's public World Light Gray Canvas raster tiles and
displays the provider attribution. The service does not require a CARTO API
key.

## Build the browser-only site

```r
install.packages("shinylive")
```

```sh
Rscript scripts/build_shinylive.R
Rscript --vanilla -e 'httpuv::runStaticServer("_site")'
```

The generated `_site/` directory is ignored by Git. GitHub Actions rebuilds it
from the committed source and cache.

## Publish to GitHub Pages

1. Open **Settings → Pages** and choose **GitHub Actions** as the source.
2. Push a reviewed change to `main`, or open **Actions → Deploy DFW PM2.5
   dashboard to GitHub Pages** and select **Run workflow**.

Reviewed changes pushed to `main` deploy automatically. The manual control can
rebuild the current commit without another push. No repository secret is
required.

## Validation

```sh
Rscript --vanilla scripts/run_tests.R
Rscript --vanilla scripts/build_shinylive.R --validate-only
```

The application validates the station bundles and their SHA-256 hashes
before creating a browser deployment.
