# Australia Solar PV Fleet

An R/Shiny dashboard of Australia's utility-scale solar PV fleet and development pipeline, built on open data with no API keys.

Created by Fernando ([@jfer2pi](https://github.com/jfer2pi)), with coding help from Claude (Anthropic).

It is the fourth app in a series, alongside the [US Solar PV Fleet](https://github.com/jfer2pi/eia_solar_pv), the [Mexico Solar PV Fleet](https://github.com/jfer2pi/mexico_pv) and the [Brazil Solar PV Fleet](https://github.com/jfer2pi/brazil_pv). It shares the US app's layout and Linear-style dark theme: KPI cards, a bubble map of sites by stage, the capacity trajectory, capacity by state, site size distribution, capacity by owner and a site table.

The whole app is one self-contained `app.R`. It downloads its own data, so nothing has to be saved next to it.

## Data

| Source | Used for | Access |
|---|---|---|
| [AEMO NEM Generation Information](https://www.aemo.com.au/energy-systems/electricity/national-electricity-market-nem/nem-forecasting-and-planning/forecasting-and-planning-data/generation-information) | Every existing and proposed solar PV site in the NEM (QLD, NSW incl. ACT, VIC, SA, TAS): owner, nameplate MW, status, expected full commercial use date | Quarterly .xlsx, found on AEMO's page and downloaded |
| [Geoscience Australia, Major Power Stations](https://services.ga.gov.au/gis/rest/services/National_Electricity_Infrastructure/MapServer/1) | Coordinates for AEMO sites (matched by state and name); WA and NT sites, which the NEM file doesn't cover | Keyless ArcGIS REST, CC BY 4.0 |
| [OpenStreetMap Nominatim](https://nominatim.openstreetmap.org) | Last-resort location for AEMO sites with no coordinates and no GA match | Keyless, 1 request per second, cached |

**Stages** follow AEMO's status fields: in service is Operating, then Commissioning, Committed (including Committed\*), Anticipated, and Proposed (publicly announced). Withdrawn and retired sites are dropped. For WA and NT, GA's Operational and Proposed statuses are used.

**Locations** come from AEMO's own coordinates when the workbook has them, then a Geoscience Australia name match, then Nominatim. Geocoded points are approximate and say so in the popup and the site table. Sites with no location stay in the tables, marked "not mapped".

**Caching.** The parsed result is cached to `au_solar.rds` for 24 h; the sidebar button rebuilds it. Geocoding results are saved to `au_geocode_cache.csv` so each site is looked up once. If AEMO can't be reached, the app reads the newest `*generation information*.xlsx` saved next to `app.R`.

## How it was built

1. **Choosing a source.** No single keyless Australian source has both coordinates and a pipeline the way EIA-860M does. AEMO's quarterly NEM Generation Information is the most current and authoritative list, so it is the primary source, with keyless auxiliary services filling the gaps: Geoscience Australia for coordinates and WA/NT coverage, and OpenStreetMap geocoding as a last resort. Open Electricity was ruled out because it needs an API key.
2. **Starting point.** The app reuses the US app's structure and theme, and the Brazil app's handling for unreachable hosts and local fallback files.
3. **Reading AEMO.** The workbook's sheets carry notes above each table, so the app finds the header row (the one with "Site Name") on each sheet, matches columns by pattern, keeps solar PV rows, and rolls unit-level rows up to one row per site and stage.
4. **Self-contained.** Everything lives in `app.R`, with no keys or manual downloads needed.

## Run locally

```r
install.packages(c("shiny", "bslib", "httr2", "jsonlite", "readxl", "dplyr", "tidyr",
                   "leaflet", "plotly", "DT", "htmltools"))
shiny::runApp()
```

The first build geocodes the sites that have no other location, at one per second, which can take several minutes. After that the cache makes it fast. Options:

- `options(au_pv.geocode = FALSE)` skips geocoding (cached results still apply).
- `options(au_pv.offline = TRUE)` skips all downloads and reads local files only.

## Deploy on Posit Connect Cloud

1. Run the app locally once, then commit `au_geocode_cache.csv` so the deployed app doesn't geocode again. Committing a copy of the AEMO workbook as well protects the app if AEMO blocks the server.
2. In Connect Cloud, create a new Shiny app from this repo, branch `main`, primary file `app.R`.

The *Write manifest* GitHub Action regenerates `manifest.json` on every push that changes `app.R`, an AEMO workbook or the geocode cache. To write it by hand:

```r
rsconnect::writeManifest(appFiles = c("app.R", list.files(
  pattern = "(generation.?information.*\\.xlsx|^au_geocode_cache\\.csv)$", ignore.case = TRUE)))
```

## Credits

NEM sites: AEMO, NEM Generation Information. WA and NT sites and coordinates: © Commonwealth of Australia (Geoscience Australia), CC BY 4.0. Map data and geocoding © [OpenStreetMap](https://www.openstreetmap.org/copyright) contributors, ODbL.
