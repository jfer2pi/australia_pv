# Australia Solar PV Fleet

An R/Shiny dashboard of Australia's utility-scale solar PV fleet and development pipeline, built on open data with no API keys.

Created by Fernando ([@jfer2pi](https://github.com/jfer2pi)), with coding help from Claude (Anthropic).

It is the fourth app in a series, alongside the [US Solar PV Fleet](https://github.com/jfer2pi/eia_solar_pv), the [Mexico Solar PV Fleet](https://github.com/jfer2pi/mexico_pv) and the [Brazil Solar PV Fleet](https://github.com/jfer2pi/brazil_pv). It shares the US app's layout and Linear-style dark theme: KPI cards, a bubble map of sites by stage, the capacity trajectory, capacity by state, site size distribution, capacity by owner and a site table.

The whole app is one self-contained `app.R`. It downloads its own data, so nothing has to be saved next to it.

## Data

| Source | Used for | Access |
|---|---|---|
| [AEMO NEM Generation Information](https://www.aemo.com.au/energy-systems/electricity/national-electricity-market-nem/nem-forecasting-and-planning/forecasting-and-planning-data/generation-information) | Every existing and proposed solar PV site in the NEM (QLD, NSW incl. ACT, VIC, SA, TAS): owner, nameplate MW, status, expected full commercial use date | Quarterly .xlsx, found on AEMO's page and downloaded |
| [Geoscience Australia, Major Power Stations](https://services.ga.gov.au/gis/rest/services/Foundation_Electricity_Infrastructure/MapServer/0) | Coordinates for AEMO sites (matched by state and name); WA and NT sites, which the NEM file doesn't cover | Keyless ArcGIS REST, CC BY 4.0 |
| [OpenStreetMap Nominatim](https://nominatim.openstreetmap.org) | Last-resort location for AEMO sites with no coordinates and no GA match | Keyless, 1 request per second, cached |

**Stages** follow AEMO's status fields: in service is Operating, then Commissioning, Committed (including Committed\*), Anticipated, and Proposed (publicly announced). Withdrawn and retired sites are dropped. For WA and NT, GA's Operational and Proposed statuses are used.

**Locations** come from AEMO's own coordinates when the workbook has them, then a Geoscience Australia name match, then Nominatim. Geocoded points are approximate and say so in the popup and the site table. Sites with no location stay in the tables, marked "not mapped".

**When the live sources refuse the server.** On the first Posit Connect Cloud deploys, AEMO answered HTTP 403 and Geoscience Australia's National service answered 404. The app now:

- sends browser-like headers to AEMO;
- tries GA's Foundation service, then the National and older services;
- falls back to snapshots committed in this repo: `ga_solar_stations.csv` and the AEMO workbook;
- if AEMO is still missing, runs on Geoscience Australia alone for every state (operating and proposed sites, no committed pipeline or dates) and says so in the sidebar.

The *Refresh data* GitHub Action tries to write those snapshots on the 3rd of every month, or on demand from the Actions tab, and rewrites `manifest.json`. In practice, in November 2026 both sources refused GitHub's servers too: AEMO with 403, and GA's current services with a site-wide 404, while GA's older service had no solar plants. So the reliable route is to download both files from a normal connection and commit them next to `app.R`:

- the AEMO NEM Generation Information `.xlsx`, keeping "generation information" in the name;
- Geoscience Australia's Major Power Stations as CSV from its [ArcGIS Hub page](https://amsis-geoscience-au.hub.arcgis.com/datasets/digitalatlas::major-power-stations-1), keeping "power stations" in the name.

If no data at all can be loaded, the app shows a page explaining this instead of failing to start.

`Major_Power_Stations.csv` in this repo is GA's November 2026 export (430 stations, 64 of them solar, attributes sourced from AEMO in January 2026). It uses GA's newer column layout (`feature_name`, `status`, `generation_capacity_mw`, `latitude`, `longitude`), which the app reads alongside the older REST field names. Note that AEMO's page also lists a "Generating Unit Expected Closure Year" workbook: that one only holds retirement years and is not the file the app needs.

`NEM Generation Information Oct 2026.xlsx` is AEMO's October 2026 release. Its "Generator Information" sheet lists 366 solar PV units at 356 sites: 12.1 GW in service, 1.2 GW in commissioning, 2.9 GW committed (including Committed\*), 6.0 GW anticipated and 56.2 GW publicly announced (AC). Checking it against the app showed that owners sit under "Site Owner", unit counts under "Unit Count", and that AC and DC capacity are separate columns; the app reads AC. Most in-service units have no Full Commercial Use Date, so the trajectory chart's operating history is thin, while the pipeline years are well populated.

Only about 64 sites can take coordinates from Geoscience Australia, so the *Refresh data* workflow also geocodes the rest with OpenStreetMap and commits `au_geocode_cache.csv`, so the deployed app never geocodes.

**Caching.** The parsed result is cached to `au_solar.rds` for 24 h; the sidebar button rebuilds it. Geocoding results are saved to `au_geocode_cache.csv` so each site is looked up once. If AEMO can't be reached, the app reads the newest `*generation information*.xlsx` saved next to `app.R`.

## How it was built

1. **Choosing a source.** No single keyless Australian source has both coordinates and a pipeline the way EIA-860M does. AEMO's quarterly NEM Generation Information is the most current and authoritative list, so it is the primary source, with keyless auxiliary services filling the gaps: Geoscience Australia for coordinates and WA/NT coverage, and OpenStreetMap geocoding as a last resort. Open Electricity was ruled out because it needs an API key.
2. **Starting point.** The app reuses the US app's structure and theme, and the Brazil app's handling for unreachable hosts and local fallback files.
3. **Reading AEMO.** The workbook's sheets carry notes above each table, so the app finds the header row (the one with "Site Name") on each sheet, matches columns by pattern, keeps solar PV rows, and rolls unit-level rows up to one row per site and stage.
4. **Self-contained.** Everything lives in `app.R`, with no keys or manual downloads needed.
5. **First deploys.** Posit Connect Cloud got HTTP 403 from AEMO and the app failed to start. AEMO requests now send browser-like headers, and when the workbook still can't be had the app falls back to Geoscience Australia for every state instead of stopping. A rebuild that loses AEMO never overwrites a cached build that had it. GA's National service then answered 404 as well, so the app now tries several GA services, searches GA's catalogue if they move, and reads committed copies of the data. Running the refresh workflow showed that GitHub's servers are refused by both sources too, which settled it: the data files are downloaded by hand and committed, as with the Brazil app.

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
