# =============================================================================
# Australia Solar PV Fleet: operating plants and development pipeline
# -----------------------------------------------------------------------------
# No API keys anywhere.
#
# Data (combined at startup, cached to au_solar.rds next to this file):
#   1. AEMO, NEM Generation Information (primary, published quarterly).
#      Every existing and proposed generating site in the National Electricity
#      Market (QLD, NSW incl. ACT, VIC, SA, TAS) with owner, nameplate MW,
#      status and expected full commercial use date. The app finds the newest
#      workbook on AEMO's Generation Information page and downloads it; if
#      AEMO can't be reached it reads the newest *generation*information*.xlsx
#      saved next to app.R.
#        In service / existing        -> Operating
#        In commissioning             -> Commissioning
#        Committed / Committed*       -> Committed
#        Anticipated                  -> Anticipated
#        Publicly announced / proposed-> Proposed
#   2. Geoscience Australia, Major Power Stations (ArcGIS REST, keyless,
#      CC BY 4.0). The Foundation and National Electricity Infrastructure
#      services are tried in turn, then ga_solar_stations.csv if present
#      (a snapshot written by the refresh workflow).
#        - coordinates for AEMO sites, matched by state and site name
#        - WA and NT solar plants, which AEMO's NEM file doesn't cover
#   3. OpenStreetMap Nominatim (keyless), last resort for AEMO sites with no
#      coordinates and no GA match. Results are cached in au_geocode_cache.csv
#      (commit it, so a deployed app never re-geocodes). 1 request per second,
#      per the Nominatim usage policy. options(au_pv.geocode = FALSE) skips it.
#
# Map:   OpenStreetMap tiles (dark-filtered + standard). Light use only, per
#        https://operations.osmfoundation.org/policies/tiles/
# Theme: Linear-style dark UI, matching the US Solar PV Fleet app.
#
# install.packages(c("shiny","bslib","httr2","jsonlite","readxl","dplyr","tidyr",
#                    "leaflet","plotly","DT","htmltools"))
# shiny::runApp()
# =============================================================================

library(shiny)
library(bslib)
library(httr2)
library(jsonlite)
library(readxl)
library(dplyr)
library(tidyr)
library(leaflet)
library(plotly)
library(DT)
library(htmltools)

# ---- Config -----------------------------------------------------------------

CACHE_FILE      <- "au_solar.rds"
CACHE_TTL       <- 24 * 3600
CACHE_VERSION   <- 1L   # bump when the cached structure changes
CONNECT_TIMEOUT <- 20   # seconds before giving up on a host
GEOCODE_FILE    <- "au_geocode_cache.csv"
GEOCODE_MAX     <- 600  # most new lookups per build (about 10 min at 1/s)
# options(au_pv.offline = TRUE) skips all downloads and uses local files only
# options(au_pv.geocode = FALSE) skips Nominatim (cached results still apply)

UA <- "Mozilla/5.0 (compatible; au-solar-fleet-dashboard; R/httr2)"
# AEMO's CDN answers 403 to obvious bots, so its requests look like a browser.
# Some cloud hosts are blocked outright; the app then runs on GA alone or on a
# committed copy of the workbook.
AEMO_UA <- paste("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36",
                 "(KHTML, like Gecko) Chrome/131.0.0.0 Safari/537.36")
aemo_req <- function(url, timeout = 120) {
  request(url) |>
    req_user_agent(AEMO_UA) |>
    req_headers(Accept = "text/html,application/xhtml+xml,application/vnd.openxmlformats-officedocument.spreadsheetml.sheet,*/*;q=0.8",
                `Accept-Language` = "en-AU,en;q=0.9") |>
    req_options(connecttimeout = CONNECT_TIMEOUT) |>
    req_timeout(timeout) |>
    req_error(is_error = \(r) FALSE)
}

AEMO_PAGE <- paste0("https://www.aemo.com.au/energy-systems/electricity/",
                    "national-electricity-market-nem/nem-forecasting-and-planning/",
                    "forecasting-and-planning-data/generation-information")
AEMO_LOCAL_PATTERN <- "generation[-_ ]?information.*\\.xlsx$"

# Geoscience Australia publishes the same power stations in more than one
# service, with different field names; the first one that answers is used
GA_BASE <- "https://services.ga.gov.au/gis/rest/services/"
GA_ENDPOINTS <- paste0(GA_BASE, c("Foundation_Electricity_Infrastructure/MapServer/0",
                                  "National_Electricity_Infrastructure/MapServer/1",
                                  "Electricity_Infrastructure/MapServer/1"))
GA_PAGE  <- GA_ENDPOINTS[1]
GA_LOCAL <- "ga_solar_stations.csv"   # snapshot written by the refresh workflow
NOMINATIM_URL <- "https://nominatim.openstreetmap.org/search"

STAGES <- c("Operating", "Commissioning", "Committed", "Anticipated", "Proposed")
STAGE_COLS <- c(
  "Operating"     = "#5E6AD2",  # Linear indigo
  "Commissioning" = "#4CB7A5",  # teal
  "Committed"     = "#F2994A",  # orange
  "Anticipated"   = "#F2C94C",  # yellow
  "Proposed"      = "#B59AF8"   # lavender
)
PIPELINE       <- STAGES[-1]
DEFAULT_STAGES <- c("Operating", "Commissioning", "Committed", "Anticipated")
CY             <- as.integer(format(Sys.Date(), "%Y"))

STATE_NAMES <- c(NSW = "New South Wales", VIC = "Victoria", QLD = "Queensland",
                 SA = "South Australia", TAS = "Tasmania", WA = "Western Australia",
                 NT = "Northern Territory", ACT = "Australian Capital Territory")
NEM_STATES  <- c("NSW", "VIC", "QLD", "SA", "TAS", "ACT")

# lat_min, lat_max, lon_min, lon_max, used to sanity-check coordinates
STATE_BBOX <- list(
  NSW = c(-37.6, -28.1, 140.9, 153.7), VIC = c(-39.2, -33.9, 140.9, 150.1),
  QLD = c(-29.3,  -9.0, 137.9, 153.6), SA  = c(-38.1, -25.9, 128.9, 141.1),
  TAS = c(-43.8, -39.4, 143.7, 148.6), WA  = c(-35.2, -13.6, 112.8, 129.1),
  NT  = c(-26.1, -10.8, 128.9, 138.1), ACT = c(-36.0, -35.1, 148.7, 149.5))

# ---- Small helpers ----------------------------------------------------------

clean_names <- function(x) {
  x <- iconv(as.character(x), "UTF-8", "ASCII//TRANSLIT", sub = "")
  x <- gsub("[^a-z0-9]+", "_", tolower(trimws(x)))
  gsub("^_|_$", "", x)
}

norm_txt <- function(x) {
  x <- iconv(tolower(as.character(x)), "UTF-8", "ASCII//TRANSLIT", sub = "")
  x[is.na(x)] <- ""
  trimws(gsub("\\s+", " ", gsub("[^a-z0-9]+", " ", x)))
}

# First column whose cleaned name matches any of the regexes, in order
pickr <- function(d, ...) {
  for (p in c(...)) {
    nm <- grep(p, names(d), value = TRUE, perl = TRUE)
    if (length(nm)) return(as.character(d[[nm[1]]]))
  }
  rep(NA_character_, nrow(d))
}

num <- function(x) suppressWarnings(as.numeric(gsub("[^0-9.eE+-]", "", as.character(x))))
na_dash <- function(x) ifelse(is.na(x) | !nzchar(trimws(x)), "-", x)

# Excel serials (text from readxl), ISO, d/m/Y, "Jun-2027", "Q3 2027", "2027"
date_au <- function(x) {
  x <- trimws(as.character(x)); x[is.na(x)] <- ""
  out <- as.Date(rep(NA_character_, length(x)))
  ser <- grepl("^[0-9]{5}(\\.[0-9]+)?$", x)
  out[ser] <- as.Date(floor(as.numeric(x[ser])), origin = "1899-12-30")
  iso <- is.na(out) & grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", x)
  out[iso] <- as.Date(substr(x[iso], 1, 10))
  dmy <- is.na(out) & grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}", x)
  out[dmy] <- as.Date(sub("^([0-9]{1,2}/[0-9]{1,2}/[0-9]{4}).*$", "\\1", x[dmy]), "%d/%m/%Y")
  my <- is.na(out) & grepl("^[A-Za-z]{3}[a-z]*[- ]+[0-9]{2,4}$", x)
  if (any(my)) {
    mo <- match(tolower(substr(x[my], 1, 3)), tolower(month.abb))
    yr <- as.integer(sub("^.*[- ]+([0-9]{2,4})$", "\\1", x[my]))
    yr <- ifelse(yr < 100, 2000 + yr, yr)
    out[my] <- as.Date(ifelse(is.na(mo), NA, sprintf("%d-%02d-01", yr, mo)))
  }
  q <- is.na(out) & grepl("Q[1-4].*(19|20)[0-9]{2}", x)
  if (any(q)) {
    qq <- as.integer(sub("^.*Q([1-4]).*$", "\\1", x[q]))
    yy <- as.integer(sub("^.*((19|20)[0-9]{2}).*$", "\\1", x[q]))
    out[q] <- as.Date(sprintf("%d-%02d-01", yy, qq * 3))
  }
  y <- is.na(out) & grepl("^(19|20)[0-9]{2}$", x)
  out[y] <- as.Date(paste0(x[y], "-07-01"))
  out[!is.na(out) & (out < as.Date("1980-01-01") | out > as.Date("2100-01-01"))] <- NA
  out
}

state_code <- function(x) {
  s <- toupper(trimws(as.character(x)))
  s <- sub("1$", "", s)                       # NEM region ids: NSW1, QLD1...
  full <- toupper(STATE_NAMES)
  hit <- match(s, full); s[!is.na(hit)] <- names(STATE_NAMES)[hit[!is.na(hit)]]
  s[s %in% c("NEW SOUTH WALES", "N.S.W.")] <- "NSW"
  ifelse(s %in% names(STATE_NAMES), s, NA_character_)
}

in_state <- function(lat, lon, st) {
  vapply(seq_along(lat), function(i) {
    b <- STATE_BBOX[[coalesce(st[i], "")]]
    if (is.null(b) || is.na(lat[i]) || is.na(lon[i])) return(FALSE)
    lat[i] >= b[1] && lat[i] <= b[2] && lon[i] >= b[3] && lon[i] <= b[4]
  }, TRUE)
}

# Name tokens for matching AEMO sites to GA points and for geocoding queries
STOPWORDS <- c("solar", "farm", "park", "pv", "plant", "project", "power", "station",
               "energy", "hub", "hybrid", "stage", "phase", "the", "and", "of", "sf",
               "renewable", "renewables", "generation", "facility", "photovoltaic",
               "i", "ii", "iii", "1", "2", "3", "4", "a", "b", "pty", "ltd", "limited")
toks <- function(x) lapply(strsplit(norm_txt(x), " ", fixed = TRUE),
                           \(t) setdiff(unique(t[nchar(t) > 1]), STOPWORDS))
jac <- function(a, b) if (!length(a) || !length(b)) 0 else
  length(intersect(a, b)) / length(union(a, b))

http_get <- function(url, query = list(), timeout = 180) {
  request(url) |>
    req_url_query(!!!query) |>
    req_user_agent(UA) |>
    req_options(connecttimeout = CONNECT_TIMEOUT) |>
    req_timeout(timeout) |>
    req_error(is_error = \(r) FALSE) |>
    req_perform()
}

is_offline <- function() isTRUE(getOption("au_pv.offline", FALSE))

# ---- 1. AEMO NEM Generation Information -------------------------------------

# The page lists the quarterly workbooks; take the newest NEM one
find_aemo_xlsx <- function() {
  resp <- req_perform(aemo_req(AEMO_PAGE, 60))
  if (resp_status(resp) != 200) stop("AEMO page returned HTTP ", resp_status(resp), call. = FALSE)
  html <- resp_body_string(resp)
  href <- regmatches(html, gregexpr("href=\"[^\"]+\\.xlsx[^\"]*\"", html, ignore.case = TRUE))[[1]]
  href <- gsub("&amp;", "&", gsub("^href=\"|\"$", "", href))
  href <- unique(href[grepl("generation[-_ %]*information", href, ignore.case = TRUE) &
                      !grepl("closure|wem|guideline", href, ignore.case = TRUE)])
  if (!length(href)) stop("No Generation Information workbook linked on the AEMO page", call. = FALSE)
  url <- ifelse(grepl("^https?://", href), href, paste0("https://www.aemo.com.au", href))
  # Prefer the newest month-year in the file name; else the first link (newest first)
  m <- regmatches(tolower(url), regexec("(jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec)[a-z]*[-_ %]*(20[0-9]{2})", tolower(url)))
  d <- vapply(m, \(x) if (length(x) == 3) as.numeric(as.Date(sprintf("%s-%02d-01", x[3],
                        match(x[2], tolower(month.abb))))) else NA_real_, 0)
  url[if (any(!is.na(d))) which.max(d) else 1]
}

get_aemo <- function() {
  local <- list.files(".", pattern = AEMO_LOCAL_PATTERN, ignore.case = TRUE, full.names = TRUE)
  local <- local[!grepl("^~\\$|closure", basename(local), ignore.case = TRUE)]
  local <- if (length(local)) local[which.max(file.mtime(local))] else NULL
  why <- NULL
  if (!is_offline()) {
    res <- tryCatch({
      url <- find_aemo_xlsx()
      tmp <- tempfile(fileext = ".xlsx")
      resp <- req_perform(aemo_req(url, 300) |> req_headers(Referer = AEMO_PAGE), path = tmp)
      if (resp_status(resp) != 200) stop("HTTP ", resp_status(resp), " for ", url, call. = FALSE)
      list(path = tmp, name = basename(sub("\\?.*$", "", url)), url = url, from = "download")
    }, error = function(e) { why <<- conditionMessage(e); NULL })
    if (!is.null(res)) return(res)
  }
  if (!is.null(local)) {
    message("AEMO: using local ", basename(local), if (!is.null(why)) paste0(" (", why, ")"))
    return(list(path = local, name = basename(local), url = AEMO_PAGE, from = "local file"))
  }
  warning("AEMO NEM Generation Information not available",
          if (!is.null(why)) paste0(" (", why, ")"),
          ". Running on Geoscience Australia only. To add the AEMO pipeline, download the ",
          "latest .xlsx from ", AEMO_PAGE, " and save it next to app.R ",
          "(keep 'generation information' in the name).", call. = FALSE)
  structure(list(path = NULL), why = why)
}

stage_from_aemo <- function(txt) {
  s <- norm_txt(txt)
  out <- case_when(
    grepl("withdrawn|cancel|retired|decommission|closed|mothball", s) ~ NA_character_,
    grepl("commissioning", s) & !grepl("commissioned", s)       ~ "Commissioning",
    grepl("in service|existing|operat", s)                       ~ "Operating",
    grepl("committed", s)                                        ~ "Committed",
    grepl("anticipated", s)                                      ~ "Anticipated",
    grepl("publicly announced|proposed|announced|maturing|emerging", s) ~ "Proposed",
    TRUE                                                         ~ NA_character_
  )
  factor(out, levels = STAGES)
}

# The workbook carries notes above each table: find the header row on every
# sheet and keep the sheets that look like generator lists.
read_aemo <- function(path) {
  sheets <- excel_sheets(path)
  sheets <- sheets[order(!grepl("existing|newdev|new dev", sheets, ignore.case = TRUE))]
  rows <- list()
  for (sh in sheets) {
    head30 <- suppressMessages(read_excel(path, sheet = sh, col_names = FALSE, n_max = 30,
                                          col_types = "text", .name_repair = "minimal"))
    if (!nrow(head30)) next
    hdr <- which(apply(head30, 1, \(r) {
      r <- norm_txt(r); any(grepl("^(site|station|project) name$", r)) && sum(nzchar(r)) >= 5
    }))[1]
    if (is.na(hdr)) next
    d <- suppressMessages(read_excel(path, sheet = sh, skip = hdr, col_names = FALSE,
                                     col_types = "text", .name_repair = "minimal"))
    nm <- clean_names(unlist(head30[hdr, ]))
    nm <- c(nm, rep("", max(0, ncol(d) - length(nm))))[seq_len(ncol(d))]
    nm[is.na(nm) | !nzchar(nm)] <- paste0("x", which(is.na(nm) | !nzchar(nm)))
    names(d) <- make.unique(nm)
    d$sheet_ <- sh
    rows[[sh]] <- d
  }
  if (!length(rows)) stop("No generator table (a 'Site Name' header) in ", basename(path), call. = FALSE)
  d <- bind_rows(rows)

  tech_combo <- paste(pickr(d, "^technology", "^tech"), pickr(d, "^fuel"),
                  pickr(d, "^asset_type", "^unit_type"))
  status <- paste(pickr(d, "^asset_type"), pickr(d, "^unit_status", "^status"),
                  pickr(d, "commitment_status", "^commitment"), pickr(d, "^project_status"))
  a <- tibble(
    sheet   = d$sheet_,
    region  = pickr(d, "^region"),
    plant   = trimws(pickr(d, "^site_name", "^station_name", "^project_name")),
    owner   = trimws(pickr(d, "^owner", "^participant", "^proponent", "^developer")),
    tech    = trimws(pickr(d, "^technology", "^tech")),
    tech_all = tech_combo,
    status_txt = gsub("\\s+", " ", trimws(gsub("\\bNA\\b", "", status))),
    duid    = pickr(d, "^duid"),
    units   = num(pickr(d, "number_of_units", "^units", "^no_of_units")),
    mw      = num(pickr(d, "upper_nameplate", "nameplate.*mw", "nameplate", "capacity_mw",
                        "^capacity", "_mw$")),
    cod_raw = pickr(d, "full_commercial_use", "commercial_use", "commercial_operation",
                    "expected.*(date|commission)", "commissioning_date", "^start_date"),
    closure = pickr(d, "closure"),
    lat     = num(pickr(d, "^lat", "latitude")),
    lon     = num(pickr(d, "^lon", "longitude"))
  ) |>
    mutate(tech_all = tolower(tech_all),
           is_solar = grepl("solar|photovolt|\\bpv\\b", tech_all) &
                      !grepl("thermal|concentrat|csp", tech_all),
           stage = stage_from_aemo(status_txt),
           state = state_code(region),
           cod = date_au(cod_raw)) |>
    filter(is_solar, !is.na(stage), !is.na(plant), nzchar(plant), !is.na(mw), mw > 0)
  if (!nrow(a)) stop("No solar PV rows recognised in ", basename(path), call. = FALSE)

  # Unit-level rows -> one row per site and stage
  a |>
    group_by(plant, state, stage) |>
    summarise(region = first(region), owner = first(na.omit(owner)), tech = first(tech),
              status = paste(unique(status_txt), collapse = "; "),
              duid = paste(unique(na.omit(duid)), collapse = ", "),
              units = suppressWarnings(sum(units, na.rm = TRUE)),
              mw = sum(mw), cod = suppressWarnings(max(cod, na.rm = TRUE)),
              lat = first(na.omit(lat)), lon = first(na.omit(lon)), .groups = "drop") |>
    mutate(cod = as.Date(ifelse(is.finite(cod), as.numeric(cod), NA), origin = "1970-01-01"),
           owner = coalesce(owner, "Unknown"),
           units = ifelse(units > 0, units, NA), duid = na_if(duid, ""), source = "AEMO")
}

# ---- 2. Geoscience Australia Major Power Stations ---------------------------

# Rough state from coordinates, for layers without a state field
state_from_xy <- function(lat, lon) {
  ord <- c("ACT", "TAS", "VIC", "NSW", "SA", "QLD", "NT", "WA")
  vapply(seq_along(lat), function(i) {
    for (st in ord) if (in_state(lat[i], lon[i], st)) return(st)
    NA_character_
  }, "")
}

state_from_text <- function(x) {
  x <- toupper(coalesce(as.character(x), ""))
  out <- rep(NA_character_, length(x))
  for (st in c("NSW", "VIC", "QLD", "TAS", "ACT", "WA", "NT", "SA")) {
    hit <- is.na(out) & (grepl(sprintf("\\b%s\\b", st), x) | grepl(toupper(STATE_NAMES[[st]]), x, fixed = TRUE))
    out[hit] <- st
  }
  out
}

# One request to an ArcGIS layer's /query, with the server's message on failure
ga_get <- function(url, q) {
  resp <- http_get(paste0(url, "/query"), q, timeout = 120)
  if (resp_status(resp) != 200)
    stop("HTTP ", resp_status(resp), ": ",
         substr(gsub("\\s+", " ", tryCatch(resp_body_string(resp), error = \(e) "")), 1, 120),
         call. = FALSE)
  j <- resp_body_json(resp, simplifyVector = FALSE)
  if (!is.null(j$error))
    stop("query error: ", j$error$message,
         if (length(j$error$details)) paste0(" (", paste(unlist(j$error$details), collapse = " "), ")"),
         call. = FALSE)
  j
}

# Solar features from one layer. Older services reject LIKE or paging, so fall
# back to fetching every station in one go and filtering here.
ga_query <- function(url) {
  base <- list(outFields = "*", returnGeometry = "true", outSR = 4326, f = "json")
  attempts <- list(
    list(where = "primaryfueltype LIKE '%olar%'", page = TRUE),
    list(where = "1=1", page = TRUE),
    list(where = "1=1", page = FALSE))
  feats <- NULL; errs <- character()
  for (at in attempts) {
    feats <- tryCatch({
      out <- list(); off <- 0
      repeat {
        q <- c(base, list(where = at$where))
        if (at$page) q <- c(q, list(resultOffset = off, resultRecordCount = 1000))
        j <- ga_get(url, q)
        out <- c(out, j$features)
        if (!at$page || !length(j$features) || !isTRUE(j$exceededTransferLimit)) break
        off <- off + length(j$features)
      }
      out
    }, error = function(e) { errs <<- c(errs, conditionMessage(e)); NULL })
    if (length(feats)) break
  }
  if (!length(feats)) stop(paste(unique(errs), collapse = " | "), call. = FALSE)
  attrs <- bind_rows(lapply(feats, \(f) {
    v <- lapply(f$attributes, \(x) if (is.null(x)) NA_character_ else as.character(x))
    names(v) <- tolower(names(v)); as_tibble(v)
  }))
  attrs$lon_ <- vapply(feats, \(f) if (is.null(f$geometry$x)) NA_real_ else f$geometry$x, 0)
  attrs$lat_ <- vapply(feats, \(f) if (is.null(f$geometry$y)) NA_real_ else f$geometry$y, 0)
  fuel <- tolower(paste(pickr(attrs, "^primaryfueltype$"), pickr(attrs, "^primarysubfueltype$"),
                        pickr(attrs, "^generationtype$"), pickr(attrs, "^class$")))
  attrs <- attrs[grepl("solar|photovolt", fuel), ]
  if (!nrow(attrs)) stop("no solar features", call. = FALSE)
  attrs
}

# Find power-station layers in GA's service catalogue, in case they move again
ga_discover <- function() {
  cat <- tryCatch(resp_body_json(http_get(GA_BASE, list(f = "json"), 60)), error = \(e) NULL)
  if (is.null(cat)) return(character())
  svc <- vapply(cat$services, \(x) paste0(x$name, "/", x$type), "")
  for (fo in unlist(cat$folders)) {
    sub <- tryCatch(resp_body_json(http_get(paste0(GA_BASE, fo), list(f = "json"), 60)),
                    error = \(e) NULL)
    if (!is.null(sub)) svc <- c(svc, vapply(sub$services, \(x) paste0(x$name, "/", x$type), ""))
  }
  svc <- svc[grepl("electric", svc, ignore.case = TRUE) & grepl("MapServer|FeatureServer", svc)]
  out <- character()
  for (sv in svc) {
    info <- tryCatch(resp_body_json(http_get(paste0(GA_BASE, sv), list(f = "json"), 60)),
                     error = \(e) NULL)
    for (ly in info$layers) if (grepl("power.?station", ly$name, ignore.case = TRUE))
      out <- c(out, sprintf("%s%s/%s", GA_BASE, sv, ly$id))
  }
  unique(out)
}

# Same columns whichever GA layer answered
ga_standardise <- function(d) {
  epoch <- function(x) { n <- num(x)
    as.Date(as.POSIXct(n / 1000, origin = "1970-01-01", tz = "UTC")) }
  st <- state_code(pickr(d, "^state$"))
  st <- coalesce(st, state_from_text(paste(pickr(d, "^address$"), pickr(d, "^locality$"))))
  st <- coalesce(st, state_from_xy(d$lat_, d$lon_))
  tibble(
    plant   = pickr(d, "^name$", "powerstation_name"),
    status  = pickr(d, "^operationalstatus$", "^operational_status$"),
    owner   = coalesce(na_if(pickr(d, "^owner$"), ""), pickr(d, "^operator$")),
    gentype = pickr(d, "^generationtype$", "^structuretype$"),
    subfuel = pickr(d, "^primarysubfueltype$"),
    mw      = num(pickr(d, "^generationmw$")),
    locality = pickr(d, "^locality$", "^address$"),
    state   = st,
    confidence = num(pickr(d, "^spatialconfidence$")),
    revised = epoch(pickr(d, "^revised$", "^attribute_date$", "^feature_date$")),
    lon = d$lon_, lat = d$lat_
  ) |>
    filter(!grepl("thermal|concentrat", tolower(paste(gentype, subfuel))))
}

get_ga <- function() {
  why <- character()
  urls <- GA_ENDPOINTS
  tried <- 0
  while (!is_offline() && tried < length(urls)) {
    tried <- tried + 1; u <- urls[tried]
    g <- tryCatch(ga_standardise(ga_query(u)), error = function(e) {
      why <<- c(why, sprintf("%s: %s", sub(GA_BASE, "", u, fixed = TRUE), conditionMessage(e)))
      NULL })
    if (!is.null(g) && nrow(g)) return(structure(g, from = u))
    # Fixed addresses all failed: look the layer up in GA's catalogue once
    if (tried == length(GA_ENDPOINTS)) urls <- unique(c(urls, ga_discover()))
  }
  if (file.exists(GA_LOCAL)) {
    message("GA: using local ", GA_LOCAL, if (length(why)) paste0(" (", paste(why, collapse = "; "), ")"))
    g <- read.csv(GA_LOCAL, colClasses = "character", encoding = "UTF-8") |> as_tibble() |>
      mutate(mw = num(mw), confidence = num(confidence), lat = num(lat), lon = num(lon),
             revised = as.Date(revised))
    return(structure(g, from = GA_LOCAL))
  }
  stop(paste(c("no GA service answered", why), collapse = "; "), call. = FALSE)
}

# GA points to AEMO sites: same state, clear name overlap
match_ga <- function(a, g) {
  a$ga_lat <- NA_real_; a$ga_lon <- NA_real_; a$ga_name <- NA_character_
  if (is.null(g) || !nrow(g)) return(a)
  at <- toks(a$plant); gt <- toks(g$plant)
  for (i in seq_len(nrow(a))) {
    cand <- which(g$state %in% a$state[i] & !is.na(g$lat))
    if (!length(cand) || !length(at[[i]])) next
    sc <- vapply(cand, \(j) jac(at[[i]], gt[[j]]), 0)
    if (max(sc) >= 0.5) {
      j <- cand[which.max(sc)]
      a$ga_lat[i] <- g$lat[j]; a$ga_lon[i] <- g$lon[j]; a$ga_name[i] <- g$plant[j]
    }
  }
  a
}

# ---- 3. Nominatim geocoding (cached) ----------------------------------------

read_geocache <- function() {
  if (!file.exists(GEOCODE_FILE)) return(tibble(key = character(), lat = numeric(),
                                                lon = numeric(), label = character()))
  g <- read.csv(GEOCODE_FILE, colClasses = c("character", "numeric", "numeric", "character"),
                encoding = "UTF-8")
  as_tibble(g)
}

geocode_missing <- function(a) {
  cache <- read_geocache()
  a$geo_key <- paste(norm_txt(a$plant), a$state, sep = "|")
  need <- unique(a$geo_key[is.na(a$lat) & !is.na(a$state) & !a$geo_key %in% cache$key])
  allow <- !is_offline() && !isFALSE(getOption("au_pv.geocode", TRUE))
  if (allow && length(need)) {
    need <- head(need, GEOCODE_MAX)
    message("Geocoding ", length(need), " sites with Nominatim (1 per second)...")
    new <- lapply(need, function(k) {
      i <- match(k, a$geo_key)
      core <- paste(setdiff(strsplit(norm_txt(a$plant[i]), " ")[[1]], c("pty", "ltd")), collapse = " ")
      q <- sprintf("%s, %s, Australia", core, STATE_NAMES[[a$state[i]]])
      Sys.sleep(1.05)
      hit <- tryCatch({
        r <- http_get(NOMINATIM_URL, list(q = q, format = "jsonv2", limit = 3,
                                          countrycodes = "au"), timeout = 30)
        if (resp_status(r) == 200) resp_body_json(r, simplifyVector = TRUE) else NULL
      }, error = \(e) NULL)
      if (is.null(hit) || !length(hit) || !nrow(as.data.frame(hit)))
        return(tibble(key = k, lat = NA_real_, lon = NA_real_, label = "no match"))
      h <- as.data.frame(hit)
      la <- as.numeric(h$lat); lo <- as.numeric(h$lon)
      ok <- in_state(la, lo, rep(a$state[i], length(la)))
      if (!any(ok)) return(tibble(key = k, lat = NA_real_, lon = NA_real_, label = "outside state"))
      j <- which(ok)[1]
      tibble(key = k, lat = la[j], lon = lo[j], label = substr(h$display_name[j], 1, 160))
    })
    cache <- bind_rows(cache, bind_rows(new)) |> distinct(key, .keep_all = TRUE)
    tryCatch(write.csv(cache, GEOCODE_FILE, row.names = FALSE, fileEncoding = "UTF-8"),
             error = \(e) warning("Could not save ", GEOCODE_FILE, call. = FALSE))
  }
  a |> left_join(rename(cache, geo_lat = lat, geo_lon = lon, geo_label = label),
                 by = c("geo_key" = "key"))
}

# ---- Build + cache ----------------------------------------------------------

build_data <- function() {
  src <- get_aemo()
  has_aemo <- !is.null(src$path)

  ga <- if (is_offline()) NULL else tryCatch(get_ga(), error = function(e) {
    warning("Geoscience Australia not available: ", conditionMessage(e), call. = FALSE); NULL })
  if (!has_aemo && is.null(ga))
    stop("Neither AEMO nor Geoscience Australia could be reached, and there is no cache. ",
         "Save the AEMO NEM Generation Information .xlsx from ", AEMO_PAGE,
         " next to app.R and restart.", call. = FALSE)

  a <- if (has_aemo) read_aemo(src$path) else
    tibble(plant = character(), state = character(), stage = factor(character(), STAGES),
           region = character(), owner = character(), tech = character(),
           status = character(), duid = character(), units = numeric(), mw = numeric(),
           cod = as.Date(character()), lat = numeric(), lon = numeric(), source = character())

  # Coordinates: AEMO's own if valid, else GA match, else Nominatim
  a <- match_ga(a, ga)
  own_ok <- in_state(a$lat, a$lon, a$state)
  a$lat[!own_ok] <- NA; a$lon[!own_ok] <- NA
  a$coord_src <- ifelse(own_ok, "AEMO", NA_character_)
  use_ga <- is.na(a$lat) & !is.na(a$ga_lat)
  a$lat[use_ga] <- a$ga_lat[use_ga]; a$lon[use_ga] <- a$ga_lon[use_ga]
  a$coord_src[use_ga] <- "Geoscience Australia"
  a <- geocode_missing(a)
  use_geo <- is.na(a$lat) & !is.na(a$geo_lat)
  a$lat[use_geo] <- a$geo_lat[use_geo]; a$lon[use_geo] <- a$geo_lon[use_geo]
  a$coord_src[use_geo] <- "OSM geocode (approx.)"

  # WA and NT come from GA (outside the NEM); every state does when AEMO is missing
  ga_states <- if (has_aemo) c("WA", "NT") else names(STATE_NAMES)
  extra <- if (!is.null(ga)) ga |>
    filter(state %in% ga_states, !is.na(mw), mw > 0, !is.na(lat)) |>
    mutate(stage = factor(case_when(
             grepl("^operational", tolower(status)) ~ "Operating",
             grepl("^proposed", tolower(status))    ~ "Proposed",
             TRUE ~ NA_character_), levels = STAGES)) |>
    filter(!is.na(stage)) |>
    transmute(plant, state, stage, region = state, owner = coalesce(na_if(owner, ""), "Unknown"),
              tech = "Solar PV", status, duid = NA_character_, units = NA_real_, mw,
              cod = as.Date(NA), lat, lon, source = "Geoscience Australia",
              coord_src = "Geoscience Australia") else NULL

  gens <- bind_rows(
    a |> select(plant, state, stage, region, owner, tech, status, duid, units, mw, cod,
                lat, lon, source, coord_src),
    extra) |>
    mutate(site_id = sprintf("%s_%s_%d", source, state, row_number()),
           year = as.integer(format(cod, "%Y")),
           month = as.integer(format(cod, "%m")))

  list(version = CACHE_VERSION, gens = gens,
       has_aemo = has_aemo, aemo_why = attr(src, "why"),
       aemo_file = if (has_aemo) src$name else NA_character_,
       aemo_url = if (has_aemo) src$url else AEMO_PAGE,
       aemo_from = if (has_aemo) src$from else "missing",
       ga_n = if (is.null(ga)) NA_integer_ else nrow(ga),
       ga_from = if (is.null(ga)) NA_character_ else attr(ga, "from"),
       ga_revised = if (is.null(ga)) NA else suppressWarnings(max(ga$revised, na.rm = TRUE)),
       coord_counts = table(factor(coalesce(gens$coord_src, "none"),
                                   c("AEMO", "Geoscience Australia", "OSM geocode (approx.)", "none"))),
       fetched = Sys.time())
}

# Cache logic:
#  - no usable cache -> build
#  - cache < 24 h    -> use it as is
#  - cache >= 24 h   -> rebuild; on failure keep the cache and reset its clock
load_data <- function() {
  cached <- if (file.exists(CACHE_FILE)) tryCatch(readRDS(CACHE_FILE), error = \(e) NULL)
  if (!is.null(cached) && !identical(cached$version, CACHE_VERSION)) cached <- NULL
  if (!is.null(cached)) {
    age <- difftime(Sys.time(), file.mtime(CACHE_FILE), units = "secs")
    if (age < CACHE_TTL) return(cached)
  }
  out <- tryCatch(build_data(), error = function(e) {
    if (is.null(cached)) stop(e)
    warning("Refresh failed, keeping cached data: ", conditionMessage(e), call. = FALSE)
    NULL
  })
  if (!is.null(out) && !isTRUE(out$has_aemo) && isTRUE(cached$has_aemo)) {
    warning("AEMO unavailable on refresh, keeping the cached build that has it", call. = FALSE)
    out <- NULL
  }
  if (is.null(out)) {
    Sys.setFileTime(CACHE_FILE, Sys.time())
    return(cached)
  }
  saveRDS(out, CACHE_FILE)
  out
}

# Everything the app derives from a snapshot, so a refresh can swap it whole
prep_data <- function(s) {
  # Pipeline sites whose expected date has already passed are shown as this year
  G <- s$gens |>
    mutate(year_plot = ifelse(stage != "Operating" & !is.na(year) & year < CY, CY, year))
  yrs <- G$year_plot[!is.na(G$year_plot)]
  list(
    raw          = s,
    G            = G,
    period_label = if (isTRUE(s$has_aemo)) sub("\\.xlsx$", "", s$aemo_file, ignore.case = TRUE)
                   else "GA only (AEMO unavailable)",
    yr_rng       = if (length(yrs)) range(yrs) else c(CY - 10, CY + 3),
    total_op     = sum(G$mw[G$stage == "Operating"]),
    site_max     = max(1, ceiling(max(G$mw))),
    states       = intersect(names(STATE_NAMES), unique(G$state)),
    sources      = sort(unique(G$source))
  )
}

# Prepped snapshot, memoised on build time so sessions share one copy
current_data <- local({
  key <- NULL; val <- NULL
  function() {
    s <- load_data()
    k <- format(s$fetched, "%Y-%m-%d %H:%M:%OS3")
    if (!identical(k, key)) { val <<- prep_data(s); key <<- k }
    val
  }
})
invisible(current_data())  # fail fast at startup with a clear message

# ---- Linear design tokens ---------------------------------------------------

lin <- list(
  bg = "#08090A", surface = "#0F1011", raised = "#141516",
  border = "rgba(255,255,255,0.08)", text = "#F7F8F8",
  muted = "#8A8F98", faint = "#62666D", accent = "#5E6AD2"
)

SIZE_BREAKS <- c(0, 5, 20, 100, 250, Inf)
SIZE_LABELS <- c("< 5 MW", "5 to 20", "20 to 100", "100 to 250", "> 250 MW")

# Bubble area proportional to MW (radius ~ sqrt(MW)), pixel radius
bubble_r <- function(mw) pmin(pmax(sqrt(mw) * 0.95, 2.5), 34)

theme <- bs_theme(
  version = 5, bg = lin$bg, fg = lin$text, primary = lin$accent,
  secondary = lin$raised,
  base_font    = font_google("Inter", local = FALSE),
  heading_font = font_google("Inter", local = FALSE),
  font_scale = 0.88,
  `border-radius` = "6px", `border-color` = lin$border,
  `card-bg` = lin$surface, `card-border-color` = lin$border,
  `input-bg` = lin$raised, `input-border-color` = lin$border
) |>
  bs_add_rules(sprintf("
  body { font-feature-settings:'cv11','ss01','tnum'; letter-spacing:-0.005em; }
  .navbar { background:%1$s !important; border-bottom:1px solid %4$s; }
  .brand { display:flex; align-items:center; gap:10px; font-weight:600; font-size:0.95rem; }
  .brand .logo { width:18px; height:18px; border-radius:5px;
                 background:linear-gradient(135deg,%5$s 0%%,#A5ABF2 100%%); }
  .brand .tag { color:%2$s; font-weight:400; font-size:0.78rem;
                border:1px solid %4$s; border-radius:999px; padding:1px 8px; }
  .bslib-sidebar-layout > .sidebar { border-right:1px solid %4$s !important; }
  .sec { color:%3$s; font-size:0.7rem; font-weight:500; text-transform:uppercase;
         letter-spacing:0.06em; margin:4px 0 6px; }
  .card { box-shadow:none; }
  .card-header { background:transparent; border-bottom:1px solid %4$s; font-size:0.8rem;
                 font-weight:500; color:%6$s; padding:10px 14px; }
  .card-header .sub { color:%3$s; font-weight:400; margin-left:6px; }
  .kpi { background:%7$s; border:1px solid %4$s; border-radius:8px; padding:14px 16px; height:100%%; }
  .kpi .lbl { color:%2$s; font-size:0.72rem; margin-bottom:6px; display:flex; align-items:center; gap:6px; }
  .kpi .dot { width:7px; height:7px; border-radius:50%%; display:inline-block; }
  .kpi .val { font-size:1.55rem; font-weight:600; letter-spacing:-0.02em; line-height:1.1; }
  .kpi .foot { color:%3$s; font-size:0.72rem; margin-top:6px; white-space:nowrap;
               overflow:hidden; text-overflow:ellipsis; }
  .form-label, .control-label { color:%2$s; font-size:0.75rem; font-weight:500; }
  .form-control, .selectize-input { font-size:0.82rem; }
  .selectize-input, .selectize-dropdown { background:#141516 !important; border-color:%4$s !important; }
  .selectize-input .item { background:rgba(94,106,210,0.18) !important; color:#C9CDFB !important;
                           border:0 !important; border-radius:4px !important; }
  .form-check-input { background-color:#141516; border-color:rgba(255,255,255,0.18); }
  .form-check-input:checked { background-color:%5$s; border-color:%5$s; }
  .form-check-label { font-size:0.8rem; }
  .stage-sw { display:inline-block; width:8px; height:8px; border-radius:50%%; margin-right:6px; }
  .irs--shiny .irs-bar { background:%5$s; border-color:%5$s; }
  .irs--shiny .irs-line { background:#1C1D1F; border-color:%4$s; }
  .irs--shiny .irs-handle { background:%6$s; border:0; box-shadow:0 0 0 3px rgba(94,106,210,0.35); }
  .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single { background:%5$s; font-size:0.7rem; }
  .irs--shiny .irs-min, .irs--shiny .irs-max { background:#1C1D1F; color:%2$s; font-size:0.68rem; }
  .btn-lin { background:#141516; border:1px solid %4$s; color:%6$s; font-size:0.78rem; }
  .btn-lin:hover { background:#1C1D1F; color:%6$s; }
  .meta { color:%3$s; font-size:0.72rem; line-height:1.5; }
  .meta a { color:%2$s; }
  /* OpenStreetMap standard tiles, recoloured to sit in the dark UI */
  .osm-dark { filter: invert(100%%) hue-rotate(180deg) grayscale(55%%) brightness(0.82) contrast(1.05); }
  .leaflet-container { background:%1$s; font-family:Inter, sans-serif; }
  .leaflet-popup-content-wrapper, .leaflet-popup-tip { background:#141516; color:%6$s;
       border:1px solid %4$s; box-shadow:0 8px 24px rgba(0,0,0,0.5); }
  .leaflet-popup-content { margin:12px 14px; font-size:0.78rem; line-height:1.55; }
  .pop-k { color:%2$s; }
  .pop-stage { display:inline-block; font-size:0.68rem; padding:1px 7px; border-radius:999px;
               border:1px solid currentColor; margin-bottom:4px; }
  .info.legend, .size-legend { background:rgba(15,16,17,0.92) !important; color:%6$s !important;
       border:1px solid %4$s; border-radius:6px; font-size:0.72rem; box-shadow:none; padding:8px 10px; }
  .size-legend .t { color:%2$s; margin-bottom:4px; }
  .leaflet-bar a, .leaflet-control-layers { background:#141516; color:%6$s; border-color:%4$s; }
  .leaflet-control-layers-expanded { font-size:0.75rem; }
  .leaflet-control-attribution { background:rgba(8,9,10,0.75) !important; color:%3$s; }
  .leaflet-control-attribution a { color:%2$s; }
  table.dataTable { font-size:0.78rem; }
  table.dataTable thead th { color:%2$s; font-weight:500; border-bottom:1px solid %4$s !important; }
  table.dataTable tbody td { border-top:1px solid rgba(255,255,255,0.04) !important; }
  table.dataTable tbody tr:hover > * { background:rgba(255,255,255,0.03) !important; }
  table.dataTable tbody tr.selected > * { box-shadow:inset 0 0 0 9999px rgba(94,106,210,0.22) !important;
       color:%6$s !important; }
  .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_filter label { color:%3$s; font-size:0.74rem; }
  .dt-buttons .btn { background:#141516; border:1px solid %4$s; color:%6$s; font-size:0.74rem; padding:3px 10px; }
  .page-link { background:transparent; border-color:%4$s; color:%2$s; }
  .active > .page-link { background:%5$s; border-color:%5$s; }
  ", lin$bg, lin$muted, lin$faint, lin$border, lin$accent, lin$text, lin$surface))

kpi <- function(label, color, value_id, foot_id) {
  div(class = "kpi",
      div(class = "lbl", span(class = "dot", style = paste0("background:", color)), label),
      div(class = "val", textOutput(value_id, inline = TRUE)),
      div(class = "foot", textOutput(foot_id, inline = TRUE)))
}

plotly_lin <- function(p, ...) {
  base <- list(
    paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
    font = list(family = "Inter, sans-serif", color = lin$muted, size = 11),
    margin = list(l = 8, r = 8, t = 6, b = 6),
    hoverlabel = list(bgcolor = lin$raised, bordercolor = "#2A2B2E",
                      font = list(color = lin$text, family = "Inter")),
    xaxis = list(gridcolor = "rgba(255,255,255,0.05)", zeroline = FALSE,
                 linecolor = lin$border, tickfont = list(color = lin$faint)),
    yaxis = list(gridcolor = "rgba(255,255,255,0.05)", zeroline = FALSE,
                 tickfont = list(color = lin$faint)),
    showlegend = FALSE
  )
  do.call(layout, c(list(p), modifyList(base, list(...)))) |>
    config(displayModeBar = FALSE)
}

fmt_mw <- function(x) {
  ifelse(x >= 1000, sprintf("%.1f GW", x / 1000),
         paste(formatC(x, format = "f", digits = 1, big.mark = ","), "MW"))
}

fmt_date <- function(d) ifelse(is.na(d), "-", format(d, "%b %Y"))

stage_labels <- lapply(STAGES, \(s) HTML(sprintf(
  "<span class='stage-sw' style='background:%s'></span>%s", STAGE_COLS[[s]], s)))

size_legend_html <- function() {
  refs <- c(10, 100, 500)
  r <- bubble_r(refs); h <- 2 * max(r) + 4
  circles <- paste0(sprintf(
    "<circle cx='%.1f' cy='%.1f' r='%.1f' fill='none' stroke='#8A8F98' stroke-width='1'/>
     <text x='%.1f' y='%.1f' fill='#8A8F98' font-size='10' text-anchor='middle'>%s</text>",
    max(r) + 2, h - r - 2, r, max(r) + 2, h - 2 * r - 5,
    c("10", "100", "500 MW")), collapse = "")
  sprintf("<div class='size-legend'><div class='t'>Bubble area = MW</div>
          <svg width='%d' height='%d'>%s</svg></div>", ceiling(2 * max(r) + 30), ceiling(h + 12), circles)
}

state_choices <- function(codes) {
  setNames(codes, ifelse(is.na(STATE_NAMES[codes]), codes, paste0(codes, " · ", STATE_NAMES[codes])))
}


# ---- UI ---------------------------------------------------------------------

# UI is a function so every new visitor gets controls built from the newest snapshot
ui <- function(req) {
d <- current_data()
yr_rng <- d$yr_rng; SITE_MW_MAX <- d$site_max

page_sidebar(
  theme = theme, fillable = FALSE, window_title = "Australia Solar PV Fleet",
  title = div(class = "brand", span(class = "logo"), "Australia Solar PV Fleet",
              span(class = "tag", "AEMO ·", textOutput("period_tag", inline = TRUE),
                   " · Geoscience Australia")),
  sidebar = sidebar(
    width = 290, bg = lin$bg,
    div(class = "sec", "Layers"),
    checkboxGroupInput("stages", NULL, choiceNames = stage_labels,
                       choiceValues = STAGES, selected = DEFAULT_STAGES),
    div(class = "sec", "Filters"),
    selectizeInput("state", "State", choices = state_choices(d$states), multiple = TRUE,
                   options = list(placeholder = "All states")),
    selectizeInput("source", "Source", choices = d$sources, multiple = TRUE,
                   options = list(placeholder = "AEMO (NEM) and GA (WA, NT)")),
    sliderInput("mw", "Site size (MW)", min = 0, max = SITE_MW_MAX,
                value = c(1, SITE_MW_MAX), step = 1, ticks = FALSE),
    sliderInput("yr", "Online year (actual or expected)", min = yr_rng[1], max = yr_rng[2],
                value = yr_rng, step = 1, sep = "", ticks = FALSE),
    checkboxInput("no_year", "Include sites without a date", value = TRUE),
    textInput("q", "Search site or owner", placeholder = "e.g. Neoen"),
    actionButton("reset", "Reset filters", class = "btn-lin w-100"),
    hr(style = sprintf("border-color:%s; opacity:1; margin:14px 0 10px;", lin$border)),
    div(class = "sec", "Data"),
    actionButton("refresh", "Rebuild from AEMO and GA", icon = icon("rotate"),
                 class = "btn-lin w-100"),
    div(style = "height:10px"),
    uiOutput("meta")
  ),

  layout_columns(
    col_widths = c(3, 3, 3, 3), fill = FALSE,
    kpi("Operating",               STAGE_COLS[["Operating"]],   "k_op",  "k_op_f"),
    kpi("Commissioning + committed", STAGE_COLS[["Committed"]], "k_uc",  "k_uc_f"),
    kpi("Anticipated + proposed",  STAGE_COLS[["Anticipated"]], "k_pl",  "k_pl_f"),
    kpi("Largest operating site",  "#D4D8FF",                   "k_big", "k_big_f")
  ),

  card(
    full_screen = TRUE, height = "640px",
    card_header("Sites", span(class = "sub",
      "one bubble per site and stage · filled = operating, dashed ring = pipeline")),
    card_body(padding = 0, leafletOutput("map", height = "100%"))
  ),

  card(
    full_screen = TRUE, height = "430px",
    card_header("Capacity trajectory", span(class = "sub",
      "annual additions by year online · cumulative fleet if pipeline stages complete")),
    card_body(plotlyOutput("trajectory", height = "100%"))
  ),

  layout_columns(
    col_widths = c(6, 6), fill = FALSE,
    card(height = "400px", full_screen = TRUE,
         card_header("Capacity by state", span(class = "sub", "selected stages")),
         card_body(plotlyOutput("by_state", height = "100%"))),
    card(height = "400px", full_screen = TRUE,
         card_header("Site size distribution", span(class = "sub", "count of sites by stage")),
         card_body(plotlyOutput("by_size", height = "100%")))
  ),

  card(
    full_screen = TRUE,
    card_header("Capacity by owner", span(class = "sub",
      "AEMO owner field (GA for WA, NT) · MW · respects all filters")),
    card_body(DTOutput("owners"))
  ),

  card(
    full_screen = TRUE,
    card_header("Sites", span(class = "sub", "select a row to fly to it on the map")),
    card_body(DTOutput("sites_tbl"))
  )
)
}

# ---- Server -----------------------------------------------------------------

server <- function(input, output, session) {

  D <- reactiveVal(current_data())

  output$period_tag <- renderText(D()$period_label)

  output$meta <- renderUI({
    s <- D()$raw; cc <- s$coord_counts
    div(class = "meta",
        if (isTRUE(s$has_aemo)) tagList(
          "NEM sites: AEMO, ",
          a("NEM Generation Information", href = AEMO_PAGE, target = "_blank"),
          " (", s$aemo_file, if (identical(s$aemo_from, "local file")) ", local copy", ").",
          " Solar PV rows, rolled up to one bubble per site and stage. MW is AEMO's nameplate rating.",
          " Pipeline sites with an expected date already past are shown in ", CY, ".",
          br(), br(),
          "WA and NT sites, and coordinates for NEM sites: ")
        else tagList(
          tags$b("AEMO's workbook could not be loaded"),
          if (!is.null(s$aemo_why)) paste0(" (", s$aemo_why, ")"),
          ", so every state comes from Geoscience Australia: operating and proposed sites only, ",
          "no commissioning dates or committed pipeline. Save the ",
          a("NEM Generation Information", href = AEMO_PAGE, target = "_blank"),
          " .xlsx next to app.R to add AEMO's pipeline.",
          br(), br(), "Sites: "),
        a("Geoscience Australia", href = GA_PAGE, target = "_blank"),
        ", Major Power Stations (CC BY 4.0",
        if (identical(s$ga_from, GA_LOCAL)) ", snapshot in the repo", ")",
        if (!is.na(s$ga_n)) sprintf(", %s solar points", s$ga_n),
        if (!is.na(s$ga_revised) && is.finite(s$ga_revised))
          sprintf(", revised up to %s", format(s$ga_revised, "%b %Y")), ".",
        br(), br(),
        sprintf("Locations: %d from AEMO, %d from GA name matches, %d geocoded with OpenStreetMap (approximate), %d not mapped.",
                cc[["AEMO"]], cc[["Geoscience Australia"]], cc[["OSM geocode (approx.)"]], cc[["none"]]),
        br(), br(),
        "Map data and geocoding © ",
        a("OpenStreetMap", href = "https://www.openstreetmap.org/copyright", target = "_blank"),
        " contributors.",
        br(), sprintf("Built %s", format(s$fetched, "%Y-%m-%d %H:%M")))
  })

  sync_inputs <- function(d) {
    updateSelectizeInput(session, "state", choices = state_choices(d$states),
                         selected = intersect(input$state, d$states))
    updateSelectizeInput(session, "source", choices = d$sources,
                         selected = intersect(input$source, d$sources))
    updateSliderInput(session, "mw", max = d$site_max,
                      value = c(min(input$mw[1], d$site_max), d$site_max))
    updateSliderInput(session, "yr", min = d$yr_rng[1], max = d$yr_rng[2], value = d$yr_rng)
  }

  observeEvent(input$refresh, {
    id <- showNotification("Rebuilding from AEMO and Geoscience Australia…", duration = NULL,
                           type = "message")
    on.exit(removeNotification(id), add = TRUE)
    updateActionButton(session, "refresh", label = "Rebuilding…")
    on.exit(updateActionButton(session, "refresh", label = "Rebuild from AEMO and GA"), add = TRUE)

    res <- tryCatch(build_data(), error = function(e) e)
    if (inherits(res, "error")) {
      showNotification(paste("Refresh failed:", conditionMessage(res)), type = "error",
                       duration = 8)
    } else {
      saveRDS(res, CACHE_FILE)
      new <- prep_data(res)
      D(new)
      sync_inputs(new)
      showNotification(sprintf("Updated to %s.", new$period_label), type = "message",
                       duration = 5)
    }
  })

  observeEvent(input$reset, {
    updateCheckboxGroupInput(session, "stages", selected = DEFAULT_STAGES)
    updateSelectizeInput(session, "state", selected = character(0))
    updateSelectizeInput(session, "source", selected = character(0))
    updateSliderInput(session, "mw", value = c(1, D()$site_max))
    updateSliderInput(session, "yr", value = D()$yr_rng)
    updateCheckboxInput(session, "no_year", value = TRUE)
    updateTextInput(session, "q", value = "")
  })

  q_d <- debounce(reactive(input$q), 300)

  # Site-level rows after attribute filters (all stages)
  sites_all <- reactive({
    d <- D()$G |>
      filter((is.na(year_plot) & isTRUE(input$no_year)) |
             (!is.na(year_plot) & year_plot >= input$yr[1] & year_plot <= input$yr[2]),
             mw >= input$mw[1], mw <= input$mw[2])
    if (length(input$state))  d <- filter(d, state %in% input$state)
    if (length(input$source)) d <- filter(d, source %in% input$source)
    q <- trimws(q_d())
    if (nzchar(q)) d <- filter(d, grepl(q, plant, ignore.case = TRUE) |
                                  grepl(q, owner, ignore.case = TRUE))
    d
  })

  sites <- reactive({
    sites_all() |>
      filter(stage %in% input$stages) |>
      mutate(size_class = cut(mw, SIZE_BREAKS, SIZE_LABELS, right = FALSE),
             radius = bubble_r(mw),
             online_lbl = fmt_date(cod)) |>
      arrange(desc(mw))
  })

  # KPIs (ignore the layer toggles so they always tell the full story)
  stage_sum <- function(st) {
    s <- sites_all() |> filter(stage %in% st)
    list(mw = sum(s$mw), n = nrow(s))
  }
  output$k_op   <- renderText(fmt_mw(stage_sum("Operating")$mw))
  output$k_op_f <- renderText(sprintf("%s sites · %.0f%% of AU utility PV",
    format(stage_sum("Operating")$n, big.mark = ","),
    100 * stage_sum("Operating")$mw / max(D()$total_op, 1)))
  output$k_uc   <- renderText(fmt_mw(stage_sum(c("Commissioning", "Committed"))$mw))
  output$k_uc_f <- renderText({
    s <- sites_all() |> filter(stage %in% c("Commissioning", "Committed"))
    if (!nrow(s)) return("no sites")
    yrs <- na.omit(s$year_plot)
    if (length(yrs)) sprintf("%s sites · online %s to %s", nrow(s), min(yrs), max(yrs))
    else sprintf("%s sites", nrow(s))
  })
  output$k_pl   <- renderText(fmt_mw(stage_sum(c("Anticipated", "Proposed"))$mw))
  output$k_pl_f <- renderText({
    s <- stage_sum(c("Anticipated", "Proposed")); op <- stage_sum("Operating")$mw
    sprintf("%s sites · %s of today's fleet", format(s$n, big.mark = ","),
            if (op > 0) sprintf("%.0f%%", 100 * s$mw / op) else "-")
  })
  big <- reactive(sites_all() |> filter(stage == "Operating") |>
                    slice_max(mw, n = 1, with_ties = FALSE))
  output$k_big   <- renderText(if (nrow(big())) fmt_mw(big()$mw) else "-")
  output$k_big_f <- renderText(if (nrow(big())) sprintf("%s, %s", big()$plant, big()$state) else "")

  # ---- Bubble map (OpenStreetMap) ----
  output$map <- renderLeaflet({
    osm_attr <- '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors'
    osm_url  <- "https://tile.openstreetmap.org/{z}/{x}/{y}.png"
    leaflet(options = leafletOptions(preferCanvas = TRUE)) |>
      addTiles(osm_url, attribution = osm_attr, group = "OSM dark",
               options = tileOptions(maxZoom = 19, className = "osm-dark")) |>
      addTiles(osm_url, attribution = osm_attr, group = "OSM standard",
               options = tileOptions(maxZoom = 19)) |>
      addLayersControl(baseGroups = c("OSM dark", "OSM standard"),
                       options = layersControlOptions(collapsed = TRUE)) |>
      setView(134, -27.5, zoom = 4) |>
      addLegend("bottomright", colors = unname(STAGE_COLS), labels = STAGES,
                title = "Stage", opacity = 1) |>
      addControl(html = HTML(size_legend_html()), position = "bottomleft",
                 className = "size-legend-wrap")
  })

  observe({
    d <- sites() |> filter(!is.na(lat), !is.na(lon)) |> mutate(stage = as.character(stage))
    proxy <- leafletProxy("map") |> clearGroup("sites") |> clearGroup("focus")
    if (!nrow(d)) return()
    col <- unname(STAGE_COLS[d$stage])
    is_op <- d$stage == "Operating"
    pop <- sprintf(
      "<span class='pop-stage' style='color:%s'>%s</span><br>
       <b>%s</b><br>
       <span class='pop-k'>Owner</span> %s<br>
       <span class='pop-k'>State</span> %s<br>
       <span class='pop-k'>Capacity</span> %s MW%s<br>
       <span class='pop-k'>%s</span> %s<br>
       <span class='pop-k'>Status</span> %s<br>
       <span class='pop-k'>Source</span> %s &middot; <span class='pop-k'>Location</span> %s%s",
      col, d$stage, htmlEscape(d$plant), htmlEscape(na_dash(d$owner)),
      unname(STATE_NAMES[d$state]),
      formatC(d$mw, format = "f", digits = 1, big.mark = ","),
      ifelse(is.na(d$units), "", sprintf(" &middot; %d unit%s", as.integer(d$units),
                                        ifelse(d$units == 1, "", "s"))),
      ifelse(is_op, "In service", "Expected full commercial use"), d$online_lbl,
      htmlEscape(na_dash(d$status)), d$source, htmlEscape(na_dash(d$coord_src)),
      ifelse(is.na(d$duid), "", sprintf("<br><span class='pop-k'>DUID</span> %s",
                                        htmlEscape(d$duid))))
    lab <- sprintf("%s · %s MW · %s", d$plant, formatC(d$mw, format = "f", digits = 1), d$stage)

    # Operating: solid bubbles. Pipeline: dashed rings drawn on top.
    if (any(is_op)) {
      proxy |> addCircleMarkers(
        lng = d$lon[is_op], lat = d$lat[is_op], radius = d$radius[is_op],
        group = "sites", layerId = paste0(d$site_id[is_op], "_op"),
        stroke = TRUE, weight = 0.8, color = col[is_op], opacity = 0.9,
        fillColor = col[is_op], fillOpacity = 0.45,
        popup = pop[is_op], label = lab[is_op])
    }
    if (any(!is_op)) {
      proxy |> addCircleMarkers(
        lng = d$lon[!is_op], lat = d$lat[!is_op], radius = d$radius[!is_op],
        group = "sites", layerId = paste0(d$site_id[!is_op], "_", d$stage[!is_op]),
        stroke = TRUE, weight = 1.4, dashArray = "3,3", color = col[!is_op], opacity = 0.95,
        fillColor = col[!is_op], fillOpacity = 0.10,
        popup = pop[!is_op], label = lab[!is_op])
    }
  })

  # ---- Capacity trajectory: history + pipeline ----
  output$trajectory <- renderPlotly({
    g <- sites_all() |> filter(!is.na(year_plot), stage %in% input$stages)
    shiny::validate(shiny::need(nrow(g) > 0, "No capacity with a date for the current filters."))

    yrs <- seq(min(g$year_plot), max(c(g$year_plot, CY)))
    annual <- g |>
      group_by(year_plot, stage) |> summarise(gw = sum(mw) / 1000, .groups = "drop") |>
      complete(year_plot = yrs, stage = factor(STAGES, STAGES), fill = list(gw = 0))

    p <- plot_ly()
    for (s in intersect(STAGES, input$stages)) {
      a <- filter(annual, stage == s, gw > 0)
      if (!nrow(a)) next
      p <- p |> add_bars(data = a, x = ~year_plot, y = ~gw, name = s,
                         marker = list(color = STAGE_COLS[[s]],
                                       opacity = if (s == "Operating") 1 else 0.85),
                         hovertemplate = paste0(s, " · %{x}: +%{y:.2f} GW<extra></extra>"))
    }

    op <- annual |> filter(stage == "Operating") |> arrange(year_plot) |>
      mutate(cum = cumsum(gw))
    hist_end <- max(c(g$year_plot[g$stage == "Operating"], CY - 1), na.rm = TRUE)
    base <- sum(op$gw)
    if ("Operating" %in% input$stages) {
      h <- filter(op, year_plot <= hist_end)
      p <- p |> add_lines(data = h, x = ~year_plot, y = ~cum, yaxis = "y2",
                          name = "Operating fleet with a date (cumulative)",
                          line = list(color = "#D4D8FF", width = 2),
                          hovertemplate = "%{x}: %{y:.1f} GW installed<extra></extra>")
    } else base <- 0

    fut_years <- yrs[yrs >= hist_end]
    pipe_on <- intersect(PIPELINE, input$stages)
    scen <- list(c("Commissioning", "Committed"),
                 c("Commissioning", "Committed", "Anticipated"),
                 PIPELINE)
    scen_name <- c("+ commissioning & committed", "+ anticipated", "+ proposed")
    dash <- c("dash", "dashdot", "dot")
    for (i in seq_along(scen)) {
      st <- intersect(scen[[i]], pipe_on)
      if (!length(st) || (i > 1 && setequal(st, intersect(scen[[i - 1]], pipe_on)))) next
      add <- annual |> filter(stage %in% st, year_plot >= hist_end) |>
        group_by(year_plot) |> summarise(gw = sum(gw), .groups = "drop") |>
        right_join(tibble(year_plot = fut_years), by = "year_plot") |>
        mutate(gw = coalesce(gw, 0)) |> arrange(year_plot) |>
        mutate(cum = base + cumsum(gw))
      add <- bind_rows(tibble(year_plot = hist_end, cum = base), add)
      p <- p |> add_lines(data = add, x = ~year_plot, y = ~cum, yaxis = "y2",
                          name = scen_name[i],
                          line = list(color = STAGE_COLS[[st[length(st)]]], width = 1.6,
                                      dash = dash[i]),
                          hovertemplate = paste0(scen_name[i], " · %{x}: %{y:.1f} GW<extra></extra>"))
    }

    p |> plotly_lin(
      barmode = "stack", bargap = 0.22, showlegend = TRUE,
      legend = list(orientation = "h", x = 0, y = 1.13, font = list(size = 10, color = lin$muted),
                    bgcolor = "rgba(0,0,0,0)"),
      xaxis = list(title = "", dtick = 2, tickfont = list(color = lin$faint),
                   range = c(max(min(yrs), CY - 16) - 0.5, max(yrs) + 0.5)),
      yaxis = list(title = list(text = "GW added per year", font = list(size = 10)),
                   gridcolor = "rgba(255,255,255,0.05)", tickfont = list(color = lin$faint)),
      yaxis2 = list(overlaying = "y", side = "right", showgrid = FALSE, rangemode = "tozero",
                    title = list(text = "cumulative GW", font = list(size = 10)),
                    tickfont = list(color = lin$faint)),
      shapes = list(list(type = "line", xref = "x", yref = "paper",
                         x0 = CY - 0.5, x1 = CY - 0.5, y0 = 0, y1 = 1,
                         line = list(color = "rgba(255,255,255,0.25)", width = 1, dash = "dot"))),
      annotations = list(list(x = CY - 0.5, y = 1, xref = "x", yref = "paper",
                              text = paste(CY, "→ pipeline"), showarrow = FALSE,
                              xanchor = "left", yanchor = "top", xshift = 4,
                              font = list(size = 10, color = lin$faint))),
      margin = list(l = 8, r = 44, t = 34, b = 6))
  })

  output$by_state <- renderPlotly({
    s <- sites_all() |> filter(stage %in% input$stages) |>
      group_by(state, stage) |> summarise(gw = sum(mw) / 1000, .groups = "drop")
    shiny::validate(shiny::need(nrow(s) > 0, "No sites for the current filters."))
    top <- s |> group_by(state) |> summarise(t = sum(gw)) |> arrange(t)
    s <- mutate(s, state = factor(state, top$state))
    p <- plot_ly()
    for (st in intersect(STAGES, unique(as.character(s$stage)))) {
      p <- p |> add_bars(data = filter(s, stage == st), x = ~gw, y = ~state,
                         orientation = "h", name = st,
                         marker = list(color = STAGE_COLS[[st]]),
                         text = ~unname(STATE_NAMES[as.character(state)]), textposition = "none",
                         hovertemplate = paste0(st, " · %{text}: %{x:.2f} GW<extra></extra>"))
    }
    p |> plotly_lin(barmode = "stack", bargap = 0.3,
                    xaxis = list(title = list(text = "GW", font = list(size = 10))),
                    yaxis = list(title = "", tickfont = list(color = lin$muted)))
  })

  output$by_size <- renderPlotly({
    z <- sites() |> count(size_class, stage, .drop = FALSE) |>
      filter(stage %in% input$stages)
    shiny::validate(shiny::need(sum(z$n) > 0, "No sites for the current filters."))
    p <- plot_ly()
    for (st in intersect(STAGES, input$stages)) {
      p <- p |> add_bars(data = filter(z, stage == st), x = ~size_class, y = ~n, name = st,
                         marker = list(color = STAGE_COLS[[st]]),
                         hovertemplate = paste0(st, " · %{x}: %{y:,} sites<extra></extra>"))
    }
    p |> plotly_lin(barmode = "group", bargap = 0.25, bargroupgap = 0.08,
                    xaxis = list(title = "", tickfont = list(color = lin$muted)),
                    yaxis = list(title = ""))
  })

  # ---- Capacity by owner ----
  owner_tbl <- reactive({
    sites_all() |>
      group_by(Owner = owner) |>
      summarise(
        `Operating sites`     = sum(stage == "Operating"),
        `Operating MW`        = sum(mw[stage == "Operating"]),
        `Commissioning MW`    = sum(mw[stage == "Commissioning"]),
        `Committed MW`        = sum(mw[stage == "Committed"]),
        `Anticipated MW`      = sum(mw[stage == "Anticipated"]),
        `Proposed MW`         = sum(mw[stage == "Proposed"]),
        States = n_distinct(state),
        `Largest site` = {
          o <- stage == "Operating"
          if (any(o)) sprintf("%s (%s MW)", plant[o][which.max(mw[o])],
                              formatC(max(mw[o]), format = "f", digits = 0))
          else "-"
        },
        .groups = "drop") |>
      mutate(`Total incl. pipeline` = `Operating MW` + `Commissioning MW` + `Committed MW` +
               `Anticipated MW` + `Proposed MW`,
             `Share of operating` = `Operating MW` / max(sum(`Operating MW`), 1)) |>
      relocate(`Share of operating`, .after = `Operating MW`) |>
      arrange(desc(`Operating MW`), desc(`Total incl. pipeline`))
  })

  output$owners <- renderDT({
    d <- owner_tbl()
    datatable(
      d, rownames = FALSE, selection = "none", extensions = "Buttons",
      style = "bootstrap5", class = "compact hover",
      options = list(pageLength = 15, dom = "Bftip", buttons = list("csv", "excel"),
                     order = list(list(2, "desc")),
                     columnDefs = list(list(className = "dt-right", targets = c(1:9, 11))))
    ) |>
      formatRound(c("Operating MW", "Commissioning MW", "Committed MW", "Anticipated MW",
                    "Proposed MW", "Total incl. pipeline"), digits = 0) |>
      formatPercentage("Share of operating", digits = 1) |>
      formatStyle("Operating MW",
                  background = styleColorBar(c(0, max(d$`Operating MW`, 1)),
                                             "rgba(94,106,210,0.35)"),
                  backgroundSize = "98% 70%", backgroundRepeat = "no-repeat",
                  backgroundPosition = "center") |>
      formatStyle("Committed MW",
                  background = styleColorBar(c(0, max(d$`Committed MW`, 1)),
                                             "rgba(242,153,74,0.30)"),
                  backgroundSize = "98% 70%", backgroundRepeat = "no-repeat",
                  backgroundPosition = "center")
  })

  # ---- Site table ----
  output$sites_tbl <- renderDT({
    datatable(
      sites() |> transmute(Site = plant, Stage = stage, Owner = owner, State = state,
                           MW = round(mw, 1), Units = units, Online = online_lbl,
                           Status = status, Source = source, Location = coord_src,
                           DUID = duid,
                           Map = ifelse(is.na(lat), "not mapped", "")),
      rownames = FALSE, selection = "single", style = "bootstrap5", class = "compact hover",
      extensions = "Buttons",
      options = list(pageLength = 12, dom = "Bftip", buttons = list("csv", "excel"),
                     order = list(list(4, "desc")),
                     columnDefs = list(list(className = "dt-right", targets = 4:5)))
    ) |>
      formatStyle("Stage", color = styleEqual(STAGES, unname(STAGE_COLS)))
  })

  observeEvent(input$sites_tbl_rows_selected, {
    r <- sites()[input$sites_tbl_rows_selected, ]
    if (is.na(r$lat)) {
      showNotification("This site has no location from AEMO, GA or OpenStreetMap.", duration = 4)
      return()
    }
    leafletProxy("map") |>
      flyTo(r$lon, r$lat, zoom = 12, options = list(duration = 0.8)) |>
      clearGroup("focus") |>
      addCircleMarkers(lng = r$lon, lat = r$lat, radius = r$radius + 6, group = "focus",
                       stroke = TRUE, color = "#FFFFFF", weight = 1.5, opacity = 0.9,
                       fill = FALSE)
  })
}

shinyApp(ui, server)
