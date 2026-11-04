# Refresh the data snapshots committed next to app.R, so a deployed app works
# even when AEMO or Geoscience Australia refuse the hosting server.
#   ga_solar_stations.csv          Geoscience Australia solar power stations
#   <AEMO NEM Generation Information>.xlsx, when AEMO allows the download
#   au_geocode_cache.csv           OpenStreetMap locations for AEMO sites that
#                                  have no Geoscience Australia match
# Run from the repo root: Rscript scripts/refresh_data.R
# Reuses the data functions in app.R (everything above "Build + cache").

src <- readLines("app.R", encoding = "UTF-8")
end <- grep("^# ---- Build \\+ cache", src)[1]
eval(parse(text = src[seq_len(end - 1)]))

changed <- character()

# Geoscience Australia
ga <- tryCatch(get_ga(), error = function(e) { message("GA failed: ", conditionMessage(e)); NULL })
if (!is.null(ga) && grepl("^https?://", attr(ga, "from"))) {
  out <- ga |> mutate(revised = format(revised, "%Y-%m-%d")) |> arrange(state, plant)
  old <- if (file.exists(GA_LOCAL)) read.csv(GA_LOCAL, colClasses = "character") else NULL
  write.csv(out, GA_LOCAL, row.names = FALSE, fileEncoding = "UTF-8")
  new <- read.csv(GA_LOCAL, colClasses = "character")
  if (!identical(old, new)) changed <- c(changed, GA_LOCAL)
  message("GA: ", nrow(out), " solar stations from ", attr(ga, "from"))
}

# AEMO NEM Generation Information
res <- tryCatch({
  url  <- find_aemo_xlsx()
  name <- utils::URLdecode(basename(sub("\\?.*$", "", url)))
  if (!grepl(AEMO_LOCAL_PATTERN, name, ignore.case = TRUE))
    name <- paste0("nem-generation-information-", format(Sys.Date(), "%Y-%m"), ".xlsx")
  if (!file.exists(name)) {
    resp <- req_perform(aemo_req(url, 300) |> req_headers(Referer = AEMO_PAGE), path = name)
    if (resp_status(resp) != 200) { unlink(name); stop("HTTP ", resp_status(resp)) }
    old <- setdiff(list.files(".", pattern = AEMO_LOCAL_PATTERN, ignore.case = TRUE), name)
    unlink(old)
    changed <- c(changed, name)
  }
  message("AEMO: ", name)
}, error = function(e) message("AEMO failed: ", conditionMessage(e)))

# Geocode AEMO sites without a GA match, so the deployed app never has to
a <- tryCatch({
  src <- get_aemo()
  if (is.null(src$path)) stop("no AEMO workbook")
  a <- match_ga(read_aemo(src$path), ga)
  a$lat <- coalesce(a$lat, a$ga_lat); a$lon <- coalesce(a$lon, a$ga_lon)
  before <- nrow(read_geocache())
  a <- geocode_missing(a)
  message("Geocode cache: ", before, " -> ", nrow(read_geocache()), " entries; ",
          sum(is.na(a$lat) & is.na(a$geo_lat)), " AEMO sites still unmapped")
  if (nrow(read_geocache()) != before) changed <- c(changed, GEOCODE_FILE)
}, error = function(e) message("Geocoding skipped: ", conditionMessage(e)))

writeLines(changed, "refresh_changed.txt")
