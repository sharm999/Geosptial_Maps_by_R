# ==============================
# 1) PACKAGES
# ==============================
pacman::p_load(
  remotes, geodata, tidyverse, sf, terra,
  rchelsa, biscale, elevatr, cowplot, ggnewscale)

# ==============================
# 3) REGION: MIDWEST STATES
#    (Census-style 12 states; tweak as needed)
# ==============================
midwest_states <- c(
  "Illinois","Indiana","Iowa","Kansas","Michigan","Minnesota",
  "Missouri","Nebraska","North Dakota","Ohio","South Dakota","Wisconsin"
)

main_dir <- getwd()
usa_states <- geodata::gadm("USA", level = 1, path = main_dir) |> sf::st_as_sf()

# per-state layer (for boundaries)
midwest_states_sf <- usa_states |>
  dplyr::filter(NAME_1 %in% midwest_states) |>
  dplyr::select(NAME_1) |>
  sf::st_transform("EPSG:5070")

# union layer (for crop/mask)
midwest_union_sf <- midwest_states_sf |>
  sf::st_union() |>
  sf::st_as_sf()

midwest_sf <- usa_states |>
  dplyr::filter(NAME_1 %in% midwest_states) |>
  dplyr::select(NAME_1) |>
  sf::st_union() |>
  sf::st_as_sf()

# Use a good equal-area CRS for CONUS
CRS_CONUS <- "EPSG:5070"   # NAD83 / Conus Albers
midwest_sf <- sf::st_transform(midwest_sf, CRS_CONUS)

#############################################################################################
# Plotting the Bivariate Graph using the PRISM 4km or 800m climate data
############################################################################################
# ==============================
# 4) DOWNLOAD & BUILD PRISM CLIMATOLOGY (2008–2025, May–Oct)
#     tmean (°C), ppt (mm)
# ==============================
pacman::p_load(prism)

# 4.0 Set PRISM download directory (make a dedicated folder)
prism_dir <- file.path(main_dir, "PRISM_archive")
dir.create(prism_dir, showWarnings = FALSE, recursive = TRUE)
prism::prism_set_dl_dir(prism_dir)  # saves to option "prism.path"

yrs <- 2008:2025
mons <- 5:10  # May–Oct
res <- "4km"  # change to "800m" if you really need it (much heavier)

# 4.1 Download PRISM monthly grids
prism::get_prism_monthlys(type = "tmean", years = yrs, mon = mons, resolution = res, keepZip = FALSE)
prism::get_prism_monthlys(type = "ppt",   years = yrs, mon = mons, resolution = res, keepZip = FALSE)

# Optional: verify downloads (helps if any files are corrupt)
# prism::prism_archive_verify(type="tmean", temp_period="monthly", years=yrs, mon=mons, resolution=res)
# prism::prism_archive_verify(type="ppt",   temp_period="monthly", years=yrs, mon=mons, resolution=res)

# 4.2 Subset the PRISM archive to the exact months/years you want
tmean_pd <- prism::prism_archive_subset("tmean", "monthly", years = yrs, mon = mons, resolution = res)
ppt_pd   <- prism::prism_archive_subset("ppt",   "monthly", years = yrs, mon = mons, resolution = res)

# Convert prism-data folders ("pd") to actual raster files
tmean_files <- vapply(tmean_pd, prism::pd_to_file, FUN.VALUE = character(1))
ppt_files   <- vapply(ppt_pd,   prism::pd_to_file, FUN.VALUE = character(1))

# Read as terra SpatRaster stacks
tmean_stack <- terra::rast(tmean_files)  # °C
ppt_stack   <- terra::rast(ppt_files)    # mm

# Build grouping index (one group per year, each year has 6 months)
# Order should already be chronological if downloaded that way; safer to group via pd dates:
t_dates <- as.Date(vapply(tmean_pd, prism::pd_get_date, FUN.VALUE = character(1)))
t_years <- as.integer(format(t_dates, "%Y"))

p_dates <- as.Date(vapply(ppt_pd, prism::pd_get_date, FUN.VALUE = character(1)))
p_years <- as.integer(format(p_dates, "%Y"))

# 4.3 Compute May–Oct seasonal metrics per year, then average across years
# Temperature: mean across May–Oct within each year, then mean across 2008–2025
tmean_mayoct_by_year <- terra::tapp(tmean_stack, index = t_years, fun = mean, na.rm = TRUE)
tmean_mayoct_clim    <- terra::app(tmean_mayoct_by_year, mean, na.rm = TRUE)

# Precipitation: sum May–Oct within each year (mm/season), then mean across 2008–2025
ppt_mayoct_by_year <- terra::tapp(ppt_stack, index = p_years, fun = sum, na.rm = TRUE)
ppt_mayoct_clim    <- terra::app(ppt_mayoct_by_year, mean, na.rm = TRUE)

# If you prefer "mean monthly precip during May–Oct" instead of seasonal total:
# ppt_mayoct_clim <- ppt_mayoct_clim / length(mons)

clim_prism <- c(tmean_mayoct_clim, ppt_mayoct_clim)
names(clim_prism) <- c("temperature", "precipitation")

# 4.4 Project to your CONUS CRS and crop/mask to Midwest union
clim_proj <- terra::project(clim_prism, CRS_CONUS, method = "bilinear")
clim_midwest <- terra::crop(clim_proj, midwest_union_sf) |>
  terra::mask(midwest_union_sf)

# (Optional speed-up)
# clim_midwest <- terra::aggregate(clim_midwest, fact = 2, fun = mean, na.rm = TRUE)

clim_df <- as.data.frame(clim_midwest, xy = TRUE, na.rm = TRUE)

# ==============================
# 5) BIVARIATE CLASSIFICATION
# ==============================
breaks <- biscale::bi_class(
  clim_df,
  x = temperature,
  y = precipitation,
  style = "fisher",
  dim = 3
)
# ==============================
# 7) PLOT
# ==============================
theme_map <- function(){
  theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text  = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(color = "grey10", hjust = .5, face = "bold"),
      plot.subtitle = element_text(hjust = .5),
      plot.caption = element_text(size = 9, color = "grey20", hjust = .5),
      plot.margin = unit(c(0.8, 0.8, 0.8, 0.8), "lines")
    )
}

pal <- "DkBlue"   # try "DkBlue", "DkViolet", "BlueGrn", etc.

map <- ggplot() +
  geom_raster(data = breaks, aes(x = x, y = y, fill = bi_class)) +
  biscale::bi_scale_fill(pal = pal, dim = 3, flip_axes = TRUE, name = "bi_class") +
  # Overlay the Midwest boundary
  geom_sf(data = midwest_states_sf, fill = NA, color = "grey25", linewidth = 0.35) +
  # optional: a thicker outer boundary of the whole Midwest region
  geom_sf(data = midwest_union_sf, fill = NA, color = "black", linewidth = 0.6) +
  labs(
    title = "U.S. Midwest: Temperature–Precipitation Bivariate Map",
    subtitle = "PRISM (May–Oct mean temperature °C) × (May–Oct total precipitation mm), 2008–2025",
    caption  = "Map: Dr. Mukta Sharma (Baaso Lab), MSU (2026) • Climate: PRISM, Oregon State University • CRS: EPSG:5070"
  ) +
  coord_sf(crs = "EPSG:5070") +
  theme_map() +
  theme(legend.position = "none")

# Legend
legend <- biscale::bi_legend(
  pal = pal, dim = 3, flip_axes = TRUE,
  xlab = "Temperature (°C)",
  ylab = "Precipitation (mm)",
  size = 8
)
legend <- legend + theme(text = element_text(face = "bold"))

full_map <- cowplot::ggdraw() +
  cowplot::draw_plot(map, x = 0, y = 0, width = 1, height = 1) +
  cowplot::draw_plot(legend, x = -0.03, y = 0.12, width = 0.25, height = 0.25)

print(full_map)

# ==============================
# 8) SAVE
# ==============================
ggsave(
  "Midwest_Bivariate_Climate_Map_with_PRISM_2008_2025_MayOct.png",
  plot   = full_map,
  width  = 14, height = 9, dpi = 600, bg = "white"
)