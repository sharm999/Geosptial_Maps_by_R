# ==============================
# 1) PACKAGES
# ==============================
pacman::p_load(
  remotes, geodata, tidyverse, sf, terra,
  rchelsa, biscale, cowplot, ggspatial, rnaturalearth, sf, dplyr
)
# If needed (first time):
# remotes::install_github("inSileco/rchelsa")
# remotes::install_github("chris-prener/biscale")

# ==============================
# 2) ADMIN BOUNDARIES (INDIA)
# ==============================
# --- robust India admin-1 fetcher to replace the geodata::gadm() line ---

suppressPackageStartupMessages({ library(sf); library(dplyr) })

get_india_states <- function(local_gadm_path = NULL) {
  # 1) Try rnaturalearth (Admin-1 states)
  st <- tryCatch(
    rnaturalearth::ne_states(country = "India", returnclass = "sf"),
    error = function(e) NULL
  )
  if (!is.null(st) && inherits(st, "sf") && nrow(st)) {
    st <- st %>%
      dplyr::select(name, postal, type_en, geometry) %>%
      dplyr::rename(NAME_1 = name) %>%
      sf::st_make_valid()
    return(st)
  }
  
  # 2) Fallback: try a local GADM file if you have one
  if (!is.null(local_gadm_path) && file.exists(local_gadm_path)) {
    st <- tryCatch(sf::read_sf(local_gadm_path), error = function(e) NULL)
    if (!is.null(st) && inherits(st, "sf") && nrow(st)) {
      # try to standardize a NAME_1 column
      if (!"NAME_1" %in% names(st)) {
        # guess a name column
        nm <- intersect(names(st), c("NAME_1","NAME_ENG","NAME","NAME_0","NAME_2"))
        if (length(nm)) st <- dplyr::rename(st, NAME_1 = !!sym(nm[1]))
      }
      st <- sf::st_make_valid(st)
      return(st)
    }
  }
  
  stop("Could not load India state boundaries: GADM server is down and no local file found.")
}

# ---- use it ----
main_dir <- getwd()
# If you already downloaded a GADM India-level-1 geopackage/shapefile, put its path here:
# e.g., local_gadm <- file.path(main_dir, "gadm41_IND_1.gpkg")
local_gadm <- NULL


india_states <- get_india_states(local_gadm_path = local_gadm)
india_union  <- india_states |> sf::st_union() |> sf::st_as_sf()

# proceed as before (e.g., keep WGS84)
india_states <- sf::st_transform(india_states, 4326)
india_union  <- sf::st_transform(india_union, 4326)

# ==============================
# 3) CHELSA BIOCLIM (GLOBAL)
#     bio1  = mean annual temperature (°C * 10, often)
#     bio12 = annual precipitation (mm)
# ==============================
# Download (cached) if not present
rchelsa::get_chelsea_data(categ = "clim", type = "bio", id = 1,  path = main_dir)
rchelsa::get_chelsea_data(categ = "clim", type = "bio", id = 12, path = main_dir)

# Read rasters (filenames created by rchelsa in working dir)
bio1  <- terra::rast(file.path(main_dir, "CHELSA_bio10_01.tif"))
bio12 <- terra::rast(file.path(main_dir, "CHELSA_bio10_12.tif"))

# Convert CHELSA temp scaling if needed (bio1 is often °C*10). If values look ~200,
# uncomment the next line to convert to °C:
# bio1 <- bio1 / 10

# Stack
clim <- c(bio1, bio12)
names(clim) <- c("temperature", "precipitation")

# ==============================
# 4) CROP & MASK TO INDIA
# ==============================
clim_india <- terra::crop(clim, india_union) |> terra::mask(india_union)

# (Optional) Speed-up by aggregating (bigger pixels):
# clim_india <- terra::aggregate(clim_india, fact = 2, fun = mean, na.rm = TRUE)

# To data frame for ggplot
clim_df <- as.data.frame(clim_india, xy = TRUE, na.rm = TRUE)

# ==============================
# 5) BIVARIATE CLASSIFICATION
# ==============================
# Choose palette dim=3 for 3x3 bins; "fisher" often gives good natural breaks
breaks <- biscale::bi_class(
  clim_df,
  x = temperature,
  y = precipitation,
  style = "fisher",
  dim = 3
)

# ==============================
# 6) THEME & PLOT
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

pal <- "DkViolet"  # try "DkBlue", "DkViolet", "BlueGrn", etc.

map <- ggplot() +
  # Optional light basemap (comment out if you prefer no tiles)
  # ggspatial::annotation_map_tile(type = "cartolight", zoom = 4) +
  geom_raster(data = breaks, aes(x = x, y = y, fill = bi_class)) +
  biscale::bi_scale_fill(pal = pal, dim = 3, flip_axes = TRUE, name = "bi_class") +
  # State boundaries
  geom_sf(data = india_states, fill = NA, color = "grey20", linewidth = 0.4) +
  # Country boundary (slightly thicker)
  geom_sf(data = india_union, fill = NA, color = "black", linewidth = 0.6) +
  labs(
    title = "India: Temperature–Precipitation Bivariate Map",
    subtitle = "CHELSA Bioclim (bio1: mean annual temperature; bio12: annual precipitation)",
    caption  = "Map: Basso Digital Agriculture Lab (2026) • Data: CHELSA Bioclim v2"
  ) +
  coord_sf(crs = "EPSG:4326") +
  theme_map() +
  theme(legend.position = "none")

# Legend
legend <- biscale::bi_legend(
  pal = pal, dim = 3, flip_axes = TRUE,
  xlab = "Temperature",
  ylab = "Precipitation",
  size = 8
) + theme(text = element_text(face = "bold"))

full_map <- cowplot::ggdraw() +
  cowplot::draw_plot(map,   x = 0, y = 0, width = 1, height = 1) +
  cowplot::draw_plot(legend, x = 0.04, y = 0.10, width = 0.25, height = 0.25)

print(full_map)

# ==============================
# 7) SAVE PNG
# ==============================
ggsave(
  "India_Bivariate_Climate_CHELSA_bio1_bio12.png",
  plot = full_map,
  width = 12, height = 9, dpi = 600, bg = "white"
)
