# =============================
# DISTÂNCIA EUCLIDIANA AOS MEUS PONTOS -> GeoTIFF (UTM 21S)
# =============================
library(sf)
library(terra)
library(dplyr)

# -------- CONFIG --------
buffer_path <- "PNI_shps_UTM/shp_buffer/buffer_10km.shp"      # buffer já em UTM 21S
points_source <- "csv"                            # "csv" ou "vector"

# Se CSV (lon/lat em WGS84):
csv_path <- "Planilha_Censo_2020_Esgotamento_presas_abundancia.csv"
lon_col  <- "Longitude"
lat_col  <- "Latitude"


out_tif   <- "out/dist_to_points_utm21s.tif"
res_m     <- 250
target_epsg <- 31981  # UTM 21S

# -------- 1) Buffer -> 31981 --------
buf <- st_read(buffer_path, quiet = TRUE)
if (is.na(st_crs(buf)$epsg) || st_crs(buf)$epsg != target_epsg) {
  buf <- st_transform(buf, target_epsg)
}
buf <- st_make_valid(buf)

# -------- 2) Ler pontos --------
if (points_source == "csv") {
  dat <- read.csv(csv_path, stringsAsFactors = FALSE)
  dat <- dat[!is.na(dat[[lon_col]]) & !is.na(dat[[lat_col]]), ]
  stopifnot(nrow(dat) > 0)
  pts <- st_as_sf(dat, coords = c(lon_col, lat_col), crs = 4326) |>
    st_transform(target_epsg)
} else if (points_source == "vector") {
  pts <- st_read(points_path, quiet = TRUE)
  stopifnot(!is.na(st_crs(pts)))
  if (st_crs(pts)$epsg != target_epsg) pts <- st_transform(pts, target_epsg)
} else {
  stop("points_source deve ser 'csv' ou 'vector'.")
}
stopifnot(any(grepl("POINT", unique(st_geometry_type(pts)))))

# dentro do buffer
pts_clip <- st_intersection(pts, buf |> select(geometry))
if (nrow(pts_clip) == 0) stop("Nenhum ponto dentro do buffer.")

# -------- 3) Raster UTM 21S e distância (m) --------
buf_v <- vect(buf)
ext_b <- ext(buf_v)

r <- rast(ext = ext_b, res = res_m, crs = paste0("EPSG:", target_epsg))

pts_v <- vect(pts_clip)
r_dist <- distance(r, pts_v)
r_dist <- mask(r_dist, buf_v)

summary(r_dist)
hist(r_dist, main = "Distribuição das distâncias (m)", xlab = "Distância (m)")

# Quantos pontos foram usados?
nrow(pts_clip)

# -------- 4) Salvar GeoTIFF (UTM 21S) --------
dir.create(dirname(out_tif), recursive = TRUE, showWarnings = FALSE)
writeRaster(
  r_dist, out_tif, overwrite = TRUE,
  datatype = "FLT4S",
  wopt = list(gdal = c("COMPRESS=LZW","TILED=YES","BIGTIFF=YES"))
)

# Checagem: deve imprimir EPSG:31981
print(crs(dist_r))
cat("\nOK! GeoTIFF salvo (UTM 21S):\n", normalizePath(out_tif), "\n")
