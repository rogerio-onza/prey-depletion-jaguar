# =============================
# DISTÂNCIA EUCLIDIANA ÀS ESTRADAS -> GeoTIFF (UTM 21S)
# =============================
library(sf)
library(terra)
library(dplyr)

# -------- CONFIG --------
buffer_path <- "PNI_shps_UTM/shp_buffer/buffer_10km.shp"
roads_path  <- "PNI_shps_UTM/shp_buffer/roads_buffer_21s.shp"  # <-- AJUSTAR AQUI
out_tif     <- "out/dist_to_roads_utm21s.tif"
res_m       <- 250
target_epsg <- 31981  # UTM 21S

# -------- 1) Carregar buffer --------
buf <- st_read(buffer_path, quiet = TRUE)
if (is.na(st_crs(buf)$epsg) || st_crs(buf)$epsg != target_epsg) {
  buf <- st_transform(buf, target_epsg)
}
buf <- st_make_valid(buf)

# -------- 2) Carregar estradas --------
roads <- st_read(roads_path, quiet = TRUE)
cat("CRS original das estradas:", st_crs(roads)$input, "\n")

# Verificar e transformar se necessário
if (is.na(st_crs(roads))) {
  cat("ATENÇÃO: Estradas sem CRS. Assumindo UTM 21S...\n")
  st_crs(roads) <- target_epsg
} else if (st_crs(roads)$epsg != target_epsg) {
  roads <- st_transform(roads, target_epsg)
}

# Garantir geometria válida
roads <- st_make_valid(roads)

# Clipar estradas pelo buffer
roads_clip <- st_intersection(roads, buf)
cat("Estradas dentro do buffer:", nrow(roads_clip), "feições\n")

if (nrow(roads_clip) == 0) stop("Nenhuma estrada dentro do buffer!")

# -------- 3) Criar raster e calcular distâncias --------
buf_v <- vect(buf)
ext_b <- ext(buf_v)
r <- rast(ext = ext_b, res = res_m, crs = paste0("EPSG:", target_epsg))

# Converter estradas para SpatVector
roads_v <- vect(roads_clip)

# Calcular distância (em metros)
r_dist_roads <- distance(r, roads_v)
r_dist_roads <- mask(r_dist_roads, buf_v)

# -------- 4) Estatísticas --------
cat("\n=== ESTATÍSTICAS DAS DISTÂNCIAS ÀS ESTRADAS ===\n")
summary(r_dist_roads)
hist(r_dist_roads, main = "Distribuição das distâncias às estradas (m)", 
     xlab = "Distância (m)", col = "steelblue")

# -------- 5) Salvar GeoTIFF --------
dir.create(dirname(out_tif), recursive = TRUE, showWarnings = FALSE)
writeRaster(
  r_dist_roads, out_tif, overwrite = TRUE,
  datatype = "FLT4S",
  wopt = list(gdal = c("COMPRESS=LZW","TILED=YES","BIGTIFF=YES"))
)

print(crs(r_dist_roads))
cat("\nOK! GeoTIFF salvo (UTM 21S):\n", normalizePath(out_tif), "\n")

# -------- 6) Visualização --------
library(tidyterra)
library(ggplot2)

ggplot() +
  geom_spatraster(data = r_dist_roads) +
  scale_fill_viridis_c(name = "Distância (m)", 
                       option = "magma", 
                       na.value = "transparent") +
  geom_sf(data = roads_clip, color = "white", linewidth = 0.3, alpha = 0.7) +
  geom_sf(data = buf, fill = NA, color = "black", linewidth = 0.8) +
  theme_minimal() +
  labs(title = "Distância euclidiana às estradas",
       subtitle = paste("Resolução:", res_m, "m | UTM 21S"))
