# =============================
# DISTÂNCIA À BORDA - VERSÃO CORRIGIDA
# =============================
library(sf)
library(terra)
library(dplyr)

# -------- CONFIG --------
park_path   <- "PNI_shps_UTM/shp_buffer/pni_boundary_21s.shp"
buffer_path <- "PNI_shps_UTM/shp_buffer/buffer_10km.shp"
out_tif     <- "out/dist_to_park_edge_corrected_utm21s.tif"
res_m       <- 250
target_epsg <- 31981

# -------- 1) Carregar dados --------
park <- st_read(park_path, quiet = TRUE) %>%
  st_transform(target_epsg) %>%
  st_make_valid()

buf <- st_read(buffer_path, quiet = TRUE) %>%
  st_transform(target_epsg) %>%
  st_make_valid()

# -------- 2) Extrair BORDA como linha --------
park_boundary <- st_cast(st_boundary(park), "LINESTRING")

# -------- 3) Criar raster base --------
buf_v <- vect(buf)
park_v <- vect(park)
ext_b <- ext(buf_v)
r <- rast(ext = ext_b, res = res_m, crs = paste0("EPSG:", target_epsg))

# -------- 4) CALCULAR DISTÂNCIA ASSINADA (signed distance) --------
# Distância à linha da borda
boundary_v <- vect(park_boundary)
r_dist <- distance(r, boundary_v)

# Criar máscara: dentro do parque = 1, fora = 0
r_inside <- rasterize(park_v, r, field = 1, background = 0)

# INVERTER SINAL: dentro do parque = negativo, fora = positivo
r_dist_signed <- r_dist * (1 - 2 * r_inside)  # Dentro = -dist, Fora = +dist

# OU: Se preferir dentro = 0, fora = positivo
# r_dist_signed <- r_dist * (1 - r_inside)  # Dentro = 0, Fora = dist

# Mascarar pelo buffer
r_dist_signed <- mask(r_dist_signed, buf_v)

# -------- 5) Estatísticas --------
cat("\n=== DISTÂNCIA À BORDA (CORRIGIDA) ===\n")
summary(r_dist_signed)
hist(r_dist_signed, 
     main = "Distância à borda do parque", 
     xlab = "Distância (m) | Negativo = dentro, Positivo = fora",
     col = "darkgreen")

# -------- 6) Salvar --------
dir.create(dirname(out_tif), recursive = TRUE, showWarnings = FALSE)
writeRaster(
  r_dist_signed, out_tif, overwrite = TRUE,
  datatype = "FLT4S",
  wopt = list(gdal = c("COMPRESS=LZW","TILED=YES","BIGTIFF=YES"))
)

# -------- 7) Visualização corrigida --------
library(tidyterra)
library(ggplot2)

ggplot() +
  geom_spatraster(data = r_dist_signed) +
  scale_fill_gradient2(
    name = "Distância\nà borda (m)",
    low = "darkblue",      # Dentro do parque (negativo)
    mid = "white",         # Na borda (0)
    high = "darkred",      # Fora do parque (positivo)
    midpoint = 0,
    na.value = "transparent"
  ) +
  geom_sf(data = park, fill = NA, color = "black", linewidth = 1.2) +
  geom_sf(data = buf, fill = NA, color = "gray50", linewidth = 0.5, linetype = "dashed") +
  theme_minimal() +
  labs(title = "Distância à borda do Parque Nacional do Iguaçu",
       subtitle = "Azul = dentro | Vermelho = fora | Preto = limite do parque")
