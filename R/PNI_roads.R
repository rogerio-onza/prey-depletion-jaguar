# Script to extract OpenStreetMap roads within a 10 km buffer of Iguaçu National Park.

# -------------------------
# 0) Pacotes e utilidades
# -------------------------
ensure_pkgs <- function(pkgs) {
  missing <- pkgs[!(pkgs %in% rownames(utils::installed.packages()))]
  if (length(missing)) install.packages(missing)
  invisible(lapply(pkgs, require, character.only = TRUE))
}

ensure_pkgs(c("sf", "osmdata", "dplyr", "stringr", "units", "ggplot2"))

# Operador coalescência (precisa estar definido ANTES do uso)
`%||%` <- function(a, b) ifelse(is.na(a) | a == "", b, a)

# -------------------------
# 1) Parâmetros do usuário
# -------------------------
buffer_km <- 10
#out_gpkg  <- "iguacu_roads.gpkg"
include_trunk_as_primary <- TRUE

# Termos de busca (caso queira ajustar)
search_place_main   <- "Parque Nacional do Iguaçu, Brasil"
search_place_fallback <- "Foz do Iguaçu, Paraná, Brasil"

# -------------------------
# 2) Funções auxiliares
# -------------------------
pick_sirgas_utm_epsg <- function(aoi_wgs84) {
  ctr <- sf::st_coordinates(sf::st_centroid(sf::st_transform(aoi_wgs84, 4326)))
  lon <- ctr[1]
  zone <- floor((lon + 180) / 6) + 1
  epsg <- 31960 + zone  # SIRGAS 2000 / UTM
  return(epsg)
}

# getbb com fallback para garantir bbox numérico
safe_getbb <- function(place_main, place_fallback) {
  bb <- try(osmdata::getbb(place_main), silent = TRUE)
  if (inherits(bb, "try-error") || is.null(bb) || all(is.na(bb))) {
    bb <- try(osmdata::getbb(place_fallback), silent = TRUE)
  }
  if (inherits(bb, "try-error") || is.null(bb) || all(is.na(bb))) {
    stop("Não foi possível obter um bbox válido via getbb().")
  }
  bb
}

# Tenta obter o polígono do PNI usando um bbox explícito
get_pni_polygon <- function(bbox_num) {
  # 1) protected_area + protect_class=2 + name
  q1 <- osmdata::opq(bbox = bbox_num) |>
    osmdata::add_osm_feature(key = "boundary", value = "protected_area") |>
    osmdata::add_osm_feature(key = "protect_class", value = "2") |>
    osmdata::add_osm_feature(key = "name", value = "Parque Nacional do Iguaçu")
  d1 <- try(osmdata::osmdata_sf(q1), silent = TRUE)
  if (!inherits(d1, "try-error")) {
    poly <- dplyr::bind_rows(d1$osm_multipolygons, d1$osm_polygons)
    if (!is.null(poly) && nrow(poly) > 0) {
      poly <- sf::st_make_valid(sf::st_union(sf::st_as_sf(poly)))
      return(sf::st_transform(poly, 4326))
    }
  }
  
  # 2) boundary=national_park + name
  q2 <- osmdata::opq(bbox = bbox_num) |>
    osmdata::add_osm_feature(key = "boundary", value = "national_park") |>
    osmdata::add_osm_feature(key = "name", value = "Parque Nacional do Iguaçu")
  d2 <- try(osmdata::osmdata_sf(q2), silent = TRUE)
  if (!inherits(d2, "try-error")) {
    poly <- dplyr::bind_rows(d2$osm_multipolygons, d2$osm_polygons)
    if (!is.null(poly) && nrow(poly) > 0) {
      poly <- sf::st_make_valid(sf::st_union(sf::st_as_sf(poly)))
      return(sf::st_transform(poly, 4326))
    }
  }
  
  # 3) Fallback: usar o bbox retangular como polígono
  #    (menos preciso, mas resolve para continuar o fluxo)
  bb <- bbox_num
  bb_poly <- sf::st_as_sfc(sf::st_bbox(c(xmin = bb[1,1], ymin = bb[2,1],
                                         xmax = bb[1,2], ymax = bb[2,2]),
                                       crs = sf::st_crs(4326)))
  sf::st_as_sf(bb_poly)
}

# -------------------------
# 3) Obter AOI (parque + buffer)
# -------------------------
message("-> Obtendo bbox do PNI no OSM…")
bb_pni <- safe_getbb(search_place_main, search_place_fallback)

message("-> Baixando polígono do Parque Nacional do Iguaçu…")
pni <- get_pni_polygon(bb_pni)

# Buffer métrico
epsg_utm <- pick_sirgas_utm_epsg(pni)
pni_utm  <- sf::st_transform(pni, epsg_utm)
aoi_buf_utm <- sf::st_buffer(pni_utm, units::set_units(buffer_km, "km"))
aoi_buf <- sf::st_transform(aoi_buf_utm, 4326)

# -------------------------
# 4) Baixar vias OSM no buffer
# -------------------------
hw_vals <- c("motorway","trunk","primary","secondary","tertiary",
             "unclassified","residential","service","track")

message("-> Consultando vias do OSM dentro do buffer…")
q_roads <- osmdata::opq(bbox = sf::st_bbox(aoi_buf)) |>
  osmdata::add_osm_feature(key = "highway", value = hw_vals)

osm_roads <- osmdata::osmdata_sf(q_roads)
roads <- osm_roads$osm_lines
if (is.null(roads) || nrow(roads) == 0) stop("Nenhuma via retornada para a área consultada.")

# Garante interseção exata com o buffer
roads <- sf::st_make_valid(roads)
aoi_buf <- sf::st_make_valid(aoi_buf)
roads <- suppressWarnings(sf::st_intersection(roads, aoi_buf))

# Garante coluna 'surface' (alguns trechos não têm essa tag)
if (!"surface" %in% names(roads)) roads$surface <- NA_character_

# -------------------------
# 5) Classificação primary vs secondary
# -------------------------
roads <- roads |>
  dplyr::mutate(
    highway = as.character(highway),
    paved_guess = dplyr::case_when(
      stringr::str_to_lower(surface %||% "") %in%
        c("asphalt","paved","concrete","concrete:lanes","paving_stones") ~ "paved",
      stringr::str_to_lower(surface %||% "") %in%
        c("gravel","ground","dirt","compacted","fine_gravel","unpaved") ~ "unpaved",
      TRUE ~ "unknown"
    ),
    class_primary_secondary = dplyr::case_when(
      include_trunk_as_primary & highway %in% c("motorway","trunk") ~ "primary",
      !include_trunk_as_primary & highway == "motorway" ~ "primary",
      highway %in% c("primary","secondary","tertiary") ~ "secondary",
      TRUE ~ "other"
    )
  )

# -------------------------
# 6) Comprimentos por classe
# -------------------------
roads_utm <- sf::st_transform(roads, epsg_utm)
roads_utm$len_km <- units::set_units(sf::st_length(roads_utm), "km") |> units::drop_units()

summary_len <- roads_utm |>
  dplyr::group_by(class_primary_secondary) |>
  dplyr::summarise(total_km = sum(len_km, na.rm = TRUE), n = dplyr::n()) |>
  dplyr::arrange(dplyr::desc(total_km))

print(summary_len)

# =========================
# 6A) BBOX do BUFFER e camadas UNIFICADAS com road_type
# (insira este bloco depois da seção 6 e antes da 7)
# =========================

# 6A.1) BBOX que circunscreve o BUFFER
aoi_bbox_from_buffer <- sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(aoi_buf)))

# 6A.2) Função de classificação (mesma lógica usada antes)
classify_roads <- function(df) {
  if (!"surface" %in% names(df)) df$surface <- NA_character_
  df |>
    dplyr::mutate(
      highway = as.character(highway),
      paved_guess = dplyr::case_when(
        stringr::str_to_lower(surface %||% "") %in%
          c("asphalt","paved","concrete","concrete:lanes","paving_stones") ~ "paved",
        stringr::str_to_lower(surface %||% "") %in%
          c("gravel","ground","dirt","compacted","fine_gravel","unpaved") ~ "unpaved",
        TRUE ~ "unknown"
      ),
      class_primary_secondary = dplyr::case_when(
        include_trunk_as_primary & highway %in% c("motorway","trunk") ~ "primary",
        !include_trunk_as_primary & highway == "motorway" ~ "primary",
        highway %in% c("primary","secondary","tertiary") ~ "secondary",
        TRUE ~ "other"
      )
    )
}

# 6A.3) Criar road_type (1=primária, 2=secundária)
encode_type <- function(x) dplyr::case_when(
  x == "primary"   ~ 1L,
  x == "secondary" ~ 2L,
  TRUE             ~ NA_integer_
)

# 6A.4) Camada UNIFICADA recortada pelo BUFFER (roads já está intersectado e classificado)
roads_buffer_final <- roads |>
  dplyr::mutate(road_type = encode_type(class_primary_secondary)) |>
  dplyr::filter(!is.na(road_type)) |>
  dplyr::select(osm_id, name, ref, highway, road_type)

# 6A.5) Camada UNIFICADA recortada pelo BBOX do BUFFER
roads_raw <- osm_roads$osm_lines |> sf::st_make_valid()
roads_class_raw <- classify_roads(roads_raw)

roads_bbox_final <- suppressWarnings(sf::st_intersection(roads_class_raw, aoi_bbox_from_buffer)) |>
  dplyr::mutate(road_type = encode_type(class_primary_secondary)) |>
  dplyr::filter(!is.na(road_type)) |>
  dplyr::select(osm_id, name, ref, highway, road_type)

# =========================
# 7) Exportar SOMENTE em SIRGAS 2000 / UTM 21S
# =========================
target_epsg <- 31981  # SIRGAS 2000 / UTM zone 21S

# 7.1) Transformar tudo para 21S
pni_21s                <- sf::st_transform(pni, target_epsg)
aoi_buffer_21s         <- sf::st_transform(aoi_buf, target_epsg)
aoi_bbox_from_buffer_21s <- sf::st_transform(aoi_bbox_from_buffer, target_epsg)

roads_buffer_21s <- sf::st_transform(roads_buffer_final, target_epsg)
roads_bbox_21s   <- sf::st_transform(roads_bbox_final,   target_epsg)

# 7.2) Exportar GPKG (tudo em 21S)
if (file.exists(out_gpkg)) file.remove(out_gpkg)

sf::st_write(pni_21s,                   out_gpkg, "pni_boundary_21s",        delete_dsn = TRUE, quiet = TRUE)
sf::st_write(aoi_buffer_21s,            out_gpkg, "aoi_buffer_21s",                               quiet = TRUE)
sf::st_write(aoi_bbox_from_buffer_21s,  out_gpkg, "aoi_bbox_from_buffer_21s",                    quiet = TRUE)

# Camadas UNIFICADAS de estradas (com road_type = 1 primária, 2 secundária)
sf::st_write(roads_buffer_21s,          out_gpkg, "roads_buffer_21s",                            quiet = TRUE)
sf::st_write(roads_bbox_21s,            out_gpkg, "roads_bbox_21s",                              quiet = TRUE)

message("✅ GPKG (UTM 21S) exportado: ", normalizePath(out_gpkg))

# 7.3) Exportar Shapefile (21S). 'road_type' já é <=10 chars e inteiro.
dir.create("shp_export_21s", showWarnings = FALSE)

sf::st_write(pni_21s,                  "shp_export_21s/pni_boundary_21s.shp",          delete_layer = TRUE, quiet = TRUE)
sf::st_write(aoi_buffer_21s,           "shp_export_21s/aoi_buffer_21s.shp",            delete_layer = TRUE, quiet = TRUE)
sf::st_write(aoi_bbox_from_buffer_21s, "shp_export_21s/aoi_bbox_from_buffer_21s.shp",  delete_layer = TRUE, quiet = TRUE)

sf::st_write(roads_buffer_21s,         "shp_export_21s/roads_buffer_21s.shp",          delete_layer = TRUE, quiet = TRUE)
sf::st_write(roads_bbox_21s,           "shp_export_21s/roads_bbox_21s.shp",            delete_layer = TRUE, quiet = TRUE)

message("✅ SHPs (UTM 21S) exportados na pasta: ", normalizePath("shp_export_21s"))

# -------------------------
# 8) Mapa rápido (QA)
# -------------------------
ggplot() +
  geom_sf(data = dplyr::filter(roads, class_primary_secondary == "secondary"),
          linewidth = 0.3, alpha = 0.8, color = "blue") +
  geom_sf(data = dplyr::filter(roads, class_primary_secondary == "primary"),
          linewidth = 0.7, alpha = 0.9, color = "red") +
  geom_sf(data = pni, fill = NA, color = "black", linewidth = 0.6) +
  ggtitle(paste0("Vias ao redor do PNI (buffer ", buffer_km, " km)\n",
                 "Classificação: primary vs secondary")) +
  theme_minimal()


