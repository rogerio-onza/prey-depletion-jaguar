# ==============================================================================
# Script para Reprojeção de EVI e VI_Quality (MODIS MOD13Q1)
# ==============================================================================

library(terra)

# ---------------------- CONFIGURAÇÕES -----------------------------------------
# Diretórios de entrada
dir_in_evi <- "PNI_EVI_raw/"
dir_in_viq <- "PNI_EVI_quality_bands/"

# Diretórios de saída
dir_out_evi <- "PNI_EVI_utm21s"
dir_out_viq <- "PNI_VIQ_utm21s"

# Criar diretórios de saída se não existirem
dir.create(dir_out_evi, showWarnings = FALSE, recursive = TRUE)
dir.create(dir_out_viq, showWarnings = FALSE, recursive = TRUE)

# Parâmetros de reprojeção
target_crs <- "EPSG:31981"  # UTM 21S
target_res <- 250            # resolução em metros

# Opções GDAL para compressão
gdal_opts <- c("COMPRESS=LZW", "PREDICTOR=2", "TILED=YES")

# Escalar EVI de inteiro para 0-1? (EVI original vem como int * 10000)
scale_evi_to_0_1 <- FALSE

# --------------------- FUNÇÕES AUXILIARES -------------------------------------

# Cria nome de saída mantendo base e acrescentando sufixo _utm21s
out_name <- function(infile, out_dir) {
  base <- tools::file_path_sans_ext(basename(infile))
  file.path(out_dir, paste0(base, "_utm21s.tif"))
}

# Reprojeção com método adequado por tipo
reproject_one <- function(infile, outfile, is_continuous = TRUE) {
  r <- rast(infile)
  
  # Define método de reamostragem
  method <- if (is_continuous) "bilinear" else "near"
  
  # Define tipo de dado e NAvalue
  if (is_continuous) {
    datatype <- "FLT4S"
    NAvalue <- -9999
  } else {
    datatype <- "INT2U"
    NAvalue <- 65535
  }
  
  # Escala EVI se necessário (de inteiro para 0-1)
  if (is_continuous && scale_evi_to_0_1) {
    r <- r / 10000
  }
  
  # Reprojetar
  r_out <- project(
    r,
    target_crs,
    method = method,
    res = target_res,
    filename = outfile,
    overwrite = TRUE,
    datatype = datatype,
    gdal = gdal_opts,
    NAflag = NAvalue
  )
  
  return(invisible(outfile))
}

# ------------------------- EXECUÇÃO -------------------------------------------

# Listar arquivos
f_evi <- list.files(dir_in_evi, pattern = "\\.tif$", full.names = TRUE)
f_viq <- list.files(dir_in_viq, pattern = "\\.tif$", full.names = TRUE)

if (length(f_evi) == 0) message("Aviso: nenhum EVI encontrado em: ", dir_in_evi)
if (length(f_viq) == 0) message("Aviso: nenhum VI_Quality encontrado em: ", dir_in_viq)

# Reprojetar EVI (contínuo, bilinear, float32)
if (length(f_evi) > 0) {
  cat("\n========================================\n")
  cat("Reprojetando EVI ->", target_crs, "\n")
  cat("Resolução:", target_res, "m | Método: bilinear\n")
  cat("========================================\n\n")
  
  for (i in seq_along(f_evi)) {
    infile  <- f_evi[i]
    outfile <- out_name(infile, dir_out_evi)
    cat(sprintf("[%02d/%02d] %s\n", i, length(f_evi), basename(infile)))
    
    tryCatch({
      reproject_one(infile, outfile, is_continuous = TRUE)
      cat("         ✓ Sucesso!\n\n")
    }, error = function(e) {
      message("         ✗ ERRO: ", conditionMessage(e), "\n")
    })
  }
  cat("Reprojeção de EVI concluída!\n")
}

# Reprojetar VI_Quality (categórico, nearest neighbor, uint16)
if (length(f_viq) > 0) {
  cat("\n========================================\n")
  cat("Reprojetando VI_Quality ->", target_crs, "\n")
  cat("Resolução:", target_res, "m | Método: nearest\n")
  cat("========================================\n\n")
  
  for (i in seq_along(f_viq)) {
    infile  <- f_viq[i]
    outfile <- out_name(infile, dir_out_viq)
    cat(sprintf("[%02d/%02d] %s\n", i, length(f_viq), basename(infile)))
    
    tryCatch({
      reproject_one(infile, outfile, is_continuous = FALSE)
      cat("         ✓ Sucesso!\n\n")
    }, error = function(e) {
      message("         ✗ ERRO: ", conditionMessage(e), "\n")
    })
  }
  cat("Reprojeção de VI_Quality concluída!\n")
}

cat("\n========================================\n")
cat("PROCESSAMENTO COMPLETO!\n")
cat("========================================\n")
cat("Arquivos EVI salvos em:", dir_out_evi, "\n")
cat("Arquivos VIQ salvos em:", dir_out_viq, "\n")
