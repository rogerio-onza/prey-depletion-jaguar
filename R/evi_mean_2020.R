# Carregar pacote
library(terra)

# Método 1: Se os rasters estão em arquivos separados
# Liste todos os arquivos
arquivos_evi <- list.files(path = "PNI_EVI/PNI_EVI_utm21s/", 
                           pattern = "\\.tif$",  # ou .img, .grd, etc
                           full.names = TRUE)

# Criar um SpatRaster com todas as camadas
rasters_evi <- rast(arquivos_evi)

# Método 3: Usando app() para mais controle
evi_medio <- app(rasters_evi, fun = mean, na.rm = TRUE)

# Reescalar para valores corretos de EVI (-1 a 1)
evi_medio_corrigido <- evi_medio / 10000

# Verificar os novos valores
summary(evi_medio_corrigido)
global(evi_medio_corrigido, range, na.rm = TRUE)

# Plotar histograma para verificar distribuição
hist(evi_medio_corrigido, main = "Distribuição EVI Médio", 
     xlab = "EVI", breaks = 50)

plot(evi_medio_corrigido)

# Salvar o resultado
writeRaster(evi_medio_corrigido, "EVI_medio_2020.tif", overwrite = TRUE)
