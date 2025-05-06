# ► Consigna práctica de aplicación (Parte I) ---------------------------------

library(fs)
library(tidyverse)
library(sf)

# 1. Buscar en el portal Buenos Aires Data (https://data.buenosaires.gob.ar/dataset/)
#     los data sets que contienen el trayecto de las vías y estaciones de ferrocarril.

  url_GCBA <- "https://cdn.buenosaires.gob.ar/datosabiertos/datasets"

  url_ffcc <- paste(url_GCBA,
                   "transporte-y-obras-publicas",
                   "estaciones-ferrocarril",
                   sep = "/" )

  # 1.a. Cargar datos de estaciones de ferrocarril

  # Usando csv
  FFCC_est1 <- read.csv2(paste(url_ffcc,
                               "estaciones-de-ferrocarril.csv",
                               sep = "/") ) |>
    filter(!is.na(long) | !is.na(lat)) |>
    st_as_sf(coords = c("long", "lat"), crs = 4326)

  FFCC_est2 <- read_sf(paste(url_ffcc,
                             "estaciones-de-ferrocarril.csv",
                             sep = "/"),
                       crs = 4326,
                       options = c("X_POSSIBLE_NAMES=long",
                                   "Y_POSSIBLE_NAMES=lat") ) |>
    filter(!st_is_empty(geometry) )

  # Usando geojson
  FFCC_est3 <- st_read(dsn = paste(url_ffcc,
                                   "estaciones-de-ferrocarril.geojson",
                                   sep = "/"))

  # Usando shapefile
  td <- tempdir()
  download.file(url = paste(url_ffcc,
                            "estaciones-de-ferrocarril-zip.zip",
                            sep = "/"),
                destfile = fs::path(td, "estaciones.zip") )
  unzip(zipfile = fs::path(td, "estaciones.zip"),
        exdir = td)
  FFCC_est4 <- read_sf(fs::path(td, "estaciones_ferrocarril.shp"))
  ## rm(td)    # BORRADO de la carpeta temporal

  # Selecciono uno
  FFCC_est <- FFCC_est3
  ##  rm(FFCC_est1, FFCC_est2, FFCC_est3, FFCC_est4)  # BORRADO 

  # 1.b. Cargar datos de red ferroviaria
  FFCC_red <- read_sf(paste(url_ffcc, "red-de-ferrocarril.geojson", sep = "/"))

  ##  rm(url_ffcc, url_GCBA)   # BORRADO 

# 2. Además, en el portal de Datos Argentina (https://datos.gob.ar/) buscar los
#     Radios censales del AMBA.

  radios <- st_read(here::here("data", "radios_AMBA", "Shape AMBA.shp"))

# 3. Con estos data sets homogeneizar la proyección entre ambos, eligiendo una que
#     sea adecuada para el el área de estudio (AMBA).

  # Colocamos las cartografías en CARTO

  CARTO <- list(FFCC_red = FFCC_red,
                FFCC_est = FFCC_est,
                radios = radios)   ######## ERROR ???????

  ##  rm(FFCC_red, FFCC_est, radios)   # BORRADO 

  CARTO$FFCC_red <- st_transform(CARTO$FFCC_red, 5347)
  CARTO$FFCC_est <- st_transform(CARTO$FFCC_est, st_crs(FFCC_red))
  CARTO$radios   <- st_transform(CARTO$radios,   st_crs(FFCC_red))

  CARTO <- CARTO |>
    map(st_transform, crs = 5347)
  # opción 1:
  # map(~st_transform(.x, crs = 5347) )
  # opción 2:
  # map(\(carto) st_transform(carto, crs = 5347) )

# 4. Asegurarse que las geometrías sean válidas (corregir en caso de ser necesario).

  sum(!st_is_valid(CARTO$FFCC_red))

  CARTO |>
    map_lgl(~sum(!st_is_valid(.x)) > 0)

  hacer_valida <- function(carto) {
    val <- sum(!st_is_valid(carto)) > 0
    if (val) {
      carto <- st_make_valid(carto)
      cat("Se corrigieron geometrías inválidas en el dataset!\n")
    }else{
      cat("No hay geometrías inválidas en el dataset!\n")
    }
    return(carto)
  }

  CARTO <- CARTO |> map(hacer_valida)

# 5. Obtener un "bounding box" de la zona de trabajo. ¿Cómo podemos interpretar
#     esta información?

  st_bbox(CARTO$FFCC_red)
  st_bbox(CARTO$FFCC_est)
  st_bbox(CARTO$radios)

# 6. Describir brevemente los atributos que acompañan estos datasets. Para ello
#     utilizar un gráfico hecho con ggplot que utilice al menos dos capas de
#     datos espaciales.
# 7. Pensar alguna pregunta que pueda ser interesante responder con estos datos.
#     ¿Es necesario un nuevo data set para responder esta pregunta?
