paquetes <- c("ggplot2", "sf", "rnaturalearth", "rnaturalearthdata",
              "geodata", "terra", "tidyterra", "elevatr", "dplyr", "ggspatial")
faltantes <- paquetes[!paquetes %in% installed.packages()[, "Package"]]
if (length(faltantes) > 0) install.packages(faltantes)


library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(geodata)
library(terra)
library(tidyterra)
library(elevatr)
library(dplyr)
library(ggspatial)

# --- 1. Directorio de trabajo ---
cat("Directorio actual:", getwd(), "\n")

# --- 2. Países (Colombia + vecinos) ---
paises <- ne_countries(
  country = c("Colombia", "Venezuela", "Ecuador", "Peru",
              "Panama", "Brazil", "Costa Rica"),
  scale = "medium", returnclass = "sf"
)
colombia <- paises[paises$name == "Colombia", ]
vecinos  <- paises[paises$name != "Colombia", ]

# --- 3. Departamentos ---
deptos <- gadm(country = "COL", level = 1, path = tempdir())
deptos <- st_as_sf(deptos)

# --- 4. Ríos ---
rios <- ne_download(scale = 50, type = "rivers_lake_centerlines",
                    category = "physical", returnclass = "sf")
bbox <- st_bbox(c(xmin = -79, xmax = -66, ymin = -4, ymax = 13),
                crs = st_crs(4326))
rios <- st_crop(rios, bbox)

# --- 5. Relieve ---
bbox_df <- data.frame(
  x = c(-79, -66, -66, -79),
  y = c(-4, -4, 13, 13)
)
elev <- get_elev_raster(locations = bbox_df, z = 6,
                        prj = "EPSG:4326", clip = "bbox")
elev <- rast(elev)
names(elev) <- "elev"

# --- 6. Cargar y depurar helechos desde CSV ---
helechos <- read.csv("Helechos_Coordenadas_JBB.csv", stringsAsFactors = FALSE)

helechos <- helechos %>%
  mutate(
    lat = decimalLatitude,
    lon = decimalLongitude,
    en_bbox = lon >= -79 & lon <= -66 & lat >= -4 & lat <= 13,
    lon_fix = lon * 10,
    corregible = !en_bbox & lon_fix >= -79 & lon_fix <= -66 &
      lat >= -4 & lat <= 13,
    lon_plot = if_else(corregible, lon_fix, lon),
    lat_plot = lat,
    estado = case_when(
      en_bbox ~ "ok",
      corregible ~ "corregido",
      TRUE ~ "excluido"
    )
  )

# Reporte en consola
cat("\n=== Resumen de registros ===\n")
print(table(helechos$estado))

cat("\nRegistros corregidos (longitud x 10):\n")
print(helechos %>% filter(estado == "corregido") %>%
        select(catalogNumber, scientificName, lat, lon, lon_plot))

cat("\nRegistros excluidos (fuera de Colombia continental):\n")
print(helechos %>% filter(estado == "excluido") %>%
        select(catalogNumber, scientificName, lat, lon))

# Objeto para el mapa
helechos_mapa <- helechos %>% filter(estado != "excluido")

# --- 7. Nombres de países vecinos ---
etiquetas_paises <- data.frame(
  pais = c("Venezuela", "Ecuador", "Perú", "Panamá", "Brasil"),
  lon  = c(-70.5, -78.2, -73.5, -77.5, -68.0),
  lat  = c(7.5, 0.3, -2.8, 8.5, 1.5)
)

# --- 8. Capitales principales ---
capitales <- data.frame(
  ciudad = c("Bogotá", "Medellín", "Cali", "Barranquilla", "Cartagena"),
  lon = c(-74.07, -75.56, -76.53, -74.80, -75.48),
  lat = c(4.71, 6.25, 3.45, 10.98, 10.39)
)

# --- 9. Mapa ---
mapa <- ggplot() +
  geom_spatraster(data = elev, aes(fill = elev)) +
  scale_fill_gradient(low = "white", high = "gray60",
                      na.value = NA, guide = "none") +
  geom_sf(data = vecinos, fill = "gray95", color = "gray50",
          linewidth = 0.3) +
  geom_sf(data = colombia, fill = NA, color = "gray50", linewidth = 0.3) +
  geom_sf(data = deptos, fill = NA, color = "gray40",
          linewidth = 0.2, linetype = "dotted") +
  geom_sf(data = rios, color = "gray30", linewidth = 0.3) +
  geom_text(data = etiquetas_paises, aes(x = lon, y = lat, label = pais),
            size = 3, fontface = "italic", color = "gray30") +
  geom_point(data = capitales, aes(x = lon, y = lat, shape = "Capital"),
             size = 1.2, color = "black") +
  geom_text(data = capitales,
            aes(x = lon - 0.20, y = lat, label = ciudad),
            size = 2, color = "black", hjust = 1) +
  geom_point(data = helechos_mapa,
             aes(x = lon_plot, y = lat_plot, shape = "Helecho"),
             size = 2.5, color = "black", alpha = 0.8) +
  scale_shape_manual(
    name = "Símbolos",
    values = c("Capital" = 16, "Helecho" = 17),
    labels = c("Capital principal", "Registros JBB")
  ) +
  coord_sf(xlim = c(-79, -66), ylim = c(-4, 13), expand = FALSE) +
  scale_x_continuous(breaks = c(-78, -75, -72, -69, -66)) +
  scale_y_continuous(breaks = c(-3, 0, 3, 6, 9, 12)) +
  annotation_north_arrow(
    location = "tr", which_north = "true",
    height = unit(1, "cm"), width = unit(0.8, "cm"),
    style = north_arrow_fancy_orienteering,
    pad_x = unit(0.25, "cm"), pad_y = unit(0.25, "cm")
  ) +
  annotation_scale(
    location = "bl", width_hint = 0.25,
    text_cex = 0.6, height = unit(0.15, "cm"),
    pad_x = unit(0.3, "cm"), pad_y = unit(0.3, "cm")
  ) +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    panel.grid = element_blank(),
    axis.text = element_text(size = 7, color = "black"),
    axis.ticks = element_line(color = "black", linewidth = 0.3),
    axis.title = element_blank(),
    legend.position = c(0.85, 0.15),
    legend.background = element_rect(fill = "white", color = "black",
                                     linewidth = 0.3),
    legend.key = element_blank(),
    legend.text = element_text(size = 7),
    legend.title = element_text(size = 8, face = "bold")
  )

# --- 10. Mostrar y guardar ---
print(mapa)

ruta <- file.path(getwd(), "mapa_helechos_jbb.png")
ggsave(ruta, plot = mapa, width = 8, height = 9, dpi = 300, bg = "white")

if (file.exists(ruta)) {
  cat("\n✔ Guardado en:", ruta, "\n")
} else {
  cat("\n✘ No se guardó. Revisa permisos o ruta.\n")
}