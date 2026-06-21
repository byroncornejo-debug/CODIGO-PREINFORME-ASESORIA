library(MASS)
library(lubridate)
library(dplyr)
library(stringr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(ineq)
library(tidyr)
library(readr)

# ==============================================================================
# 1. CARGA, UNIFICACIÓN Y PROCESAMIENTO DE DATOS
# ==============================================================================

chile_raw <- read_csv("C:/Users/byron/OneDrive/Desktop/asesoria1/Chile00-25.csv") %>% mutate(Pais = "Chile")
japon_raw <- read_csv("C:/Users/byron/OneDrive/Desktop/asesoria1/Japon00-25.csv") %>% mutate(Pais = "Japón")

datos <- bind_rows(chile_raw, japon_raw) %>%
  filter(
    (Pais == "Chile" & str_detect(place, "Chile") & !str_detect(place, "Argentina")) |
      (Pais == "Japón" & str_detect(place, "Japan")),
    !(magType %in% c("m", "ml"))
  ) %>%
  mutate(
    # Variable tricotomizada de profundidad
    depth_cat = case_when(
      depth < 70 ~ "Poco profunda",
      depth >= 70 & depth < 300 ~ "Intermedia",
      depth >= 300 & depth <= 700 ~ "Profunda",
      TRUE ~ NA_character_
    ),
    # Magnitudes homogéneas (transMw)
    transMw = case_when(
      magType %in% c("mw", "mwb", "mwc", "mwr", "mww") ~ mag,
      magType == "ms" & mag >= 3.0 & mag <= 6.1 ~ 0.67 * mag + 2.07,
      magType == "ms" & mag >= 6.2 & mag <= 8.2 ~ 0.99 * mag + 0.08,
      magType == "mb" & mag >= 3.5 & mag <= 6.7 ~ 0.85 * mag + 1.03,
      TRUE ~ NA_real_
    ),
    # Categorización de magnitud Mw_cat
    Mw_cat = factor(case_when(
      transMw >= 3 & transMw < 4 ~ "Minor",
      transMw >= 4 & transMw < 5 ~ "Light",
      transMw >= 5 & transMw < 6 ~ "Moderate",
      transMw >= 6 & transMw < 7 ~ "Strong",
      transMw >= 7 & transMw < 8 ~ "Major",
      transMw >= 8              ~ "Great",
      TRUE ~ NA_character_
    ), levels = c("Minor", "Light", "Moderate", "Strong", "Major", "Great")),
    year = year(time)
  )

# Verificación inicial de tipos de magnitud
with(datos, table(Pais, magType))

# ==============================================================================
# 2. CONFIGURACIÓN ESTÉTICA GLOBAL
# ==============================================================================

paleta_pais  <- c("Chile" = "#F8766D", "Japón" = "#619CFF")
paleta_mw    <- c("Minor" = "#D6EAF8", "Light" = "#AED6F1", "Moderate" = "#5DADE2", "Strong" = "#2874A6", "Major" = "#154360", "Great" = "red")
paleta_depth <- c("Poco profunda" = "#1f78b4", "Intermedia" = "#ff7f00", "Profunda" = "#e31a1c")

tema_base <- theme_minimal() +
  theme(
    plot.title   = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title   = element_text(size = 16, face = "bold"),
    axis.text    = element_text(size = 14),
    legend.title = element_text(size = 16, face = "bold"),
    legend.text  = element_text(size = 14)
  )

# ==============================================================================
# 3. MAPAS GEOREFERENCIADOS 
# ==============================================================================

world <- ne_countries(scale = "medium", returnclass = "sf")

# Colores fijos de profundidad
paleta_depth <- c(
  "Poco profunda" = "#1f78b4",
  "Intermedia" = "#ff7f00",
  "Profunda" = "#e31a1c"
)

generar_mapa <- function(pais_nombre, xlims, ylims, r_size, forma_base, titulo) {
  
  # Creamos una paleta de formas dinámica para este mapa:
  # Todas las categorías comunes usan la forma del país, pero "Great" usa el Rombo (23)
  paleta_formas <- c(
    "Moderate" = 21,
    "Strong"   = 22,
    "Major"    = 24,
    "Great"    = 23  # Rombo para sismos excepcionales
  )
  
  ggplot() +
    geom_sf(data = world, fill = "grey90", color = "grey40", linewidth = 0.2) +
    
    # Mapeamos 'fill' a profundidad y 'shape' a la categoría de magnitud
    geom_point(data = datos %>% filter(Pais == pais_nombre, transMw >= 5),
               aes(x = longitude, y = latitude, size = transMw, fill = depth_cat, shape = Mw_cat),
               color = "black", stroke = 0.3, alpha = 0.8) +
    
    coord_sf(xlim = xlims, ylim = ylims, expand = FALSE) +
    
    # Ocultamos la leyenda de tamaño (name = NULL, guide = "none")
    scale_size_continuous(name = NULL, range = r_size, guide = "none") + 
    scale_fill_manual(values = paleta_depth, name = "Profundidad") +
    
    # Activamos la leyenda de formas mostrando las figuras utilizadas
    scale_shape_manual(values = paleta_formas, name = "Categoría Mag", drop = TRUE) +
    
    labs(title = titulo, x = "Longitud", y = "Latitud") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 16, face = "bold"),
      axis.text  = element_text(size = 10),
      legend.title = element_text(size = 19, face = "bold"),
      legend.text  = element_text(size = 17),
      panel.grid.major = element_line(colour = "grey80", linewidth = 0.3)
    ) +
    
    # Forzamos a que el cuadro de Profundidad se previsualice de forma limpia en la leyenda
    guides(
      fill  = guide_legend(override.aes = list(shape = forma_base, size = 5)),
      shape = guide_legend(override.aes = list(size = 4, fill = "grey70"))
    )
}

# Invocación de los mapas:
# Chile: Base de cuadrados (22). Si hay un sismo >= 8, aparecerá como Rombo (23)
generar_mapa("Chile", c(-80, -65), c(-55, -17), c(1, 5), forma_base = 22, "Sismicidad de Chile (Mw ≥ 5, 2000-2025)")

# Japón: Base de triángulos (24). Si hay un sismo >= 8, aparecerá como Rombo (23)
generar_mapa("Japón", c(120, 148), c(22, 50), c(1, 8), forma_base = 24, "Sismicidad de Japón (Mw ≥ 5, 2000-2025)")
# ==============================================================================
# 4. ESTADÍSTICAS DESCRIPTIVAS Y TABLAS DE CONTINGENCIA
# ==============================================================================

# Resumen numérico general
resumen_cuant <- function(data) {
  data %>%
    select(where(is.numeric) & c(depth, transMw)) %>%
    summarise(across(everything(), list(
      n = ~sum(!is.na(.)), Media = ~mean(., na.rm = TRUE), DE = ~sd(., na.rm = TRUE),
      Min = ~min(., na.rm = TRUE), Q25 = ~quantile(., 0.25, na.rm = TRUE),
      Mediana = ~median(., na.rm = TRUE), Q75 = ~quantile(., 0.75, na.rm = TRUE), Max = ~max(., na.rm = TRUE)
    ))) %>%
    pivot_longer(everything(), names_to = c("Variable", ".value"), names_sep = "_")
}

resumen_cuant(datos %>% filter(Pais == "Chile"))
resumen_cuant(datos %>% filter(Pais == "Japón"))

# Generación automática de tablas de frecuencias relativas
cats <- c("status", "type", "locationSource", "magSource", "depth_cat", "Mw_cat")
tablas <- lapply(cats, function(v) {
  datos %>%
    group_by(Pais, across(all_of(v))) %>%
    summarise(Frecuencia = n(), .groups = "drop_last") %>%
    mutate(Porcentaje = round(100 * Frecuencia / sum(Frecuencia), 2))
})
names(tablas) <- cats

# Ejemplo para revisar una tabla específica:
tablas$depth_cat
tablas$Mw_cat

# Resumen detallado de profundidad según categoría de profundidad
datos %>%
  group_by(Pais, depth_cat) %>%
  summarise(n = n(), Media = mean(depth, na.rm=T), DE = sd(depth, na.rm=T), Min = min(depth, na.rm=T),
            Q1 = quantile(depth, 0.25, na.rm=T), Mediana = median(depth, na.rm=T),
            Q3 = quantile(depth, 0.75, na.rm=T), Max = max(depth, na.rm=T), .groups = "drop")

# ==============================================================================
# 5. ANÁLISIS GRÁFICO (Cajas, Violines y Densidades)
# ==============================================================================

# Función para gráficos de Caja y Violín básicos por país
graficar_box_violin <- function(y_var, ylab, titulo, ylims = NULL, t_size = 22) {
  p <- ggplot(datos, aes(x = Pais, y = .data[[y_var]], fill = Pais)) +
    geom_violin(alpha = 0.35, colour = NA, trim = FALSE) +
    geom_boxplot(width = 0.18, colour = "black", alpha = 0.8, outlier.shape = 21, outlier.size = 2) +
    stat_summary(fun = median, geom = "point", shape = 23, size = 0, fill = "yellow") +
    scale_fill_manual(values = paleta_pais) +
    labs(title = titulo, x = "", y = ylab) +
    tema_base + theme(legend.position = "none", panel.grid.major.x = element_blank(), plot.title = element_text(size = t_size, face = "bold", hjust = 0.5))
  if(!is.null(ylims)) p <- p + coord_cartesian(ylim = ylims)
  print(p)
}

graficar_box_violin("transMw", expression(M[w]), "Comparación de la Magnitud de Momento")
graficar_box_violin("depth", "Profundidad (km)", "Comparación de la Profundidad de los Sismos", c(0, 350), t_size = 15)

# Distribución de la profundidad según categoría y país
datos %>%
  mutate(Grupo = factor(paste(depth_cat, Pais), levels = c("Poco profunda Chile", "Poco profunda Japón", "Intermedia Chile", "Intermedia Japón", "Profunda Chile", "Profunda Japón"))) %>%
  ggplot(aes(x = Grupo, y = depth, fill = Pais)) +
  geom_violin(alpha = 0.3, colour = NA, trim = FALSE) +
  geom_boxplot(width = 0.18, colour = "black", alpha = 0.8, outlier.shape = 21, outlier.size = 2) +
  scale_fill_manual(values = paleta_pais) +
  scale_y_continuous(breaks = seq(0, 700, 100)) + coord_cartesian(ylim = c(0, 700)) +
  labs(title = "Distribución de la profundidad según categoría y país", x = "", y = "Profundidad (km)", fill = "País") +
  tema_base + theme(axis.text.x = element_text(size = 12, angle = 20, hjust = 1))

# Función para distribuciones de densidad
graficar_densidad <- function(x_var, xlab, titulo, facet_var = NULL, filtrar_mw = FALSE) {
  df_plot <- if(filtrar_mw) datos %>% filter(Mw_cat %in% c("Moderate", "Strong", "Major")) else datos
  p <- ggplot(df_plot, aes(x = .data[[x_var]], fill = Pais, colour = Pais)) +
    geom_density(alpha = 0.35, linewidth = 1.2, adjust = if(x_var == "transMw") 1.2 else 1) +
    scale_fill_manual(values = paleta_pais) + scale_colour_manual(values = paleta_pais) +
    labs(title = titulo, x = xlab, y = "Densidad", fill = "País", colour = "País") +
    tema_base + theme(plot.title = element_text(size = if(is.null(facet_var)) 20 else 15, face = "bold", hjust = 0.5))
  
  if(!is.null(facet_var)) {
    f_levels <- if(facet_var == "Mw_cat") c("Moderate", "Strong", "Major") else unique(datos[[facet_var]])
    p <- p + facet_wrap(vars(factor(.data[[facet_var]], levels = f_levels)), nrow = 1, scales = "free_y") +
      theme(strip.text = element_text(size = 16, face = "bold"))
  }
  print(p)
}

graficar_densidad("transMw", expression(M[w]), "Distribución de la Magnitud de Momento")
graficar_densidad("transMw", expression(M[w]), "Distribución de la Magnitud de Momento según Profundidad", "depth_cat")
graficar_densidad("depth", "Profundidad (km)", "Distribución de la Profundidad de los Sismos")
graficar_densidad("depth", "Profundidad (km)", "Distribución de la Profundidad según Magnitud", "Mw_cat", filtrar_mw = TRUE)

# ==============================================================================
# 6. ANÁLISIS DE FRECUENCIAS Y PROPORCIONES (Gráficos de Barras)
# ==============================================================================

# Frecuencia anual de sismos
ggplot(datos, aes(x = year, fill = Mw_cat)) +
  geom_bar() + facet_wrap(~Pais, ncol = 1) +
  labs(title = "Frecuencia anual según categoría de magnitud", x = "Año", y = "Número de sismos", fill = "Categoría") +
  theme_minimal()

# Proporción anual, por profundidad y por magnitud
graficar_barras_prop <- function(x_var, fill_var, paleta, titulo, xlab, ylab) {
  ggplot(datos, aes(x = .data[[x_var]], fill = .data[[fill_var]])) +
    geom_bar(position = "fill") + facet_wrap(~Pais, ncol = if(x_var == "year") 1 else NULL) +
    scale_fill_manual(values = paleta) +
    labs(title = titulo, x = xlab, y = ylab, fill = fill_var) +
    tema_base + theme(axis.text = element_text(size = 18), strip.text = element_text(size = 18, face = "bold"))
}

graficar_barras_prop("year", "Mw_cat", paleta_mw, "Proporción anual según categoría de magnitud", "Año", "Proporción")
graficar_barras_prop("depth_cat", "Mw_cat", paleta_mw, "Magnitud según categoría de profundidad", "Profundidad", "Proporción")
graficar_barras_prop("Mw_cat", "depth_cat", c("Poco profunda"="#9ECAE1", "Intermedia"="#4292C6", "Profunda"="#084594"), 
                     "Distribución de la profundidad según magnitud", "Categoría de magnitud", "Proporción")

# ==============================================================================
# 7. ANÁLISIS DE SERIES TEMPORALES
# ==============================================================================

# Preparar las series mensuales rellenando vacíos con cero sismos
preparar_ts <- function(df) {
  df %>%
    mutate(mes = floor_date(time, "month")) %>%
    count(Pais, mes) %>%
    group_by(Pais) %>%
    complete(mes = seq(min(mes), max(mes), by = "month"), fill = list(n = 0)) %>%
    ungroup()
}

datos_ts    <- preparar_ts(datos)
datos_ts_m6 <- preparar_ts(datos %>% filter(transMw >= 6))

# Gráfico de líneas temporales mensuales
graficar_lineas_ts <- function(df, titulo) {
  ggplot(df, aes(x = mes, y = n, colour = Pais)) +
    geom_line(linewidth = if(stringr::str_detect(titulo, "Mw ≥ 6")) 1.2 else 1.1) +
    scale_colour_manual(values = paleta_pais) +
    labs(title = titulo, x = "Tiempo", y = "Número de sismos", colour = "País") +
    tema_base + theme(axis.text = element_text(size = 18), strip.text = element_text(size = 18, face = "bold"))
}

graficar_lineas_ts(datos_ts, "Frecuencia mensual de sismos")
graficar_lineas_ts(datos_ts_m6, "Frecuencia mensual de sismos Mw ≥ 6")

# Gráfico de medias anuales agregadas
graficar_medias_anuales <- function(df, titulo) {
  df %>%
    mutate(Año = year(mes)) %>%
    group_by(Pais, Año) %>%
    summarise(Nivel = mean(n), .groups = "drop") %>%
    ggplot(aes(x = Año, y = Nivel, colour = Pais)) +
    geom_line(linewidth = 1.3) + geom_point(size = 2) +
    scale_colour_manual(values = paleta_pais) +
    labs(title = titulo, x = "Año", y = "Frecuencia mensual promedio", colour = "País") +
    tema_base + theme(axis.text = element_text(size = 18), strip.text = element_text(size = 18, face = "bold"))
}

graficar_medias_anuales(datos_ts, "Media de las frecuencias mensuales por año")
graficar_medias_anuales(datos_ts_m6, "Media anual de las frecuencias mensuales Mw ≥ 6")

# ==============================================================================
# 8. CURVAS DE LORENZ
# ==============================================================================

Lc_chile <- Lc((datos_ts %>% filter(Pais == "Chile"))$n)
Lc_japon <- Lc((datos_ts %>% filter(Pais == "Japón"))$n)

plot(Lc_chile, col = "#F8766D", lwd = 3, main = "Curvas de Lorenz de las frecuencias mensuales",
     xlab = "Proporción acumulada de meses", ylab = "Proporción acumulada de sismos",
     cex.main = 1.6, cex.lab = 1.4, cex.axis = 1.6)
lines(Lc_japon$p, Lc_japon$L, col = "#619CFF", lwd = 3)
abline(0, 1, lty = 2, lwd = 2)
legend("topleft", legend = c("Chile", "Japón"), col = paleta_pais, lwd = 3, bty = "n", cex = 1.3)

# ==============================================================================
# 9. TIEMPOS INTER-EVENTOS
# ==============================================================================

# Bloque 1: Filtrar y calcular la diferencia de tiempo
inter_eventos <- datos %>%
  filter(Mw_cat %in% c("Strong", "Major", "Great")) %>%
  arrange(Pais, Mw_cat, time) %>%
  group_by(Pais, Mw_cat) %>%
  mutate(Tiempo = as.numeric(difftime(time, lag(time), units = "days"))) %>%
  filter(!is.na(Tiempo)) %>% 
  ungroup()

# Bloque 2: Generar la tabla de estadísticas descriptivas
tabla_inter <- inter_eventos %>%
  group_by(Pais, Mw_cat) %>%
  summarise(
    n       = n(), 
    Media   = mean(Tiempo), 
    DE      = sd(Tiempo), 
    Min     = min(Tiempo),
    Q1      = quantile(Tiempo, .25), 
    Mediana = median(Tiempo), 
    Q3      = quantile(Tiempo, .75),
    Max     = max(Tiempo), 
    .groups = "drop"
  )

# Desplegar la tabla en consola
print(tabla_inter)
