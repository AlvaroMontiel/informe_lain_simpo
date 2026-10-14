normalizar_comuna <- function(x) {
  x %>%
    str_squish() %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    str_to_upper()
}

categorizar_edad <- function(x) {
  cut(
    x,
    breaks = c(0, 5, 11, 19, 24, 59, Inf),
    labels = c(
      "Infancia 0-5",
      "Niñez 6-11",
      "Adolescencia 12-19",
      "Juventud 20-24",
      "Adultez 25-59",
      "Ancianidad 60 y más"
    ),
    include.lowest = TRUE,
    right = TRUE  # Intervalos cerrados por la derecha
  )
}


# Curvas con intervalos semanales desde el 1 de enero; no son semanas ISO.
curva_semanal <- function(datos, anio, variable = NULL) {
  inicio <- as.Date(paste0(anio, "-01-01"))
  fin <- as.Date(paste0(anio + 1, "-01-01"))
  cortes <- sort(unique(c(seq(inicio, fin, by = "1 week"), fin)))
  d <- filter(datos, year(fecha_del_evento) == anio)
  g <- ggplot(d, aes(x = fecha_del_evento))
  if (!is.null(variable)) g <- g + aes(fill = .data[[variable]])
  g <- if (is.null(variable)) g + geom_histogram(breaks = cortes, color = "white", fill = "steelblue") else g + geom_histogram(breaks = cortes, color = "white")
  g +
    scale_x_date(date_breaks = "4 weeks", date_labels = "%d %b") +
    scale_y_continuous(expand = c(0, 0)) +
    labs(title = paste("Eventos por intervalos de siete días", anio),
         subtitle = if (!is.null(variable)) paste("Según", variable),
         x = "Fecha del evento", y = "Número de eventos", fill = variable) +
    theme_minimal() + theme(legend.position = "bottom")
}

# Preserva los porcentajes por columna del análisis original.
tabla_por_anio <- function(conteos, variables, anios) {
  conteos %>% group_by(across(all_of(variables))) %>%
    summarise(across(n, ~ list(setNames(vapply(anios, function(a) sum(n[anio == a]), numeric(1)), paste0("año_", anios)))), .groups = "drop") %>%
    tidyr::unnest_wider(n) %>% janitor::adorn_totals() %>%
    janitor::adorn_percentages("col") %>% janitor::adorn_pct_formatting() %>%
    janitor::adorn_ns(position = "front")
}
