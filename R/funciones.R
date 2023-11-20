# Construir URL
construir_url <- function(fecha_sel = fecha, id_estacion_sel = id_estacion) {
  url <- paste0(
    'https://www.wunderground.com/dashboard/pws/',
    id_estacion_sel,
    '/table/',
    fecha_sel,
    '/',
    fecha_sel,
    '/daily')
  return(url)
}

# Define una función personalizada para limpiar y convertir a numérico
clean_and_convert <- function(x) {
  # Aplica la limpieza solo si la columna es de tipo caracter
  if(is.character(x)) {
    # Limpia la cadena
    cleaned <- gsub("^(\\d+\\.?\\d*)\\s*[^0-9.]*$", "\\1", x)
    # Intenta convertir a numérico, si falla, devuelve la cadena original
    num_converted <- suppressWarnings(as.numeric(cleaned))
    if(all(is.na(num_converted))) {
      return(x)
    } else {
      return(num_converted)
    }
  } else {
    # Si no es una cadena, la deja sin cambios
    return(x)
  }
}

# Columna Time
clean_and_convert_time <- function(x) {
  gsub("\u00A0", " ", x)
}

graficar <- function(id_estacion_sel = id_estacion, fecha_sel = fecha,
                     titulo = paste0(id_estacion_sel, ', ', fecha_sel, ' (Weather Underground)'),
                     tabla_sel = tabla, variable_sel = 'Precip. Accum.',
                     color_linea = 'blue', in2mm = T, unidad = NULL,
                     intervalo_mins = 120) {
  if(in2mm) {
    tabla_sel[[variable_sel]] <- tabla_sel[[variable_sel]] * 25.4
  }

  # Crear el gráfico base
  p <- tabla_sel %>%
    ggplot(aes(x = Date_Time, y = !!sym(variable_sel))) +
    geom_line(color = color_linea, linewidth = 1) +
    scale_x_datetime(date_labels = "%H:%M", date_breaks = paste(intervalo_mins, "min")) +
    theme_bw() +
    theme(text = element_text(size=24)) +
    labs(title = titulo)
  
  # Calcular el valor máximo y su correspondiente fecha
  max_value <- max(tabla_sel[[variable_sel]], na.rm = TRUE)
  if(in2mm) {
    max_value_label <- paste0(max_value, ' mm')
  } else {
    max_value_label <- paste(max_value, unidad)
  }
  max_date_time <- tabla_sel[['Date_Time']][which.max(tabla_sel[[variable_sel]])]
  label_data <- data.frame(Date_Time = max_date_time, value = max_value, label = max_value_label)
  
  p + geom_label(data = label_data, aes(x = max_date_time, y = max_value),
                 label = paste0(max_value_label, '\n', '(día y hora: ', max_date_time, ')'),
                 size = 5,  # Tamaño del texto
                 label.size = NA,  # Sin borde alrededor del rectángulo
                 label.padding = unit(0.5, "lines"),  # Ajustar el padding dentro del rectángulo
                 alpha = 0.5,  # Ajustar la opacidad del fondo del rectángulo
                 fill = 'grey',
                 vjust = "inward",  # Ajustar la justificación vertical
                 hjust = "inward")  # Ajustar la justificación horizontal
}

guardar_grafico <- function(grafico = NULL, id_estacion_sel = id_estacion, dir_figuras = 'img',
                          archivo = paste0(dir_figuras, "/", id_estacion_sel, '.jpg'),
                          anchura = 3000, altura = 1500, resolucion = 200,
                          sobreescribir = !file.exists(archivo)) {
  # Comprueba si el archivo ya existe
  if (sobreescribir) {
    # Si el archivo no existe, escribe el archivo JPEG
    jpeg(archivo,
         width = anchura, height = altura, res = resolucion)
    print(grafico)
    dev.off()
  } else {
    # Aquí puedes decidir si lanzar un error o simplemente retornar un mensaje
    message("El archivo de gráfico ya existe. No se sobreescribirá. Si desea forzarlo, use sobreescribir = T")
  }
}

guardar_tabla <- function(tabla_sel = tabla, id_estacion_sel = id_estacion, dir_datos = 'data',
                          archivo = paste0(dir_datos, '/', id_estacion_sel, '.csv'),
                          sobreescribir = !file.exists(archivo)) {
  # Comprueba si el archivo ya existe
  if (sobreescribir) {
    # Si el archivo no existe, escribe el archivo CSV
    write_csv(tabla_sel, archivo)
  } else {
    # Aquí puedes decidir si lanzar un error o simplemente retornar un mensaje
    message("El archivo ya existe. No se sobreescribirá. Si desea forzarlo, use sobreescribir = T")
  }
}

graficar_panel <- function(tabla_sel = tabla, variable_sel = 'Precip. Accum.',
                           facet_var = NULL, color_linea = 'blue', in2mm = T,
                           unidad = NULL, intervalo_mins = 120) {
  facet_formula <- as.formula(paste0(". ~ ", facet_var))
  if(in2mm) {
    tabla_sel[[variable_sel]] <- tabla_sel[[variable_sel]] * 25.4
  }

  # Crear el gráfico base
  tabla_sel %>%
    ggplot(aes(x = Date_Time, y = !!sym(variable_sel))) +
    geom_line(color = color_linea, linewidth = 1) +
    scale_x_datetime(date_labels = "%H:%M", date_breaks = paste(intervalo_mins, "min")) +
    theme_bw() +
    theme(text = element_text(size=24)) +
    # facet_grid(facet_formula)
    facet_wrap(vars(!!sym(facet_var)))
}
