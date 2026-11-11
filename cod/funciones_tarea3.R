# Funciones de los ejercicios 1, 2 y 3.

# Función 1
# alerta.movil(df, ventana, k.alerta)
# Recibe: df = base de transacciones (columnas fecha e id_tx)
# ventana = días previos que se usan para la media
# k.alerta = número de desviaciones estándar
# Devuelve: data frame con fecha, n.tx, media, desv y alerta

alerta.movil <- function(df, ventana, k.alerta) {
  # Contar transacciones por día
  por.dia <- aggregate(id_tx ~ fecha, data = df, FUN = length)
  names(por.dia) <- c("fecha", "n.tx")
  por.dia$fecha <- as.Date(por.dia$fecha)
  por.dia <- por.dia[order(por.dia$fecha), ]

  # Columnas vacías
  n.dias <- nrow(por.dia)
  por.dia$media <- rep(NA_real_, n.dias)
  por.dia$desv <- rep(NA_real_, n.dias)

  # Media y desviación de los ventana días anteriores (sin el día t)
  for (t in (ventana + 1):n.dias) {
    dias.previos <- por.dia$n.tx[(t - ventana):(t - 1)]
    por.dia$media[t] <- mean(dias.previos)
    por.dia$desv[t] <- sd(dias.previos)
  }

  # alerta: el día supera la media en más de k desviaciones
  por.dia$alerta <- por.dia$n.tx > por.dia$media + k.alerta * por.dia$desv

  rownames(por.dia) <- NULL
  por.dia
}