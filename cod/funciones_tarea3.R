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

# función 2
# benford.mad(x)
# Recibe: x = vector de montos
# Devuelve: la MAD entre la proporción
# observada de cada primer dígito y la ley de Benford
benford.mad <- function(x) {
  digito <- x %/% 10^floor(log10(x))
  p.obs <- tabulate(digito, nbins = 9) / length(digito)
  p.benford <- log10(1 + 1 / (1:9))
  mean(abs(p.obs - p.benford))
}



# benford.canton(df, canal, cantones)
# Recibe: df = base de transacciones, canal = canal a auditar,
# cantones = catálogo con cod_canton y canton.
# Devuelve: data frame con cod_canton, canton, n, mad y clase, 
# ordenado de mayor a menor MAD.
benford.canton <- function(df, canal, cantones) {
  datos.canal <- df[df$canal == canal, ]

  n <- tapply(datos.canal$monto_crc, datos.canal$cod_canton, length)
  mad <- tapply(datos.canal$monto_crc, datos.canal$cod_canton, benford.mad)

  resultado <- data.frame(cod_canton = as.integer(names(mad)),
                          n = as.integer(n),
                          mad = as.numeric(mad))
  resultado$canton <- cantones$canton[match(resultado$cod_canton,
                                            cantones$cod_canton)]
  resultado$clase <- cut(resultado$mad,
                         breaks = c(-Inf, 0.006, 0.012, 0.015, Inf),
                         labels = c("conformidad cercana",
                                    "conformidad aceptable",
                                    "conformidad marginal",
                                    "no conformidad"))

  resultado <- resultado[order(resultado$mad, decreasing = TRUE),
                         c("cod_canton", "canton", "n", "mad", "clase")]
  rownames(resultado) <- NULL
  resultado
}