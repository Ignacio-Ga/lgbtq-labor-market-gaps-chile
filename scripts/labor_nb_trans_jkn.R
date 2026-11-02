# =============================================================================
# LABOR JKn: TRANS Y NO BINARIO, ESPECIFICACION FINAL, CASEN 2022-2024
# =============================================================================

library(dplyr)
library(haven)
library(survey)
library(readr)

#  CONFIGURACION -------------------------------------------------------------
archivo <- "Intermediates/Panel_post.csv"
carpeta_datos <- "Data"
carpeta <- "Tables/labor-jkn"
regla_boleta <- "literal"  # "y": sensibilidad a la disyunción publicada
options(survey.lonely.psu = "adjust")
dir.create(carpeta, recursive = TRUE, showWarnings = FALSE)

# INFORMALIDAD: ALGORITMO DOCUMENTADO ---------------------------------------
# MDSF (2026), Nota tecnica 7, pp. 10-12 y 15-17.
# literal reproduce el OR publicado; y evalua la alternativa documentada.
reconstruir_informalidad_nt7 <- function(x) {
  n <- nrow(x)
  elegible <- x$activ %in% 1 & is.finite(x$edad) & x$edad>=15
  dependiente <- elegible & x$o15 %in% 3:8
  independiente <- elegible & x$o15 %in% c(1,2)
  familiar <- elegible & x$o15 %in% 9
  p31 <- x$o31; p32 <- x$o32; boleta <- x$o14
  p31[p31 %in% -88] <- 9
  p32[p32 %in% -88] <- 9
  boleta[boleta %in% -88] <- 9
  mayor15 <- is.finite(x$edad) & x$edad>=15
  afp <- rep(NA_integer_,n)
  afp[mayor15 & (p31 %in% 2 | p32 %in% 6)] <- 0L
  afp[mayor15 & p31 %in% 1 & p32 %in% 1:5] <- 1L
  edad_retencion <- (x$sexo %in% 1 & is.finite(x$edad) & x$edad<55) |
    (x$sexo %in% 2 & is.finite(x$edad) & x$edad<50)
  afp[mayor15 & (p31 %in% 2 | p32 %in% 6) & boleta %in% 1 & edad_retencion] <- 1L
  afp[mayor15 & (p31 %in% 9 | p32 %in% 9)] <- 9L
  # s13 en 2022/2024: se siguen los CODIGOS operativos del documento,
  # no las etiquetas heredadas de las series anteriores que aparecen en el PDF.
  health <- rep(NA_integer_,n)
  health[x$s13 %in% c(1,2,3,5)] <- 1L
  health[x$s13 %in% 4] <- 0L
  health[x$s13 %in% c(-88,-99,99)] <- 9L
  oficio <- x$oficio1_08
  oficio[oficio %in% -66] <- 10
  oficio[oficio %in% c(-88,-99)] <- 99
  y <- rep(NA_integer_,n); motivo <- rep("No clasificable",n)
  asignar <- function(cond, valor, texto) {
    idx <- which(cond %in% TRUE)
    y[idx] <<- valor; motivo[idx] <<- texto
  }
  
  asignar(dependiente & afp %in% 1 & health %in% 1,0L,"Dependiente: prevision y salud")
  asignar(independiente & x$o16 %in% 1,0L,"Independiente: SII registrado")
  asignar(is.na(y) & independiente & oficio %in% 1:3,0L,"Independiente: respaldo oficio calificado")
  # Orden de replace del documento: honorarios y SII=2 prevalecen.
  asignar(dependiente & boleta %in% 1,1L,"Dependiente: honorarios")
  asignar(dependiente & (afp %in% 0 | health %in% 0),1L,"Dependiente: ausencia prevision o salud")
  asignar(independiente & x$o16 %in% 2,1L,"Independiente: sin registro SII")
  asignar(familiar,1L,"Familiar no remunerado")
  asignar(is.na(y) & independiente & oficio %in% 4:10,1L,"Independiente: respaldo oficio no calificado")
  motivo[!elegible] <- "Fuera del universo de ocupados de 15 o mas"
  y_and <- y
  afectado <- dependiente & y %in% 0 & boleta %in% 9
  y_and[afectado] <- NA_integer_
  data.frame(informal_nt7=y,informal_nt7_and=y_and,
             nt7_afp=afp,nt7_health=health,nt7_boleta=boleta,
             nt7_oficio=oficio,nt7_motivo=motivo,nt7_sensible_OR_AND=afectado)
}


# LECTURA Y RECUPERACION DE FUENTES -----------------------------------------
raw <- read.csv(archivo)
fuentes <- c("activ", "edad", "sexo", "o15", "o14", "o16", "o31", "o32",
             "s13", "oficio1_08", "ypchtotcor", "ypchtrabcor")
partes <- mapas <- list()
for (anio in c(2022, 2024)) {
  cat("Leyendo fuentes CASEN", anio, "...\n")
  z <- haven::read_sav(
    file.path(carpeta_datos, paste0("casen_", anio, ".sav")),
    col_select = tidyselect::any_of(c(fuentes, "expr_osig", "folio", "id_persona", "orden"))
  ) |> haven::zap_labels() |> as.data.frame()
  z$.fila_SAV <- seq_len(nrow(z))
  z <- z[!is.na(z$expr_osig), , drop = FALSE]
  idx <- which(raw$year == anio)
  id <- intersect(c("id_persona", "orden"), names(z))[1]
  texto_id <- function(v) if (is.numeric(v)) sprintf("%.17g", v) else as.character(v)
  # Sin ID en el CSV se reproduce el orden del preprocesamiento previamente
  # verificado: SAV filtrado por expr_osig no faltante, por anio.
  # Requiere el mismo Panel_post sin reordenar ni aplicar filtros adicionales.
  if (!is.na(id) && id %in% names(raw)) {
    clave_csv <- paste(texto_id(raw$folio[idx]), texto_id(raw[[id]][idx]), sep = "|")
    clave_sav <- paste(texto_id(z$folio), texto_id(z[[id]]), sep = "|")
    z <- z[match(clave_csv, clave_sav), , drop = FALSE]
  }
  partes[[as.character(anio)]] <- cbind(z[fuentes], year = anio, fila_CSV = idx)
  mapas[[as.character(anio)]] <- data.frame(
    fila_CSV = idx, year = anio, folio = texto_id(z$folio),
    identificador = id, id_persona = texto_id(z[[id]]), fila_SAV = z$.fila_SAV
  )
}
fuentes_nt7 <- bind_rows(partes)
fuentes_nt7 <- fuentes_nt7[match(seq_len(nrow(raw)), fuentes_nt7$fila_CSV), ]
write.csv(bind_rows(mapas), file.path(carpeta, "mapa_personas.csv"), row.names = FALSE)
nt7 <- reconstruir_informalidad_nt7(fuentes_nt7)

# VARIABLES Y MUESTRAS ------------------------------------------------------
# Clasificacion fija: ocupados de 18--65, CASEN completa 2022 y 2024.
# Mismo catalogo y universo del clasificador principal. No restringir a SOGI.
catalogo_ciuo4 <- c(
  110, 210, 310, 1111, 1112, 1113, 1114, 1120, 1211, 1212, 1213, 1219, 1221, 1222, 1223, 1311,
  1312, 1321, 1322, 1323, 1324, 1330, 1341, 1342, 1343, 1344, 1345, 1346, 1349, 1411, 1412, 1420,
  1431, 1439, 2111, 2112, 2113, 2114, 2120, 2131, 2132, 2133, 2134, 2141, 2142, 2143, 2144, 2145,
  2146, 2147, 2149, 2151, 2152, 2153, 2161, 2162, 2163, 2164, 2165, 2166, 2211, 2212, 2221, 2222,
  2230, 2241, 2242, 2243, 2244, 2245, 2246, 2247, 2248, 2249, 2310, 2320, 2330, 2341, 2342, 2351,
  2352, 2353, 2354, 2355, 2356, 2359, 2411, 2412, 2413, 2421, 2422, 2423, 2424, 2431, 2432, 2433,
  2434, 2511, 2512, 2513, 2514, 2519, 2521, 2522, 2523, 2529, 2611, 2612, 2619, 2621, 2622, 2631,
  2632, 2633, 2634, 2635, 2636, 2641, 2642, 2643, 2651, 2652, 2653, 2654, 2655, 2656, 2659, 3111,
  3112, 3113, 3114, 3115, 3116, 3117, 3118, 3119, 3121, 3122, 3123, 3131, 3132, 3133, 3134, 3135,
  3139, 3141, 3142, 3143, 3151, 3152, 3153, 3154, 3155, 3211, 3212, 3213, 3214, 3215, 3216, 3221,
  3222, 3223, 3231, 3232, 3240, 3251, 3252, 3253, 3254, 3255, 3256, 3257, 3258, 3259, 3311, 3312,
  3313, 3314, 3315, 3321, 3322, 3323, 3324, 3331, 3332, 3333, 3334, 3339, 3341, 3342, 3343, 3344,
  3351, 3352, 3353, 3354, 3355, 3359, 3411, 3412, 3413, 3421, 3422, 3423, 3431, 3432, 3433, 3434,
  3435, 3439, 3511, 3512, 3513, 3514, 3521, 3522, 3611, 3612, 4110, 4120, 4131, 4132, 4211, 4212,
  4213, 4214, 4221, 4222, 4223, 4224, 4225, 4226, 4227, 4229, 4311, 4312, 4313, 4321, 4322, 4323,
  4411, 4412, 4413, 4414, 4415, 4416, 4419, 5111, 5112, 5113, 5120, 5131, 5132, 5141, 5142, 5151,
  5152, 5153, 5161, 5162, 5163, 5164, 5165, 5169, 5211, 5212, 5221, 5222, 5223, 5230, 5241, 5242,
  5243, 5244, 5245, 5246, 5249, 5311, 5312, 5321, 5322, 5329, 5411, 5412, 5413, 5414, 5419, 6111,
  6112, 6113, 6114, 6121, 6122, 6123, 6129, 6130, 6210, 6221, 6222, 6223, 6224, 6310, 6320, 6330,
  6340, 7111, 7112, 7113, 7114, 7115, 7119, 7121, 7122, 7123, 7124, 7125, 7126, 7127, 7131, 7132,
  7133, 7211, 7212, 7213, 7214, 7215, 7221, 7222, 7223, 7224, 7231, 7232, 7233, 7234, 7311, 7312,
  7313, 7314, 7315, 7316, 7317, 7318, 7319, 7321, 7322, 7323, 7411, 7412, 7413, 7421, 7422, 7511,
  7512, 7513, 7514, 7515, 7516, 7521, 7522, 7523, 7531, 7532, 7533, 7534, 7535, 7536, 7541, 7542,
  7543, 7544, 7549, 8111, 8112, 8113, 8114, 8121, 8122, 8131, 8132, 8141, 8142, 8143, 8151, 8152,
  8153, 8154, 8155, 8156, 8157, 8159, 8160, 8171, 8172, 8181, 8182, 8183, 8189, 8211, 8212, 8219,
  8311, 8312, 8321, 8322, 8331, 8332, 8341, 8342, 8343, 8344, 8350, 9111, 9112, 9121, 9122, 9123,
  9129, 9211, 9212, 9213, 9214, 9215, 9216, 9311, 9312, 9313, 9321, 9329, 9331, 9332, 9333, 9334,
  9411, 9412, 9510, 9520, 9611, 9612, 9613, 9621, 9622, 9623, 9624, 9629
)

codigo_ocupacion <- function(x, digitos = 3L) {
  # Permite numeros o cadenas como "0110". Un factor se lee por su etiqueta.
  z <- suppressWarnings(as.numeric(as.character(x)))
  valido <- !is.na(z) & z %in% catalogo_ciuo4
  salida <- rep(NA_character_, length(z))
  salida[valido] <- substr(sprintf("%04d", as.integer(z[valido])), 1L, digitos)
  salida
}


bases_ocupaciones <- lapply(c(2022L, 2024L), function(anio) {
  cat("Clasificando ocupaciones: CASEN", anio, "...\n")
  z <- haven::read_sav(
    file.path(carpeta_datos, paste0("casen_", anio, ".sav")),
    col_select = tidyselect::all_of(c("edad", "sexo", "activ", "oficio4_08",
                                    "expr", "varstrat", "varunit"))
  ) |> haven::zap_labels() |> as.data.frame()
  z$ocupacion <- codigo_ocupacion(z$oficio4_08, 3L)
  z$year <- anio
  z |> filter(activ %in% 1, is.finite(edad), edad >= 18, edad <= 65,
              sexo %in% c(1, 2), !is.na(ocupacion),
              is.finite(expr), expr > 0, !is.na(varstrat), !is.na(varunit))
})
clasificacion_ocupaciones <- bind_rows(bases_ocupaciones) |>
  group_by(ocupacion) |>
  summarise(N_referencia = n(), N_mujeres = sum(sexo == 2),
            peso_total = sum(expr), peso_mujeres = sum(expr * (sexo == 2)),
            .groups = "drop") |>
  mutate(proporcion_mujeres = peso_mujeres / peso_total,
         feminizada = as.integer(proporcion_mujeres >= 0.50))
# Dividir los pesos de ambas olas por 2 no altera estas proporciones.
write.csv(clasificacion_ocupaciones,
          file.path(carpeta, "clasificacion_ocupaciones_50.csv"), row.names = FALSE)
rm(bases_ocupaciones)

d <- raw
conocido_os <- d$os1 %in% 1:4
conocido_g <- d$genero %in% 1:6 &
  ((d$GM %in% 1 & d$cis %in% 0) | (d$GM %in% 0 & d$cis %in% 1))
d$AFAB <- ifelse(d$sexo %in% 2, 1L, 0L)
d$GM <- ifelse(conocido_g, as.integer(d$GM %in% 1), NA_integer_)
d$LG <- ifelse(conocido_os, as.integer(d$os1 %in% 2), NA_integer_)
d$bisexual <- ifelse(conocido_os, as.integer(d$os1 %in% 3), NA_integer_)
d$Other <- ifelse(conocido_os, as.integer(d$os1 %in% 4), NA_integer_)

# Ambas fuentes ministeriales ya son per capita: no dividir nuevamente.
d$ypchtotcor <- fuentes_nt7$ypchtotcor
d$ypchtrabcor <- fuentes_nt7$ypchtrabcor
fuentes_validas <- is.finite(d$ypchtotcor) & is.finite(d$ypchtrabcor) &
  !(d$ypchtotcor %in% c(-88, -99)) & !(d$ypchtrabcor %in% c(-88, -99))
d$ing_nl <- with(d, ypchtotcor - ypchtrabcor)
d$ing_nl[!fuentes_validas | !is.finite(d$ing_nl)] <- NA_real_
# Se conservan ceros y diferencias negativas validas; no se imputan faltantes.
fuente_ingreso <- "Non-labor household income per capita is ypchtotcor minus ypchtrabcor, in levels. Both sources are already per capita; missing or invalid sources are not imputed as zero. Valid negative differences are retained."

d$Employed_population <- case_when(d$activ == 1 ~ 1L, d$activ %in% c(2,3) ~ 0L, TRUE ~ NA_integer_)
d$Employed <- case_when(d$activ == 1 ~ 1L, d$activ == 2 ~ 0L, TRUE ~ NA_integer_)
d$Labor_force <- case_when(d$activ %in% c(1,2) ~ 1L, d$activ == 3 ~ 0L, TRUE ~ NA_integer_)
d$Self_employed <- ifelse(d$activ %in% 1, as.integer(d$o15 %in% c(1,2)), NA_integer_)
d$Public_sector <- ifelse(d$activ %in% 1, as.integer(d$o15 %in% c(3,4,8)), NA_integer_)
# Mantener el orden de las personas; codigos invalidos o sin referencia quedan NA.
d$ocupacion_ciuo3 <- codigo_ocupacion(d$CIIU, 3L)
d$Feminized_sector <- clasificacion_ocupaciones$feminizada[
  match(d$ocupacion_ciuo3, clasificacion_ocupaciones$ocupacion)]
d$Feminized_sector[!(d$activ %in% 1)] <- NA_integer_
d$Monthly_hours <- NA_real_
horas_validas <- d$activ %in% 1 & is.finite(d$hrs) & d$hrs > 0
d$Monthly_hours[horas_validas] <- log(d$hrs[horas_validas] * 4.33)
d$Informal_worker <- if (regla_boleta == "literal") nt7$informal_nt7 else nt7$informal_nt7_and

outcomes <- c("Employed_population", "Employed", "Labor_force", "Self_employed",
              "Public_sector", "Feminized_sector", "Monthly_hours", "Informal_worker")

# Universo reconstruido con las mismas reglas del labor principal.
universo_general <- (
  d$year %in% c(2022, 2024) &
    is.finite(d$edad) &
    d$edad >= 18 &
    d$edad <= 65 &
    d$sexo %in% c(1, 2) &
    is.finite(d$expr_osig) &
    d$expr_osig > 0
)

mascara_universo <- function(resultado) {
  actividad_valida <- switch(
    resultado,
    Employed_population = d$activ %in% 1:3,
    Labor_force         = d$activ %in% 1:3,
    Employed            = d$activ %in% c(1, 2),
    Self_employed       = d$activ %in% 1,
    Public_sector       = d$activ %in% 1,
    Feminized_sector    = d$activ %in% 1,
    Monthly_hours       = d$activ %in% 1,
    Informal_worker     = d$activ %in% 1
  )
  
  universo_general & actividad_valida
}

filtro <- (
  conocido_os &
    conocido_g &
    is.finite(d$ing_nl) &
    d$ecivil %in% 1:8
)

mascaras <- setNames(
  lapply(outcomes, function(y) {
    mascara_universo(y) &
      filtro &
      is.finite(d[[y]])
  }),
  outcomes
)

rango_edad <- range(d$edad[mascaras$Labor_force], na.rm = TRUE)
write.csv(cbind(fila_CSV = seq_len(nrow(d)), year = d$year, nt7),
          file.path(carpeta, "clasificacion_informalidad.csv"), row.names = FALSE)

# DESAGREGACION:
d$NB_explicit <- as.integer(d$NB %in% 1 | d$genero %in% 5)
d$Trans_explicit <- as.integer(d$TRANS %in% 1 & d$NB_explicit == 0)
d$Cis_explicit <- as.integer(d$cis %in% 1 & d$Trans_explicit == 0 & d$NB_explicit == 0)
d$genero_explicito_valido <- d$sexo %in% c(1,2) &
  (d$Cis_explicit + d$Trans_explicit + d$NB_explicit == 1)
N_principal <- vapply(mascaras, sum, numeric(1))
mascaras <- lapply(mascaras, function(m) m & d$genero_explicito_valido)
write.csv(data.frame(resultado = outcomes, N_principal = N_principal,
                    N_desagregado = vapply(mascaras, sum, numeric(1))),
          file.path(carpeta, "muestras_desagregadas.csv"), row.names = FALSE)
grupo <- case_when(d$NB_explicit == 1 ~ "Non-binary",
                   d$Trans_explicit == 1 ~ "Transgender",
                   d$Cis_explicit == 1 ~ "Cis",
                   TRUE ~ "Residual/excluded")
write.csv(as.data.frame(table(grupo, d$AFAB, useNA = "ifany")),
          file.path(carpeta, "conteos_grupos_base.csv"), row.names = FALSE)
rango_edad <- range(d$edad[mascaras$Labor_force], na.rm = TRUE)

#  ESPECIFICACION Y DISENO COMPLEJO ------------------------------------------
rhs <- paste("AFAB * (Trans_explicit + NB_explicit + LG + bisexual + Other)",
             "edad + edad2 + esc + factor(area) + factor(ecivil)",
             "men11c + disc_wg + extranjero + pueblos_indigenas",
             "factor(region) + factor(year) + ing_nl", sep = " + ")
diseno <- svydesign(ids = ~varunit_panel, strata = ~varstrat_panel,
                    weights = ~expr_osig, nest = TRUE, data = d)

#  CONTRASTES PROMEDIO PONDERADOS Y METODO DELTA ------------------------------

# DISENO JKn COMPRIMIDO: EVITA EL QR DE LA MATRIZ COMPLETA ----------------------
crear_diseno_jkn_grande <- function(
    design_original,
    grados_libertad,
    mse = FALSE
) {
  
  pesos_jkn <- jknweights(
    strata = design_original$strata[, 1],
    psu = design_original$cluster[, 1],
    compress = TRUE
  )
  
  gl_usuario <- as.numeric(grados_libertad)
  attr(gl_usuario, "set-by-user") <- TRUE
  
  diseno_jkn <- list(
    repweights = pesos_jkn$repweights,
    pweights = as.numeric(1 / design_original$prob),
    type = "JKn",
    rho = 0,
    scale = pesos_jkn$scale,
    rscales = pesos_jkn$rscales,
    call = match.call(),
    combined.weights = FALSE,
    selfrep = NULL,
    mse = mse,
    variables = design_original$variables,
    degf = gl_usuario
  )
  
  class(diseno_jkn) <- "svyrep.design"
  
  diseno_jkn
}



grados_libertad_diseno <- degf(diseno)
design_base <- crear_diseno_jkn_grande(diseno, grados_libertad_diseno, mse = FALSE)
numero_replicas_jkn <- if (inherits(design_base$repweights, "repweights_compressed")) {
  ncol(design_base$repweights$weights)
} else ncol(design_base$repweights)
cat("\nJKn: ", numero_replicas_jkn, " replicas; ", grados_libertad_diseno,
    " grados de libertad del diseno completo.\n", sep = "")
write.csv(data.frame(metodo = "JKn", replicas = numero_replicas_jkn,
                    gl = grados_libertad_diseno, mse = FALSE),
          file.path(carpeta, "diseno_jkn.csv"), row.names = FALSE)

# CONTRASTES: ESTANDARIZACION SOBRE LA MUESTRA OBSERVADA ------------------------
  prediccion_promedio <- function(modelo, datos_cf) {
    beta <- coef(modelo)

    
    
    X_sin_alinear <- model.matrix(
      delete.response(terms(modelo)),
      data = datos_cf,
      contrasts.arg = modelo$contrasts,
      xlev = modelo$xlevels
    )
    
    X <- matrix(
      0,
      nrow = nrow(X_sin_alinear),
      ncol = length(beta),
      dimnames = list(NULL, names(beta))
    )
    
    columnas_comunes <- intersect(colnames(X_sin_alinear), names(beta))
    X[, columnas_comunes] <- X_sin_alinear[, columnas_comunes, drop = FALSE]
    
    eta <- as.numeric(X %*% beta)
    mu <- modelo$family$linkinv(eta)
    derivada <- modelo$family$mu.eta(eta)
    
    pesos <- as.numeric(weights(modelo$survey.design, type = "sampling"))
    pesos <- pesos / sum(pesos)

    estimacion <- sum(pesos * mu)
    gradiente <- colSums(X * as.numeric(pesos * derivada))
    
    list(
      estimacion = estimacion,
      gradiente = gradiente
    )
  }
  
  aplicar_escenario <- function(datos, cambios) {
    for (variable in names(cambios)) {
      datos[[variable]] <- cambios[[variable]]
    }
    datos
  }
  
  contraste_promedio <- function(modelo, escenario_1, escenario_0) {
    datos_base <- modelo$survey.design$variables
    
    pred_1 <- prediccion_promedio(
      modelo,
      aplicar_escenario(datos_base, escenario_1)
    )
    
    pred_0 <- prediccion_promedio(
      modelo,
      aplicar_escenario(datos_base, escenario_0)
    )
    
    diferencia <- pred_1$estimacion - pred_0$estimacion
    gradiente <- pred_1$gradiente - pred_0$gradiente
    matriz_vcov <- vcov(modelo)
    se <- sqrt(as.numeric(t(gradiente) %*% matriz_vcov %*% gradiente))
    
    grados_libertad <- degf(modelo$survey.design)
    estadistico <- diferencia / se
    p_value <- 2 * pt(abs(estadistico), df = grados_libertad, lower.tail = FALSE)
    critico <- qt(0.975, df = grados_libertad)
    
    data.frame(
      estimacion = diferencia,
      se = se,
      estadistico = estadistico,
      p_value = p_value,
      ci_95_inferior = diferencia - critico * se,
      ci_95_superior = diferencia + critico * se
    )
  }
  
escenarios <- list(
  list(
    orden = 1, panel = "A",
    contraste = "AFAB cis vs AMAB cis",
    escenario_1 = list(AFAB = 1, Trans_explicit = 0, NB_explicit = 0),
    escenario_0 = list(AFAB = 0, Trans_explicit = 0, NB_explicit = 0)
  ),
  list(
    orden = 2, panel = "A",
    contraste = "AMAB non-binary vs AMAB cis",
    escenario_1 = list(AFAB = 0, Trans_explicit = 0, NB_explicit = 1),
    escenario_0 = list(AFAB = 0, Trans_explicit = 0, NB_explicit = 0)
  ),
  list(
    orden = 3, panel = "A",
    contraste = "AFAB non-binary vs AMAB cis",
    escenario_1 = list(AFAB = 1, Trans_explicit = 0, NB_explicit = 1),
    escenario_0 = list(AFAB = 0, Trans_explicit = 0, NB_explicit = 0)
  ),
  list(
    orden = 4, panel = "A",
    contraste = "AFAB non-binary vs AFAB cis",
    escenario_1 = list(AFAB = 1, Trans_explicit = 0, NB_explicit = 1),
    escenario_0 = list(AFAB = 1, Trans_explicit = 0, NB_explicit = 0)
  ),
  list(
    orden = 5, panel = "A",
    contraste = "AFAB non-binary vs AMAB non-binary",
    escenario_1 = list(AFAB = 1, Trans_explicit = 0, NB_explicit = 1),
    escenario_0 = list(AFAB = 0, Trans_explicit = 0, NB_explicit = 1)
  ),
  list(
    orden = 6, panel = "B",
    contraste = "AFAB cis vs AMAB cis",
    escenario_1 = list(AFAB = 1, Trans_explicit = 0, NB_explicit = 0),
    escenario_0 = list(AFAB = 0, Trans_explicit = 0, NB_explicit = 0)
  ),
  list(
    orden = 7, panel = "B",
    contraste = "AMAB transgender vs AMAB cis",
    escenario_1 = list(AFAB = 0, Trans_explicit = 1, NB_explicit = 0),
    escenario_0 = list(AFAB = 0, Trans_explicit = 0, NB_explicit = 0)
  ),
  list(
    orden = 8, panel = "B",
    contraste = "AFAB transgender vs AMAB cis",
    escenario_1 = list(AFAB = 1, Trans_explicit = 1, NB_explicit = 0),
    escenario_0 = list(AFAB = 0, Trans_explicit = 0, NB_explicit = 0)
  ),
  list(
    orden = 9, panel = "B",
    contraste = "AFAB transgender vs AFAB cis",
    escenario_1 = list(AFAB = 1, Trans_explicit = 1, NB_explicit = 0),
    escenario_0 = list(AFAB = 1, Trans_explicit = 0, NB_explicit = 0)
  ),
  list(
    orden = 10, panel = "B",
    contraste = "AFAB transgender vs AMAB transgender",
    escenario_1 = list(AFAB = 1, Trans_explicit = 1, NB_explicit = 0),
    escenario_0 = list(AFAB = 0, Trans_explicit = 1, NB_explicit = 0)
  )
)

  calcular_contrastes_modelo <- function(nombre_resultado, modelo, horas_log = FALSE) {
    multiplicador <- 1  # Escala original: probabilidades, no puntos porcentuales
    unidad <- if (nombre_resultado == "Monthly_hours") {
      if (horas_log) "log points" else "hours"
    } else "probability units"
    
    bind_rows(lapply(escenarios, function(escenario) {
      resultado <- contraste_promedio(
        modelo,
        escenario$escenario_1,
        escenario$escenario_0
      )
      
      resultado %>%
        mutate(
          orden = escenario$orden,
          panel = escenario$panel,
          contraste = escenario$contraste,
          resultado = nombre_resultado,
          unidad = unidad,
          n = nrow(model.frame(modelo)),
          across(
            c(estimacion, se, ci_95_inferior, ci_95_superior),
            ~ .x * multiplicador
          ),
          .before = 1
        )
    }))
  }
  
  
  


# ESTIMACION SECUENCIAL SIN CHECKPOINTS; EXPORTACION INCREMENTAL CSV ------------------------------------
r2_ponderado <- function(modelo) {
  if (modelo$family$family != "gaussian" || modelo$family$link != "identity") return(NA_real_)
  y <- as.numeric(model.response(model.frame(modelo)))
  yhat <- as.numeric(fitted(modelo))
  w <- as.numeric(weights(modelo$survey.design, type = "sampling"))
  w <- w / sum(w)
  media <- sum(w * y)
  sct <- sum(w * (y - media)^2)
  if (!is.finite(sct) || sct <= 0) return(NA_real_)
  1 - sum(w * (y - yhat)^2) / sct
}

coefs <- efectos <- registro <- compactos <- list()
inicio <- proc.time()[[3]]
for (j in seq_along(outcomes)) {
  y <- outcomes[j]
  formula <- as.formula(paste(y, "~", rhs))
  mascara <- mascaras[[y]]
  mf <- model.frame(formula, data = d, na.action = na.pass)
  completos <- complete.cases(mf)
  for (v in names(mf)) if (is.numeric(mf[[v]])) completos <- completos & is.finite(mf[[v]])
  mascara <- mascara & completos
  cat(sprintf("\n[%d/%d] %s | N=%d | %d replicas | %.1f min acumulados\n",
              j, length(outcomes), y, sum(mascara), numero_replicas_jkn,
              (proc.time()[[3]] - inicio)/60)); flush.console()
    t0 <- proc.time()[[3]]
    m <- svyglm(formula, design = design_base[mascara, ],
                family = gaussian(link = "identity"),
                return.replicates = FALSE)
    cc <- as.data.frame(summary(m)$coefficients)
    names(cc)[1:4] <- c("estimacion", "se", "estadistico", "p_value")
    cc$termino <- rownames(cc); rownames(cc) <- NULL
    cc$resultado <- y; cc$n <- nrow(model.frame(m))
    cc$escala <- if (y == "Monthly_hours") "log_horas" else "probabilidad"
    cc$especificacion <- "Labor_trans_nb_MPL_JKn"
    cat("Modelo ajustado; calculando contrastes...\n"); flush.console()
    ee <- calcular_contrastes_modelo(y, m, horas_log = TRUE)
    ee$especificacion <- "Labor_trans_nb_MPL_JKn"
    coefs[[y]] <- cc; efectos[[y]] <- ee
    registro[[y]] <- data.frame(resultado = y, N = nrow(model.frame(m)),
      r2 = r2_ponderado(m),
      N_mascara = sum(mascaras[[y]]), excluidos_adicionales = sum(mascaras[[y]] & !completos),
      segundos = proc.time()[[3]] - t0, replicas_diseno = numero_replicas_jkn,
      formula = paste(deparse(formula), collapse = " "))
    compactos[[y]] <- list(coeficientes = coef(m), vcov = vcov(m),
      formula = formula, familia = m$family$family, enlace = m$family$link,
      grados_libertad = degf(m$survey.design), N = nrow(model.frame(m)),
      r2 = r2_ponderado(m))
    cat(sprintf("Terminado: %.1f minutos\n", (proc.time()[[3]] - t0)/60))
    rm(m); gc(verbose = FALSE)
  write.csv(bind_rows(coefs), file.path(carpeta, "coeficientes_completos.csv"), row.names = FALSE)
  write.csv(bind_rows(efectos), file.path(carpeta, "contrastes.csv"), row.names = FALSE)
  write.csv(bind_rows(registro), file.path(carpeta, "registro_estimacion.csv"), row.names = FALSE)
}

# TABLAS LATEX: CONTRASTES Y APENDICE CON TODOS LOS COEFICIENTES ----------------
  # Tablas LaTeX: contrastes por especificacion y coeficientes de todos los modelos.
  ef <- bind_rows(efectos); cf <- bind_rows(coefs)
  estrellas <- function(p) {
    if (is.na(p)) return("")
    if (p < .001) "^{***}" else if (p < .01) "^{**}" else if (p < .05) "^{*}" else if (p < .10) "^{\\dagger}" else ""
  }
  esc_tex <- function(x) gsub("_", "\\_", x, fixed = TRUE)
  fin <- " \\\\"
  nombres_col <- c("Employment in population", "Employment among participants",
                   "Labor-force participation", "Self-employed", "Public sector",
                   "Female-dominated occupation", "Log monthly hours", "Informal worker")
  encabezados <- c(
    "\\begin{tabular}[c]{@{}c@{}}Employment in\\\\ population\\\\ (1)\\end{tabular}",
    "\\begin{tabular}[c]{@{}c@{}}Employment among\\\\ participants\\\\ (2)\\end{tabular}",
    "\\begin{tabular}[c]{@{}c@{}}Labor-force\\\\ participation\\\\ (3)\\end{tabular}",
    "\\begin{tabular}[c]{@{}c@{}}Self-employed\\\\ (4)\\end{tabular}",
    "\\begin{tabular}[c]{@{}c@{}}Public sector\\\\ (5)\\end{tabular}",
    "\\begin{tabular}[c]{@{}c@{}}Female-dominated\\\\ occupation\\\\ (6)\\end{tabular}",
    "\\begin{tabular}[c]{@{}c@{}}Log monthly hours\\\\ (7)\\end{tabular}",
    "\\begin{tabular}[c]{@{}c@{}}Informal worker\\\\ (8)\\end{tabular}")
  # Solo cambia la exportacion: las ocho ecuaciones permanecen estimadas.
  for (version_tabla in c("principal", "robustez_8_columnas")) {
    incluir_poblacion <- version_tabla == "robustez_8_columnas"
    columnas_tabla <- if (incluir_poblacion) outcomes else
      outcomes[outcomes != "Employed_population"]
    encabezados_tabla <- encabezados[match(columnas_tabla, outcomes)]
    for (j in seq_along(encabezados_tabla))
      encabezados_tabla[j] <- sub("\\([0-9]+\\)", paste0("(", j, ")"), encabezados_tabla[j])
    if (!incluir_poblacion)
      encabezados_tabla[match("Employed", columnas_tabla)] <-
        "\\begin{tabular}[c]{@{}c@{}}Employment\\\\ (1)\\end{tabular}"
    nc_tabla <- length(columnas_tabla) + 1L
    nombre_tabla <- if (incluir_poblacion)
      "table9_labor_trans_nb_robustez_8_columnas.tex" else "table9_labor_trans_nb.tex"
    etiqueta_tabla <- if (incluir_poblacion)
      "tab:labor_trans_nb_joint_robustez_8_columnas" else "tab:labor_trans_nb_joint"
  if (nrow(ef)) for (spec in unique(ef$especificacion)) {
    ss <- ef[ef$especificacion == spec, ]
    lineas <- c("\\begin{table}[htpb]",
                "\\caption{Labor Outcomes for Transgender and Non-Binary Respondents}",
                paste0("\\label{", etiqueta_tabla, "}"), "\\centering",
                "\\resizebox{\\textwidth}{!}{%", paste0("\\begin{tabular}{l ", paste(rep("c", length(columnas_tabla)), collapse = " "), "}"), "\\hline",
                paste0("Contrast & ", paste(encabezados_tabla, collapse = " & "), fin), "\\hline")
    for (pa in c("A", "B")) {
      lineas <- c(lineas, paste0("\\multicolumn{", nc_tabla, "}{l}{\\textit{Panel ", pa,
                                 if (pa=="A") ": Non-Binary Respondents" else ": Transgender Respondents",
                                 "}}", fin))
      for (od in sort(unique(ss$orden[ss$panel == pa]))) {
        rr <- ss[ss$orden == od, ]; rr <- rr[match(columnas_tabla, rr$resultado), ]
        celda <- vapply(seq_len(nrow(rr)), function(i) {
          if (is.na(rr$estimacion[i])) "--" else
            sprintf("$%.3f%s$", rr$estimacion[i], estrellas(rr$p_value[i]))
        }, character(1))
        eec <- ifelse(is.na(rr$se), "--", sprintf("$(%.3f)$", rr$se))
        etiqueta <- ss$contraste[match(od, ss$orden)]
        lineas <- c(lineas, paste0(etiqueta, " & ", paste(celda, collapse = " & "), fin),
                    paste0(" & ", paste(eec, collapse = " & "), fin))
      }
      lineas <- c(lineas, "\\hline")
    }
    nn <- vapply(columnas_tabla, function(y) {z <- ss$n[ss$resultado == y]; if (length(z)) z[1] else NA_real_}, numeric(1))
    nota <- paste0("Column (1) estimates employment over the same population used for labor-force participation in column (3), including employed, unemployed and inactive respondents. ",
                   "Observed ages in the final participation sample range from ",rango_edad[1]," to ",rango_edad[2],". ",
                   "Column (2) estimates employment conditional on labor-force participation. Columns (4)--(8) are conditional on employment. ",
                   " Column (8) uses eligible employed respondents from the participation sample with classifiable reconstructed informality. ",
                   "Columns (1)--(6) and (8) use linear probability models. Entries are survey-weighted average contrasts in probability units, without multiplication by 100. ",
                   "Column (7) uses a linear model of log(weekly hours times 4.33) and reports differences in log points. ",
                   "All models jointly include AFAB interacted with explicit transgender status, non-binary status and sexual orientation. Non-binary status has priority over transgender status; the two categories are mutually exclusive. The residual GM category is excluded. Controls are age, age squared, schooling, rural residence, separate marital-status indicators (ecivil 1--8), the household child-count variable (men11c), disability, immigrant status, Indigenous background, non-labor household income, and region and year fixed effects. ",
                   "Unclassifiable gender and sexual-orientation responses are excluded. Other sexual orientation remains included. Contrasts average over each estimation sample, retaining observed sexual orientation and controls. ",
                   "Column (6) equals one when at least 50 percent of weighted employment in the 3-digit CIUO-08 occupation is female (sexo=2). The fixed classification pools employed respondents aged 18--65 from the complete CASEN 2022 and 2024 surveys using expr weights; regression weights remain the SOGI module weights. Unclassifiable occupations are excluded only from column (6). Classification uncertainty is not propagated into regression standard errors. ",
                   esc_tex(fuente_ingreso), " ",
                   "Estimation accounts for sampling weights, strata and primary sampling units; stratified delete-one-PSU jackknife (JKn) covariance is used for coefficients and their linear contrasts. R-squared is the weighted coefficient of determination computed on each estimation sample. ",
                   paste0("The full JKn design contains ", format(numero_replicas_jkn, big.mark = ",", trim = TRUE), " replicate weights. "),
                   paste0("Informality follows the operational algorithm in MDSF (2026), Technical Note 7, for CASEN 2022 and 2024. Boleta rule: ",regla_boleta,". The literal option reproduces the published OR condition; the and option evaluates a documented sensitivity. This is the CASEN proxy, not an exact reproduction of the ENE measure. "),
                   "$^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$; $^{\\dagger}p<0.10$.")
    if (!incluir_poblacion) {
      nota <- gsub(paste0("Column (1) estimates employment over the same population used for labor-force participation in column (3), ",
                         "including employed, unemployed and inactive respondents. "), "", nota, fixed = TRUE)
      for (j in 2:8)
        nota <- gsub(paste0("(", j, ")"), paste0("(", j-1L, ")"), nota, fixed = TRUE)
      nota <- paste("Column (2) reports labor-force participation, including employed, unemployed and inactive respondents.", nota)
    }
    r2 <- vapply(columnas_tabla, function(y) compactos[[y]]$r2, numeric(1))
    lineas <- c(lineas,
                paste0("R$^2$ & ", paste(ifelse(is.na(r2), "--", sprintf("$%.3f$", r2)), collapse = " & "), fin),
                paste0("Num. obs. & ", paste(ifelse(is.na(nn), "--", format(nn, big.mark = ",", trim = TRUE)), collapse = " & "), fin),
                "\\hline", "\\end{tabular}%", "}", "\\par\\smallskip",
                "\\begin{minipage}{\\textwidth}\\footnotesize",
                paste0("\\textit{Note:} ", nota), "\\end{minipage}", "\\end{table}")
    writeLines(lineas, file.path(carpeta, nombre_tabla))
  }
  } # Fin de versiones de la tabla de contrastes.
# APENDICE HORIZONTAL: EJECUTAR SOLO ESTE BLOQUE PARA REEXPORTAR DESDE CSV
# No estima modelos. Requiere los CSV generados por el bucle anterior.
exportar_apendice_labor_jkn <- function(carpeta = "Tables/labor-jkn") {
  cf <- read.csv(file.path(carpeta, "coeficientes_completos.csv"),
                 stringsAsFactors = FALSE, check.names = FALSE)
  reg <- read.csv(file.path(carpeta, "registro_estimacion.csv"),
                  stringsAsFactors = FALSE, check.names = FALSE)
  ys <- c("Employed_population", "Employed", "Labor_force", "Self_employed",
          "Public_sector", "Feminized_sector", "Monthly_hours", "Informal_worker")
  headers <- c("Employment in\\\\population", "Employment among\\\\participants",
               "Labor-force\\\\participation", "Self-employed", "Public sector",
               "Female-dominated\\\\occupation", "Log monthly hours", "Informal worker")
  headers <- paste0("\\shortstack{", headers, "\\\\ (", seq_along(ys), ")}")
  fin <- " \\\\"
  star <- function(p) {
    if (!is.finite(p)) "" else if (p < .001) "^{***}" else
      if (p < .01) "^{**}" else if (p < .05) "^{*}" else
        if (p < .10) "^{\\dagger}" else ""
  }
  numero <- function(x) {
    if (!is.finite(x)) return(NA_character_)
    if (x != 0 && abs(x) < .0005) {
      z <- strsplit(sprintf("%.2e", x), "e", fixed = TRUE)[[1]]
      return(paste0(z[1], "\\times 10^{", as.integer(z[2]), "}"))
    }
    sprintf("%.3f", x)
  }
  etiquetas <- c(
    "(Intercept)" = "Constant", "AFAB" = "AFAB",
    "Trans_explicit" = "Transgender", "NB_explicit" = "Non-binary",
    "LG" = "Gay/Lesbian", "bisexual" = "Bisexual", "Other" = "Other sexual orientation",
    "AFAB:Trans_explicit" = "AFAB $\\times$ Transgender",
    "AFAB:NB_explicit" = "AFAB $\\times$ Non-binary",
    "AFAB:LG" = "AFAB $\\times$ Gay/Lesbian",
    "AFAB:bisexual" = "AFAB $\\times$ Bisexual",
    "AFAB:Other" = "AFAB $\\times$ Other sexual orientation",
    "edad" = "Age", "edad2" = "Age squared", "esc" = "Years of schooling",
    "men11c" = "Household child count (men11c)", "ing_nl" = "Non-labor income per capita",
    "disc_wg" = "Disability", "extranjero" = "Immigrant",
    "pueblos_indigenas" = "Indigenous", "factor(area)2" = "Rural",
    "factor(year)2024" = "Year 2024",
    "factor(ecivil)2" = "Cohabiting without civil union",
    "factor(ecivil)3" = "Civil union", "factor(ecivil)4" = "Annulled",
    "factor(ecivil)5" = "Separated", "factor(ecivil)6" = "Divorced",
    "factor(ecivil)7" = "Widowed", "factor(ecivil)8" = "Single")
  terminos <- unique(cf$termino[!grepl("^factor\\(region\\)", cf$termino)])
  terminos <- c(intersect(names(etiquetas), terminos), setdiff(terminos, names(etiquetas)))
  lineas <- c(
    "% Requiere \\usepackage{adjustbox} en el preambulo.",
    "\\begin{table}[htpb]",
    "\\caption{Linear Probability and Log-Hours Models for Transgender and Non-Binary Respondents}",
    "\\label{tab:labor_trans_nb_full}", "\\centering", "\\begingroup\\small",
    "\\renewcommand{\\arraystretch}{0.9}",
    "\\begin{adjustbox}{max totalsize={\\textwidth}{0.76\\textheight},center}",
    "\\begin{tabular}{l c c c c c c c c}", "\\hline",
    paste0(" & ", paste(headers, collapse = " & "), fin), "\\hline")
  for (tt in terminos) {
    rr <- cf[cf$termino == tt, ]; rr <- rr[match(ys, rr$resultado), ]
    vals <- vapply(seq_along(ys), function(i) {
      z <- numero(rr$estimacion[i])
      if (is.na(z)) "--" else paste0("${", z, "}", star(rr$p_value[i]), "$")
    }, character(1))
    ses <- vapply(rr$se, function(x) {
      z <- numero(x); if (is.na(z)) "--" else paste0("$(", z, ")$")
    }, character(1))
    lab <- if (tt %in% names(etiquetas)) etiquetas[[tt]] else gsub("_", "\\_", tt, fixed = TRUE)
    lineas <- c(lineas, paste0(lab, " & ", paste(vals, collapse = " & "), fin),
                paste0(" & ", paste(ses, collapse = " & "), fin))
  }
  reg <- reg[match(ys, reg$resultado), ]
  r2 <- ifelse(is.finite(reg$r2), sprintf("$%.3f$", reg$r2), "--")
  ns <- ifelse(is.finite(reg$N), format(reg$N, big.mark = ",", trim = TRUE), "--")
  lineas <- c(lineas, "\\hline",
    paste0("R$^2$ & ", paste(r2, collapse = " & "), fin),
    paste0("Num. obs. & ", paste(ns, collapse = " & "), fin), "\\hline",
    paste0("\\multicolumn{9}{l}{\\scriptsize $^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$; $^{\\dagger}p<0.10$.}", fin),
    "\\end{tabular}", "\\end{adjustbox}", "\\endgroup", "\\par\\smallskip",
    "\\begin{minipage}{\\textwidth}\\footnotesize\\raggedright",
    paste0("\\textit{Note:} Authors' calculations using CASEN 2022 and 2024. ",
      "Entries are original regression coefficients, with stratified delete-one-PSU jackknife (JKn) standard errors in parentheses. ",
      "Binary outcomes use linear probability models and are reported in probability units, without multiplication by 100. Column (7) uses log monthly hours. ",
      "These conditional coefficients differ from the average contrasts in the main table. Transgender and non-binary status are mutually exclusive; the residual gender-minority category is excluded. ",
      "Region fixed effects are included but omitted from the table. Marital-status category 1 is the reference. ",
      "Non-labor income per capita is \\texttt{ypchtotcor} minus \\texttt{ypchtrabcor}, in levels; the child-count control is \\texttt{men11c}. ",
      "Female-dominated occupations use the fixed 50-percent classification. R-squared is the weighted coefficient of determination. ",
      "All samples, controls and outcome definitions follow the main table."),
    "\\end{minipage}", "\\end{table}")
  destino <- file.path(carpeta, "table9_labor_trans_nb_full.tex")
  writeLines(lineas, destino)
  message("Tabla guardada en: ", destino)
  invisible(destino)
}
exportar_apendice_labor_jkn("Tables/labor-jkn")
# FIN DEL BLOQUE DE REEXPORTACION

# REGISTRO FINAL --------------------------------------------------------------
capture.output(sessionInfo(), file = file.path(carpeta, "sessionInfo.txt"))
writeLines(c(paste("Formula:", rhs), fuente_ingreso,
  "Estado civil: factor(ecivil), categorias separadas.",
  "Horas: log(hrs * 4.33). Informalidad: algoritmo NT7, regla literal por defecto.",
  "NB y trans mutuamente excluyentes; residuo GM excluido.",
  paste("Replicas JKn del diseno completo:", numero_replicas_jkn),
  "Contrastes promedio ponderados; metodo delta con vcov JKn.",
  "Sin checkpoints ni archivos de modelos RDS; resultados CSV actualizados tras cada modelo.",
  "Modelos gaussianos con enlace identidad: MPL binarios y modelo lineal para log horas.",
  "Contrastes binarios sin multiplicar por 100; R2 ponderado y N una vez al final.",
  "Ocupaciones feminizadas: proporcion ponderada de mujeres >= 0.50, CIUO-08 de 3 digitos; CASEN completa 2022-2024, ocupados de 18-65, pesos expr.",
  "Control de ninos: men11c del panel, sin reconstruir ni modificar su corte de edad."),
  file.path(carpeta, "especificacion.txt"))
resultado_labor_jkn <- list(coeficientes = bind_rows(coefs), contrastes = bind_rows(efectos),
                           registro = bind_rows(registro), modelos = compactos)
print(resultado_labor_jkn$registro, row.names = FALSE)
cat("\nResultados en:", normalizePath(carpeta), "\n")
