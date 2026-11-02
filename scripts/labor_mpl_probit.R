# =============================================================================
# LABOR: MPL PRINCIPAL Y PROBIT DE ROBUSTEZ, CASEN 2022-2024
# =============================================================================

library(dplyr)
library(haven)
library(survey)

#  CONFIGURACION -------------------------------------------------------------
archivo <- "Intermediates/Panel_post.csv"
carpeta_datos <- "Data"
carpeta_raiz <- "Tables/labor"
carpeta <- carpeta_raiz


prefijo_salida <- ""
ruta_principal <- function(directorio, nombre) {
  file.path(directorio, paste0(prefijo_salida, nombre))
}
ruta_desag <- function(directorio, nombre) {
  file.path(directorio, paste0(prefijo_salida, "desagregacion_", nombre))
}
ruta_sector <- function(directorio, nombre) {
  file.path(directorio, paste0(prefijo_salida, "sector_", nombre))
}


comparar_ocupaciones <- TRUE # requiere ocupaciones_feminizadas_casen.R
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
  if (!is.na(id) && id %in% names(raw)) {
    clave_csv <- paste(texto_id(raw$folio[idx]), texto_id(raw[[id]][idx]), sep = "|")
    clave_sav <- paste(texto_id(z$folio), texto_id(z[[id]]), sep = "|")
    z <- z[match(clave_csv, clave_sav), , drop = FALSE]
  }
  partes[[as.character(anio)]] <- cbind(z[fuentes], year = anio, fila_CSV = idx)
  mapas[[as.character(anio)]] <- data.frame(
    fila_CSV = idx, year = anio, folio = texto_id(z$folio),
    identificador = id, id_persona = if (!is.na(id)) texto_id(z[[id]]) else rep(NA_character_, nrow(z)), fila_SAV = z$.fila_SAV
  )
}
fuentes_nt7 <- bind_rows(partes)
fuentes_nt7 <- fuentes_nt7[match(seq_len(nrow(raw)), fuentes_nt7$fila_CSV), ]
write.csv(bind_rows(mapas), ruta_principal(carpeta, "mapa_personas.csv"), row.names = FALSE)
nt7 <- reconstruir_informalidad_nt7(fuentes_nt7)

# VARIABLES Y MUESTRAS ------------------------------------------------------
  profesiones_feminizadas <- c(
    "1341", "1344", "2221", "2341", "2342", "2352", "2423", "2424",
    "2634", "2635", "3221", "3313", "3342", "3343", "3344", "3412",
    "3611", "4120", "4211", "4224", "4226", "4411", "4416", "5111",
    "5120", "5131", "5141", "5142", "5151", "5152", "5223", "5230",
    "5242", "5246", "5311", "5312", "5321", "5322", "5329", "7531",
    "7533", "8152", "8154", "9111", "9112", "9129", "9411"
  )
  
  
  

d <- raw
conocido_os <- d$os1 %in% 1:4
conocido_g <- d$genero %in% 1:6 &
  ((d$GM %in% 1 & d$cis %in% 0) | (d$GM %in% 0 & d$cis %in% 1))
d$AFAB <- ifelse(d$sexo %in% 2, 1L, 0L)
d$GM <- ifelse(conocido_g, as.integer(d$GM %in% 1), NA_integer_)
d$LG <- ifelse(conocido_os, as.integer(d$os1 %in% 2), NA_integer_)
d$bisexual <- ifelse(conocido_os, as.integer(d$os1 %in% 3), NA_integer_)
d$Other <- ifelse(conocido_os, as.integer(d$os1 %in% 4), NA_integer_)

# Ingreso no laboral PER CAPITA en niveles.
d$ypchtotcor <- fuentes_nt7$ypchtotcor
d$ypchtrabcor <- fuentes_nt7$ypchtrabcor
fuentes_validas <- is.finite(d$ypchtotcor) & is.finite(d$ypchtrabcor) &
  !(d$ypchtotcor %in% c(-88, -99)) & !(d$ypchtrabcor %in% c(-88, -99))
d$ing_nl <- with(d, ypchtotcor - ypchtrabcor)
d$ing_nl[!fuentes_validas | !is.finite(d$ing_nl)] <- NA_real_

fuente_ingreso <- paste(
  "Non-labor household income per capita is ypchtotcor minus ypchtrabcor,",
  "in levels. Both sources are already per capita; missing or invalid",
  "sources are not imputed as zero. Valid negative differences are retained."
)
resumen_ingreso <- bind_rows(lapply(c(2022, 2024), function(anio) {
  x <- d$ing_nl[d$year == anio]
  data.frame(year = anio, N = length(x), N_validos = sum(is.finite(x)),
    N_faltantes = sum(!is.finite(x)), N_cero = sum(x == 0, na.rm = TRUE),
    N_negativos = sum(x < 0, na.rm = TRUE))
}))
write.csv(resumen_ingreso, file.path(carpeta_raiz, "resumen_ingreso_no_laboral.csv"), row.names = FALSE)
print(resumen_ingreso, row.names = FALSE)

d$Employed_population <- case_when(d$activ == 1 ~ 1L, d$activ %in% c(2,3) ~ 0L, TRUE ~ NA_integer_)
d$Employed <- case_when(d$activ == 1 ~ 1L, d$activ == 2 ~ 0L, TRUE ~ NA_integer_)
d$Labor_force <- case_when(d$activ %in% c(1,2) ~ 1L, d$activ == 3 ~ 0L, TRUE ~ NA_integer_)
d$Self_employed <- ifelse(d$activ %in% 1, as.integer(d$o15 %in% c(1,2)), NA_integer_)
d$Public_sector <- ifelse(d$activ %in% 1, as.integer(d$o15 %in% c(3,4,8)), NA_integer_)
d$Feminized_sector <- ifelse(d$activ %in% 1, as.integer(d$CIIU %in% profesiones_feminizadas), NA_integer_)
d$Monthly_hours <- NA_real_
horas_validas <- d$activ %in% 1 & is.finite(d$hrs) & d$hrs > 0
d$Monthly_hours[horas_validas] <- log(d$hrs[horas_validas] * 4.33)
d$Informal_worker <- if (regla_boleta == "literal") nt7$informal_nt7 else nt7$informal_nt7_and

outcomes <- c("Employed_population", "Employed", "Labor_force", "Self_employed",
              "Public_sector", "Feminized_sector", "Monthly_hours", "Informal_worker")

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

rango_edad <- range(
  d$edad[mascaras$Labor_force],
  na.rm = TRUE
)
write.csv(cbind(fila_CSV = seq_len(nrow(d)), year = d$year, nt7),
          ruta_principal(carpeta, "clasificacion_informalidad.csv"), row.names = FALSE)

#  ESPECIFICACION Y DISEñO COMPLEJO ------------------------------------------
rhs <- paste("AFAB * (GM + LG + bisexual + Other)",
             "edad + edad2 + esc + factor(area) + factor(ecivil)",
             "men11c + disc_wg + extranjero + pueblos_indigenas",
             "factor(region) + factor(year) + ing_nl", sep = " + ")
diseno <- svydesign(ids = ~varunit_panel, strata = ~varstrat_panel,
                    weights = ~expr_osig, nest = TRUE, data = d)

# =============================================================================
# DESCRIPTIVOS ADICIONALES: INGRESO, N POR SUBGRUPO E INFORMALIDAD
# =============================================================================
crear_descriptivos_labor <- function() {
  peso <- d$expr_osig
  peso_valido <- is.finite(peso) & peso > 0
  universo <- d$year %in% c(2022, 2024) & is.finite(d$edad) &
    d$edad >= 18 & d$edad <= 65 & peso_valido
  ingreso_valido <- is.finite(d$ing_nl)
  proporcion <- function(numerador, denominador, ponderado = TRUE) {
    a <- numerador %in% TRUE; b <- denominador %in% TRUE
    q <- if (ponderado) peso else rep(1, nrow(d))
    den <- sum(q[b], na.rm = TRUE)
    if (den > 0) 100 * sum(q[a & b], na.rm = TRUE) / den else NA_real_
  }
  grupos <- list()
  agregar_grupo <- function(dimension, nombre, pertenece) {
    grupos[[length(grupos) + 1L]] <<- list(
      dimension = dimension, grupo = nombre, pertenece = pertenece %in% TRUE)
  }
  agregar_grupo("Total", "Total", rep(TRUE, nrow(d)))
  agregar_grupo("Genero agregado", "GM total", conocido_g & d$GM %in% 1)
  agregar_grupo("Genero agregado", "Cis total", conocido_g & d$GM %in% 0)
  agregar_grupo("Genero agregado", "Genero no clasificable", !conocido_g)
  for (sexo in c(1, 2)) for (gm in c(0, 1)) {
    agregar_grupo("Sexo y genero", paste(if (sexo == 1) "AMAB" else "AFAB",
      if (gm == 0) "cis" else "GM"), conocido_g & d$sexo %in% sexo & d$GM %in% gm)
  }
  nombres_os <- c("Heterosexual", "LG", "Bisexual", "Other")
  agregar_grupo("Orientacion agregada", "Diversidad sexual total (LG, bisexual, Other)",
                conocido_os & d$os1 %in% 2:4)
  agregar_grupo("Orientacion agregada", "Orientacion no clasificable", !conocido_os)
  for (os in 1:4) {
    agregar_grupo("Orientacion sexual", nombres_os[os], conocido_os & d$os1 %in% os)
    for (sexo in c(1, 2)) agregar_grupo("Sexo y orientacion",
      paste(if (sexo == 1) "AMAB" else "AFAB", nombres_os[os]),
      conocido_os & d$sexo %in% sexo & d$os1 %in% os)
    agregar_grupo("Interseccion GM y orientacion", paste("GM", nombres_os[os]),
      conocido_g & d$GM %in% 1 & conocido_os & d$os1 %in% os)
  }
  periodos <- list(`2022-2024` = rep(TRUE, nrow(d)),
                   `2022` = d$year %in% 2022, `2024` = d$year %in% 2024)
  completos_formula <- function(f) {
    mf <- model.frame(f, data = d, na.action = na.pass)
    ok <- complete.cases(mf)
    for (v in names(mf)) if (is.numeric(mf[[v]])) ok <- ok & is.finite(mf[[v]])
    ok
  }
 
  antes_ingreso <- finales <- list()
  
  for (y in outcomes) {
    
    base <- (
      mascara_universo(y) &
        conocido_os &
        conocido_g &
        d$ecivil %in% 1:8 &
        is.finite(d[[y]])
    )
    
    f <- as.formula(paste(y, "~", rhs))
    
    # Todos los requisitos del modelo, excepto ingreso no laboral.
    antes_ingreso[[y]] <- (
      base &
        completos_formula(update(f, . ~ . - ing_nl))
    )
    
    # Muestra final: exige tambien ingreso no laboral valido.
    finales[[y]] <- (
      mascaras[[y]] %in% TRUE &
        completos_formula(f)
    )
  }
  
  tablas_ingreso <- tablas_n <- list()
  k <- 0L; j <- 0L
  for (periodo in names(periodos)) for (g in grupos) {
    dominio <- periodos[[periodo]] & g$pertenece
    # Universo descriptivo antes de cualquier filtro de controles/SOGI;
    # permite observar faltantes que las regresiones ya excluyeron.
    etapas <- c(list(Poblacion_18_65_antes_controles = universo), antes_ingreso)
    for (etapa in names(etapas)) {
      sel <- dominio & etapas[[etapa]]
      k <- k + 1L
      tablas_ingreso[[k]] <- data.frame(
        periodo = periodo, dimension = g$dimension, grupo = g$grupo, etapa = etapa,
        N_antes_ingreso = sum(sel), N_ingreso_valido = sum(sel & ingreso_valido),
        N_ingreso_faltante_o_invalido = sum(sel & !ingreso_valido),
        porcentaje_N_sin_ingreso = proporcion(!ingreso_valido, sel, FALSE),
        porcentaje_ponderado_sin_ingreso = proporcion(!ingreso_valido, sel),
        N_ingreso_cero = sum(sel & ingreso_valido & d$ing_nl %in% 0),
        N_ingreso_negativo = sum(sel & ingreso_valido & (d$ing_nl < 0), na.rm = TRUE))
    }
    for (y in outcomes) {
      sel <- finales[[y]] & periodos[[periodo]]
      j <- j + 1L
      tablas_n[[j]] <- data.frame(
        periodo = periodo, dimension = g$dimension, grupo = g$grupo, resultado = y,
        N_subgrupo = sum(sel & g$pertenece), N_total_modelo_periodo = sum(sel),
        porcentaje_N_modelo = proporcion(g$pertenece, sel, FALSE),
        porcentaje_ponderado_modelo = proporcion(g$pertenece, sel))
    }
  }
  ingreso <- bind_rows(tablas_ingreso)
  conteos <- bind_rows(tablas_n)
  write.csv(ingreso, file.path(carpeta_raiz, "diagnostico_ingreso_no_laboral_subgrupos.csv"), row.names = FALSE)
  write.csv(conteos, file.path(carpeta_raiz, "N_subgrupos_muestras_estimacion.csv"), row.names = FALSE)

  
  fin <- " \\\\" 
  tex <- c("% Requiere booktabs; conteos no ponderados.", "\\begin{table}[htbp]", "\\centering",
    "\\caption{Unweighted subgroup counts in the labor estimation samples}",
    "\\label{tab:labor_subgroup_counts}", "\\resizebox{\\textwidth}{!}{%",
    "\\begin{tabular}{lrrrrrrrr}", "\\toprule",
    paste0("Group & ", paste(seq_along(outcomes), collapse = " & "), fin), "\\midrule")
  pooled <- conteos[conteos$periodo == "2022-2024", ]
  for (dimension in unique(pooled$dimension)) {
    tex <- c(tex, paste0("\\multicolumn{9}{l}{\\textit{", dimension, "}}", fin))
    for (grupo in unique(pooled$grupo[pooled$dimension == dimension])) {
      z <- pooled[pooled$dimension == dimension & pooled$grupo == grupo, ]
      z <- z[match(outcomes, z$resultado), ]
      tex <- c(tex, paste0(grupo, " & ", paste(format(z$N_subgrupo,
        big.mark = ",", trim = TRUE, scientific = FALSE), collapse = " & "), fin))
    }
    tex <- c(tex, "\\midrule")
  }
  tex <- c(tex, "\\bottomrule", "\\end{tabular}%", "}",
    "\\par\\smallskip\\footnotesize\\textit{Note:} Counts refer to observed respondents, not survey-expanded totals. Columns match the eight-column robustness table: (1) employment in the population; (2) employment among participants; (3) participation; (4) self-employment; (5) public sector; (6) original female-dominated occupation measure; (7) log monthly hours; (8) informality. MPL and Probit use identical samples within each outcome. Dimensions and aggregate rows overlap and must not be added together. The occupation-definition comparison can use a smaller common sample and is not represented in column (6) here.",
    "\\end{table}")
  writeLines(tex, file.path(carpeta_raiz, "tabla_N_subgrupos.tex"), useBytes = TRUE)

  # Tasas descriptivas, no predicciones de los modelos.
  # Base amplia: poblacion SOGI 18-65 con peso valido, sin exigir controles.
  # Base de estimacion: muestra final de participacion, para evaluar seleccion.
  bases <- list(Poblacion_SOGI_18_65 = universo,
                Muestra_final_participacion = finales$Labor_force & universo)
  tasas <- list(); k <- 0L
  for (periodo in names(periodos)) for (base_nombre in names(bases)) for (g in grupos) {
    sel <- bases[[base_nombre]] & periodos[[periodo]] & g$pertenece
    actividad_conocida <- d$activ %in% 1:3
    ocupados <- sel & d$activ %in% 1
    clasificados <- ocupados & d$Informal_worker %in% 0:1
    informales <- ocupados & d$Informal_worker %in% 1
    desconocidos <- ocupados & !(d$Informal_worker %in% 0:1)
    poblacion_con_actividad <- sel & actividad_conocida
    poblacion_clasificable <- sel & (d$activ %in% c(2, 3) |
      (d$activ %in% 1 & d$Informal_worker %in% 0:1))
    k <- k + 1L
    tasas[[k]] <- data.frame(
      periodo = periodo, universo = base_nombre, dimension = g$dimension, grupo = g$grupo,
      N_poblacion = sum(sel), N_actividad_desconocida = sum(sel & !actividad_conocida),
      N_ocupados = sum(ocupados), N_ocupados_clasificados = sum(clasificados),
      N_informales = sum(informales), N_ocupados_sin_clasificar = sum(desconocidos),
      N_poblacion_con_actividad = sum(poblacion_con_actividad),
      N_poblacion_clasificable = sum(poblacion_clasificable),
      porcentaje_informal_entre_ocupados_clasificados = proporcion(informales, clasificados),
      porcentaje_ocupados_sin_clasificar = proporcion(desconocidos, ocupados),
      porcentaje_empleo_informal_poblacion_clasificable = proporcion(informales, poblacion_clasificable),
      limite_inferior_informal_entre_ocupados = proporcion(informales, ocupados),
      limite_superior_informal_entre_ocupados = proporcion(informales | desconocidos, ocupados),
      limite_inferior_empleo_informal_poblacion = proporcion(informales, poblacion_con_actividad),
      limite_superior_empleo_informal_poblacion = proporcion(informales | desconocidos, poblacion_con_actividad))
  }
  informalidad <- bind_rows(tasas)
  write.csv(informalidad, file.path(carpeta_raiz, "tasas_informalidad_poblacion_subgrupos.csv"), row.names = FALSE)
  writeLines(c(
    "DESCRIPTIVOS ADICIONALES - NO MODIFICAN LAS ESTIMACIONES",
    "N: conteos no ponderados. Porcentajes ponderados: expr_osig, salvo columnas expresamente porcentaje_N.",
    "Se reportan 2022, 2024 y ambas olas agrupadas; el agrupado usa los pesos originales, sin normalizacion por ola.",
    "Las dimensiones se solapan: GM y diversidad sexual no son grupos excluyentes. No sumar sus filas.",
    "INGRESO: la base descriptiva incluye no clasificables; cada fila de modelo muestra la muestra antes de exigir ingreso valido, manteniendo los demas requisitos.",
    "Esto separa las perdidas exclusivamente por ing_nl de las perdidas ya causadas por controles, filtros historicos o clasificacion SOGI.",
    "Los faltantes incluyen fuentes invalidas; los ingresos validos iguales a cero no se excluyen.",
    "N DE MODELOS: muestras efectivas reconstruidas con la misma formula y mascara de cada regresion; comunes a MPL y Probit.",
    "INFORMALIDAD: tasas observadas, no ajustadas. La base amplia no exige ingresos ni controles completos; la segunda base exige la muestra final de participacion.",
    "La tasa habitual es informales / ocupados con informalidad clasificable. No equivale al porcentaje de empleo informal sobre toda la poblacion.",
    "El porcentaje de empleo informal en poblacion clasificable incluye no ocupados en el denominador, como no empleados informales; no los llama trabajadores formales.",
    "Los ocupados sin clasificacion no se imputan formales: se muestran su N, porcentaje y limites suponiendolos todos formales o todos informales.",
    "Los limites no son intervalos de confianza. El denominador poblacional de los limites exige actividad conocida.",
    "Las tasas describen la poblacion del modulo SOGI de 18-65 anos; no son directamente comparables con una tasa nacional de ocupados de 15 anos o mas.",
    "La precision de grupos pequenos requiere revisar los N; este archivo no incluye errores estandar."
  ), file.path(carpeta_raiz, "notas_descriptivos_subgrupos.txt"))
  cat("\nDescriptivos guardados en", normalizePath(carpeta_raiz), "\n")
  invisible(list(ingreso = ingreso, conteos = conteos, informalidad = informalidad))
}
descriptivos_labor <- crear_descriptivos_labor()

#  CONTRASTES PROMEDIO PONDERADOS Y METODO DELTA ------------------------------
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
      escenario_1 = list(AFAB = 1, GM = 0),
      escenario_0 = list(AFAB = 0, GM = 0)
    ),
    list(
      orden = 2, panel = "A",
      contraste = "AMAB GM vs AMAB cis",
      escenario_1 = list(AFAB = 0, GM = 1),
      escenario_0 = list(AFAB = 0, GM = 0)
    ),
    list(
      orden = 3, panel = "A",
      contraste = "AFAB GM vs AMAB cis",
      escenario_1 = list(AFAB = 1, GM = 1),
      escenario_0 = list(AFAB = 0, GM = 0)
    ),
    list(
      orden = 4, panel = "A",
      contraste = "AFAB GM vs AFAB cis",
      escenario_1 = list(AFAB = 1, GM = 1),
      escenario_0 = list(AFAB = 1, GM = 0)
    ),
    list(
      orden = 5, panel = "A",
      contraste = "AFAB GM vs AMAB GM",
      escenario_1 = list(AFAB = 1, GM = 1),
      escenario_0 = list(AFAB = 0, GM = 1)
    ),
    list(
      orden = 6, panel = "B",
      contraste = "AMAB LG vs AMAB heterosexual",
      escenario_1 = list(AFAB = 0, LG = 1, bisexual = 0, Other = 0),
      escenario_0 = list(AFAB = 0, LG = 0, bisexual = 0, Other = 0)
    ),
    list(
      orden = 7, panel = "B",
      contraste = "AFAB LG vs AFAB heterosexual",
      escenario_1 = list(AFAB = 1, LG = 1, bisexual = 0, Other = 0),
      escenario_0 = list(AFAB = 1, LG = 0, bisexual = 0, Other = 0)
    ),
    list(
      orden = 8, panel = "B",
      contraste = "AMAB bisexual vs AMAB heterosexual",
      escenario_1 = list(AFAB = 0, LG = 0, bisexual = 1, Other = 0),
      escenario_0 = list(AFAB = 0, LG = 0, bisexual = 0, Other = 0)
    ),
    list(
      orden = 9, panel = "B",
      contraste = "AFAB bisexual vs AFAB heterosexual",
      escenario_1 = list(AFAB = 1, LG = 0, bisexual = 1, Other = 0),
      escenario_0 = list(AFAB = 1, LG = 0, bisexual = 0, Other = 0)
    ),
    list(
      orden = 10, panel = "B",
      contraste = "AMAB other vs AMAB heterosexual",
      escenario_1 = list(AFAB = 0, LG = 0, bisexual = 0, Other = 1),
      escenario_0 = list(AFAB = 0, LG = 0, bisexual = 0, Other = 0)
    ),
    list(
      orden = 11, panel = "B",
      contraste = "AFAB other vs AFAB heterosexual",
      escenario_1 = list(AFAB = 1, LG = 0, bisexual = 0, Other = 1),
      escenario_0 = list(AFAB = 1, LG = 0, bisexual = 0, Other = 0)
    )
  )
  
  calcular_contrastes_modelo <- function(nombre_resultado, modelo, horas_log = FALSE) {
    es_lineal <- modelo$family$family == "gaussian" && modelo$family$link == "identity"
    multiplicador <- if (nombre_resultado == "Monthly_hours" || es_lineal) 1 else 100
    unidad <- if (nombre_resultado == "Monthly_hours") {
      if (horas_log) "log points" else "hours"
    } else if (es_lineal) "probability units" else "percentage points"
    
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
  
  
  

# =============================================================================
# COMPARACION OCUPACIONAL: FUNCIONES AUXILIARES PARA AMBOS ESTIMADORES
# =============================================================================

.sf_cache <- new.env(parent = emptyenv())

sf_clasificador <- function(archivo = "scripts/ocupaciones_feminizadas_casen.R") {
  if (!exists("modulo", envir = .sf_cache, inherits = FALSE)) {
    # Aislamiento: el script de clasificacion no sobrescribe carpeta, d, rhs,
    # diseno ni otros objetos del script de regresiones.
    e <- new.env(parent = globalenv())
    source(archivo, local = e, encoding = "UTF-8")
    .sf_cache$modulo <- e
  }
  .sf_cache$modulo
}

sf_lista_antigua <- c(
  "1341", "1344", "2221", "2341", "2342", "2352", "2423", "2424",
  "2634", "2635", "3221", "3313", "3342", "3343", "3344", "3412",
  "3611", "4120", "4211", "4224", "4226", "4411", "4416", "5111",
  "5120", "5131", "5141", "5142", "5151", "5152", "5223", "5230",
  "5242", "5246", "5311", "5312", "5321", "5322", "5329", "7531",
  "7533", "8152", "8154", "9111", "9112", "9129", "9411")

sf_agregar <- function(diseno, modulo, var_ocupacion = "CIIU",
                       lista_antigua = sf_lista_antigua) {
  x <- modulo$agregar_ocupacion_feminizada(
    diseno$variables, resultado = modulo$resultado_ocupaciones,
    var_ocupacion = var_ocupacion)

  x$SF_antigua <- if ("Feminized_sector" %in% names(x)) {
    x$Feminized_sector
  } else {
    ifelse(x$activ %in% 1,
           as.integer(x[[var_ocupacion]] %in% lista_antigua), NA_integer_)
  }
  x$SF_nueva <- x$Female_dominated
  
  diseno$variables <- x
  diseno
}

sf_completos <- function(formula, datos) {
  mf <- model.frame(formula, data = datos, na.action = na.pass)
  ok <- complete.cases(mf)
  for (v in names(mf)) if (is.numeric(mf[[v]])) ok <- ok & is.finite(mf[[v]])
  ok
}

sf_escenarios <- list(
  list("A", "AFAB cis vs AMAB cis", list(AFAB=1,GM=0), list(AFAB=0,GM=0)),
  list("A", "AMAB GM vs AMAB cis", list(AFAB=0,GM=1), list(AFAB=0,GM=0)),
  list("A", "AFAB GM vs AMAB cis", list(AFAB=1,GM=1), list(AFAB=0,GM=0)),
  list("A", "AFAB GM vs AFAB cis", list(AFAB=1,GM=1), list(AFAB=1,GM=0)),
  list("A", "AFAB GM vs AMAB GM", list(AFAB=1,GM=1), list(AFAB=0,GM=1)),
  list("B", "AMAB LG vs AMAB heterosexual", list(AFAB=0,LG=1,bisexual=0,Other=0), list(AFAB=0,LG=0,bisexual=0,Other=0)),
  list("B", "AFAB LG vs AFAB heterosexual", list(AFAB=1,LG=1,bisexual=0,Other=0), list(AFAB=1,LG=0,bisexual=0,Other=0)),
  list("B", "AMAB bisexual vs AMAB heterosexual", list(AFAB=0,LG=0,bisexual=1,Other=0), list(AFAB=0,LG=0,bisexual=0,Other=0)),
  list("B", "AFAB bisexual vs AFAB heterosexual", list(AFAB=1,LG=0,bisexual=1,Other=0), list(AFAB=1,LG=0,bisexual=0,Other=0)),
  list("B", "AMAB other vs AMAB heterosexual", list(AFAB=0,LG=0,bisexual=0,Other=1), list(AFAB=0,LG=0,bisexual=0,Other=0)),
  list("B", "AFAB other vs AFAB heterosexual", list(AFAB=1,LG=0,bisexual=0,Other=1), list(AFAB=1,LG=0,bisexual=0,Other=0)))

sf_contrastes <- function(m, resultado, multiplicador = 1) {
  b <- coef(m); V <- vcov(m)
  q <- as.numeric(weights(m$survey.design, type = "sampling")); q <- q / sum(q)
  pred <- function(cambios) {
    x <- m$survey.design$variables
    for (v in names(cambios)) x[[v]] <- cambios[[v]]
    X <- model.matrix(delete.response(terms(m)), data=x,
                      contrasts.arg=m$contrasts, xlev=m$xlevels)
    X <- X[, names(b), drop=FALSE]
    eta <- as.numeric(X %*% b)
    list(mu=sum(q*m$family$linkinv(eta)),
         grad=colSums(X*as.numeric(q*m$family$mu.eta(eta))))
  }
  do.call(rbind, lapply(seq_along(sf_escenarios), function(i) {
    s <- sf_escenarios[[i]]; a <- pred(s[[3]]); z <- pred(s[[4]])
    g <- a$grad-z$grad; beta <- a$mu-z$mu
    se <- sqrt(as.numeric(t(g)%*%V%*%g))
    df <- survey::degf(m$survey.design)
    data.frame(orden=i, panel=s[[1]], contraste=s[[2]], resultado=resultado,
      estimacion=beta*multiplicador, se=se*multiplicador,
      p_value=2*pt(abs(beta/se), df=df, lower.tail=FALSE), n=nobs(m))
  }))
}

sf_coeficientes <- function(m, resultado) {
  a <- coef(summary(m))
  data.frame(resultado=resultado, termino=rownames(a), estimacion=a[,1],
    se=a[,2], estadistico=a[,3], p_value=a[,4], n=nobs(m), row.names=NULL)
}

sf_estrellas <- function(p) {
  if (is.na(p)) "" else if (p<.001) "^{***}" else if (p<.01) "^{**}" else
    if (p<.05) "^{*}" else if (p<.10) "^{\\dagger}" else ""
}
sf_num <- function(x,p) if (is.na(x)) "--" else sprintf("$%.3f%s$",x,sf_estrellas(p))
sf_tex <- function(x) gsub("_", "\\_", x, fixed=TRUE)

# R2 ponderado convencional
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

titulo_labor <- "Labor Outcomes by Gender and Sexual diversity"
encabezados_labor <- c(
  "\\begin{tabular}[c]{@{}c@{}}Employment in\\\\ population\\\\ (1)\\end{tabular}",
  "\\begin{tabular}[c]{@{}c@{}}Employment among\\\\ participants\\\\ (2)\\end{tabular}",
  "\\begin{tabular}[c]{@{}c@{}}Labor-force\\\\ participation\\\\ (3)\\end{tabular}",
  "\\begin{tabular}[c]{@{}c@{}}Self-employed\\\\ (4)\\end{tabular}",
  "\\begin{tabular}[c]{@{}c@{}}Public sector\\\\ (5)\\end{tabular}",
  "\\begin{tabular}[c]{@{}c@{}}Female-dominated\\\\ occupation\\\\ (6)\\end{tabular}",
  "\\begin{tabular}[c]{@{}c@{}}Log monthly\\\\ hours\\\\ (7)\\end{tabular}",
  "\\begin{tabular}[c]{@{}c@{}}Informal worker\\\\ (8)\\end{tabular}"
)

sf_tabla <- function(ef, columnas, cabeceras, titulo, etiqueta, nota, ruta,
                     ajustes = NULL, incluir_poblacion = FALSE) {
  # Exportar la version completa por separado y mostrar siete resultados
  # en las tablas principales. No cambia ningun modelo ni muestra.
  if (!incluir_poblacion && "Employed_population" %in% columnas) {
    ruta_robustez <- sub("\\.tex$", "_robustez_8_columnas.tex", ruta)
    sf_tabla(ef, columnas, cabeceras,
      paste0(titulo, ": Including Employment in the Population"),
      paste0(etiqueta, "_robustez_8_columnas"), nota, ruta_robustez,
      ajustes = ajustes, incluir_poblacion = TRUE)
    conservar <- columnas != "Employed_population"
    columnas <- columnas[conservar]
    cabeceras <- cabeceras[conservar]
    for (j in seq_along(cabeceras))
      cabeceras[j] <- sub("\\([0-9]+\\)", paste0("(", j, ")"), cabeceras[j])
    cabeceras[match("Employed", columnas)] <-
      "\\begin{tabular}[c]{@{}c@{}}Employment\\\\ (1)\\end{tabular}"

    # Quitar la descripcion de la columna eliminada y renumerar las demas.
    nota <- gsub(paste0("Column (1) estimates employment over the same population used for labor-force participation in column (3), ",
                       "including employed, unemployed and inactive respondents. "),
                 "", nota, fixed = TRUE)
    frase_sector <- paste0("Columns (1), (2), and (3) report employment in the population, ",
                           "employment among participants, and participation, respectively.")
    tiene_frase_sector <- grepl(frase_sector, nota, fixed = TRUE)
    nota <- gsub(frase_sector, "", nota, fixed = TRUE)
    for (j in 2:8) nota <- gsub(paste0("(", j, ")"), paste0("(", j-1L, ")"), nota, fixed = TRUE)
    if (tiene_frase_sector) nota <- paste(
      "Column (1) reports employment conditional on labor-force participation; column (2) reports labor-force participation.", nota)
  }

  fin <- " \\\\"; nc <- length(columnas) + 1L
  nn <- vapply(columnas, function(y) ef$n[match(y, ef$resultado)], numeric(1))
  r2 <- rep(NA_real_, length(columnas))
  if (!is.null(ajustes)) r2 <- ajustes$r2[match(columnas, ajustes$resultado)]
  formato_r2 <- ifelse(is.finite(r2), sprintf("$%.3f$", r2), "--")
  formato_n <- ifelse(is.na(nn), "--", paste0("$", format(nn,
    big.mark = ",", trim = TRUE, scientific = FALSE), "$"))
  out <- c("\\begin{table}[htpb!]", paste0("\\caption{", titulo, "}"),
    paste0("\\label{", etiqueta, "}"), "\\centering",
    "\\resizebox{\\textwidth}{!}{%",
    paste0("\\begin{tabular}{l ", paste(rep("c", nc-1L), collapse = " "), "}"),
    "\\hline", paste0(" & ", paste(cabeceras, collapse = " & "), fin), "\\hline")
  for (pa in c("A", "B")) {
    titulo_panel <- if (pa == "A") "Assigned Sex and Gender Minority Status" else
      "Sexual Orientation Within Assigned Sex"
    out <- c(out, paste0("\\multicolumn{", nc, "}{l}{\\textit{Panel ", pa,
                         ": ", titulo_panel, "}}", fin))
    for (od in sort(unique(ef$orden[ef$panel == pa]))) {
      r <- ef[ef$orden == od, ]; r <- r[match(columnas, r$resultado), ]
      out <- c(out, paste0(sf_escenarios[[od]][[2]], " & ",
        paste(mapply(sf_num, r$estimacion, r$p_value), collapse = " & "), fin),
        paste0(" & ", paste(ifelse(is.na(r$se), "--", sprintf("$(%.3f)$", r$se)),
                             collapse = " & "), fin))
    }
    out <- c(out, "\\hline")
    if (any(is.finite(r2))) out <- c(out,
      paste0("R$^2$ & ", paste(formato_r2, collapse = " & "), fin))
    out <- c(out, paste0("Num. obs. & ", paste(formato_n, collapse = " & "), fin), "\\hline")
  }
  nota_ajuste <- paste(
    "Panels A and B report contrasts from the same fitted model within each column;",
    "sample sizes and fit statistics are repeated for presentation.")
  if (any(is.finite(r2))) nota_ajuste <- paste(nota_ajuste,
    "R-squared is one minus the ratio of the survey-weighted residual sum of squares",
    "to the survey-weighted total sum of squares, on the estimation scale.")
  if (any(!is.finite(r2))) nota_ajuste <- paste(nota_ajuste,
    "A dash indicates that conventional R-squared is not reported for probit models or is undefined.")
  if (!grepl("$^{***}", nota, fixed = TRUE)) nota <- paste(nota,
    "$^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$; $^{\\dagger}p<0.10$.")
  out <- c(out, "\\end{tabular}%", "}", "\\par\\vspace{1ex}",
    "\\begin{minipage}{\\textwidth}\\footnotesize\\raggedright",
    paste0("\\textit{Note:} ", nota, " ", nota_ajuste),
    "\\end{minipage}", "\\end{table}")
  writeLines(out, ruta, useBytes = TRUE)
}

# Tablas completas: coeficientes originales, no contrastes ni efectos marginales.
sf_apendice <- function(cf, ruta, ajustes = NULL, metodo = "MPL",
                        nota_extra = "", etiqueta = NULL) {
  columnas <- outcomes[outcomes %in% unique(cf$resultado)]
  cabeceras <- encabezados_labor[match(columnas, outcomes)]
  cf <- cf[!grepl("^factor\\(region\\)", cf$termino), , drop = FALSE]
  etiquetas <- c(
    "(Intercept)" = "Constant", "AFAB" = "AFAB", "GM" = "Gender minority",
    "LG" = "Gay/Lesbian", "bisexual" = "Bisexual", "Other" = "Other sexual orientation",
    "AFAB:GM" = "AFAB $\\times$ Gender minority",
    "AFAB:LG" = "AFAB $\\times$ Gay/Lesbian",
    "AFAB:bisexual" = "AFAB $\\times$ Bisexual",
    "AFAB:Other" = "AFAB $\\times$ Other sexual orientation",
    "edad" = "Age", "edad2" = "Age squared", "esc" = "Years of schooling",
    "men11c" = "Household child count (men11c)", "disc_wg" = "Disability",
    "extranjero" = "Immigrant", "pueblos_indigenas" = "Indigenous",
    "factor(area)2" = "Rural", "factor(year)2024" = "Year 2024",
    "ing_nl" = "Non-labor income per capita",
    "factor(ecivil)2" = "Marital status: category 2",
    "factor(ecivil)3" = "Marital status: category 3",
    "factor(ecivil)4" = "Marital status: category 4",
    "factor(ecivil)5" = "Separated", "factor(ecivil)6" = "Divorced",
    "factor(ecivil)7" = "Widowed", "factor(ecivil)8" = "Single"
  )
  # Las categorias 2--4 se mantienen numericas para no introducir traducciones
  # no verificadas (por ejemplo, ecivil=3 no significa "engaged").
  terminos <- unique(cf$termino)
  orden <- c(names(etiquetas)[names(etiquetas) %in% terminos],
             setdiff(terminos, names(etiquetas)))
  fin <- " \\\\"
  fila <- function(nombre, valores) paste0(nombre, " & ", paste(valores, collapse=" & "), fin)
  numero <- function(x) {
    if (!is.finite(x)) return("--")
    # No redondear los coeficientes pequenos a cero: especialmente ing_nl.
    if (x != 0 && abs(x) < 0.0005) {
      partes <- strsplit(sprintf("%.2e", x), "e", fixed=TRUE)[[1]]
      return(paste0(partes[1], "\\times 10^{", as.integer(partes[2]), "}"))
    }
    sprintf("%.3f", x)
  }
  if (is.null(etiqueta)) etiqueta <- paste0("tab:", gsub("[^A-Za-z0-9]+", "_", basename(ruta)))
  titulo <- paste0("Labor Outcomes by Gender and Sexual Diversity: ",
                   if (metodo == "MPL") "Linear Probability Models" else "Probit Models")
  out <- c("% Requires graphicx. Original regression coefficients.",
    "\\begin{table}[htpb]", paste0("\\caption{", titulo, "}"),
    paste0("\\label{", etiqueta, "}"), "\\centering",
    "\\resizebox{\\textwidth}{!}{%",
    paste0("\\begin{tabular}{l ", paste(rep("c", length(columnas)), collapse=" "), "}"),
    "\\hline", fila("", cabeceras), "\\hline")
  for (termino in orden) {
    z <- cf[cf$termino == termino, , drop=FALSE]
    z <- z[match(columnas, z$resultado), , drop=FALSE]
    nombre <- unname(etiquetas[termino])
    if (is.na(nombre)) nombre <- sf_tex(termino)
    betas <- vapply(seq_along(columnas), function(i) {
      if (!is.finite(z$estimacion[i])) return("--")
      paste0("$", numero(z$estimacion[i]), sf_estrellas(z$p_value[i]), "$")
    }, character(1))
    errores <- vapply(z$se, function(x) {
      if (!is.finite(x)) "--" else paste0("$(", numero(x), ")$")
    }, character(1))
    out <- c(out, fila(nombre, betas), fila("", errores))
  }
  nn <- vapply(columnas, function(y) as.numeric(cf$n[match(y, cf$resultado)]), numeric(1))
  r2 <- rep(NA_real_, length(columnas))
  if (!is.null(ajustes)) {
    pos <- match(columnas, ajustes$resultado)
    nn <- as.numeric(ajustes$N[pos])
    r2 <- ajustes$r2[pos]
  }
  out <- c(out, "\\hline")
  if (any(is.finite(r2))) out <- c(out, fila("R$^2$",
    ifelse(is.finite(r2), sprintf("$%.3f$", r2), "--")))
  out <- c(out, fila("Num. obs.", vapply(nn, function(n) {
    if (!is.finite(n)) "--" else paste0("$", format(n, big.mark=",", trim=TRUE, scientific=FALSE), "$")
  }, character(1))), "\\hline",
    paste0("\\multicolumn{", length(columnas)+1L,
      "}{l}{\\scriptsize $^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$; $^{\\dagger}p<0.10$.}", fin),
    "\\end{tabular}%", "}", "\\par\\smallskip",
    "\\begin{minipage}{\\textwidth}\\footnotesize\\raggedright")
  nota <- paste(
    "Authors' calculations using CASEN 2022 and 2024. Entries are original regression coefficients, with survey-design standard errors in parentheses.",
    if (metodo == "MPL")
      "Binary outcomes use linear probability models; coefficients are in probability units, without multiplication by 100." else
      "Binary outcomes use probit models; coefficients refer to the latent index and are not marginal effects or probability differences.",
    "Monthly hours use a linear model of log(weekly hours times 4.33). Employment in the population and participation include inactive respondents; employment among participants excludes them. Other outcomes are conditional on employment.",
    "All models jointly include AFAB, gender-minority status, sexual orientation, and their specified interactions. Main effects are conditional on the reference categories and should not be confused with the average contrasts in the main tables.",
    "Region fixed effects are included in all models but omitted from the table. Marital-status category 1 is the reference; categories 2--4 retain their CASEN codes.",
    "Non-labor income per capita is ypchtotcor minus ypchtrabcor, in Chilean pesos and in levels. Very small coefficients and standard errors use scientific notation.",
    "R-squared is the weighted coefficient of determination for linear models; it is not reported for probit models. Stars use the p-values from each model's coefficient summary.", nota_extra)
  out <- c(out, paste0("\\textit{Note:} ", nota), "\\end{minipage}", "\\end{table}")
  writeLines(out, ruta, useBytes=TRUE)
  invisible(ruta)
}

sf_nota_medida <- function(modulo, version) {
  p <- modulo$resultado_ocupaciones$parametros
  if (version=="antigua") return("The female-dominated occupation indicator uses the original occupation list.")
  paste0("The indicator equals one when at least ",100*p$corte,
    " percent of weighted employment in the ",p$digitos,
    "-digit CIUO-08 occupation is female (sexo=2). The fixed classification pools CASEN 2022 and 2024, ages ",
    p$edad_min,"--",p$edad_max,", using full-survey expr weights. Regression weights remain expr\\_osig. ",
    "Occupation labels are treated as fixed; classification uncertainty is not propagated into regression standard errors.")
}

# LABOR:

comparar_sector_labor <- function(entorno=parent.frame(),
    salida="Tables/labor",
    metodo="Probit", var_ocupacion="CIIU", clasificador="scripts/ocupaciones_feminizadas_casen.R") {
  modulo <- sf_clasificador(clasificador)
  ds <- sf_agregar(get("diseno",entorno),modulo,var_ocupacion)
  base <- get("resultado_metodo",entorno)
  rhs_local <- get("rhs",entorno)
  mask <- get("mascaras",entorno)[["Feminized_sector"]] %in% TRUE
  f0 <- as.formula(paste("SF_antigua ~",rhs_local))
  f1 <- as.formula(paste("SF_nueva ~",rhs_local))
  original <- mask & sf_completos(f0,ds$variables)
  comun <- original & sf_completos(f1,ds$variables)
  dir.create(salida,recursive=TRUE,showWarnings=FALSE)
  cols <- c("Employed_population","Employed","Labor_force","Self_employed",
            "Public_sector","Feminized_sector","Monthly_hours","Informal_worker")
  heads <- encabezados_labor
  comparaciones <- completos <- list()
  for (v in c("antigua","nueva")) {
    t0 <- proc.time()[[3]]
    cat("\nLabor: medida",v,"| N comun =",sum(comun),"\n"); flush.console()
    f <- if(v=="antigua") f0 else f1
    m <- survey::svyglm(f,design=ds[comun,],family=if (metodo == "MPL") gaussian() else quasibinomial(link="probit"),na.action=na.fail)
    ee <- sf_contrastes(m,"Feminized_sector",if (metodo == "MPL") 1 else 100)
    ee$unidad <- if (metodo == "MPL") "probability units" else "percentage points"
    ef <- dplyr::bind_rows(base$contrastes[base$contrastes$resultado!="Feminized_sector",],ee)
    cf <- dplyr::bind_rows(base$coeficientes[base$coeficientes$resultado!="Feminized_sector",],
                          sf_coeficientes(m,"Feminized_sector"))
    nota <- paste(if (metodo == "MPL") "Binary outcomes use linear probability models." else "Binary outcomes use probit models.", fuente_ingreso, "Columns (1), (2), and (3) report employment in the population, employment among participants, and participation, respectively. Columns (4)--(8) are conditional on employment. Hours are in log points. The joint specification, controls and survey design are inherited from the main labor script. Only column (6) is reestimated: both definitions use identical respondents in that column; all other columns reproduce the main estimates.",
      if (metodo == "MPL") "Binary-outcome contrasts are in probability units, without multiplication by 100." else "Binary-outcome contrasts are in percentage points.",
      sf_nota_medida(modulo,v),
      "Contrasts average over observed covariates. Taylor covariance and delta-method standard errors are used.")
    ajustes_sector <- base$registro[c("resultado", "N", "r2")]
    k_sector <- match("Feminized_sector", ajustes_sector$resultado)
    ajustes_sector$N[k_sector] <- nobs(m)
    ajustes_sector$r2[k_sector] <- r2_ponderado(m)
    sf_tabla(ef,cols,heads,titulo_labor,
      paste0("tab:labor_fem_",metodo,"_",v),nota,
      ruta_sector(salida,paste0("tabla_labor_sector_",v,".tex")), ajustes=ajustes_sector)
    write.csv(ajustes_sector, ruta_sector(salida,paste0("ajuste_modelos_",v,".csv")), row.names=FALSE)
    sf_apendice(cf, ruta_sector(salida,paste0("apendice_labor_sector_",v,".tex")),
      ajustes=ajustes_sector, metodo=metodo, nota_extra=sf_nota_medida(modulo,v))
    write.csv(ef,ruta_sector(salida,paste0("contrastes_",v,".csv")),row.names=FALSE)
    write.csv(cf,ruta_sector(salida,paste0("coeficientes_",v,".csv")),row.names=FALSE)
    comparaciones[[v]] <- ef; completos[[v]] <- cf
    cat(sprintf("Terminado en %.1f segundos\n",proc.time()[[3]]-t0)); flush.console()
  }
  nd <- data.frame(resultado="Feminized_sector",N_original=sum(original),
    N_comparacion_antigua=sum(comun),N_comparacion_nueva=sum(comun),
    excluidos_sin_nueva_clasificacion=sum(original & !comun))
  write.csv(nd,ruta_sector(salida,"comparacion_N.csv"),row.names=FALSE)
  x <- ds$variables[comun,]; w <- as.numeric(weights(ds[comun,],type="sampling"))
  cruce <- aggregate(cbind(N=rep(1,nrow(x)),peso=w),
    by=list(antigua=x$SF_antigua,nueva=x$SF_nueva),FUN=sum)
  write.csv(cruce,ruta_sector(salida,"cruce_clasificaciones.csv"),row.names=FALSE)
  writeLines(paste("RHS heredado:",rhs_local),ruta_sector(salida,"formula.txt"))
  print(nd,row.names=FALSE)
  cat("\nTablas de comparacion:",normalizePath(salida),"\n")
  invisible(list(contrastes=comparaciones,coeficientes=completos,N=nd))
}



resultados_metodos <- list()
for (metodo in c("MPL", "Probit")) {
carpeta <- carpeta_raiz
prefijo_salida <- paste0(tolower(metodo), "_")
dir.create(carpeta, recursive = TRUE, showWarnings = FALSE)
especificacion_actual <- if (metodo == "MPL") "Labor_MPL_principal" else "Labor_Probit_robustez"
cat("\n================ ", especificacion_actual, " ================\n")
# ESTIMACION: SIETE RESULTADOS BINARIOS Y LOG HORAS --------------------
coefs <- efectos <- registro <- compactos <- list()
inicio <- proc.time()[[3]]
for (j in seq_along(outcomes)) {
  y <- outcomes[j]
  # Horas tiene exactamente el mismo modelo lineal en las dos tablas.
  if (metodo == "Probit" && y == "Monthly_hours") {
    previo <- resultados_metodos$MPL
    coefs[[y]] <- previo$coeficientes[previo$coeficientes$resultado == y, ]
    efectos[[y]] <- previo$contrastes[previo$contrastes$resultado == y, ]
    coefs[[y]]$especificacion <- efectos[[y]]$especificacion <- especificacion_actual
    registro[[y]] <- previo$registro[previo$registro$resultado == y, ]
    registro[[y]]$segundos <- 0
    compactos[[y]] <- previo$modelos[[y]]
    cat("\nHoras: se reutiliza el mismo modelo lineal ya estimado.\n")
    next
  }
  formula <- as.formula(paste(y, "~", rhs))
  mascara <- mascaras[[y]]
  mf <- model.frame(formula, data = d, na.action = na.pass)
  completos <- complete.cases(mf)
  for (v in names(mf)) if (is.numeric(mf[[v]])) completos <- completos & is.finite(mf[[v]])
  mascara <- mascara & completos
  cat(sprintf("\n[%d/%d] %s | N=%d | %.1f min acumulados\n",
              j, length(outcomes), y, sum(mascara), (proc.time()[[3]] - inicio)/60))
  flush.console()
  t0 <- proc.time()[[3]]
  m <- svyglm(formula, design = diseno[mascara, ],
              family = if (metodo == "MPL" || y == "Monthly_hours") gaussian() else quasibinomial(link = "probit"))
  cc <- as.data.frame(summary(m)$coefficients)
  names(cc)[1:4] <- c("estimacion", "se", "estadistico", "p_value")
  cc$termino <- rownames(cc); rownames(cc) <- NULL
  cc$resultado <- y; cc$n <- nrow(model.frame(m))
  cc$escala <- if (y == "Monthly_hours") "log_horas" else if (metodo == "MPL") "probabilidad_no_pp" else "indice_probit_no_pp"
  cc$especificacion <- especificacion_actual
  cat("Modelo ajustado; calculando contrastes...\n"); flush.console()
  ee <- calcular_contrastes_modelo(y, m, horas_log = TRUE)
  ee$especificacion <- especificacion_actual
  coefs[[y]] <- cc; efectos[[y]] <- ee
  registro[[y]] <- data.frame(resultado = y, N = nrow(model.frame(m)),
    r2 = r2_ponderado(m),
    N_mascara = sum(mascaras[[y]]), excluidos_adicionales = sum(mascaras[[y]] & !completos),
    convergio = m$converged, segundos = proc.time()[[3]] - t0,
    formula = paste(deparse(formula), collapse = " "))
  compactos[[y]] <- list(coeficientes = coef(m), vcov = vcov(m),
    formula = formula, familia = m$family$family, enlace = m$family$link,
    grados_libertad = degf(m$survey.design), N = nrow(model.frame(m)),
    r2 = r2_ponderado(m))
  # Exporta resultados numericos; no guarda modelos ni checkpoints.
  write.csv(bind_rows(coefs), ruta_principal(carpeta, "coeficientes_completos.csv"), row.names = FALSE)
  write.csv(bind_rows(efectos), ruta_principal(carpeta, "contrastes.csv"), row.names = FALSE)
  write.csv(bind_rows(registro), ruta_principal(carpeta, "registro_estimacion.csv"), row.names = FALSE)
  cat(sprintf("Terminado: %.1f segundos\n", proc.time()[[3]] - t0)); flush.console()
  rm(m)
}

# TABLAS LATEX: RESULTADOS PRINCIPALES Y REGRESIONES COMPLETAS ----------------
  # Tablas LaTeX: contrastes por especificacion y coeficientes de todos los modelos.
  ef <- bind_rows(efectos); cf <- bind_rows(coefs)
  estrellas <- function(p) {
    if (is.na(p)) return("")
    if (p < .001) "^{***}" else if (p < .01) "^{**}" else if (p < .05) "^{*}" else if (p < .10) "^{\\dagger}" else ""
  }
  esc_tex <- function(x) gsub("_", "\\_", x, fixed = TRUE)
  fin <- " \\\\"
  if (nrow(ef)) for (spec in unique(ef$especificacion)) {
    ss <- ef[ef$especificacion == spec, ]
    nota <- paste0("Column (1) estimates employment over the same population used for labor-force participation in column (3), including employed, unemployed and inactive respondents. ",
                   "Observed ages in the final participation sample range from ",rango_edad[1]," to ",rango_edad[2],". ",
                   "Column (2) estimates employment conditional on labor-force participation. Columns (4)--(8) are conditional on employment. ",
                   " Column (8) uses eligible employed respondents from the participation sample with classifiable reconstructed informality. ",
                   paste0("Columns (1)--(6) and (8) use ", if (metodo == "MPL") "linear probability models" else "probit models", " and report survey-weighted average contrasts in ", if (metodo == "MPL") "probability units, without multiplication by 100. " else "percentage points. "),
                   "Column (7) uses a linear model of log(weekly hours times 4.33) and reports differences in log points. ",
                   "All models jointly include AFAB interacted with GM and sexual orientation. Controls are age, age squared, schooling, rural residence, separate marital-status indicators (ecivil 1--8), the household child-count variable (men11c), disability, immigrant status, Indigenous background, non-labor household income, and region and year fixed effects. ",
                   esc_tex(fuente_ingreso), " ",
                   "Estimation accounts for sampling weights, strata and primary sampling units; Taylor covariance and delta-method contrast standard errors are used. ",
                   paste0("Informality follows the operational algorithm in MDSF (2026), Technical Note 7, for CASEN 2022 and 2024. Boleta rule: ",regla_boleta,". The literal option reproduces the published OR condition; the and option evaluates a documented sensitivity. This is the CASEN proxy, not an exact reproduction of the ENE measure. "),
                   "$^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$; $^{\\dagger}p<0.10$.")
    sf_tabla(ss, outcomes, encabezados_labor, titulo_labor,
      paste0("tab:labor_joint_informalidad_nt7_", metodo), nota,
      ruta_principal(carpeta, paste0("tabla_labor_", tolower(metodo), ".tex")),
      ajustes = bind_rows(registro))
  }
  if (nrow(cf)) sf_apendice(
    cf, ruta_principal(carpeta, "apendice_coeficientes_completos.tex"),
    ajustes = bind_rows(registro), metodo = metodo,
    nota_extra = "The occupation indicator in this table uses the original occupation list.")


# REGISTRO FINAL ------------------------------------------------------------
capture.output(sessionInfo(), file = ruta_principal(carpeta, "sessionInfo.txt"))
writeLines(c(paste("Formula:", rhs), fuente_ingreso,
             paste("Regla de boleta:", regla_boleta),
             "Estado civil: categorias separadas mediante factor(ecivil).",
             "Horas: log(hrs * 4.33), ocupados con horas positivas y finitas.",
             "Covarianza Taylor del diseno; metodo delta para contrastes.",
             "Muestras de referencia: mascaras historicas verificadas del script adjunto."),
           ruta_principal(carpeta, "especificacion.txt"))
resultado_metodo <- list(coeficientes = bind_rows(coefs),
  contrastes = bind_rows(efectos), registro = bind_rows(registro), modelos = compactos)
print(resultado_metodo$registro, row.names = FALSE)
cat("\nResultados guardados en:", normalizePath(carpeta), "\n")

# =============================================================================
# EXTENSION: CATEGORIA OCUPACIONAL E INFORMALIDAD
# =============================================================================

carpeta_desag <- carpeta_raiz
dir.create(carpeta_desag, recursive = TRUE, showWarnings = FALSE)

# RECUPERAR LA ESPECIFICACION ESTIMADA --------------------------

registro_participacion <- resultado_metodo$registro[
  resultado_metodo$registro$resultado == "Labor_force",
  ,
  drop = FALSE
]

formula_participacion_desag <- as.formula(
  registro_participacion$formula[[1]]
)

# Usar las variables del diseño principal.
datos_desag <- diseno$variables

# Cinco categorías mutuamente excluyentes.
datos_desag$tipo_empleo <- dplyr::case_when(
  datos_desag$activ %in% 1 &
    datos_desag$o15 %in% 3:8 &
    datos_desag$Informal_worker %in% 0 ~ "Dependiente_formal",
  
  datos_desag$activ %in% 1 &
    datos_desag$o15 %in% 3:8 &
    datos_desag$Informal_worker %in% 1 ~ "Dependiente_informal",
  
  datos_desag$activ %in% 1 &
    datos_desag$o15 %in% c(1, 2) &
    datos_desag$Informal_worker %in% 0 ~ "Independiente_formal",
  
  datos_desag$activ %in% 1 &
    datos_desag$o15 %in% c(1, 2) &
    datos_desag$Informal_worker %in% 1 ~ "Independiente_informal",
  
  datos_desag$activ %in% 1 &
    datos_desag$o15 %in% 9 ~ "Familiar_no_remunerado",
  
  TRUE ~ NA_character_
)

categorias_desag <- c(
  "Dependiente_formal",
  "Dependiente_informal",
  "Independiente_formal",
  "Independiente_informal",
  "Familiar_no_remunerado"
)

etiquetas_desag <- c(
  "Formal dependent",
  "Informal dependent",
  "Formal independent",
  "Informal independent",
  "Unpaid family worker"
)

# MUESTRA COMUN -------------------------------------------------------------

marco_desag <- model.frame(
  formula_participacion_desag,
  data = datos_desag,
  na.action = na.pass
)

completos_desag <- complete.cases(marco_desag)

for (v in names(marco_desag)) {
  if (is.numeric(marco_desag[[v]])) {
    completos_desag <- completos_desag &
      is.finite(marco_desag[[v]])
  }
}

muestra_desag <- (
  (mascaras$Labor_force %in% TRUE) &
    datos_desag$activ %in% 1 &
    !is.na(datos_desag$tipo_empleo) &
    completos_desag
)

# Reutiliza el diseño complejo
diseno_desag <- diseno
diseno_desag$variables$tipo_empleo <- datos_desag$tipo_empleo

for (categoria in categorias_desag) {
  diseno_desag$variables[[categoria]] <- ifelse(
    is.na(datos_desag$tipo_empleo),
    NA_integer_,
    as.integer(datos_desag$tipo_empleo == categoria)
  )
}

diseno_desag <- diseno_desag[muestra_desag, ]

cat("\nMuestra común de ocupados:", sum(muestra_desag), "\n")
cat("Controles recuperados de:\n")
print(formula_participacion_desag)

# COMPOSICION OBSERVADA -----------------------------------------------------

q_desag <- as.numeric(
  weights(diseno_desag, type = "sampling")
)

composicion_desag <- dplyr::bind_rows(
  lapply(categorias_desag, function(categoria) {
    pertenece <- diseno_desag$variables$tipo_empleo == categoria
    
    data.frame(
      categoria = categoria,
      N = sum(pertenece),
      porcentaje_ponderado = 100 *
        sum(q_desag[pertenece]) / sum(q_desag)
    )
  })
)

print(composicion_desag, row.names = FALSE)

write.csv(
  composicion_desag,
  ruta_desag(carpeta_desag, "composicion_muestra.csv"),
  row.names = FALSE
)

# ESTIMACION Y CONTRASTES ---------------------------------------------------


efectos_desag <- list()
coefs_desag <- list()
registro_desag <- list()

for (j in seq_along(categorias_desag)) {
  
  categoria <- categorias_desag[j]
  
  # Mantener exactamente el lado derecho de la fórmula principal.
  formula_desag <- formula_participacion_desag
  formula_desag[[2]] <- as.name(categoria)
  
  cat(sprintf(
    "\n[%d/%d] %s | N = %d\n",
    j, length(categorias_desag), categoria, sum(muestra_desag)
  ))
  flush.console()
  
  inicio_desag <- Sys.time()
  
  modelo_desag <- svyglm(
    formula_desag,
    design = diseno_desag,
    family = if (metodo == "MPL") gaussian() else quasibinomial(link = "probit"),
    na.action = na.fail
  )
  
  # Contrastes promedio: MPL en unidades de probabilidad; Probit en puntos porcentuales.
  efectos_desag[[categoria]] <- calcular_contrastes_modelo(
    categoria,
    modelo_desag
  )
  
  matriz_desag <- summary(modelo_desag)$coefficients
  
  coefs_desag[[categoria]] <- data.frame(
    resultado = categoria,
    termino = rownames(matriz_desag),
    beta = matriz_desag[, 1],
    se = matriz_desag[, 2],
    estadistico = matriz_desag[, 3],
    p_value = matriz_desag[, 4],
    row.names = NULL
  )
  
  registro_desag[[categoria]] <- data.frame(
    resultado = categoria,
    N = nobs(modelo_desag),
    r2 = r2_ponderado(modelo_desag),
    convergio = modelo_desag$converged,
    segundos = as.numeric(
      difftime(Sys.time(), inicio_desag, units = "secs")
    ),
    formula = paste(deparse(formula_desag), collapse = " ")
  )
  
  cat(
    "Terminado:",
    round(registro_desag[[categoria]]$segundos, 1),
    "segundos\n"
  )
  flush.console()
  rm(modelo_desag)
}

ef_desag <- dplyr::bind_rows(efectos_desag)
cf_desag <- dplyr::bind_rows(coefs_desag)
reg_desag <- dplyr::bind_rows(registro_desag)

write.csv(
  ef_desag,
  ruta_desag(carpeta_desag, "contrastes.csv"),
  row.names = FALSE
)

write.csv(
  cf_desag,
  ruta_desag(carpeta_desag, "coeficientes_completos.csv"),
  row.names = FALSE
)

write.csv(
  reg_desag,
  ruta_desag(carpeta_desag, "registro_estimacion.csv"),
  row.names = FALSE
)

# TABLA LATEX DESAGREGADA:

nota_desag <- paste0(
  "All columns use the same sample of employed respondents with complete ",
  "controls and a classifiable employment category. Dependent workers ",
  "include occupational categories 3--8; independent workers include ",
  "categories 1--2; unpaid family workers are category 9. Informality ",
  "uses the reconstructed measure from the main labor-outcome analysis. ",
  paste0("Each column is a separate ", if (metodo == "MPL") "linear probability model" else "probit", " for membership in that category "),
  "versus all other categories. Entries are survey-weighted average ",
  if (metodo == "MPL") "contrasts in probability units, without multiplication by 100, using the main specification " else "discrete changes in percentage points, using the main specification ",
  "and contrast scenarios. Standard errors use Taylor survey covariance ",
  "and the delta method. ",
  if (metodo == "Probit") paste(
    "Because the probits are estimated separately, predicted category probabilities",
    "need not sum to one and contrasts need not sum to zero. "
  ) else "Linear-model predictions are not constrained to the unit interval. ",
  "These estimates do not identify causal effects ",
  "or workers' motivations. ",
  "$^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$; ",
  "$^{\\dagger}p<0.10$."
)

sf_tabla(ef_desag, categorias_desag, etiquetas_desag,
  "Employment Categories and Informality by Gender and Sexual Diversity",
  paste0("tab:employment_informality_disaggregated_", metodo), nota_desag,
  ruta_desag(carpeta_desag, "tabla_desagregacion_informalidad.tex"), ajustes=reg_desag)

cat(
  "\nResultados guardados en:",
  normalizePath(carpeta_desag, winslash = "/"),
  "\n"
)


if (comparar_ocupaciones) {
  comparacion_sector_labor <- comparar_sector_labor(
    salida = carpeta_raiz, metodo = metodo)
}
resultados_metodos[[metodo]] <- resultado_metodo
} # Fin de MPL / Probit

resultado_labor_mpl <- resultados_metodos$MPL
resultado_labor_probit <- resultados_metodos$Probit
comparacion_N <- merge(
  resultado_labor_mpl$registro[c("resultado", "N")],
  resultado_labor_probit$registro[c("resultado", "N")],
  by = "resultado", suffixes = c("_MPL", "_Probit"), sort = FALSE)
comparacion_N$diferencia <- comparacion_N$N_MPL - comparacion_N$N_Probit
write.csv(comparacion_N, file.path(carpeta_raiz, "comparacion_N_MPL_Probit.csv"), row.names = FALSE)
print(comparacion_N, row.names = FALSE)
cat("\nTablas principales y robustez en:", normalizePath(carpeta_raiz), "\n")
