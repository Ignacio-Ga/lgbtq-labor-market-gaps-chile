# =============================================================================
# TABLA 10: SALARIOS TRANS Y NO BINARIOS, ESPECIFICACION FINAL + JKn
# CASEN 2022-2024
# =============================================================================
library(dplyr)
library(readr)
library(survey)
options(survey.lonely.psu = "adjust")

archivo <- "Intermediates/Panel_post.csv"
carpeta <- "Tables/wages-jkn"
dir.create(carpeta, recursive = TRUE, showWarnings = FALSE)


panel_22_24 <- read_csv(archivo, show_col_types = FALSE)

# Recuperar las fuentes originales si no fueron exportadas al CSV.
# Se realiza ANTES de filtrar pesos para conservar la correspondencia.
fuentes_ingreso <- c("ypchtotcor", "ypchtrabcor")
faltan_ingreso <- setdiff(fuentes_ingreso, names(panel_22_24))
if (length(faltan_ingreso)) {
  for (v in faltan_ingreso) panel_22_24[[v]] <- NA_real_
  texto_id <- function(x) {
    if (is.numeric(x)) sprintf("%.17g", x) else as.character(x)
  }
  for (anio in c(2022, 2024)) {
    cat("Recuperando ingreso per capita CASEN", anio, "...\n")
    z <- haven::read_sav(
      file.path("Data", paste0("casen_", anio, ".sav")),
      col_select = tidyselect::any_of(c(
        fuentes_ingreso, "expr_osig", "folio", "id_persona",
        "orden", "numper", "sexo", "edad"
      ))
    ) |> haven::zap_labels() |> as.data.frame()
    z <- z[!is.na(z$expr_osig), , drop = FALSE]
    idx <- which(panel_22_24$year == anio)
    correspondencia <- NULL
    for (id in intersect(c("id_persona", "orden", "numper"),
                         intersect(names(z), names(panel_22_24)))) {
      claves <- c("folio", id)
      if (!all(complete.cases(z[, claves])) ||
          !all(complete.cases(panel_22_24[idx, claves]))) next
      ks <- paste(texto_id(z$folio), texto_id(z[[id]]), sep = "|")
      kc <- paste(texto_id(panel_22_24$folio[idx]),
                  texto_id(panel_22_24[[id]][idx]), sep = "|")
      if (anyDuplicated(ks) || anyDuplicated(kc)) next
      candidato <- match(kc, ks)
      if (!anyNA(candidato)) {
        correspondencia <- candidato
        break
      }
    }
    # El preprocesamiento original conserva el orden del SAV tras quitar
    # los pesos SOGI faltantes. Solo usarlo si coinciden todas estas columnas.
    if (is.null(correspondencia)) {
      verificacion <- c("folio", "sexo", "edad")
      mismo_orden <- length(idx) == nrow(z) &&
        all(vapply(verificacion, function(v) {
          identical(texto_id(panel_22_24[[v]][idx]), texto_id(z[[v]]))
        }, logical(1)))
      if (!mismo_orden) stop(
        "No se pudo verificar la correspondencia CSV-SAV para ", anio,
        ". Incluya ypchtotcor y ypchtrabcor en el preprocesamiento."
      )
      correspondencia <- seq_len(nrow(z))
    }
    for (v in faltan_ingreso)
      panel_22_24[[v]][idx] <- z[[v]][correspondencia]
  }
}
panel_22_24 <- panel_22_24 %>%
  filter(year %in% c(2022, 2024), is.finite(expr_osig), expr_osig > 0)

panel_22_24 <- panel_22_24 %>% mutate(
  AFAB = case_when(sexo == 2 ~ 1L, sexo == 1 ~ 0L, TRUE ~ NA_integer_),
  edad2 = edad^2,
  Employed = case_when(activ == 1 ~ 1L, activ %in% c(2,3) ~ 0L, TRUE ~ NA_integer_),
  orientacion_valida = os1 %in% 1:4,
  genero_valido = genero %in% 1:6 &
    ((GM %in% 1 & cis %in% 0) | (GM %in% 0 & cis %in% 1)),
  LG = ifelse(orientacion_valida, as.integer(os1 %in% 2), NA_integer_),
  bisexual = ifelse(orientacion_valida, as.integer(os1 %in% 3), NA_integer_),
  Other = ifelse(orientacion_valida, as.integer(os1 %in% 4), NA_integer_),
  GM = ifelse(genero_valido, as.integer(GM %in% 1), NA_integer_)
)

# NB tiene prioridad; el residual GM no entra en esta tabla desagregada.
panel_22_24 <- panel_22_24 %>% mutate(
  NB_explicit = as.integer(NB %in% 1 | genero %in% 5),
  Trans_explicit = as.integer(TRANS %in% 1 & NB_explicit == 0),
  Cis_explicit = as.integer(cis %in% 1 & Trans_explicit == 0 & NB_explicit == 0),
  genero_explicito_valido = Cis_explicit + Trans_explicit + NB_explicit == 1,
  grupo_genero_explicito = case_when(
    Cis_explicit == 1 & AFAB == 0 ~ "AMAB cis",
    Cis_explicit == 1 & AFAB == 1 ~ "AFAB cis",
    Trans_explicit == 1 & AFAB == 0 ~ "AMAB transgender",
    Trans_explicit == 1 & AFAB == 1 ~ "AFAB transgender",
    NB_explicit == 1 & AFAB == 0 ~ "AMAB non-binary",
    NB_explicit == 1 & AFAB == 1 ~ "AFAB non-binary",
    TRUE ~ "Excluded residual/unclassified"
  )
)

# Ingreso no laboral per capita EN NIVELES.
# Ambas fuentes ya son per capita: no dividir por tot_per_h ni usar log1p.
# No se imputan faltantes a cero; las diferencias negativas validas se conservan.
p <- panel_22_24
fuentes_validas <- is.finite(p$ypchtotcor) & is.finite(p$ypchtrabcor) &
  !(p$ypchtotcor %in% c(-88, -99)) & !(p$ypchtrabcor %in% c(-88, -99))
nivel_ingreso <- p$ypchtotcor - p$ypchtrabcor
valido <- fuentes_validas & is.finite(nivel_ingreso)
panel_22_24$ing_nl <- NA_real_
panel_22_24$ing_nl[valido] <- nivel_ingreso[valido]

# hrs es la medida semanal; hrs_m queda en niveles.
panel_22_24$hrs_m <- NA_real_
horas_validas <- is.finite(p$hrs) & p$hrs > 0
panel_22_24$hrs_m[horas_validas] <- p$hrs[horas_validas] * 4.33
salario_valido <- p$activ %in% 1 & is.finite(p$w_mdsf) & p$w_mdsf > 0
panel_22_24$ln_month_mdsf <- NA_real_
panel_22_24$ln_month_mdsf[salario_valido] <- log(p$w_mdsf[salario_valido])
panel_22_24$ln_w_mdsf <- NA_real_
validos_horarios <- salario_valido & horas_validas
panel_22_24$ln_w_mdsf[validos_horarios] <- log(
  p$w_mdsf[validos_horarios] / panel_22_24$hrs_m[validos_horarios])
rm(p)

# =============================================================================
# MUESTRA COMUN PARA SELECCION Y SALARIOS
# =============================================================================
variables_probit <- c(
  "Employed", "AFAB", "Trans_explicit", "NB_explicit", "LG", "bisexual", "Other",
  "esc", "edad", "edad2", "ing_nl", "men11c",
  "pueblos_indigenas", "disc_wg", "extranjero", "area", "ecivil", "region", "year"
)
completos <- complete.cases(panel_22_24[, variables_probit])
for (v in variables_probit) if (is.numeric(panel_22_24[[v]]))
  completos <- completos & is.finite(panel_22_24[[v]])
base_edad <- is.finite(panel_22_24$edad) & panel_22_24$edad >= 18 & panel_22_24$edad <= 65
base_educacion <- is.finite(panel_22_24$esc) & panel_22_24$esc >= 0
base_sogi <- panel_22_24$orientacion_valida & panel_22_24$genero_valido &
  panel_22_24$genero_explicito_valido
base_ecivil <- panel_22_24$ecivil %in% 1:8
base_modulo <- rep(TRUE, nrow(panel_22_24))
if ("os_presente" %in% names(panel_22_24))
  base_modulo <- panel_22_24$os_presente %in% 1
panel_22_24$muestra_probit <- base_edad & base_educacion & base_sogi &
  base_ecivil & base_modulo & completos

# Diseño:.
design_taylor <- svydesign(ids = ~varunit_panel, strata = ~varstrat_panel,
  weights = ~expr_osig, nest = TRUE, data = panel_22_24)

write_csv(data.frame(
  etapa = c("Base SOGI 2022-2024 con peso valido", "Edad 18-65",
            "Educacion valida y modulo", "SOGI clasificable", "Estado civil valido", "Casos completos seleccion"),
  N = c(nrow(panel_22_24), sum(base_edad), sum(base_edad & base_educacion & base_modulo),
        sum(base_edad & base_educacion & base_modulo & base_sogi),
        sum(base_edad & base_educacion & base_modulo & base_sogi & base_ecivil),
        sum(panel_22_24$muestra_probit))
), file.path(carpeta, "muestra_seleccion.csv"))

# Grados de libertad del diseno original: PSU menos estratos.
grados_libertad_diseno <- degf(design_taylor)

# Construccion manual del JKn para evitar que as.svrepdesign() intente calcular
# el rango de una matriz demasiado grande mediante LINPACK.
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

design_base <- crear_diseno_jkn_grande(
  design_original = design_taylor,
  grados_libertad = grados_libertad_diseno,
  mse = FALSE
)

numero_replicas_jkn <- if (
  inherits(design_base$repweights, "repweights_compressed")
) {
  ncol(design_base$repweights$weights)
} else {
  ncol(design_base$repweights)
}

descripcion_se <- paste0(
  "Standard errors are based on ",
  formatC(numero_replicas_jkn, format = "d", big.mark = ","),
  " stratified delete-one-PSU jackknife replicate weights (JKn)."
)

cat("\n============================================\n")
cat("DISENO DE REPLICACION\n")
cat("============================================\n")
cat("Metodo: JKn\n")
cat("Numero de replicas:", numero_replicas_jkn, "\n")
cat("Grados de libertad:", grados_libertad_diseno, "\n")
cat("============================================\n\n")

design_analisis <- subset(design_base, muestra_probit)
write_csv(data.frame(replicas = numero_replicas_jkn, df = grados_libertad_diseno,
                     metodo = "JKn", mse = FALSE), file.path(carpeta, "diseno_jkn.csv"))
write_csv(design_analisis$variables %>%
            count(grupo_genero_explicito, Employed, name = "n"),
          file.path(carpeta, "conteos_grupos_seleccion.csv"))


# =============================================================================
# FUNCIONES DE PROGRESO 
# =============================================================================

formatear_duracion <- function(segundos) {
  if (!is.finite(segundos)) return("no disponible")

  horas <- floor(segundos / 3600)
  minutos <- floor((segundos %% 3600) / 60)
  segundos_restantes <- round(segundos %% 60)

  sprintf("%02d:%02d:%02d", horas, minutos, segundos_restantes)
}

inicio_estimaciones <- Sys.time()
tiempos_modelos <- numeric(0)


estimar_en_memoria <- function(
    etiqueta, paso, total_pasos,
    funcion_estimacion, asignar_resultado
) {
  cat(sprintf(
    "\n[%d/%d] Iniciando: %s | %s\n",
    paso, total_pasos, etiqueta,
    format(Sys.time(), "%H:%M:%S")
  ))
  cat("Réplicas internas:", numero_replicas_jkn, "\n")
  flush.console()
  
  inicio <- Sys.time()
  
  modelo <- funcion_estimacion()
  
  # Conservar inmediatamente el resultado, antes de otros cálculos.
  asignar_resultado(modelo)
  
  modelo$duracion_segundos <- as.numeric(
    difftime(Sys.time(), inicio, units = "secs")
  )
  
  cat(sprintf(
    "[%d/%d] Estimación terminada: %s | duración %s\n",
    paso, total_pasos, etiqueta,
    formatear_duracion(modelo$duracion_segundos)
  ))
  flush.console()
  
  modelo
}


preparar_modelo <- function(modelo) {
  
  modelo$df_diseno <- degf(modelo$survey.design)
  modelo$numero_replicas_jkn <- numero_replicas_jkn
  
  datos_modelo <- model.frame(modelo)
  
  q <- as.numeric(
    weights(modelo$survey.design, type = "sampling")
  )
  q <- q / sum(q)
  
  modelo$medias_orientacion <- vapply(
    c("LG", "bisexual", "Other"),
    function(v) sum(q * datos_modelo[[v]]),
    numeric(1)
  )
  
  # R2 ponderado convencional solo para las ecuaciones lineales de salarios.
  modelo$r2_ponderado <- NA_real_
  if (modelo$family$family == "gaussian" && modelo$family$link == "identity") {
    y <- as.numeric(model.response(datos_modelo))
    sct <- sum(q * (y - sum(q * y))^2)
    if (is.finite(sct) && sct > 0)
      modelo$r2_ponderado <- 1 - sum(q * (y - as.numeric(fitted(modelo)))^2) / sct
  }
  # Ya se extrajo la información necesaria para los contrastes.
  modelo$survey.design <- NULL
  
  modelo
}


tabla_coeficientes_modelo <- function(modelo) {
  beta <- coef(modelo)
  se <- sqrt(diag(vcov(modelo)))
  estadistico <- beta / se

  grados_residuales <- if (!is.null(modelo$df.residual)) {
    modelo$df.residual
  } else if (!is.null(modelo$df_diseno)) {
    modelo$df_diseno
  } else {
    Inf
  }

  p_value <- if (is.finite(grados_residuales) && grados_residuales > 0) {
    2 * pt(abs(estadistico), df = grados_residuales, lower.tail = FALSE)
  } else {
    2 * pnorm(abs(estadistico), lower.tail = FALSE)
  }

  tabla <- cbind(
    Estimate = beta,
    `Std. Error` = se,
    `t value` = estadistico,
    `Pr(>|t|)` = p_value
  )
  rownames(tabla) <- names(beta)
  tabla
}


# =============================================================================
# PRIMERA ETAPA: UN PROBIT DE EMPLEO Y UNA SOLA LAMBDA
# =============================================================================

rhs_selection <- paste(
  "AFAB * (Trans_explicit + NB_explicit + LG + bisexual + Other)",
  "esc + edad + edad2 + ing_nl + men11c",
  "pueblos_indigenas + disc_wg + extranjero",
  "factor(area) + factor(ecivil) + factor(region) + factor(year)",
  sep = " + "
)

formula_probit_empleo <- as.formula(
  paste("Employed ~", rhs_selection)
)

probit_empleo <- estimar_en_memoria(
  etiqueta = "Probit de selección por empleo",
  paso = 1,
  total_pasos = 9,
  
  funcion_estimacion = function() {
    svyglm(
      formula_probit_empleo,
      design = design_analisis,
      family = quasibinomial(link = "probit"),
      return.replicates = FALSE
    )
  },
  
  asignar_resultado = function(m) {
    probit_empleo <<- m
  }
)

probit_empleo <- preparar_modelo(probit_empleo)

# SALIDA COMPLETA DE LA SELECCION: se guarda antes de estimar salarios.
# El apendice informa coeficientes del indice probit, NO efectos marginales.
exportar_seleccion_completa <- function(modelo) {
  m <- tabla_coeficientes_modelo(modelo)
  gl <- modelo$df.residual
  if (is.null(gl) || !is.finite(gl) || gl <= 0) gl <- Inf
  critico <- if (is.finite(gl)) qt(.975, gl) else qnorm(.975)
  resultado <- data.frame(
    columna = 0L, modelo = "Probit: employment selection",
    variable = rownames(m), beta = m[, 1], se = m[, 2],
    estadistico_t = m[, 3], p_value = m[, 4],
    ci95_inferior = m[, 1] - critico * m[, 2],
    ci95_superior = m[, 1] + critico * m[, 2], row.names = NULL)
  write_csv(resultado, file.path(carpeta, "table10_probit_empleo_completo.csv"))
  write_csv(data.frame(N = nobs(modelo), gl_inferencia = gl,
    replicas_JKn = numero_replicas_jkn),
    file.path(carpeta, "table10_probit_estadisticos.csv"))
  capture.output({
    cat("Employment selection probit; JKn covariance\n")
    print(formula(modelo))
    cat("N:", nobs(modelo), "| df:", gl, "| JKn replicas:", numero_replicas_jkn, "\n")
    printCoefmat(m)
  }, file = file.path(carpeta, "table10_probit_empleo_completo.txt"))
  # Seis cifras significativas: el coeficiente por peso de ingreso no queda 0.000.
  fmt <- function(x) ifelse(is.na(x), "--", formatC(x, digits = 6, format = "g"))
  tex <- function(x) gsub("_", "\\_", x, fixed = TRUE)
  fin <- " \\\\"
  encabezado <- paste0("Coefficient & Estimate & SE & t & p-value & CI lower & CI upper", fin)
  lineas <- c(
    "% Requiere longtable y pdflscape; incluir directamente, fuera de table.",
    "\\begin{landscape}", "\\begingroup\\scriptsize",
    "\\begin{longtable}{l c c c c c c}",
    paste0("\\caption{Complete employment selection probit}\\label{tab:wages_trans_nb_selection}", fin),
    "\\hline", encabezado, "\\hline", "\\endfirsthead",
    "\\hline", encabezado, "\\hline", "\\endhead",
    "\\hline\\endfoot")
  for (i in seq_len(nrow(resultado))) {
    vals <- as.numeric(unlist(resultado[i, c("beta", "se", "estadistico_t", "p_value", "ci95_inferior", "ci95_superior")], use.names = FALSE))
    lineas <- c(lineas, paste0(paste(c(tex(resultado$variable[i]), fmt(vals)), collapse = " & "), fin))
  }
  lineas <- c(lineas, "\\hline",
    paste0("Num. obs. & ", format(nobs(modelo), big.mark = ",", trim = TRUE), " & & & & &", fin),
    "\\end{longtable}",
    "\\par\\noindent\\textit{Note:} Employment equals one for employed respondents and zero for unemployed or inactive respondents aged 18--65. Entries are coefficients of the probit index, not marginal effects. Standard errors use JKn; intervals are 95 percent confidence intervals. All controls, interactions and fixed effects are reported. Non-labor household income per capita enters in Chilean pesos, in levels, as ypchtotcor minus ypchtrabcor. The household child-count control is men11c. Non-binary and transgender categories are mutually exclusive; residual gender minorities and unclassifiable SOGI responses are excluded. Sampling weights are expr\\_osig. Scientific notation preserves small income coefficients.",
    "\\endgroup", "\\end{landscape}")
  writeLines(lineas, file.path(carpeta, "table10_probit_full.tex"))
  invisible(resultado)
}
resultados_probit <- exportar_seleccion_completa(probit_empleo)


cat("\n============================================================\n")
cat("PROBIT DE SELECCION: EMPLEO\n")
cat("============================================================\n")
printCoefmat(tabla_coeficientes_modelo(probit_empleo))

calcular_lambda <- function(modelo, datos) {
  matriz_original <- model.matrix(
    delete.response(terms(modelo)),
    data = model.frame(delete.response(terms(modelo)), data = datos,
                       xlev = modelo$xlevels, na.action = na.pass),
    contrasts.arg = modelo$contrasts
  )

  beta <- coef(modelo)
  matriz_alineada <- matrix(
    0,
    nrow = nrow(matriz_original),
    ncol = length(beta),
    dimnames = list(NULL, names(beta))
  )

  columnas_comunes <- intersect(colnames(matriz_original), names(beta))
  matriz_alineada[, columnas_comunes] <-
    matriz_original[, columnas_comunes, drop = FALSE]

  xb <- as.numeric(matriz_alineada %*% beta)

  # Forma numericamente estable de phi(xb) / Phi(xb).
  lambda <- exp(
    dnorm(xb, log = TRUE) -
      pnorm(xb, log.p = TRUE)
  )

  list(
    xb = xb,
    probabilidad_empleo = pnorm(xb),
    lambda = lambda
  )
}

pred_empleo <- calcular_lambda(
  probit_empleo,
  design_analisis$variables
)

design_analisis <- update(
  design_analisis,
  lambda_employment = pred_empleo$lambda
)

cat("\nResumen de probabilidades predichas de empleo:\n")
print(summary(pred_empleo$probabilidad_empleo))

cat("\nResumen del inverso de Mills:\n")
print(summary(pred_empleo$lambda))


# =============================================================================
#  DISENOS PARA LAS OCHO COLUMNAS
# =============================================================================

design_hourly_18_65 <- subset(
  design_analisis,
  Employed == 1 & is.finite(ln_w_mdsf)
)

design_monthly_18_65 <- subset(
  design_analisis,
  Employed == 1 & is.finite(ln_month_mdsf)
)

design_hourly_25_65 <- subset(
  design_analisis,
  Employed == 1 & is.finite(ln_w_mdsf) & edad >= 25
)

design_monthly_25_65 <- subset(
  design_analisis,
  Employed == 1 & is.finite(ln_month_mdsf) & edad >= 25
)

# Asalariados dependientes: se excluyen empleadores, trabajadores por cuenta
# propia, fuerzas armadas y familiares no remunerados.
design_hourly_salaried_18_65 <- subset(
  design_analisis,
  Employed == 1 & is.finite(ln_w_mdsf) & o15 %in% c(3, 4, 5, 6, 7)
)

design_monthly_salaried_18_65 <- subset(
  design_analisis,
  Employed == 1 & is.finite(ln_month_mdsf) & o15 %in% c(3, 4, 5, 6, 7)
)


# =============================================================================
# FORMULAS Y ESTIMACION
# =============================================================================

rhs_wage <- paste(
  "AFAB * (Trans_explicit + NB_explicit + LG + bisexual + Other)",
  "esc + edad + edad2",
  "pueblos_indigenas + disc_wg + extranjero",
  "factor(area) + factor(ecivil)",
  "factor(region) + factor(year)",
  sep = " + "
)

crear_formula_ols <- function(dependiente) {
  as.formula(paste(dependiente, "~", rhs_wage))
}

crear_formula_heckman <- function(dependiente) {
  as.formula(
    paste(dependiente, "~", rhs_wage, "+ lambda_employment")
  )
}

estimar_ols <- function(dependiente, diseno) {
  svyglm(
    crear_formula_ols(dependiente),
    design = diseno,
    family = gaussian(),
    return.replicates = FALSE,
    na.action = na.omit
  )
}

estimar_heckman <- function(dependiente, diseno) {
  svyglm(
    crear_formula_heckman(dependiente),
    design = diseno,
    family = gaussian(),
    return.replicates = FALSE,
    na.action = na.omit
  )
}

especificaciones_modelos <- list(
  col1_hourly_all = list(
    dependiente = "ln_w_mdsf",
    diseno = design_hourly_18_65,
    metodo = "ols",
    etiqueta = "Columna 1: salario horario, 18-65, OLS"
  ),
  col2_monthly_all = list(
    dependiente = "ln_month_mdsf",
    diseno = design_monthly_18_65,
    metodo = "ols",
    etiqueta = "Columna 2: salario mensual, 18-65, OLS"
  ),
  col3_hourly_25_65 = list(
    dependiente = "ln_w_mdsf",
    diseno = design_hourly_25_65,
    metodo = "ols",
    etiqueta = "Columna 3: salario horario, 25-65, OLS"
  ),
  col4_monthly_25_65 = list(
    dependiente = "ln_month_mdsf",
    diseno = design_monthly_25_65,
    metodo = "ols",
    etiqueta = "Columna 4: salario mensual, 25-65, OLS"
  ),
  col5_hourly_salaried = list(
    dependiente = "ln_w_mdsf",
    diseno = design_hourly_salaried_18_65,
    metodo = "ols",
    etiqueta = "Columna 5: salario horario, asalariados, OLS"
  ),
  col6_monthly_salaried = list(
    dependiente = "ln_month_mdsf",
    diseno = design_monthly_salaried_18_65,
    metodo = "ols",
    etiqueta = "Columna 6: salario mensual, asalariados, OLS"
  ),
  col7_hourly_heckman = list(
    dependiente = "ln_w_mdsf",
    diseno = design_hourly_18_65,
    metodo = "heckman",
    etiqueta = "Columna 7: salario horario, Heckman"
  ),
  col8_monthly_heckman = list(
    dependiente = "ln_month_mdsf",
    diseno = design_monthly_18_65,
    metodo = "heckman",
    etiqueta = "Columna 8: salario mensual, Heckman"
  )
)

modelos <- vector("list", length(especificaciones_modelos))
names(modelos) <- names(especificaciones_modelos)

for (i in seq_along(especificaciones_modelos)) {
  
  nombre_modelo <- names(especificaciones_modelos)[i]
  especificacion <- especificaciones_modelos[[i]]
  
  modelos[[i]] <- estimar_en_memoria(
    etiqueta = especificacion$etiqueta,
    paso = i + 1,
    total_pasos = 9,
    
    funcion_estimacion = function() {
      if (especificacion$metodo == "ols") {
        estimar_ols(
          especificacion$dependiente,
          especificacion$diseno
        )
      } else {
        estimar_heckman(
          especificacion$dependiente,
          especificacion$diseno
        )
      }
    },
    
    asignar_resultado = function(m) {
      modelos[[i]] <<- m
    }
  )
  
  modelos[[i]] <- preparar_modelo(modelos[[i]])
}



nombres_modelos <- c(
  "Ln(Hourly) (1)",
  "Ln(Monthly) (2)",
  "Ln(Hourly) (3)",
  "Ln(Monthly) (4)",
  "Ln(Hourly) (5)",
  "Ln(Monthly) (6)",
  "Ln(Hourly) (7)",
  "Ln(Monthly) (8)"
)

cat("\n============================================================\n")
cat("RESUMENES COMPLETOS DE LAS OCHO REGRESIONES\n")
cat("============================================================\n")

for (i in seq_along(modelos)) {
  cat("\n---", nombres_modelos[i], "---\n")
  printCoefmat(tabla_coeficientes_modelo(modelos[[i]]))
}


# =============================================================================
#  CONTRASTES DE INTERES
# =============================================================================

# Contrastes medios, como en salarios: se conservan las orientaciones observadas.
# Los pesos siguientes son la base; si cambia AFAB se agregan sus interacciones
# con orientacion ponderadas por las proporciones de cada muestra.
lista_contrastes <- list(
  "AFAB Cis vs AMAB Cis" =
    c("AFAB" = 1),

  "AMAB Non-binary vs AMAB Cis" =
    c("NB_explicit" = 1),

  "AFAB Non-binary vs AMAB Cis" =
    c("AFAB" = 1, "NB_explicit" = 1, "AFAB:NB_explicit" = 1),

  "AFAB Non-binary vs AFAB Cis" =
    c("NB_explicit" = 1, "AFAB:NB_explicit" = 1),

  "AFAB Non-binary vs AMAB Non-binary" =
    c("AFAB" = 1, "AFAB:NB_explicit" = 1),

  "AMAB Transgender vs AMAB Cis" =
    c("Trans_explicit" = 1),

  "AFAB Transgender vs AMAB Cis" =
    c("AFAB" = 1, "Trans_explicit" = 1, "AFAB:Trans_explicit" = 1),

  "AFAB Transgender vs AFAB Cis" =
    c("Trans_explicit" = 1, "AFAB:Trans_explicit" = 1),

  "AFAB Transgender vs AMAB Transgender" =
    c("AFAB" = 1, "AFAB:Trans_explicit" = 1)
)

calcular_contraste <- function(modelo, pesos, nombre_contraste) {
  beta_modelo <- coef(modelo)
  matriz_vcov <- vcov(modelo)

  if ("AFAB" %in% names(pesos)) {
    for (k in c("LG", "bisexual", "Other")) {
      pesos[paste0("AFAB:", k)] <- pesos[["AFAB"]] * modelo$medias_orientacion[[k]]
    }
  }

  vector <- rep(0, length(beta_modelo))
  names(vector) <- names(beta_modelo)
  vector[names(pesos)] <- pesos

  beta <- sum(vector * beta_modelo)
  varianza <- as.numeric(t(vector) %*% matriz_vcov %*% vector)
  se <- sqrt(varianza)
  estadistico_t <- beta / se
  grados_libertad <- if (!is.null(modelo$df_diseno)) {
    modelo$df_diseno
  } else {
    degf(modelo$survey.design)
  }

  if (is.finite(grados_libertad) && grados_libertad > 0) {
    p_value <- 2 * pt(
      abs(estadistico_t),
      df = grados_libertad,
      lower.tail = FALSE
    )
    valor_critico <- qt(0.975, df = grados_libertad)
  } else {
    p_value <- 2 * pnorm(abs(estadistico_t), lower.tail = FALSE)
    valor_critico <- qnorm(0.975)
  }

  data.frame(
    contraste = nombre_contraste,
    beta = beta,
    se = se,
    estadistico_t = estadistico_t,
    p_value = p_value,
    porcentaje = 100 * (exp(beta) - 1),
    ci_95_inferior = 100 * (exp(beta - valor_critico * se) - 1),
    ci_95_superior = 100 * (exp(beta + valor_critico * se) - 1)
  )
}

resultados_contrastes <- bind_rows(
  lapply(seq_along(modelos), function(i) {
    bind_rows(
      lapply(names(lista_contrastes), function(nombre) {
        calcular_contraste(
          modelos[[i]],
          lista_contrastes[[nombre]],
          nombre
        )
      })
    ) %>%
      mutate(
        modelo = names(modelos)[i],
        columna = i,
        nombre_modelo = nombres_modelos[i],
        .before = 1
      )
  })
)

cat("\n============================================================\n")
cat("CONTRASTES DE LAS OCHO COLUMNAS\n")
cat("============================================================\n")
print(resultados_contrastes, digits = 4)


# =============================================================================
# ESTADISTICOS Y RESULTADOS COMPLETOS
# =============================================================================

calcular_r2 <- function(modelo) modelo$r2_ponderado

valores_r2 <- vapply(modelos, calcular_r2, numeric(1))
valores_n <- vapply(modelos, nobs, numeric(1))

extraer_resultados_completos <- function(modelo, nombre_modelo, columna) {
  matriz <- tabla_coeficientes_modelo(modelo)

  data.frame(
    columna = columna,
    modelo = nombre_modelo,
    variable = rownames(matriz),
    beta = matriz[, 1],
    se = matriz[, 2],
    estadistico_t = matriz[, 3],
    p_value = matriz[, 4],
    row.names = NULL
  )
}

resultados_completos <- bind_rows(
  lapply(seq_along(modelos), function(i) {
    extraer_resultados_completos(
      modelos[[i]],
      nombres_modelos[i],
      i
    )
  })
)

write_csv(
  resultados_contrastes,
  file.path(carpeta, "table10_wages_trans_nb_contrastes.csv")
)

write_csv(
  resultados_completos,
  file.path(carpeta, "table10_wages_trans_nb_coeficientes_completos.csv")
)

write_csv(
  resultados_probit,
  file.path(carpeta, "table10_probit_empleo_completo.csv")
)

write_csv(
  data.frame(
    columna = seq_along(modelos),
    modelo = nombres_modelos,
    observaciones = valores_n,
    r2 = valores_r2
  ),
  file.path(carpeta, "table10_wages_trans_nb_estadisticos.csv")
)


# =============================================================================
# TABLA 10 EN LATEX
# =============================================================================

estrellas_tex <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.001) return("^{***}")
  if (p < 0.01) return("^{**}")
  if (p < 0.05) return("^{*}")
  if (p < 0.10) return("^{\\dagger}")
  ""
}

formatear_estimacion <- function(beta, p) {
  paste0("$", sprintf("%.3f", beta), estrellas_tex(p), "$")
}

formatear_se <- function(se) {
  paste0("$(", sprintf("%.3f", se), ")$")
}

crear_filas_contraste <- function(nombre, etiqueta) {
  datos <- resultados_contrastes %>%
    filter(contraste == nombre) %>%
    arrange(columna)

  fila_beta <- paste0(
    paste(
      c(
        etiqueta,
        mapply(
          formatear_estimacion,
          datos$beta,
          datos$p_value,
          USE.NAMES = FALSE
        )
      ),
      collapse = " & "
    ),
    " \\\\"
  )

  fila_se <- paste0(
    paste(
      c("", vapply(datos$se, formatear_se, character(1))),
      collapse = " & "
    ),
    " \\\\"
  )

  c(fila_beta, fila_se)
}

encabezado_columnas <- c(
  " & \\begin{tabular}[c]{@{}c@{}}Ln(Hourly)\\\\ (1)\\end{tabular}",
  "\\begin{tabular}[c]{@{}c@{}}Ln(Monthly)\\\\ (2)\\end{tabular}",
  "\\begin{tabular}[c]{@{}c@{}}Ln(Hourly)\\\\ (3)\\end{tabular}",
  "\\begin{tabular}[c]{@{}c@{}}Ln(Monthly)\\\\ (4)\\end{tabular}",
  "\\begin{tabular}[c]{@{}c@{}}Ln(Hourly)\\\\ (5)\\end{tabular}",
  "\\begin{tabular}[c]{@{}c@{}}Ln(Monthly)\\\\ (6)\\end{tabular}",
  "\\begin{tabular}[c]{@{}c@{}}Ln(Hourly)\\\\ (7)\\end{tabular}",
  "\\begin{tabular}[c]{@{}c@{}}Ln(Monthly)\\\\ (8)\\end{tabular} \\\\"
)

filas_nb <- c(
  crear_filas_contraste(
    "AFAB Cis vs AMAB Cis",
    "AFAB cis vs AMAB cis"
  ),
  crear_filas_contraste(
    "AMAB Non-binary vs AMAB Cis",
    "AMAB non-binary vs AMAB cis"
  ),
  crear_filas_contraste(
    "AFAB Non-binary vs AMAB Cis",
    "AFAB non-binary vs AMAB cis"
  ),
  crear_filas_contraste(
    "AFAB Non-binary vs AFAB Cis",
    "AFAB non-binary vs AFAB cis"
  ),
  crear_filas_contraste(
    "AFAB Non-binary vs AMAB Non-binary",
    "AFAB non-binary vs AMAB non-binary"
  )
)

filas_trans <- c(
  crear_filas_contraste(
    "AFAB Cis vs AMAB Cis",
    "AFAB cis vs AMAB cis"
  ),
  crear_filas_contraste(
    "AMAB Transgender vs AMAB Cis",
    "AMAB transgender vs AMAB cis"
  ),
  crear_filas_contraste(
    "AFAB Transgender vs AMAB Cis",
    "AFAB transgender vs AMAB cis"
  ),
  crear_filas_contraste(
    "AFAB Transgender vs AFAB Cis",
    "AFAB transgender vs AFAB cis"
  ),
  crear_filas_contraste(
    "AFAB Transgender vs AMAB Transgender",
    "AFAB transgender vs AMAB transgender"
  )
)

lambda_beta <- rep("", 8)
lambda_se <- rep("", 8)

for (i in 7:8) {
  matriz <- tabla_coeficientes_modelo(modelos[[i]])
  lambda_beta[i] <- formatear_estimacion(
    matriz["lambda_employment", 1],
    matriz["lambda_employment", 4]
  )
  lambda_se[i] <- formatear_se(matriz["lambda_employment", 2])
}

fila_lambda_beta <- paste0(
  paste(c("Inverse Mills ratio", lambda_beta), collapse = " & "),
  " \\\\"
)

fila_lambda_se <- paste0(
  paste(c("", lambda_se), collapse = " & "),
  " \\\\"
)

fila_r2 <- paste0(
  paste(
    c("R$^2$", sprintf("%.3f", valores_r2)),
    collapse = " & "
  ),
  " \\\\"
)

fila_n <- paste0(
  paste(
    c("Num. obs.", format(valores_n, big.mark = ",", scientific = FALSE)),
    collapse = " & "
  ),
  " \\\\"
)

tabla_salarios <- c(
  "\\begin{table}[htpb]",
  "\\caption{Wage Estimates for Transgender and Non-Binary Respondents}",
  "\\label{tab:wages_trans_nb_joint}",
  "\\centering",
  "\\resizebox{\\textwidth}{!}{%",
  "\\begin{tabular}{l c c c c c c c c}",
  "\\hline",
  paste(encabezado_columnas, collapse = " & "),
  "\\hline",
  "\\multicolumn{9}{l}{\\textit{Panel A: Non-Binary Respondents}} \\\\ ",
  filas_nb,
  "\\hline",
  "\\multicolumn{9}{l}{\\textit{Panel B: Transgender Respondents}} \\\\ ",
  filas_trans,
  "\\hline",
  fila_lambda_beta,
  fila_lambda_se,
  "\\hline",
  fila_r2,
  fila_n,
  "\\hline",
  "\\end{tabular}%",
  "}",
  "\\par\\smallskip",
  "\\begin{minipage}{\\textwidth}\\footnotesize",
  paste0(
    "\\textit{Note:} Entries are linear combinations of regression coefficients; standard errors are in parentheses. Columns (1)--(2) use employed individuals aged 18--65 with the corresponding wage measure observed. Columns (3)--(4) restrict the sample to ages 25--65. Columns (5)--(6) restrict it to dependent (salaried) workers aged 18--65, excluding employers, self-employed workers, armed forces personnel, and unpaid family workers. Columns (7)--(8) apply Heckman's two-step correction using one common employment-selection probit estimated over the population aged 18--65; unemployed and inactive respondents are coded as unselected, and the same estimated inverse Mills ratio enters both wage equations. Non-binary and transgender status are mutually exclusive, and the residual gender-minority category is excluded from this disaggregated analysis. All models jointly include explicit transgender status, non-binary status, and sexual orientation, interacted with AFAB. Reported gender contrasts are survey-weighted averages over the observed sexual-orientation distribution in each estimation sample. Unclassifiable SOGI responses are excluded; other sexual orientation is retained. The wage equations control for years of schooling, age, age squared, Indigenous background, disability status, immigrant status, rural residence, and marital-status categories, as well as region and year fixed effects. The selection equation additionally includes non-labor household income per capita in levels (ypchtotcor minus ypchtrabcor) and the household child-count variable (men11c). Missing or invalid income sources are not imputed as zero; valid negative differences are retained. R-squared is the weighted coefficient of determination for each wage equation. Estimation incorporates sampling weights, strata, and primary sampling units. ",
    descripcion_se,
    " In columns (7)--(8), the covariance calculation treats the estimated inverse Mills ratio as a generated regressor fixed at its full-sample value. $^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$; $^{\\dagger}p<0.10$."
  ),
  "\\end{minipage}",
  "\\end{table}"
)

writeLines(
  tabla_salarios,
  file.path(carpeta, "table10_wages_trans_nb.tex")
)


# =============================================================================
#  RESUMEN FINAL
# =============================================================================

cat(
  "Tiempo transcurrido en esta ejecucion:",
  formatear_duracion(
    as.numeric(difftime(Sys.time(), inicio_estimaciones, units = "secs"))
  ),
  "\n"
)


# =============================================================================
# APENDICE COMPLETO (TODOS LOS COEFICIENTES)
# =============================================================================
# Preambulo LaTeX: \usepackage{booktabs,longtable,pdflscape,graphicx}
# Las tablas longtable se incluyen directamente, fuera de un entorno table.
exportar_apendice <- function(datos, columnas, etiquetas, destino, titulo, label) {
  variables <- unique(datos$variable)
  nc <- length(columnas) + 1L
  salida <- c("\\begin{landscape}", "\\begingroup", "\\scriptsize",
    "\\setlength{\\tabcolsep}{3pt}",
    paste0("\\begin{longtable}{l", paste(rep("c", length(columnas)), collapse = ""), "}"),
    paste0("\\caption{", titulo, "}\\label{", label, "} \\\\"),
    "\\hline", paste0(paste(c("Coefficient", etiquetas), collapse = " & "), " \\\\"),
    "\\hline", "\\endfirsthead",
    "\\hline", paste0(paste(c("Coefficient", etiquetas), collapse = " & "), " \\\\"),
    "\\hline", "\\endhead", "\\hline",
    paste0("\\multicolumn{", nc, "}{r}{Continued on next page} \\\\"),
    "\\endfoot", "\\hline", "\\endlastfoot")
  for (v in variables) {
    betas <- errores <- rep("--", length(columnas))
    for (j in seq_along(columnas)) {
      r <- datos[datos$variable == v & datos$columna == columnas[j], , drop = FALSE]
      if (nrow(r) == 1) {
        betas[j] <- formatear_estimacion(r$beta, r$p_value)
        errores[j] <- formatear_se(r$se)
      }
    }
    etiqueta <- gsub("_", "\\_", v, fixed = TRUE)
    salida <- c(salida,
      paste0(paste(c(etiqueta, betas), collapse = " & "), " \\\\"),
      paste0(paste(c("", errores), collapse = " & "), " \\\\") )
  }
  salida <- c(salida, "\\end{longtable}",
    "JKn standard errors in parentheses. Heckman covariance holds the estimated inverse Mills ratio fixed.",
    "\\endgroup", "\\end{landscape}")
  writeLines(salida, file.path(carpeta, destino))
}
exportar_apendice(resultados_completos, 1:8, nombres_modelos,
  "table10_wages_trans_nb_full.tex", "Complete wage regression coefficients",
  "tab:wages_trans_nb_full")
# La transformacion exponencial es del contraste logaritmico promedio;
# no representa una diferencia entre medias aritmeticas de salarios.
writeLines(c(
  "Salarios conjuntos trans/NB: NB prioritario, residual GM excluido.",
  "Estado civil separado: factor(ecivil). SOGI no clasificable excluido.",
  "ing_nl = ypchtotcor - ypchtrabcor, en niveles; ceros y diferencias negativas validas se conservan; sin imputar NA como cero.",
  "Control de ninos en seleccion: men11c del panel, sin modificar su corte de edad.",
  "Horas mensuales = horas semanales * 4.33.",
  "Contrastes de genero promediados sobre la orientacion observada de cada muestra.",
  "Una lambda comun; covarianza de segunda etapa condicional a lambda fija.",
  paste("Replicas JKn:", numero_replicas_jkn)
), file.path(carpeta, "especificacion.txt"))
capture.output(sessionInfo(), file = file.path(carpeta, "sessionInfo.txt"))
cat("\nResultados guardados en:", normalizePath(carpeta), "\n")
