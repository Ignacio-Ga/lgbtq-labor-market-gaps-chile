# ROBUSTEZ: expr en seleccion, salarios y promedios de contrastes.
# =============================================================================
# SALARIOS LGBTQ+: OLS + HECKMAN EN OCHO COLUMNAS
# CASEN 2022-2024
# =============================================================================

library(dplyr)
library(readr)
library(survey)
library(texreg)

options(survey.lonely.psu = "adjust")

# Resultados:.
carpeta <- "tables/wages/robustez_expr"
dir.create(carpeta, recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# BASE, RESPUESTAS CLASIFICABLES Y VARIABLES
# =============================================================================

panel_22_24 <- read_csv("Intermediates/Panel_post.csv", show_col_types = FALSE)

# Recuperar las fuentes originales si no fueron exportadas al CSV.
# Se realiza ANTES de filtrar pesos para conservar la correspondencia.
fuentes_ingreso <- c("ypchtotcor", "ypchtrabcor", "expr")
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
  empleado = case_when(activ == 1 ~ 1L, activ %in% c(2,3) ~ 0L, TRUE ~ NA_integer_),
  orientacion_valida = os1 %in% 1:4,
  genero_valido = genero %in% 1:6 &
    ((GM %in% 1 & cis %in% 0) | (GM %in% 0 & cis %in% 1)),
  LG = ifelse(orientacion_valida, as.integer(os1 %in% 2), NA_integer_),
  bisexual = ifelse(orientacion_valida, as.integer(os1 %in% 3), NA_integer_),
  Other = ifelse(orientacion_valida, as.integer(os1 %in% 4), NA_integer_),
  GM = ifelse(genero_valido, as.integer(GM %in% 1), NA_integer_)
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
  "empleado", "AFAB", "GM", "LG", "bisexual", "Other",
  "esc", "edad", "edad2", "ing_nl", "men11c",
  "pueblos_indigenas", "disc_wg", "extranjero", "area", "ecivil", "region", "year"
)
completos <- complete.cases(panel_22_24[, variables_probit])
for (v in variables_probit) if (is.numeric(panel_22_24[[v]]))
  completos <- completos & is.finite(panel_22_24[[v]])
base_edad <- is.finite(panel_22_24$edad) & panel_22_24$edad >= 18 & panel_22_24$edad <= 65
base_educacion <- is.finite(panel_22_24$esc) & panel_22_24$esc >= 0
base_sogi <- panel_22_24$orientacion_valida & panel_22_24$genero_valido
base_ecivil <- panel_22_24$ecivil %in% 1:8
base_modulo <- rep(TRUE, nrow(panel_22_24))
if ("os_presente" %in% names(panel_22_24))
  base_modulo <- panel_22_24$os_presente %in% 1
panel_22_24$muestra_probit <- base_edad & base_educacion & base_sogi &
  base_ecivil & base_modulo & completos

# Diseño.
# No eliminar observaciones por el nuevo ponderador: preservar la muestra original.
if (!"expr" %in% names(panel_22_24)) stop("No se recupero expr de CASEN.")
if (any(!is.finite(panel_22_24$expr) | panel_22_24$expr <= 0))
  stop("Hay expr faltantes o no positivos. Revisar la union CSV-SAV; no se modifico la muestra para acomodar el ponderador.")
design_completo <- svydesign(ids = ~varunit_panel, strata = ~varstrat_panel,
  weights = ~expr, nest = TRUE, data = panel_22_24)
design_analisis <- subset(design_completo, muestra_probit)
write_csv(data.frame(
  etapa = c("Base SOGI 2022-2024 con peso valido", "Edad 18-65",
            "Educacion valida y modulo", "SOGI clasificable", "Estado civil valido", "Casos completos seleccion"),
  N = c(nrow(panel_22_24), sum(base_edad), sum(base_edad & base_educacion & base_modulo),
        sum(base_edad & base_educacion & base_modulo & base_sogi),
        sum(base_edad & base_educacion & base_modulo & base_sogi & base_ecivil),
        sum(panel_22_24$muestra_probit))
), file.path(carpeta, "muestra_seleccion.csv"))

# =============================================================================
#  PRIMERA ETAPA: PROBIT DE EMPLEO
# =============================================================================

formula_probit <- empleado ~
  AFAB * (GM + LG + bisexual + Other) +
  esc +
  edad +
  edad2 +
  ing_nl +
  men11c +
  pueblos_indigenas +
  disc_wg +
  extranjero +
  factor(area) +
  factor(ecivil) +
  factor(region) +
  factor(year)

cat("\nPrimera etapa: probit de empleo...\n"); flush.console()
probit_pooled <- svyglm(
  formula_probit,
  design = design_analisis,
  family = quasibinomial(link = "probit"),
  na.action = na.omit
)

cat("\n============================================================\n")
cat("PROBIT DE SELECCION\n")
cat("============================================================\n")
print(summary(probit_pooled))

# =============================================================================
# EFECTOS MARGINALES PROMEDIO DEL PROBIT DE EMPLEO
# =============================================================================


design_ame <- probit_pooled$survey.design
datos_ame <- design_ame$variables

# Pesos muestrales normalizados
pesos_ame <- as.numeric(
  weights(design_ame, type = "sampling")
)

pesos_ame <- pesos_ame / sum(pesos_ame)


# -----------------------------------------------------------------------------
# Construir la matriz de diseño para datos contrafactuales
# -----------------------------------------------------------------------------

construir_matriz_modelo <- function(modelo, nuevos_datos) {
  
  terminos_sin_respuesta <- delete.response(
    terms(modelo)
  )
  
  marco_modelo <- model.frame(
    terminos_sin_respuesta,
    data = nuevos_datos,
    na.action = na.pass,
    xlev = modelo$xlevels
  )
  
  matriz <- model.matrix(
    terminos_sin_respuesta,
    data = marco_modelo,
    contrasts.arg = modelo$contrasts
  )
  
  nombres_beta <- names(coef(modelo))
  
  matriz[, nombres_beta, drop = FALSE]
}


# -----------------------------------------------------------------------------
# Aplicar un escenario contrafactual
# -----------------------------------------------------------------------------

aplicar_escenario <- function(datos, escenario) {
  
  datos_nuevos <- datos
  
  for (variable in names(escenario)) {
    datos_nuevos[[variable]] <- escenario[[variable]]
  }
  
  datos_nuevos
}


# -----------------------------------------------------------------------------
# Probabilidad promedio y gradiente para el método delta
# -----------------------------------------------------------------------------

predecir_escenario_promedio <- function(
    modelo,
    datos,
    pesos,
    escenario
) {
  
  datos_escenario <- aplicar_escenario(
    datos,
    escenario
  )
  
  matriz <- construir_matriz_modelo(
    modelo,
    datos_escenario
  )
  
  beta <- coef(modelo)
  
  
  indice_lineal <- as.numeric(
    matriz %*% beta
  )
  
  probabilidad <- pnorm(indice_lineal)
  
  probabilidad_promedio <- sum(
    pesos * probabilidad
  )
  
  # Derivada de la probabilidad promedio respecto de beta:
  # promedio ponderado de phi(Xb) X
  gradiente <- colSums(
    sweep(
      matriz,
      MARGIN = 1,
      STATS = pesos * dnorm(indice_lineal),
      FUN = "*"
    )
  )
  
  list(
    probabilidad = probabilidad_promedio,
    gradiente = gradiente
  )
}


# -----------------------------------------------------------------------------
# Contraste entre dos escenarios
# -----------------------------------------------------------------------------

calcular_ame_contraste <- function(
    modelo,
    datos,
    pesos,
    escenario_grupo,
    escenario_referencia,
    nombre_contraste,
    panel
) {
  
  resultado_grupo <- predecir_escenario_promedio(
    modelo = modelo,
    datos = datos,
    pesos = pesos,
    escenario = escenario_grupo
  )
  
  resultado_referencia <- predecir_escenario_promedio(
    modelo = modelo,
    datos = datos,
    pesos = pesos,
    escenario = escenario_referencia
  )
  
  diferencia <- (
    resultado_grupo$probabilidad -
      resultado_referencia$probabilidad
  )
  
  gradiente_diferencia <- (
    resultado_grupo$gradiente -
      resultado_referencia$gradiente
  )
  
  matriz_vcov <- vcov(modelo)
  
  varianza <- as.numeric(
    t(gradiente_diferencia) %*%
      matriz_vcov %*%
      gradiente_diferencia
  )
  
  se <- sqrt(varianza)
  
  grados_libertad <- degf(modelo$survey.design)
  
  estadistico_t <- diferencia / se
  
  p_value <- 2 * pt(
    abs(estadistico_t),
    df = grados_libertad,
    lower.tail = FALSE
  )
  
  valor_critico <- qt(
    0.975,
    df = grados_libertad
  )
  
  data.frame(
    panel = panel,
    contraste = nombre_contraste,
    
    probabilidad_grupo = 100 *
      resultado_grupo$probabilidad,
    
    probabilidad_referencia = 100 *
      resultado_referencia$probabilidad,
    
    efecto_pp = 100 * diferencia,
    se_pp = 100 * se,
    estadistico_t = estadistico_t,
    p_value = p_value,
    
    ci_95_inferior_pp = 100 * (
      diferencia - valor_critico * se
    ),
    
    ci_95_superior_pp = 100 * (
      diferencia + valor_critico * se
    )
  )
}

# =============================================================================
# ESCENARIOS CONTRAFACTUALES
# =============================================================================

escenarios_ame <- list(
  
  # ---------------------------------------------------------------------------
  # Panel A: sexo asignado y condición GM
  # Se conserva la orientación sexual observada de cada persona.
  # ---------------------------------------------------------------------------
  
  AMAB_Cis = list(
    AFAB = 0,
    GM = 0
  ),
  
  AFAB_Cis = list(
    AFAB = 1,
    GM = 0
  ),
  
  AMAB_GM = list(
    AFAB = 0,
    GM = 1
  ),
  
  AFAB_GM = list(
    AFAB = 1,
    GM = 1
  ),
  
  
  # ---------------------------------------------------------------------------
  # Panel B: orientación sexual
  # Se conserva el estado GM observado de cada persona.
  # ---------------------------------------------------------------------------
  
  AMAB_Hetero = list(
    AFAB = 0,
    LG = 0,
    bisexual = 0,
    Other = 0
  ),
  
  AMAB_LG = list(
    AFAB = 0,
    LG = 1,
    bisexual = 0,
    Other = 0
  ),
  
  AMAB_Bisexual = list(
    AFAB = 0,
    LG = 0,
    bisexual = 1,
    Other = 0
  ),
  
  AMAB_Other = list(
    AFAB = 0,
    LG = 0,
    bisexual = 0,
    Other = 1
  ),
  
  AFAB_Hetero = list(
    AFAB = 1,
    LG = 0,
    bisexual = 0,
    Other = 0
  ),
  
  AFAB_LG = list(
    AFAB = 1,
    LG = 1,
    bisexual = 0,
    Other = 0
  ),
  
  AFAB_Bisexual = list(
    AFAB = 1,
    LG = 0,
    bisexual = 1,
    Other = 0
  ),
  
  AFAB_Other = list(
    AFAB = 1,
    LG = 0,
    bisexual = 0,
    Other = 1
  )
)

# =============================================================================
# CONTRASTES DE EMPLEO
# =============================================================================

definiciones_ame <- list(
  
  # Panel A
  list(
    panel = "A",
    nombre = "AFAB Cis vs AMAB Cis",
    grupo = "AFAB_Cis",
    referencia = "AMAB_Cis"
  ),
  
  list(
    panel = "A",
    nombre = "AMAB GM vs AMAB Cis",
    grupo = "AMAB_GM",
    referencia = "AMAB_Cis"
  ),
  
  list(
    panel = "A",
    nombre = "AFAB GM vs AMAB Cis",
    grupo = "AFAB_GM",
    referencia = "AMAB_Cis"
  ),
  
  list(
    panel = "A",
    nombre = "AFAB GM vs AFAB Cis",
    grupo = "AFAB_GM",
    referencia = "AFAB_Cis"
  ),
  
  list(
    panel = "A",
    nombre = "AFAB GM vs AMAB GM",
    grupo = "AFAB_GM",
    referencia = "AMAB_GM"
  ),
  
  
  # Panel B
  list(
    panel = "B",
    nombre = "AMAB LG vs AMAB Hetero",
    grupo = "AMAB_LG",
    referencia = "AMAB_Hetero"
  ),
  
  list(
    panel = "B",
    nombre = "AFAB LG vs AFAB Hetero",
    grupo = "AFAB_LG",
    referencia = "AFAB_Hetero"
  ),
  
  list(
    panel = "B",
    nombre = "AMAB Bisexual vs AMAB Hetero",
    grupo = "AMAB_Bisexual",
    referencia = "AMAB_Hetero"
  ),
  
  list(
    panel = "B",
    nombre = "AFAB Bisexual vs AFAB Hetero",
    grupo = "AFAB_Bisexual",
    referencia = "AFAB_Hetero"
  ),
  
  list(
    panel = "B",
    nombre = "AMAB Other vs AMAB Hetero",
    grupo = "AMAB_Other",
    referencia = "AMAB_Hetero"
  ),
  
  list(
    panel = "B",
    nombre = "AFAB Other vs AFAB Hetero",
    grupo = "AFAB_Other",
    referencia = "AFAB_Hetero"
  )
)


resultados_ame_empleo <- bind_rows(
  lapply(definiciones_ame, function(definicion) {
    
    calcular_ame_contraste(
      modelo = probit_pooled,
      datos = datos_ame,
      pesos = pesos_ame,
      
      escenario_grupo =
        escenarios_ame[[definicion$grupo]],
      
      escenario_referencia =
        escenarios_ame[[definicion$referencia]],
      
      nombre_contraste =
        definicion$nombre,
      
      panel =
        definicion$panel
    )
  })
)

cat("\n============================================================\n")
cat("EFECTOS MARGINALES PROMEDIO SOBRE LA PROBABILIDAD DE EMPLEO\n")
cat("============================================================\n")

print(
  resultados_ame_empleo,
  digits = 4,
  row.names = FALSE
)

write_csv(
  resultados_ame_empleo,
  "tables/wages/robustez_expr/ame_empleo_probit.csv"
)

# =============================================================================
# TABLA LATEX DE EFECTOS MARGINALES DE EMPLEO
# =============================================================================

estrellas_ame <- function(p) {
  
  if (is.na(p)) {
    return("")
  }
  
  if (p < 0.001) {
    return("^{***}")
  }
  
  if (p < 0.01) {
    return("^{**}")
  }
  
  if (p < 0.05) {
    return("^{*}")
  }
  
  if (p < 0.10) {
    return("^{\\dagger}")
  }
  
  ""
}


crear_filas_ame <- function(panel_seleccionado) {
  
  datos_panel <- resultados_ame_empleo %>%
    filter(panel == panel_seleccionado)
  
  unlist(
    lapply(seq_len(nrow(datos_panel)), function(i) {
      
      fila <- datos_panel[i, ]
      
      fila_efecto <- paste0(
        fila$contraste,
        " & $",
        sprintf("%.3f", fila$efecto_pp),
        estrellas_ame(fila$p_value),
        "$ \\\\"
      )
      
      fila_se <- paste0(
        " & $(",
        sprintf("%.3f", fila$se_pp),
        ")$ \\\\"
      )
      
      c(
        fila_efecto,
        fila_se
      )
    })
  )
}


filas_ame_genero <- crear_filas_ame("A")
filas_ame_orientacion <- crear_filas_ame("B")


tabla_ame_empleo <- c(
  "\\begin{table}[htpb]",
  "\\caption{Average Marginal Effects on Employment Probability}",
  "\\label{tab:ame_employment}",
  "\\centering",
  "\\begin{tabular}{l c}",
  "\\hline",
  " & \\begin{tabular}[c]{@{}c@{}}Employment\\\\ probability (pp)\\end{tabular} \\\\",
  "\\hline",
  
  "\\multicolumn{2}{l}{\\textit{Panel A: Assigned Sex and Gender Minority Status}} \\\\",
  filas_ame_genero,
  
  "\\hline",
  "\\multicolumn{2}{l}{\\textit{Panel B: Sexual Orientation Within Assigned Sex}} \\\\",
  filas_ame_orientacion,
  
  "\\hline",
  paste0(
    "Num. obs. & ",
    format(
      nobs(probit_pooled),
      big.mark = ",",
      scientific = FALSE
    ),
    " \\\\"
  ),
  
  "\\hline",
  "\\end{tabular}",
  "\\par\\smallskip",
  "\\begin{minipage}{\\textwidth}\\footnotesize",
  
  paste0(
    "\\footnotesize\\textit{Note:} ",
    "The table reports survey-weighted average discrete changes ",
    "in the predicted probability of employment, expressed in percentage points. ",
    "Panel A standardizes over the observed distribution of sexual orientation, ",
    "whereas Panel B standardizes over the observed distribution of ",
    "gender-minority status. All remaining covariates are held at their ",
    "observed values. Delta-method standard errors based on the complex-survey ",
    "variance-covariance matrix are reported in parentheses. ",
    "$^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$; ",
    "$^{\\dagger}p<0.10$."
  ),
  
  "\\end{minipage}",
  "\\end{table}"
)

writeLines(
  tabla_ame_empleo,
  "tables/wages/robustez_expr/ame_empleo_probit.tex"
)

# Calculo numericamente estable de phi(xb) / Phi(xb).
xb <- as.numeric(
  predict(
    probit_pooled,
    newdata = design_analisis$variables,
    type = "link"
  )
)

lambda <- exp(
  dnorm(xb, log = TRUE) -
    pnorm(xb, log.p = TRUE)
)

design_analisis <- update(
  design_analisis,
  lambda = lambda
)

cat("\nResumen de probabilidades predichas de empleo:\n")
print(summary(pnorm(xb)))

cat("\nResumen del inverso de Mills:\n")
print(summary(lambda))


# =============================================================================
# DISEÑOS PARA LAS DISTINTAS COLUMNAS
# =============================================================================

# Todos los ocupados de 18-65.
design_work_18_65 <- subset(
  design_analisis,
  empleado == 1
)

# Todos los ocupados de 25-65.
design_work_25_65 <- subset(
  design_analisis,
  empleado == 1 & edad >= 25
)

# Dependientes de 18-65.
design_salaried_18_65 <- subset(
  design_analisis,
  empleado == 1 & o15 %in% c(3, 4, 5, 6, 7)
)


# =============================================================================
#  FORMULAS Y FUNCIONES DE ESTIMACION
# =============================================================================

rhs_wage <- paste(
  "AFAB * (GM + LG + bisexual + Other)",
  "esc", "edad", "edad2",
  "pueblos_indigenas", "disc_wg", "extranjero",
  "factor(area)", "factor(ecivil)",
  "factor(region)", "factor(year)",
  sep = " + "
)

crear_formula_ols <- function(dependiente) {
  as.formula(paste(dependiente, "~", rhs_wage))
}

crear_formula_heckman <- function(dependiente) {
  as.formula(paste(dependiente, "~", rhs_wage, "+ lambda"))
}

estimar_ols <- function(dependiente, diseno) {
  svyglm(
    crear_formula_ols(dependiente),
    design = diseno[is.finite(diseno$variables[[dependiente]]), ],
    family = gaussian(),
    na.action = na.omit
  )
}

estimar_heckman <- function(dependiente, diseno) {
  svyglm(
    crear_formula_heckman(dependiente),
    design = diseno[is.finite(diseno$variables[[dependiente]]), ],
    family = gaussian(),
    na.action = na.omit
  )
}


# =============================================================================
#  ESTIMAR LAS OCHO COLUMNAS
# =============================================================================

plan_modelos <- data.frame(
  nombre = c("col1_hourly_all", "col2_monthly_all", "col3_hourly_25_65", "col4_monthly_25_65",
             "col5_hourly_salaried", "col6_monthly_salaried", "col7_hourly_heckman", "col8_monthly_heckman"),
  dependiente = rep(c("ln_w_mdsf", "ln_month_mdsf"), 4),
  muestra = c("all", "all", "25_65", "25_65", "salaried", "salaried", "all", "all"),
  heckman = c(rep(FALSE, 6), TRUE, TRUE)
)
disenos <- list(all = design_work_18_65, `25_65` = design_work_25_65,
                salaried = design_salaried_18_65)
modelos <- list(); registro_estimacion <- list()
for (i in seq_len(nrow(plan_modelos))) {
  cfg <- plan_modelos[i, ]
  cat(sprintf("\n[%d/8] %s ...\n", i, cfg$nombre)); flush.console()
  inicio <- proc.time()[[3]]
  funcion <- if (cfg$heckman) estimar_heckman else estimar_ols
  modelos[[cfg$nombre]] <- funcion(cfg$dependiente, disenos[[cfg$muestra]])
  registro_estimacion[[i]] <- data.frame(modelo = cfg$nombre,
    N = nobs(modelos[[cfg$nombre]]), segundos = proc.time()[[3]] - inicio)
  print(registro_estimacion[[i]], row.names = FALSE)
}
write_csv(bind_rows(registro_estimacion), file.path(carpeta, "registro_estimacion.csv"))

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
  print(summary(modelos[[i]]))
}


# =============================================================================
#  CONTRASTES PROMEDIO DE INTERES
# =============================================================================

lista_contrastes <- list(
  "AFAB Cis vs AMAB Cis" =
    c("AFAB" = 1),

  "AMAB GM vs AMAB Cis" =
    c("GM" = 1),

  "AFAB GM vs AMAB Cis" =
    c("AFAB" = 1, "GM" = 1, "AFAB:GM" = 1),

  "AFAB GM vs AFAB Cis" =
    c("GM" = 1, "AFAB:GM" = 1),

  "AFAB GM vs AMAB GM" =
    c("AFAB" = 1, "AFAB:GM" = 1),

  "AMAB LG vs AMAB Hetero" =
    c("LG" = 1),

  "AFAB LG vs AFAB Hetero" =
    c("LG" = 1, "AFAB:LG" = 1),

  "AMAB Bisexual vs AMAB Hetero" =
    c("bisexual" = 1),

  "AFAB Bisexual vs AFAB Hetero" =
    c("bisexual" = 1, "AFAB:bisexual" = 1),

  "AMAB Other vs AMAB Hetero" =
    c("Other" = 1),

  "AFAB Other vs AFAB Hetero" =
    c("Other" = 1, "AFAB:Other" = 1)
)

calcular_contraste <- function(modelo, pesos, nombre_contraste) {
  beta_modelo <- coef(modelo)
  matriz_vcov <- vcov(modelo)

  definicion <- definiciones_ame[[match(nombre_contraste,
    vapply(definiciones_ame, function(z) z$nombre, character(1)))]]
  datos <- modelo$survey.design$variables
  X1 <- construir_matriz_modelo(modelo,
    aplicar_escenario(datos, escenarios_ame[[definicion$grupo]]))
  X0 <- construir_matriz_modelo(modelo,
    aplicar_escenario(datos, escenarios_ame[[definicion$referencia]]))
  q <- as.numeric(weights(modelo$survey.design, type = "sampling"))
  q <- q / sum(q)
  vector <- colSums((X1 - X0) * q)

  beta <- sum(vector * beta_modelo)
  varianza <- as.numeric(t(vector) %*% matriz_vcov %*% vector)
  se <- sqrt(varianza)
  estadistico_t <- beta / se
  grados_libertad <- degf(modelo$survey.design)
  p_value <- 2 * pt(
    abs(estadistico_t),
    df = grados_libertad,
    lower.tail = FALSE
  )
  valor_critico <- qt(0.975, df = grados_libertad)

  data.frame(
    contraste = nombre_contraste,
    beta = beta,
    se = se,
    estadistico_t = estadistico_t,
    p_value = p_value,
    porcentaje = 100 * (exp(beta) - 1),
    ci_95_inferior = 100 * (
      exp(beta - valor_critico * se) - 1
    ),
    ci_95_superior = 100 * (
      exp(beta + valor_critico * se) - 1
    )
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
# ESTADISTICOS DE AJUSTE Y COEFICIENTES COMPLETOS
# =============================================================================

calcular_r2 <- function(modelo) {
  if (
    is.null(modelo$deviance) ||
      is.null(modelo$null.deviance) ||
      modelo$null.deviance == 0
  ) {
    return(NA_real_)
  }

  1 - modelo$deviance / modelo$null.deviance
}

valores_r2 <- vapply(modelos, calcular_r2, numeric(1))
valores_n <- vapply(modelos, nobs, numeric(1))

extraer_resultados_completos <- function(modelo, nombre_modelo, columna) {
  matriz <- coef(summary(modelo))

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

resultados_probit <- extraer_resultados_completos(
  probit_pooled,
  "Probit de empleo",
  0
)


# =============================================================================
#  EXPORTAR RESULTADOS EN CSV
# =============================================================================

write_csv(
  resultados_contrastes,
  "tables/wages/robustez_expr/contrastes_wages_8_columnas.csv"
)

write_csv(
  resultados_completos,
  "tables/wages/robustez_expr/regresiones_completas_wages_8_columnas.csv"
)

write_csv(
  resultados_probit,
  "tables/wages/robustez_expr/probit_empleo_completo.csv"
)

write_csv(
  data.frame(
    columna = seq_along(modelos),
    modelo = nombres_modelos,
    observaciones = valores_n,
    r2 = valores_r2
  ),
  "tables/wages/robustez_expr/estadisticos_wages_8_columnas.csv"
)


# =============================================================================
#  TABLA LATEX PRINCIPAL: CONTRASTES EN LOGARITMOS
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

filas_genero <- c(
  crear_filas_contraste(
    "AFAB Cis vs AMAB Cis",
    "AFAB cis vs AMAB cis"
  ),
  crear_filas_contraste(
    "AMAB GM vs AMAB Cis",
    "AMAB GM vs AMAB cis"
  ),
  crear_filas_contraste(
    "AFAB GM vs AMAB Cis",
    "AFAB GM vs AMAB cis"
  ),
  crear_filas_contraste(
    "AFAB GM vs AFAB Cis",
    "AFAB GM vs AFAB cis"
  ),
  crear_filas_contraste(
    "AFAB GM vs AMAB GM",
    "AFAB GM vs AMAB GM"
  )
)

filas_orientacion <- c(
  crear_filas_contraste(
    "AMAB LG vs AMAB Hetero",
    "AMAB LG vs AMAB heterosexual"
  ),
  crear_filas_contraste(
    "AFAB LG vs AFAB Hetero",
    "AFAB LG vs AFAB heterosexual"
  ),
  crear_filas_contraste(
    "AMAB Bisexual vs AMAB Hetero",
    "AMAB bisexual vs AMAB heterosexual"
  ),
  crear_filas_contraste(
    "AFAB Bisexual vs AFAB Hetero",
    "AFAB bisexual vs AFAB heterosexual"
  ),
  crear_filas_contraste(
    "AMAB Other vs AMAB Hetero",
    "AMAB other vs AMAB heterosexual"
  ),
  crear_filas_contraste(
    "AFAB Other vs AFAB Hetero",
    "AFAB other vs AFAB heterosexual"
  )
)

lambda_beta <- rep("", 8)
lambda_se <- rep("", 8)

for (i in 7:8) {
  matriz <- coef(summary(modelos[[i]]))
  lambda_beta[i] <- formatear_estimacion(
    matriz["lambda", 1],
    matriz["lambda", 4]
  )
  lambda_se[i] <- formatear_se(matriz["lambda", 2])
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

tabla_contrastes <- c(
  "\\begin{table}[htpb]",
  "\\caption{Wage Estimates by Gender and Sexual Diversity}",
  "\\label{tab:wages_joint_specification}",
  "\\centering",
  "\\resizebox{\\textwidth}{!}{%",
  "\\begin{tabular}{l c c c c c c c c}",
  "\\hline",
  paste(encabezado_columnas, collapse = " & "),
  "\\hline",
  "\\multicolumn{9}{l}{\\textit{Panel A: Assigned Sex and Gender Minority Status}} \\\\ ",
  filas_genero,
  "\\hline",
  "\\multicolumn{9}{l}{\\textit{Panel B: Sexual Orientation Within Assigned Sex}} \\\\ ",
  filas_orientacion,
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
    "\\footnotesize\\textit{Note:} ",
    "Entries are survey-weighted average contrasts in predicted log wages, evaluated over each wage estimation sample; ",
    "survey-design standard errors are reported in parentheses. ",
    "Columns (1)--(2) use employed individuals aged 18--65. ",
    "Columns (3)--(4) restrict the sample to employed individuals aged 25--65. ",
    "Columns (5)--(6) restrict the sample to dependent (salaried) workers aged 18--65, ",
    "excluding employers, self-employed workers, armed forces personnel, ",
    "and unpaid family workers. ",
    "Columns (7)--(8) apply Heckman's two-step selection correction ",
    "to the employed sample aged 18--65. ",
    "All models jointly include gender-minority status and sexual orientation, ",
    "interacted with AFAB. ",
    "The wage equations control for years of schooling, age, age squared, ",
    "Indigenous background, disability status, immigrant status, rural residence, ",
    "and separate marital-status indicators, as well as region and year fixed effects. ",
    "The employment probit additionally includes non-labor household income per capita and the household child-count control (men11c). ",
    "Non-labor income is corrected total household income per capita minus corrected labor income per capita ",
    "(\\texttt{ypchtotcor} minus \\texttt{ypchtrabcor}), in levels. Both sources are already per capita; ",
    "missing or invalid sources are not imputed as zero, and valid negative differences are retained. ",
    "R$^2$ and the number of observations are reported once per column because both panels use the same model. ",
    "Taylor second-stage covariance treats that estimated ratio as fixed and does not propagate first-stage estimation uncertainty. ",
    "Estimation incorporates sampling weights, strata, and primary sampling units. ",
    "$^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$; ",
    "$^{\\dagger}p<0.10$."
  ),
  "\\end{minipage}",
  "\\end{table}"
)

writeLines(
  tabla_contrastes,
  "tables/wages/robustez_expr/reg_wages_contrastes_8_columnas.tex"
)


# =============================================================================
#  TABLA LATEX DE PORCENTAJES E INTERVALOS DE CONFIANZA
# =============================================================================

formatear_porcentaje <- function(porcentaje, p) {
  paste0(
    "$",
    sprintf("%.1f", porcentaje),
    "\\%",
    estrellas_tex(p),
    "$"
  )
}

formatear_ic <- function(inferior, superior) {
  paste0(
    "$[",
    sprintf("%.1f", inferior),
    "\\%, ",
    sprintf("%.1f", superior),
    "\\%]$"
  )
}

crear_filas_porcentaje <- function(nombre, etiqueta) {
  datos <- resultados_contrastes %>%
    filter(contraste == nombre) %>%
    arrange(columna)

  fila_pct <- paste0(
    paste(
      c(
        etiqueta,
        mapply(
          formatear_porcentaje,
          datos$porcentaje,
          datos$p_value,
          USE.NAMES = FALSE
        )
      ),
      collapse = " & "
    ),
    " \\\\"
  )

  fila_ic <- paste0(
    paste(
      c(
        "",
        mapply(
          formatear_ic,
          datos$ci_95_inferior,
          datos$ci_95_superior,
          USE.NAMES = FALSE
        )
      ),
      collapse = " & "
    ),
    " \\\\"
  )

  c(fila_pct, fila_ic)
}

filas_pct_genero <- c(
  crear_filas_porcentaje("AFAB Cis vs AMAB Cis", "AFAB cis vs AMAB cis"),
  crear_filas_porcentaje("AMAB GM vs AMAB Cis", "AMAB GM vs AMAB cis"),
  crear_filas_porcentaje("AFAB GM vs AMAB Cis", "AFAB GM vs AMAB cis"),
  crear_filas_porcentaje("AFAB GM vs AFAB Cis", "AFAB GM vs AFAB cis"),
  crear_filas_porcentaje("AFAB GM vs AMAB GM", "AFAB GM vs AMAB GM")
)

filas_pct_orientacion <- c(
  crear_filas_porcentaje("AMAB LG vs AMAB Hetero", "AMAB LG vs AMAB heterosexual"),
  crear_filas_porcentaje("AFAB LG vs AFAB Hetero", "AFAB LG vs AFAB heterosexual"),
  crear_filas_porcentaje("AMAB Bisexual vs AMAB Hetero", "AMAB bisexual vs AMAB heterosexual"),
  crear_filas_porcentaje("AFAB Bisexual vs AFAB Hetero", "AFAB bisexual vs AFAB heterosexual"),
  crear_filas_porcentaje("AMAB Other vs AMAB Hetero", "AMAB other vs AMAB heterosexual"),
  crear_filas_porcentaje("AFAB Other vs AFAB Hetero", "AFAB other vs AFAB heterosexual")
)

tabla_porcentajes <- c(
  "\\begin{table}[htpb]",
  "\\caption{Conditional Wage Differences by Gender and Sexual Diversity (Percent)}",
  "\\label{tab:wages_joint_percent}",
  "\\centering",
  "\\resizebox{\\textwidth}{!}{%",
  "\\begin{tabular}{l c c c c c c c c}",
  "\\hline",
  paste(encabezado_columnas, collapse = " & "),
  "\\hline",
  "\\multicolumn{9}{l}{\\textit{Panel A: Assigned Sex and Gender Minority Status}} \\\\ ",
  filas_pct_genero,
  "\\hline",
  "\\multicolumn{9}{l}{\\textit{Panel B: Sexual Orientation Within Assigned Sex}} \\\\ ",
  filas_pct_orientacion,
  "\\hline",
  fila_r2,
  fila_n,
  "\\hline",
  "\\end{tabular}%",
  "}",
  "\\par\\smallskip",
  "\\begin{minipage}{\\textwidth}\\footnotesize",
  "\\footnotesize\\textit{Note:} Percentage differences are calculated as $100[\\exp(\\widehat{\\beta})-1]$. Ninety-five percent confidence intervals are reported in brackets. Contrasts average over each estimation sample, retaining observed sexual orientation for gender contrasts and observed GM status for orientation contrasts. Percentages transform the average log-wage contrast, not a difference in arithmetic mean wages. Heckman intervals condition on the estimated inverse Mills ratio. $^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$; $^{\\dagger}p<0.10$.",
  "\\end{minipage}",
  "\\end{table}"
)

writeLines(
  tabla_porcentajes,
  "tables/wages/robustez_expr/reg_wages_porcentajes_8_columnas.tex"
)


# =============================================================================
# APENDICE: COEFICIENTES EN COLUMNAS, EE DEBAJO, SIN FILAS DE REGION
# Puede ejecutarse desde aqui si modelos y probit_pooled siguen en memoria.
# =============================================================================
exportar_tabla_completa_wages <- function(lista, cabeceras, titulo, etiqueta,
                                         ruta, seleccion = FALSE) {
  etiquetas <- c(
    "(Intercept)"="Constant", "AFAB"="AFAB", "GM"="Gender minority",
    "LG"="Gay/Lesbian", "bisexual"="Bisexual", "Other"="Other sexual orientation",
    "AFAB:GM"="AFAB $\\times$ Gender minority",
    "AFAB:LG"="AFAB $\\times$ Gay/Lesbian",
    "AFAB:bisexual"="AFAB $\\times$ Bisexual",
    "AFAB:Other"="AFAB $\\times$ Other sexual orientation",
    "edad"="Age", "edad2"="Age squared", "esc"="Years of schooling",
    "men11c"="Household child count (men11c)",
    "ing_nl"="Non-labor income per capita", "disc_wg"="Disability",
    "extranjero"="Immigrant", "pueblos_indigenas"="Indigenous",
    "factor(area)2"="Rural", "factor(year)2024"="Year 2024",
    "factor(ecivil)2"="Cohabiting without civil union",
    "factor(ecivil)3"="Civil union", "factor(ecivil)4"="Annulled",
    "factor(ecivil)5"="Separated", "factor(ecivil)6"="Divorced",
    "factor(ecivil)7"="Widowed", "factor(ecivil)8"="Single",
    "lambda"="Inverse Mills ratio")
  matrices <- lapply(lista, function(m) coef(summary(m)))
  terminos <- unique(unlist(lapply(matrices, rownames)))
  terminos <- terminos[!grepl("^factor\\(region\\)", terminos)]
  orden <- c(names(etiquetas)[names(etiquetas) %in% terminos],
             setdiff(terminos, names(etiquetas)))
  numero <- function(x) {
    if (!is.finite(x)) return("--")
    if (x != 0 && abs(x) < 0.0005) {
      z <- strsplit(sprintf("%.2e", x), "e", fixed=TRUE)[[1]]
      return(paste0(z[1], "\\times 10^{", as.integer(z[2]), "}"))
    }
    sprintf("%.3f", x)
  }
  estrellas <- function(p) {
    if (!is.finite(p)) "" else if (p<.001) "^{***}" else if (p<.01) "^{**}" else
      if (p<.05) "^{*}" else if (p<.10) "^{\\dagger}" else ""
  }
  fin <- " \\\\"
  fila <- function(nombre, x) paste0(nombre, " & ", paste(x, collapse=" & "), fin)
  out <- c("% Requires graphicx.", "\\begin{table}[htpb]",
    paste0("\\caption{", titulo, "}"), paste0("\\label{", etiqueta, "}"), "\\centering")
  if (!seleccion) out <- c(out, "\\resizebox{\\textwidth}{!}{%")
  out <- c(out, paste0("\\begin{tabular}{l ", paste(rep("c",length(lista)),collapse=" "),"}"),
           "\\hline", fila("", cabeceras), "\\hline")
  for (v in orden) {
    nombre <- unname(etiquetas[v])
    if (is.na(nombre)) nombre <- gsub("_", "\\_", v, fixed=TRUE)
    betas <- vapply(matrices, function(m) {
      if (!v %in% rownames(m) || !is.finite(m[v,1])) return("--")
      paste0("$", numero(m[v,1]), estrellas(m[v,4]), "$")
    }, character(1))
    errores <- vapply(matrices, function(m) {
      if (!v %in% rownames(m) || !is.finite(m[v,2])) return("--")
      paste0("$(",numero(m[v,2]),")$")
    }, character(1))
    out <- c(out, fila(nombre,betas), fila("",errores))
  }
  out <- c(out,"\\hline")
  if (!seleccion) {
    r2 <- vapply(lista, function(m) {
      y <- as.numeric(model.response(model.frame(m)))
      q <- as.numeric(weights(m, type="prior"))
      media <- weighted.mean(y,q)
      sct <- sum(q*(y-media)^2)
      if (!is.finite(sct) || sct<=0) return(NA_real_)
      1-sum(q*(y-as.numeric(fitted(m)))^2)/sct
    }, numeric(1))
    out <- c(out,fila("R$^2$",ifelse(is.finite(r2),sprintf("$%.3f$",r2),"--")))
  }
  nn <- vapply(lista,nobs,numeric(1))
  out <- c(out,fila("Num. obs.",paste0("$",format(nn,big.mark=",",trim=TRUE,scientific=FALSE),"$")),
    "\\hline",paste0("\\multicolumn{",length(lista)+1L,
      "}{l}{\\scriptsize $^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$; $^{\\dagger}p<0.10$.}",fin),
    "\\end{tabular}%")
  if (!seleccion) out <- c(out,"}")
  nota <- paste(
    "Authors' calculations using CASEN 2022 and 2024. Entries are original regression coefficients, with survey-design standard errors in parentheses. Models jointly include assigned sex, gender-minority status, sexual orientation, and the reported interactions. These coefficients are not the average contrasts reported in the main tables.",
    "Region fixed effects are included but omitted from the table. Year effects are displayed. Marital-status category 1 is the reference.",
    if (seleccion) paste(
      "The dependent variable equals one for employed respondents and zero for unemployed or inactive respondents. Entries are employment-probit index coefficients, not marginal effects. Non-labor household income per capita is ypchtotcor minus ypchtrabcor, in levels. The household child-count control is men11c. Conventional R-squared is not reported for the probit.") else paste(
      "Columns (1)--(2) use employed respondents aged 18--65; (3)--(4) restrict ages to 25--65; (5)--(6) use salaried workers aged 18--65, excluding employers, self-employed workers, armed forces personnel, and unpaid family workers. Columns (7)--(8) apply Heckman's two-step correction. The common selection probit additionally includes non-labor household income per capita in levels and men11c. Second-stage covariance treats the estimated inverse Mills ratio as fixed and does not propagate first-stage uncertainty. R-squared is the weighted coefficient of determination."),
    "Small nonzero coefficients and standard errors use scientific notation where necessary. Stars use each model's coefficient-summary p-values.")
  out <- c(out,"\\par\\smallskip","\\begin{minipage}{\\textwidth}\\footnotesize\\raggedright",
           paste0("\\textit{Note:} ",nota),"\\end{minipage}","\\end{table}")
  writeLines(out,ruta,useBytes=TRUE)
  invisible(ruta)
}

cabeceras_wages_completas <- vapply(seq_along(modelos), function(i) {
  paste0("\\shortstack{", if(i %% 2L == 1L) "Ln(Hourly)" else "Ln(Monthly)",
         "\\\\ (",i,")}")
},character(1))
exportar_tabla_completa_wages(
  modelos,cabeceras_wages_completas,
  "Full Wage Regressions by Gender and Sexual Diversity", "tab:wages_joint_full",
  file.path(carpeta,"reg_wages_completas_8_columnas.tex"))
exportar_tabla_completa_wages(
  list(probit_pooled),"Employment",
  "Employment Selection Probit: Full Coefficients", "tab:wages_selection_full",
  file.path(carpeta,"probit_empleo_completo.tex"),seleccion=TRUE)

# =============================================================================
#  RESUMEN FINAL
# =============================================================================

cat("\n============================================================\n")
cat("ARCHIVOS GENERADOS\n")
cat("============================================================\n")
cat("tables/wages/robustez_expr/contrastes_wages_8_columnas.csv\n")
cat("tables/wages/robustez_expr/regresiones_completas_wages_8_columnas.csv\n")
cat("tables/wages/robustez_expr/probit_empleo_completo.csv\n")
cat("tables/wages/robustez_expr/estadisticos_wages_8_columnas.csv\n")
cat("tables/wages/robustez_expr/reg_wages_contrastes_8_columnas.tex\n")
cat("tables/wages/robustez_expr/reg_wages_porcentajes_8_columnas.tex\n")
cat("tables/wages/robustez_expr/reg_wages_completas_8_columnas.tex\n")


compactar_modelo <- function(m) list(coeficientes = coef(m), vcov = vcov(m),
  formula = formula(m), N = nobs(m), df_diseno = degf(m$survey.design),
  df_residual = m$df.residual)
saveRDS(list(probit = compactar_modelo(probit_pooled),
             salarios = lapply(modelos, compactar_modelo),
             contrastes = resultados_contrastes, efectos_empleo = resultados_ame_empleo),
        file.path(carpeta, "modelos_compactos.rds"))
capture.output(sessionInfo(), file = file.path(carpeta, "sessionInfo.txt"))
cat("\nDirectorio final:", normalizePath(carpeta), "\n")


# Identificar las tablas de robustez sin modificar los originales.
nota_ponderador <- paste(
  "Weighting sensitivity exercise: regressions and average contrasts use the full-survey expansion factor (expr),",
  "retaining the original SOGI eligibility rules, controls, and outcome definitions.",
  "This reweighting does not turn the SOGI subsample into the full CASEN sample.")
for (f_tex in list.files(carpeta, pattern = "\\.tex$", full.names = TRUE)) {
  texto <- readLines(f_tex, warn = FALSE, encoding = "UTF-8")
  texto <- gsub("label[{]([^}]+)[}]", "label{\\1_expr}", texto, perl = TRUE)
  texto <- sub("\\textit{Note:}", paste0("\\textit{Note:} ", nota_ponderador),
               texto, fixed = TRUE)
  writeLines(texto, f_tex, useBytes = TRUE)
}

# Comparar los N con los resultados principales ya guardados, si existen.
# No se leen modelos ni checkpoints y no se sobrescriben las tablas principales.
for (f_reg in list.files(carpeta, pattern = "registro_estimacion\\.csv$", full.names = TRUE)) {
  f_base <- file.path("tables/wages", basename(f_reg))
  if (!file.exists(f_base)) next
  base_n <- read.csv(f_base, check.names = FALSE)
  expr_n <- read.csv(f_reg, check.names = FALSE)
  clave <- intersect(c("resultado", "modelo"), intersect(names(base_n), names(expr_n)))[1]
  if (is.na(clave) || !all(c("N") %in% intersect(names(base_n), names(expr_n)))) next
  nn <- merge(base_n[c(clave, "N")], expr_n[c(clave, "N")], by = clave,
              all = TRUE, suffixes = c("_expr_osig", "_expr"))
  nn$diferencia_N <- nn$N_expr - nn$N_expr_osig
  write.csv(nn, file.path(carpeta, paste0("comparacion_N_", basename(f_reg))), row.names = FALSE)
  print(nn, row.names = FALSE)
}
