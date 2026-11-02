# ============================================================================
# SENSIBILIDAD AL UMBRAL DE OCUPACION FEMINIZADA: 40%, 50%, 60%, 70%
# Continuacion del script principal de LABOR, en la misma sesion de R.
# ============================================================================

sensibilidad_umbral_feminizacion <- function(
    entorno = parent.frame(),
    umbrales = c(0.40, 0.50, 0.60, 0.70),
    salida = "Tables/labor/sensibilidad_umbral_feminizacion",
    var_ocupacion = "CIIU",
    modulo = NULL) {

  for (obj in c("diseno", "rhs", "mascaras")) {
    if (!exists(obj, envir = entorno, inherits = TRUE)) {
      stop("Ejecute primero labor en esta sesion: falta el objeto ", obj, ".")
    }
  }
  if (!requireNamespace("survey", quietly = TRUE) ||
      !requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Instale los paquetes survey y ggplot2 antes de ejecutar este archivo.")
  }
  ds <- get("diseno", envir = entorno, inherits = TRUE)
  rhs_local <- get("rhs", envir = entorno, inherits = TRUE)
  mascara <- get("mascaras", envir = entorno, inherits = TRUE)[["Feminized_sector"]]
  if (is.null(mascara) || length(mascara) != nrow(ds$variables)) {
    stop("La mascara Feminized_sector no corresponde al diseno de labor.")
  }

  if (!inherits(ds, "survey.design") || inherits(ds, "svyrep.design")) {
    stop("Use el diseno Taylor del labor principal, no el diseno JKn desagregado.")
  }
  umbrales <- sort(unique(as.numeric(umbrales)))
  if (any(!is.finite(umbrales)) || any(umbrales <= 0 | umbrales >= 1) ||
      !any(abs(umbrales - 0.50) < 1e-12)) {
    stop("Los umbrales deben estar entre 0 y 1 e incluir 0.50.")
  }


  if (is.null(modulo)) {
    if (exists(".sf_cache", envir = entorno, inherits = TRUE)) {
      cache <- get(".sf_cache", envir = entorno, inherits = TRUE)
      if (is.environment(cache) && exists("modulo", envir = cache, inherits = FALSE)) {
        modulo <- get("modulo", envir = cache, inherits = FALSE)
      }
    }
    if (is.null(modulo) &&
        exists("resultado_ocupaciones", envir = entorno, inherits = TRUE) &&
        exists("agregar_ocupacion_feminizada", envir = entorno, inherits = TRUE)) {
      modulo <- list(
        resultado_ocupaciones = get("resultado_ocupaciones", envir = entorno, inherits = TRUE),
        agregar_ocupacion_feminizada = get("agregar_ocupacion_feminizada", envir = entorno, inherits = TRUE)
      )
    }
  }
  if (is.null(modulo) || is.null(modulo$resultado_ocupaciones) ||
      !is.function(modulo$agregar_ocupacion_feminizada)) {
    stop(paste(
      "Falta el clasificador. Ejecute primero la comparacion de ocupaciones de labor",
      "o source('scripts/ocupaciones_feminizadas_casen.R') en un entorno separado",
      "y paselo mediante el argumento modulo."
    ))
  }
  referencia <- modulo$resultado_ocupaciones
  parametros <- referencia$parametros
  if (!isTRUE(parametros$digitos == 3) ||
      !setequal(parametros$anios, c(2022, 2024)) ||
      !isTRUE(parametros$edad_min == 18) || !isTRUE(parametros$edad_max == 65)) {
    stop("Este ejercicio requiere el clasificador conjunto 2022-2024, edades 18-65, CIUO-08 de 3 digitos.")
  }
  x <- modulo$agregar_ocupacion_feminizada(
    ds$variables, resultado = referencia, var_ocupacion = var_ocupacion)
  if (!all(c("Female_share", "Feminized_sector", "Ocupacion_CIUO_grupo") %in% names(x))) {
    stop("Faltan Female_share, Feminized_sector o el codigo de ocupacion.")
  }
  proporcion <- x$Female_share
  clasificada <- is.finite(proporcion) & proporcion >= 0 & proporcion <= 1
  x$SF_sensibilidad <- ifelse(clasificada, as.integer(proporcion >= 0.50), NA_integer_)
  formula <- stats::as.formula(paste("SF_sensibilidad ~", rhs_local))
  f_original <- stats::as.formula(paste("Feminized_sector ~", rhs_local))
  completos <- function(f, datos) {
    mf <- stats::model.frame(f, data = datos, na.action = stats::na.pass)
    ok <- stats::complete.cases(mf)
    for (v in names(mf)) if (is.numeric(mf[[v]])) ok <- ok & is.finite(mf[[v]])
    ok
  }

  original <- mascara %in% TRUE & completos(f_original, x)
  comun <- original & completos(formula, x) & clasificada
  ds$variables <- x
  ds_comun <- ds[comun, ]
  q <- as.numeric(stats::weights(ds_comun, type = "sampling"))
  if (!length(q) || any(!is.finite(q) | q <= 0)) {
    stop("La muestra comun esta vacia o contiene pesos invalidos.")
  }
  q <- q / sum(q)
  gl <- survey::degf(ds_comun)
  if (!is.finite(gl) || gl <= 0) stop("No hay grados de libertad de diseno positivos.")
  critico <- stats::qt(0.975, df = gl)
  cat("\nMuestra fija:", sum(comun), "ocupados | gl de diseno:", gl, "\n")

  escenarios <- list(
    list("A", "AFAB cis vs AMAB cis", list(AFAB=1, GM=0), list(AFAB=0, GM=0)),
    list("A", "AMAB GM vs AMAB cis", list(AFAB=0, GM=1), list(AFAB=0, GM=0)),
    list("A", "AFAB GM vs AMAB cis", list(AFAB=1, GM=1), list(AFAB=0, GM=0)),
    list("A", "AFAB GM vs AFAB cis", list(AFAB=1, GM=1), list(AFAB=1, GM=0)),
    list("A", "AFAB GM vs AMAB GM", list(AFAB=1, GM=1), list(AFAB=0, GM=1)),
    list("B", "AMAB LG vs AMAB heterosexual", list(AFAB=0, LG=1, bisexual=0, Other=0), list(AFAB=0, LG=0, bisexual=0, Other=0)),
    list("B", "AFAB LG vs AFAB heterosexual", list(AFAB=1, LG=1, bisexual=0, Other=0), list(AFAB=1, LG=0, bisexual=0, Other=0)),
    list("B", "AMAB bisexual vs AMAB heterosexual", list(AFAB=0, LG=0, bisexual=1, Other=0), list(AFAB=0, LG=0, bisexual=0, Other=0)),
    list("B", "AFAB bisexual vs AFAB heterosexual", list(AFAB=1, LG=0, bisexual=1, Other=0), list(AFAB=1, LG=0, bisexual=0, Other=0)),
    list("B", "AMAB other vs AMAB heterosexual", list(AFAB=0, LG=0, bisexual=0, Other=1), list(AFAB=0, LG=0, bisexual=0, Other=0)),
    list("B", "AFAB other vs AFAB heterosexual", list(AFAB=1, LG=0, bisexual=0, Other=1), list(AFAB=1, LG=0, bisexual=0, Other=0))
  )
  # Cada contraste es L'beta; se preserva la orientacion observada en A,
  # y GM observado en B, igual que en la tabla principal del modelo conjunto.
  contrastes <- function(m) {
    b <- stats::coef(m)
    if (anyNA(b)) stop("Hay coeficientes no identificados; no se exportaran contrastes ambiguos.")
    V <- stats::vcov(m)[names(b), names(b), drop = FALSE]
    promedio_X <- function(cambios) {
      datos <- m$survey.design$variables
      for (v in names(cambios)) datos[[v]] <- cambios[[v]]
      X <- stats::model.matrix(stats::delete.response(stats::terms(m)),
                               data = datos, contrasts.arg = m$contrasts,
                               xlev = m$xlevels)
      colSums(X[, names(b), drop = FALSE] * q)
    }
    do.call(rbind, lapply(seq_along(escenarios), function(i) {
      e <- escenarios[[i]]
      L <- promedio_X(e[[3]]) - promedio_X(e[[4]])
      estimacion <- sum(L * b)
      ee <- sqrt(max(0, as.numeric(t(L) %*% V %*% L)))
      p <- if (ee > 0) 2 * stats::pt(abs(estimacion / ee), df = gl, lower.tail = FALSE) else NA_real_
      data.frame(orden = i, panel = e[[1]], contraste = e[[2]],
                 estimacion = estimacion, error_estandar = ee,
                 ic95_inferior = estimacion - critico * ee,
                 ic95_superior = estimacion + critico * ee,
                 p_value = p, N = stats::nobs(m), gl_diseno = gl)
    }))
  }

  dir.create(salida, recursive = TRUE, showWarnings = FALSE)
  modelos <- efectos <- coeficientes <- resumen <- list()
  y50 <- as.integer(ds_comun$variables$Female_share >= 0.50)
  for (j in seq_along(umbrales)) {
    corte <- umbrales[j]
    cat("Estimando MPL con umbral", 100 * corte, "%...\n")
    ds_j <- ds_comun
    y <- as.integer(ds_j$variables$Female_share >= corte)
    ds_j$variables$SF_sensibilidad <- y
    m <- survey::svyglm(formula, design = ds_j,
                        family = stats::gaussian(), na.action = stats::na.fail)
    modelos[[j]] <- m
    e <- contrastes(m); e$umbral <- corte; e$umbral_pct <- 100 * corte
    efectos[[j]] <- e
    cs <- stats::coef(summary(m))
    coeficientes[[j]] <- data.frame(
      umbral = corte, termino = rownames(cs), beta = cs[, 1],
      error_estandar = cs[, 2], estadistico = cs[, 3],
      p_value = cs[, 4], N = stats::nobs(m), row.names = NULL)
    mu <- sum(q * y)
    sst <- sum(q * (y - mu)^2)
    resumen[[j]] <- data.frame(
      umbral = corte, N = stats::nobs(m),
      N_en_ocupacion_feminizada = sum(y),
      porcentaje_ponderado_en_ocupacion_feminizada = 100 * mu,
      ocupaciones_clasificadas_en_muestra = length(unique(ds_j$variables$Ocupacion_CIUO_grupo)),
      ocupaciones_feminizadas_en_muestra = length(unique(ds_j$variables$Ocupacion_CIUO_grupo[y == 1])),
      N_que_cambia_respecto_50 = sum(y != y50),
      porcentaje_ponderado_que_cambia_respecto_50 = 100 * sum(q * (y != y50)),
      R2_ponderado = if (sst > 0) 1 - sum(q * stats::residuals(m)^2) / sst else NA_real_)
  }
  efectos <- do.call(rbind, efectos)
  resumen <- do.call(rbind, resumen)
  nombres <- vapply(escenarios, function(e) e[[2]], character(1))

  # DIFERENCIA PAREADA ENTRE UMBRALES:
  # Con MPL, mismos X, pesos y personas: beta(Y_c - Y_50) = beta_c - beta_50.
  # Ajustar la diferencia de resultados incorpora su covarianza.
  diferencias <- list()
  for (j in seq_along(umbrales)) {
    corte <- umbrales[j]
    if (abs(corte - 0.50) < 1e-12) {
      e <- efectos[efectos$umbral == corte, ]
      e$estimacion <- e$error_estandar <- e$ic95_inferior <- e$ic95_superior <- 0
      e$p_value <- NA_real_
    } else {
      dd <- ds_comun
      dd$variables$SF_diferencia <- as.integer(dd$variables$Female_share >= corte) - y50
      md <- survey::svyglm(stats::as.formula(paste("SF_diferencia ~", rhs_local)),
                           design = dd, family = stats::gaussian(), na.action = stats::na.fail)
      e <- contrastes(md); e$umbral <- corte; e$umbral_pct <- 100 * corte
      esperado <- efectos$estimacion[efectos$umbral == corte] -
        efectos$estimacion[abs(efectos$umbral - 0.50) < 1e-12]
      if (!isTRUE(all.equal(e$estimacion, esperado, tolerance = 1e-7))) {
        stop("La diferencia pareada no coincide con la diferencia de contrastes.")
      }
    }
    diferencias[[j]] <- e
  }
  diferencias <- do.call(rbind, diferencias)
  diferencias$comparacion <- "contraste(umbral) - contraste(50%)"

  exportar <- function(x, nombre) {
    utils::write.csv(x, file.path(salida, nombre), row.names = FALSE,
                     na = "", fileEncoding = "UTF-8")
  }
  exportar(efectos, "contrastes_por_umbral.csv")
  exportar(do.call(rbind, coeficientes), "coeficientes_completos_por_umbral.csv")
  exportar(resumen, "muestra_y_prevalencia_por_umbral.csv")
  exportar(diferencias, "diferencias_pareadas_respecto_50.csv")
  exportar(data.frame(fila_diseno = which(comun),
                     year = ds_comun$variables$year,
                     ocupacion = ds_comun$variables$Ocupacion_CIUO_grupo,
                     proporcion_mujeres = ds_comun$variables$Female_share),
           "muestra_comun.csv")
  clasificaciones <- referencia$conjunta
  for (corte in umbrales) {
    clasificaciones[[paste0("feminizada_", round(100 * corte))]] <-
      as.integer(clasificaciones$proporcion_mujeres >= corte)
  }
  exportar(clasificaciones, "clasificacion_ocupaciones_por_umbral.csv")

  # No se multiplica por 100 el eje vertical: sigue la escala de las tablas MPL.
  # Los intervalos son puntuales del 95%, no bandas simultaneas.
  grafico <- function(datos, titulo, eje_y, ncol = 3) {
    datos$contraste <- factor(datos$contraste, levels = nombres)
    datos$principal <- abs(datos$umbral - 0.50) < 1e-12
    ggplot2::ggplot(datos, ggplot2::aes(x = umbral_pct, y = estimacion, group = contraste)) +
      ggplot2::geom_hline(yintercept = 0, colour = "grey65", linewidth = 0.35) +
      ggplot2::geom_vline(xintercept = 50, colour = "#206095", linetype = "dotted", linewidth = 0.45) +
      ggplot2::geom_line(colour = "grey45", linewidth = 0.5) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = ic95_inferior, ymax = ic95_superior,
                                        colour = principal), width = 1.6, linewidth = 0.6) +
      ggplot2::geom_point(ggplot2::aes(colour = principal), size = 2.5) +
      ggplot2::scale_colour_manual(values = c("FALSE" = "grey35", "TRUE" = "#206095"), guide = "none") +
      ggplot2::scale_x_continuous(breaks = 100 * umbrales,
                                 labels = paste0(round(100 * umbrales), "%")) +
      ggplot2::facet_wrap(~contraste, ncol = ncol,
                          labeller = ggplot2::label_wrap_gen(width = 29)) +
      ggplot2::labs(title = titulo,
        subtitle = paste0("Linear probability models | Fixed sample: N = ",
                          format(sum(comun), big.mark = ",", scientific = FALSE)),
        x = "Minimum female share in occupation", y = eje_y,
        caption = paste("Points: survey-weighted contrasts; bars: pointwise 95% confidence intervals.",
                        "Blue marks the 50% definition. Classification uncertainty is not included.", sep = "\n")) +
      ggplot2::theme_bw(base_size = 11) +
      ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                     strip.background = ggplot2::element_rect(fill = "grey95"),
                     plot.caption = ggplot2::element_text(hjust = 0, size = 9),
                     plot.title = ggplot2::element_text(face = "bold"))
  }
  guardar_grafico <- function(g, nombre, alto) {
    ggplot2::ggsave(file.path(salida, paste0(nombre, ".pdf")),
                    plot = g, width = 12, height = alto, units = "in")
    ggplot2::ggsave(file.path(salida, paste0(nombre, ".png")),
                    plot = g, width = 12, height = alto, units = "in", dpi = 300)
  }
  ga <- grafico(efectos[efectos$panel == "A", ],
                 "Sensitivity to the female-dominated occupation threshold: gender",
                 "Adjusted difference (probability units)")
  gb <- grafico(efectos[efectos$panel == "B", ],
                 "Sensitivity to the female-dominated occupation threshold: sexual orientation",
                 "Adjusted difference (probability units)")
  gt <- grafico(efectos, "Sensitivity to the female-dominated occupation threshold",
                 "Adjusted difference (probability units)")
  gd <- grafico(diferencias, "Change in contrasts relative to the 50% threshold",
                 "Difference from the 50% contrast (probability units)")
  guardar_grafico(ga, "sensibilidad_panel_A", 7.5)
  guardar_grafico(gb, "sensibilidad_panel_B", 7.5)
  guardar_grafico(gt, "sensibilidad_todos_contrastes", 12)
  guardar_grafico(gd, "diferencias_respecto_50", 12)

  # Tabla auxiliar de contrastes, sin estrellas para no elegir el umbral por p.
  fin <- " \\\\"
  filas <- c("\\begin{table}[htbp]", "\\centering\\small",
             "\\caption{Sensitivity to the Female-Dominated Occupation Threshold}",
             "\\label{tab:sensibilidad_umbral_feminizacion}",
             "\\resizebox{\\textwidth}{!}{%",
             paste0("\\begin{tabular}{l", paste(rep("c", length(umbrales)), collapse = ""), "}"),
             "\\hline", paste0("Contrast & ", paste(paste0(round(100 * umbrales), "\\%"), collapse = " & "), fin),
             "\\hline")
  for (i in seq_along(escenarios)) {
    e <- efectos[efectos$orden == i, ]; e <- e[order(e$umbral), ]
    if (i %in% c(1, 6)) filas <- c(filas, paste0("\\multicolumn{", length(umbrales) + 1,
        "}{l}{\\textit{Panel ", escenarios[[i]][[1]], "}}", fin))
    filas <- c(filas, paste0(nombres[i], " & ", paste(sprintf("$%.3f$", e$estimacion), collapse = " & "), fin),
                paste0(" & ", paste(sprintf("$(%.3f)$", e$error_estandar), collapse = " & "), fin))
  }
  filas <- c(filas, "\\hline", paste0("Num. obs. & ",
    paste(format(resumen$N, big.mark = ",", trim = TRUE, scientific = FALSE), collapse = " & "), fin),
    "\\hline", "\\end{tabular}}", "\\par\\smallskip",
    "\\begin{minipage}{\\textwidth}\\footnotesize",
    "\\textit{Note:} All columns use the same respondents, controls, and survey design. Entries are survey-weighted average contrasts in probability units, with Taylor standard errors in parentheses. Occupations are classified using the fixed female employment shares from the complete CASEN 2022--2024 surveys, ages 18--65, weighted by expr. Regressions use expr\\_osig. The threshold is inclusive. Classification uncertainty is not propagated into standard errors.",
    "\\end{minipage}", "\\end{table}")
  writeLines(filas, file.path(salida, "tabla_sensibilidad_umbrales.tex"), useBytes = TRUE)

  notas <- c(
    "Ejercicio de sensibilidad: la variable dependiente cambia con el umbral.",
    "No demuestra que 50% sea el umbral verdadero u optimo.",
    "50% corresponde a al menos la mitad del empleo de la ocupacion femenino.",
    "40% describe presencia femenina elevada; no necesariamente mayoria.",
    "60% y 70% exigen concentracion femenina mas intensa.",
    "El umbral inclusivo >=50% conserva la definicion de la tabla principal.",
    "La clasificacion se hereda del referente nacional conjunto 2022-2024, con expr.",
    "No se calculan proporciones de ocupacion dentro de la muestra OSIG.",
    "Regresiones MPL con expr_osig; mismas personas y controles en todos los umbrales.",
    paste("Formula:", paste(deparse(formula), collapse = " ")),
    paste("N original con controles:", sum(original)),
    paste("N comun:", sum(comun)),
    paste("Excluidos por no disponer de clasificacion:", sum(original & !comun)),
    "Los graficos muestran los contrastes de la tabla principal, no solo beta_AFAB.",
    "Para ver parametros individuales utilice coeficientes_completos_por_umbral.csv.",
    "Escala de contrastes: probabilidad; multiplicar por 100 para puntos porcentuales.",
    "IC puntuales al 95% con t y grados de libertad del diseno, como sf_contrastes.",
    "No se corrige por comparaciones multiples ni se propaga incertidumbre de clasificacion.",
    "La comparacion pareada usa regresiones de Y_umbral - Y_50 sobre el mismo X.",
    "Los IC de diferencias incorporan la dependencia entre estimaciones; no asumir independencia.",
    "Los coeficientes completos usan los p-values de summary(svyglm); los contrastes usan gl del diseno.",
    "Ningun umbral se selecciona en funcion de significancia, magnitud o R2.",
    "Las lineas unen los cuatro umbrales evaluados; no representan una curva estimada entre ellos.")
  writeLines(notas, file.path(salida, "LEEME_metodologia.txt"))
  capture.output(utils::sessionInfo(), file = file.path(salida, "sessionInfo.txt"))
  print(resumen, row.names = FALSE)
  print(gt)
  cat("\nResultados guardados en:", normalizePath(salida, winslash = "/"), "\n")
  invisible(list(contrastes = efectos, diferencias = diferencias,
                 resumen = resumen, graficos = list(A = ga, B = gb, todos = gt)))
}

# EJECUCION: agregar al final de labor o ejecutar por source en la misma sesion.
resultado_sensibilidad_feminizacion <- sensibilidad_umbral_feminizacion()
