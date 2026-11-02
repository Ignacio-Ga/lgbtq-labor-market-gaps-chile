# =============================================================================
# OCUPACIONES CON AL MENOS 50% DE MUJERES SEGUN SEXO REGISTRADO
# CASEN 2022 y 2024. Ejecutar desde la raiz del proyecto LGBT-Gaps.
#
# Requiere: haven, dplyr, survey, openxlsx.
# Entradas: Data/casen_2022.sav y Data/casen_2024.sav COMPLETAS.
# Salidas: Tables/ocupaciones-feminizadas/ciuo_3d/ (por defecto).
#
# No modifica los SAV ni los paneles existentes. No estima regresiones ni JKn.
# No escribe checkpoints ni modelos RDS.
# =============================================================================

library(haven)
library(dplyr)
library(survey)
library(openxlsx)

# 1. DECISIONES METODOLOGICAS ---------------------------------------------------
carpeta_datos <- "Data"
carpeta_salida <- file.path("Tables", "ocupaciones-feminizadas")
anios_referencia <- c(2022L, 2024L)
edad_minima <- 18L
edad_maxima <- 65L

# 3 digitos: subgrupos CIUO-08, compromiso entre detalle y tamano muestral.
# Usar 4L para las ocupaciones detalladas de la lista antigua; 2L para grupos
# mas amplios. Cada nivel se guarda en su propia carpeta.
digitos_ocupacion <- 3L
umbral_principal <- 0.50
umbral_sensibilidad <- 0.60
n_revision <- 30L  # Marca de revision, NO regla oficial ni exclusion automatica.

# Los 444 codigos de los Anexos 1 de los libros CASEN 2022 y 2024 coinciden.
# Se excluyen NA, -66, -88, -99 y valores fuera de este catalogo.
# Se conservan FFAA: 0110, 0210, 0310. No se confunden con directivos.
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

codigo_ocupacion <- function(x, digitos = digitos_ocupacion) {
  # Permite numeros o cadenas como "0110". Un factor se lee por su etiqueta.
  z <- suppressWarnings(as.numeric(as.character(x)))
  valido <- !is.na(z) & z %in% catalogo_ciuo4
  salida <- rep(NA_character_, length(z))
  salida[valido] <- substr(sprintf("%04d", as.integer(z[valido])), 1L, digitos)
  salida
}

# 2. CONSTRUIR LA CLASIFICACION -------------------------------------------------
construir_clasificacion_ocupaciones <- function(
    carpeta_datos, carpeta_salida, anios, edad_min, edad_max, digitos,
    corte, corte_alternativo, minimo_n) {

  carpeta <- file.path(carpeta_salida, paste0("ciuo_", digitos, "d"))
  dir.create(carpeta, recursive = TRUE, showWarnings = FALSE)

  # Opcion explicita para estratos con una sola UPM; se restaura al terminar.
  opciones_previas <- options(
    survey.lonely.psu = "adjust",
    survey.adjust.domain.lonely = TRUE
  )
  on.exit(options(opciones_previas), add = TRUE)

  variables <- c("edad", "sexo", "activ", "oficio4_08",
                 "expr", "varstrat", "varunit")
  bases <- diccionarios <- vector("list", length(anios))

  for (j in seq_along(anios)) {
    anio <- anios[j]
    cat(sprintf("\n[%d/%d] Leyendo CASEN %d...\n", j, length(anios), anio))
    flush.console()

    z <- haven::read_sav(
      file.path(carpeta_datos, paste0("casen_", anio, ".sav")),
      col_select = tidyselect::all_of(variables)
    )

    etiquetas <- attr(z$oficio4_08, "labels")
    diccionarios[[j]] <- data.frame(
      oficio4 = as.numeric(etiquetas),
      glosa = if (is.null(names(etiquetas))) character(0) else names(etiquetas),
      stringsAsFactors = FALSE
    )

    z <- as.data.frame(haven::zap_labels(z))
    z$year <- anio
    bases[[j]] <- z
  }

  b <- dplyr::bind_rows(bases)
  b$ocupacion <- codigo_ocupacion(b$oficio4_08, digitos)
  b$mujer_sexo <- ifelse(b$sexo %in% c(1, 2),
                         as.numeric(b$sexo == 2), NA_real_)
  b$edad_objetivo <- !is.na(b$edad) & b$edad >= edad_min & b$edad <= edad_max
  b$ocupado_objetivo <- b$activ %in% 1 & b$edad_objetivo
  b$diseno_valido <- is.finite(b$expr) & b$expr > 0 &
    !is.na(b$varstrat) & !is.na(b$varunit)
  b$elegible <- b$ocupado_objetivo & b$sexo %in% c(1, 2) &
    !is.na(b$ocupacion) & b$diseno_valido

  # Conteos secuenciales: no dependen de ingresos, educacion, SOGI u otros
  # controles de las regresiones. Las exclusiones no se superponen aqui.
  cobertura <- b |>
    group_by(year) |>
    summarise(
      N_SAV = n(),
      N_ocupados_edad = sum(ocupado_objetivo),
      N_diseno_invalido = sum(ocupado_objetivo & !diseno_valido),
      N_sexo_no_clasificable = sum(
        ocupado_objetivo & diseno_valido & !(sexo %in% c(1, 2))
      ),
      N_ocupacion_no_clasificable = sum(
        ocupado_objetivo & diseno_valido & sexo %in% c(1, 2) &
          is.na(ocupacion)
      ),
      N_referencia = sum(elegible),
      .groups = "drop"
    )

  # Identificadores independientes por ola. No se unen personas entre olas.
  b$estrato <- paste(b$year, b$varstrat, sep = "_")
  b$upm <- paste(b$year, b$varstrat, b$varunit, sep = "_")

  # expr es el peso GENERAL: el referente es el empleo nacional, no solo SOGI.
  # Dividir ambas olas por 2 no cambia las proporciones ni sus EE. Los totales
  # conjuntos representaran un promedio de los stocks poblacionales de las olas.
  # No es la media simple de las dos proporciones anuales.
  b$peso <- b$expr / length(anios)

  # Construir antes de seleccionar el dominio conserva la estructura de muestreo.
  diseno <- survey::svydesign(
    ids = ~upm, strata = ~estrato, weights = ~peso, nest = TRUE,
    data = b[b$diseno_valido, , drop = FALSE]
  )
  referencia <- subset(diseno, elegible)

  dic4 <- dplyr::bind_rows(diccionarios) |>
    filter(oficio4 %in% catalogo_ciuo4) |>
    distinct(oficio4, .keep_all = TRUE)
  dic4 <- data.frame(oficio4 = catalogo_ciuo4) |>
    left_join(dic4, by = "oficio4") |>
    mutate(ocupacion = codigo_ocupacion(oficio4, digitos)) |>
    arrange(oficio4)

  # Ejemplos de contenido, NO nombres oficiales de los subgrupos de 2/3 digitos.
  ejemplos <- dic4 |>
    group_by(ocupacion) |>
    summarise(
      ejemplos = paste(head(glosa[!is.na(glosa)], 3L), collapse = " / "),
      .groups = "drop"
    )

  resumir <- function(ds, periodo, numero_olas) {
    cat(sprintf("Calculando composicion y EE Taylor: %s...\n", periodo))
    flush.console()
    v <- ds$variables

    conteos <- v |>
      group_by(ocupacion) |>
      summarise(
        n = n(),
        n_mujeres = sum(mujer_sexo == 1),
        n_hombres = sum(mujer_sexo == 0),
        n_upm = n_distinct(upm),
        empleo_expandido = sum(expr) / numero_olas,
        mujeres_expandidas = sum(expr * mujer_sexo) / numero_olas,
        .groups = "drop"
      )

    medias <- survey::svyby(
      ~mujer_sexo, ~ocupacion, design = ds, FUN = survey::svymean,
      na.rm = TRUE, keep.var = TRUE, drop.empty.groups = TRUE
    )
    estimados <- data.frame(
      ocupacion = as.character(medias$ocupacion),
      proporcion_mujeres = as.numeric(stats::coef(medias)),
      se_taylor = as.numeric(survey::SE(medias)),
      stringsAsFactors = FALSE
    )
    p_global <- as.numeric(stats::coef(survey::svymean(~mujer_sexo, ds)))

    conteos |>
      left_join(estimados, by = "ocupacion") |>
      left_join(ejemplos, by = "ocupacion") |>
      mutate(
        periodo = periodo,
        digitos_ciuo = digitos,
        umbral = corte,
        umbral_alternativo = corte_alternativo,
        porcentaje_mujeres = 100 * proporcion_mujeres,
        porcentaje_mujeres_global = 100 * p_global,
        feminizada = as.integer(proporcion_mujeres >= corte),
        feminizada_umbral_alternativo =
          as.integer(proporcion_mujeres >= corte_alternativo),
        sobrerrepresentacion_femenina =
          as.integer(proporcion_mujeres > p_global),
        n_pequeno = n < minimo_n,
        # IC Wald aproximados solo para proporciones interiores con EE positivo.
        # En celdas 0/1, EE=0 no implica certeza poblacional: se marca revision.
        ic95_inferior = ifelse(
          proporcion_mujeres > 0 & proporcion_mujeres < 1 & se_taylor > 0,
          pmax(0, proporcion_mujeres - qnorm(0.975) * se_taylor), NA_real_
        ),
        ic95_superior = ifelse(
          proporcion_mujeres > 0 & proporcion_mujeres < 1 & se_taylor > 0,
          pmin(1, proporcion_mujeres + qnorm(0.975) * se_taylor), NA_real_
        ),
        ic95_cruza_umbral =
          ic95_inferior <= corte & ic95_superior >= corte,
        revisar_precision = n_pequeno | n_upm < 2 |
          is.na(ic95_cruza_umbral) | ic95_cruza_umbral %in% TRUE
      ) |>
      arrange(ocupacion)
  }

  conjunta <- resumir(
    referencia, paste(anios, collapse = "-"), length(anios)
  )
  anuales <- lapply(anios, function(a) {
    da <- referencia[referencia$variables$year == a, ]
    resumir(da, as.character(a), 1L)
  })
  anual <- bind_rows(anuales)

  # Compara la clasificacion por ola sin cambiar la definicion conjunta.
  estabilidad <- conjunta |>
    select(ocupacion, feminizada_conjunta = feminizada)
  for (a in anios) {
    ta <- anual[anual$periodo == as.character(a),
                c("ocupacion", "proporcion_mujeres", "feminizada", "n")]
    names(ta)[-1L] <- paste0(names(ta)[-1L], "_", a)
    estabilidad <- left_join(estabilidad, ta, by = "ocupacion")
  }
  if (all(c(2022L, 2024L) %in% anios)) {
    estabilidad$cambia_2022_2024 <-
      estabilidad$feminizada_2022 != estabilidad$feminizada_2024
  }

  exportar <- function(x, nombre) {
    write.csv(x, file.path(carpeta, nombre), row.names = FALSE,
              na = "", fileEncoding = "UTF-8")
  }
  exportar(conjunta, "clasificacion_conjunta.csv")
  exportar(anual, "clasificacion_por_anio.csv")
  exportar(estabilidad, "estabilidad_2022_2024.csv")
  exportar(dic4, "diccionario_ciuo4_a_grupo.csv")
  exportar(cobertura, "cobertura_muestra.csv")

  notas <- c(
    "Referente: ocupados de 18 a 65 anos de los SAV completos (ver parametros).",
    paste("Edades configuradas:", edad_min, "-", edad_max),
    paste("Nivel CIUO-08:", digitos, "digitos."),
    paste("Umbral principal >= ", corte, "; alternativo >= ", corte_alternativo),
    "Numerador: suma de expr para sexo=2; denominador: suma de expr para sexo=1 o 2.",
    "sexo es el sexo registrado por CASEN; no se sustituye por genero ni cis.",
    "No se excluyen personas por orientacion sexual, identidad, ingreso o controles.",
    "Se excluyen codigos ocupacionales no clasificables; no se recodifican como 0.",
    "La definicion conjunta se aplica de forma fija a ambas olas.",
    "Los totales conjuntos son promedios de stocks; las proporciones son cocientes",
    "de esos totales, no promedios simples de las proporciones de cada ola.",
    "Los EE se calculan por Taylor con estratos y UPM distintos por ola.",
    "IC Wald aproximados; se dejan vacios si la proporcion es 0/1 o el EE no es positivo.",
    "n<30 es una marca descriptiva de revision, no un estandar oficial de confiabilidad.",
    "Las marcas de precision no excluyen casos automaticamente.",
    "La clasificacion es descriptiva: no mide aceptacion LGBTQ+ ni identifica causalidad.",
    "Una regresion que use esta clasificacion como fija no propaga su incertidumbre.",
    "Fuentes: libros CASEN 2022/2024, hojas Factor, H, Var MDSF, O y Anexo1."
  )
  writeLines(notas, file.path(carpeta, "metodologia.txt"), useBytes = TRUE)

  cat("\nMuestra utilizada:\n")
  print(as.data.frame(cobertura), row.names = FALSE)
  cat("\nGrupos con al menos el umbral de mujeres:",
      sum(conjunta$feminizada == 1, na.rm = TRUE),
      "de", nrow(conjunta), "\n")
  cat("Grupos marcados para revisar precision:",
      sum(conjunta$revisar_precision), "\n")
  cat("Resultados en:", normalizePath(carpeta, winslash = "/"), "\n\n")

  list(
    conjunta = conjunta, anual = anual, estabilidad = estabilidad,
    diccionario = dic4, cobertura = cobertura, carpeta = carpeta,
    parametros = list(anios = anios, edad_min = edad_min, edad_max = edad_max,
                      digitos = digitos, corte = corte,
                      corte_alternativo = corte_alternativo,
                      minimo_n = minimo_n)
  )
}

# 3. INCORPORAR LA MEDIDA A UN PANEL SIN UNIR PERSONAS --------------------------
agregar_ocupacion_feminizada <- function(
    base, resultado = resultado_ocupaciones, var_ocupacion = "oficio4_08") {

  p <- resultado$parametros
  llave <- codigo_ocupacion(base[[var_ocupacion]], p$digitos)
  tab <- resultado$conjunta
  pos <- match(llave, tab$ocupacion)
  elegible <- base$year %in% p$anios & base$activ %in% 1 &
    !is.na(base$edad) & base$edad >= p$edad_min & base$edad <= p$edad_max

  # Estas columnas son ADICIONALES; Feminized_sector se conserva intacta.
  base$Ocupacion_CIUO_grupo <- llave
  base$Female_share <- ifelse(elegible, tab$proporcion_mujeres[pos], NA_real_)
  base$Female_dominated <- ifelse(elegible, tab$feminizada[pos], NA_integer_)
  base$Female_dominated_alt <- ifelse(
    elegible, tab$feminizada_umbral_alternativo[pos], NA_integer_
  )
  base$Female_overrepresented <- ifelse(
    elegible, tab$sobrerrepresentacion_femenina[pos], NA_integer_
  )
  base$Ocupacion_N_referencia <- ifelse(elegible, tab$n[pos], NA_integer_)
  base$Ocupacion_revisar_precision <- ifelse(
    elegible, tab$revisar_precision[pos], NA
  )

  anual <- resultado$anual
  pos_anual <- match(paste(base$year, llave, sep = "|"),
                     paste(anual$periodo, anual$ocupacion, sep = "|"))
  base$Female_dominated_anual <- ifelse(
    elegible, anual$feminizada[pos_anual], NA_integer_
  )

  # Sensibilidad: etiqueta de 2022 aplicada a ambas olas; si no hay soporte,
  # conserva NA. Se calcula solo cuando 2022 forma parte del referente.
  base22 <- anual[anual$periodo == "2022", ]
  pos22 <- match(llave, base22$ocupacion)
  base$Female_dominated_base2022 <- ifelse(
    elegible, base22$feminizada[pos22], NA_integer_
  )
  base
}

# 4. COMPARACION EXCEL: LISTA IA Y CLASIFICACION EMPIRICA ------------------------
# Lista historica compartida por el usuario. La etiqueta IA describe su origen,
# no significa que sus resultados hayan sido validados como clasificacion oficial.
profesiones_feminizadas_IA <- c(
  "1341", "1344", "2221", "2341", "2342", "2352", "2423", "2424",
  "2634", "2635", "3221", "3313", "3342", "3343", "3344", "3412",
  "3611", "4120", "4211", "4224", "4226", "4411", "4416", "5111",
  "5120", "5131", "5141", "5142", "5151", "5152", "5223", "5230",
  "5242", "5246", "5311", "5312", "5321", "5322", "5329", "7531",
  "7533", "8152", "8154", "9111", "9112", "9129", "9411")

exportar_comparacion_ocupaciones_excel <- function(
    resultado = resultado_ocupaciones,
    lista_IA = profesiones_feminizadas_IA,
    archivo = file.path(resultado$carpeta, "comparacion_ocupaciones_IA_vs_datos.xlsx")) {

  p <- resultado$parametros
  dic <- as.data.frame(resultado$diccionario)
  tab <- as.data.frame(resultado$conjunta)
  lista_IA <- unique(sprintf("%04d", as.integer(lista_IA)))
  dic$Codigo_CIUO4 <- sprintf("%04d", as.integer(dic$oficio4))
  # Una fila por ocupacion detallada, incluyendo codigos de la lista antigua
  # que eventualmente no existan en el catalogo. Estos no se cuentan como salidas.
  codigos <- sort(unique(c(dic$Codigo_CIUO4, lista_IA)))
  pos_dic <- match(codigos, dic$Codigo_CIUO4)
  grupo <- dic$ocupacion[pos_dic]
  pos_grupo <- match(grupo, tab$ocupacion)
  valido <- !is.na(pos_dic)
  nombres <- dic$glosa[pos_dic]
  nombres[!valido] <- "Codigo de la lista IA fuera del catalogo CASEN"
  sin_etiqueta <- valido & (is.na(nombres) | trimws(nombres) == "")
  nombres[sin_etiqueta] <- "Nombre no disponible en las etiquetas del SAV"

  ia <- as.integer(codigos %in% lista_IA)
  nueva <- tab$feminizada[pos_grupo]
  estado <- rep("Sin clasificacion nueva", length(codigos))
  estado[!valido] <- "Codigo fuera del catalogo"
  estado[valido & ia == 0 & nueva %in% 1] <- "Entra"
  estado[valido & ia == 1 & nueva %in% 0] <- "Sale"
  estado[valido & ia == 1 & nueva %in% 1] <- "Igual: feminizada"
  estado[valido & ia == 0 & nueva %in% 0] <- "Igual: no feminizada"
  si_no <- function(x) ifelse(is.na(x), "Sin clasificar", ifelse(x == 1, "Si", "No"))

  comparacion <- data.frame(
    Codigo_CIUO4 = codigos,
    Nombre_ocupacion = nombres,
    Metodologia_IA = si_no(ia),
    Metodologia_datos = si_no(nueva),
    Cambio = estado,
    Grupo_CIUO_clasificacion = grupo,
    Proporcion_mujeres_grupo = tab$proporcion_mujeres[pos_grupo],
    N_personas_grupo = tab$n[pos_grupo],
    IC95_inferior_grupo = tab$ic95_inferior[pos_grupo],
    IC95_superior_grupo = tab$ic95_superior[pos_grupo],
    Revisar_precision_grupo = ifelse(is.na(tab$revisar_precision[pos_grupo]),
      "Sin informacion", ifelse(tab$revisar_precision[pos_grupo], "Si", "No")),
    stringsAsFactors = FALSE
  )
  etiqueta_datos <- paste0("Metodologia_datos_", format(100*p$corte, trim=TRUE), "pct_o_mas")
  names(comparacion)[4] <- etiqueta_datos
  estados <- c("Entra", "Sale", "Igual: feminizada", "Igual: no feminizada",
               "Sin clasificacion nueva", "Codigo fuera del catalogo")
  resumen <- data.frame(
    Resultado = c(estados, "Total de codigos comparados"),
    Numero_ocupaciones = c(vapply(estados, function(z) sum(estado == z), integer(1)),
                           length(codigos)), stringsAsFactors=FALSE)

  # Una fila por grupo: aqui los N no se repiten entre ocupaciones detalladas.
  grupos <- data.frame(
    Grupo_CIUO = as.character(tab$ocupacion),
    Proporcion_mujeres = tab$proporcion_mujeres,
    Metodologia_datos = si_no(tab$feminizada),
    N_personas = tab$n,
    N_mujeres = tab$n_mujeres,
    N_hombres = tab$n_hombres,
    N_UPM = tab$n_upm,
    IC95_inferior = tab$ic95_inferior,
    IC95_superior = tab$ic95_superior,
    Revisar_precision = ifelse(tab$revisar_precision, "Si", "No"),
    Ejemplos_ocupaciones = tab$ejemplos,
    stringsAsFactors=FALSE)

  metodologia <- data.frame(
    Tema = c("Unidad de comparacion", "Metodologia IA", "Metodologia de datos",
      "Umbral y expresion 50+1", "Nivel de agregacion", "Porcentajes repetidos",
      "Lectura de N", "Entra", "Sale", "Iguales", "Sin clasificacion",
      "Universo de referencia", "Ponderacion", "Fuente de nombres",
      "Precision", "Cobertura del catalogo", "Interpretacion"),
    Explicacion = c(
      "Una fila por codigo CIUO-08 de cuatro digitos, con su nombre en las etiquetas CASEN.",
      "Pertenencia a la lista historica proporcionada por el usuario y descrita como originada en IA. Los codigos no incluidos se marcan No.",
      paste0("Se copia la clasificacion feminizada del resultado utilizado por las regresiones: proporcion ponderada de sexo=2 >= ", p$corte, "."),
      "Se conserva la regla >=50% del script original. No se denomina 50+1 porque esa expresion indicaria una mayoria estricta >50%. No se cambian las regresiones ni el tratamiento de empates.",
      paste0("La lista IA usa cuatro digitos. La metodologia de datos utiliza ",p$digitos," digitos; cada ocupacion detallada hereda la clasificacion de ese grupo."),
      "Si el nivel es dos o tres digitos, el porcentaje mostrado es el del grupo completo, NO una estimacion individual de cada ocupacion de cuatro digitos.",
      "N_personas_grupo es el N muestral del grupo de referencia y se repite en sus ocupaciones detalladas. No sumar esa columna. La hoja Grupos_CIUO muestra cada grupo una sola vez.",
      "No pertenece a la lista IA y la metodologia de datos la clasifica como feminizada.",
      "Pertenece a la lista IA y la metodologia de datos la clasifica como no feminizada.",
      "Se distingue coincidencia como feminizada de coincidencia como no feminizada.",
      "No hay referencia empirica para el grupo o el codigo no pertenece al catalogo. No se interpreta como no feminizada ni como salida.",
      paste0("Personas ocupadas de ",p$edad_min," a ",p$edad_max," anos; CASEN ",paste(p$anios,collapse=" y "),", bases completas, no solo modulo SOGI."),
      "Peso general expr. Proporcion conjunta calculada como razon de totales ponderados. El umbral se evalua sobre la proporcion sin redondear.",
      "Etiquetas de oficio4_08 de los SAV CASEN 2022/2024; codigos contrastados con Anexo1 de los libros de codigos. No se inventan nombres para codigos sin correspondencia.",
      "Se muestran los IC aproximados y las marcas de revision producidos por el clasificador. Esas marcas no eliminan ocupaciones automaticamente.",
      "Incluye todo el catalogo, no solo ocupaciones presentes en la muestra de regresion. Un codigo sin observaciones propias puede heredar una clasificacion si su grupo mas amplio tiene referencia.",
      "Composicion por sexo registrado; no mide aceptacion de personas LGBTQ+ ni identifica causalidad."),
    stringsAsFactors=FALSE)

  hojas <- list(
    Resumen = resumen,
    Comparacion = comparacion,
    Entran = comparacion[estado == "Entra", , drop=FALSE],
    Salen = comparacion[estado == "Sale", , drop=FALSE],
    Iguales = comparacion[estado %in% c("Igual: feminizada", "Igual: no feminizada"), , drop=FALSE],
    Sin_clasificar = comparacion[estado %in% c("Sin clasificacion nueva", "Codigo fuera del catalogo"), , drop=FALSE],
    Grupos_CIUO = grupos,
    Metodologia = metodologia)

  wb <- openxlsx::createWorkbook()
  cabecera <- openxlsx::createStyle(fontColour="#FFFFFF", fgFill="#17365D",
    textDecoration="bold", wrapText=TRUE, valign="center")
  porcentaje <- openxlsx::createStyle(numFmt="0.0%")
  entero <- openxlsx::createStyle(numFmt="#,##0")
  texto <- openxlsx::createStyle(wrapText=TRUE, valign="top")
  titulo <- openxlsx::createStyle(fontSize=15, textDecoration="bold", fontColour="#17365D")
  colores <- c("Entra"="#E2F0D9", "Sale"="#FCE4D6",
               "Igual: feminizada"="#DDEBF7", "Igual: no feminizada"="#F2F2F2",
               "Sin clasificacion nueva"="#FFF2CC", "Codigo fuera del catalogo"="#FFF2CC")

  for (hoja in names(hojas)) {
    z <- hojas[[hoja]]; nc <- ncol(z)
    openxlsx::addWorksheet(wb, hoja, gridLines=FALSE)
    openxlsx::writeData(wb, hoja, paste("Ocupaciones feminizadas -",hoja), startRow=1)
    openxlsx::mergeCells(wb, hoja, cols=seq_len(nc), rows=1)
    openxlsx::addStyle(wb, hoja, titulo, rows=1, cols=1)
    openxlsx::writeData(wb, hoja,
      paste0("IA versus datos: proporcion >= ",100*p$corte,"%; CIUO a ",p$digitos," digitos."),startRow=2)
    openxlsx::mergeCells(wb, hoja, cols=seq_len(nc), rows=2)
    if (nrow(z)) {
      openxlsx::writeDataTable(wb, hoja, z, startRow=4, tableStyle="TableStyleLight9",
        tableName=paste0("Tabla_",hoja), headerStyle=cabecera, keepNA=FALSE)
      openxlsx::addStyle(wb, hoja, texto, rows=5:(nrow(z)+4), cols=seq_len(nc),
        gridExpand=TRUE, stack=TRUE)
      openxlsx::setRowHeights(wb, hoja, rows=5:(nrow(z)+4), heights=44)
      pct <- grep("Proporcion|IC95",names(z))
      if(length(pct)) openxlsx::addStyle(wb, hoja, porcentaje,
        rows=5:(nrow(z)+4),cols=pct,gridExpand=TRUE,stack=TRUE)
      cnt <- grep("^N_|^Numero_",names(z))
      if(length(cnt)) openxlsx::addStyle(wb, hoja, entero,
        rows=5:(nrow(z)+4),cols=cnt,gridExpand=TRUE,stack=TRUE)
      if("Cambio" %in% names(z)) for (st in names(colores)) {
        rr <- which(z$Cambio==st)+4L
        if(length(rr)) openxlsx::addStyle(wb, hoja,
          openxlsx::createStyle(fgFill=unname(colores[st])),rows=rr,
          cols=match("Cambio",names(z)),gridExpand=TRUE,stack=TRUE)
      }
    } else {
      openxlsx::writeData(wb, hoja, z, startRow=4, headerStyle=cabecera)
      openxlsx::writeData(wb, hoja, "No hay ocupaciones en esta categoria.",startRow=5)
    }
    openxlsx::setColWidths(wb, hoja, cols=seq_len(nc), widths=22)
    nombres_col <- grep("Nombre_ocupacion|Ejemplos_ocupaciones",names(z))
    if(length(nombres_col)) openxlsx::setColWidths(wb, hoja, cols=nombres_col,widths=70)
    openxlsx::setRowHeights(wb, hoja, rows=4, heights=44)
    openxlsx::freezePane(wb, hoja, firstActiveRow=5, firstActiveCol=3)
  }
  openxlsx::setColWidths(wb,"Resumen",cols=1:2,widths=c(42,26))
  openxlsx::setColWidths(wb,"Metodologia",cols=1:2,widths=c(30,110))
  openxlsx::setRowHeights(wb,"Metodologia",rows=5:(nrow(metodologia)+4),heights=64)
  dir.create(dirname(archivo),recursive=TRUE,showWarnings=FALSE)
  openxlsx::saveWorkbook(wb, archivo, overwrite=TRUE)
  cat("\nExcel comparativo guardado en:\n",normalizePath(archivo,winslash="/"),"\n")
  print(resumen,row.names=FALSE)
  invisible(list(archivo=archivo,comparacion=comparacion,resumen=resumen))
}

# 5. EJECUCION ----------------------------------------------------------------
resultado_ocupaciones <- construir_clasificacion_ocupaciones(
  carpeta_datos = carpeta_datos,
  carpeta_salida = carpeta_salida,
  anios = anios_referencia,
  edad_min = edad_minima, edad_max = edad_maxima,
  digitos = digitos_ocupacion,
  corte = umbral_principal,
  corte_alternativo = umbral_sensibilidad,
  minimo_n = n_revision
)
clasificacion_ocupaciones <- resultado_ocupaciones$conjunta
comparacion_ocupaciones_excel <- exportar_comparacion_ocupaciones_excel(
  resultado = resultado_ocupaciones,
  lista_IA = profesiones_feminizadas_IA
)

# =============================================================================
# 7. TABLA LATEX: OCUPACIONES INCLUIDAS EN LA CLASIFICACION FEMINIZADA
# Ejecutar despues de construir resultado_ocupaciones.
# No requiere volver a estimar las regresiones.
# =============================================================================

exportar_ocupaciones_feminizadas_tex <- function(
    resultado = resultado_ocupaciones) {
  
  p <- resultado$parametros
  
  grupos <- resultado$conjunta |>
    dplyr::filter(feminizada == 1L) |>
    dplyr::select(
      ocupacion,
      porcentaje_mujeres
    )
  
  tabla <- resultado$diccionario |>
    dplyr::inner_join(grupos, by = "ocupacion") |>
    dplyr::mutate(
      codigo = sprintf("%04d", as.integer(oficio4)),
      nombre = dplyr::if_else(
        is.na(glosa) | trimws(glosa) == "",
        "Nombre no disponible en las etiquetas del SAV",
        glosa
      )
    ) |>
    dplyr::arrange(ocupacion, oficio4) |>
    dplyr::distinct(codigo, .keep_all = TRUE)
  
  # Escapar caracteres especiales sin alterar los comandos introducidos.
  escapar_tex <- function(x) {
    equivalencias <- c(
      "\\" = "\\textbackslash{}",
      "&"  = "\\&",
      "%"  = "\\%",
      "$"  = "\\$",
      "#"  = "\\#",
      "_"  = "\\_",
      "{"  = "\\{",
      "}"  = "\\}",
      "~"  = "\\textasciitilde{}",
      "^"  = "\\textasciicircum{}"
    )
    
    vapply(as.character(x), function(texto) {
      caracteres <- strsplit(texto, "", fixed = TRUE)[[1]]
      posiciones <- match(caracteres, names(equivalencias))
      cambiar <- !is.na(posiciones)
      caracteres[cambiar] <- unname(
        equivalencias[posiciones[cambiar]]
      )
      paste0(caracteres, collapse = "")
    }, character(1), USE.NAMES = FALSE)
  }
  
  filas <- sprintf(
    "%s & %s & %s & %.1f \\\\",
    tabla$codigo,
    escapar_tex(tabla$nombre),
    tabla$ocupacion,
    tabla$porcentaje_mujeres
  )
  
  nota <- sprintf(
    paste0(
      "\\textit{Notes:} Four-digit CIUO-08 occupations belonging ",
      "to groups classified as female-dominated at the %d-digit level. ",
      "A group is classified as female-dominated when women ",
      "(CASEN recorded sex: sexo=2) account for at least %.0f\\%% ",
      "of its weighted employment. The classification pools CASEN %s, ",
      "using employed respondents aged %d--%d and the full-survey ",
      "expansion factor (expr). Female shares refer to the classification ",
      "group, not necessarily to each four-digit occupation. ",
      "All catalogued occupations within qualifying groups are listed; ",
      "their inclusion does not imply that each detailed occupation ",
      "was observed in the reference sample. ",
      "Occupation names reproduce the Spanish labels in the CASEN files. ",
      "This measure describes employment composition and does not ",
      "measure workplace inclusion or acceptance."
    ),
    p$digitos,
    100 * p$corte,
    paste(p$anios, collapse = " and "),
    p$edad_min,
    p$edad_max
  )
  
  encabezado <- c(
    "\\begingroup",
    "\\small",
    "\\setlength{\\tabcolsep}{4pt}",
    "\\renewcommand{\\arraystretch}{1.08}",
    "\\begin{longtable}{@{}c p{0.56\\textwidth} c r@{}}",
    "\\caption{Female-Dominated Occupation Variable Construction}",
    "\\label{Atab:Feminized}\\\\",
    "\\toprule",
    paste0(
      "\\textbf{Code} & \\textbf{Occupation} & ",
      "\\textbf{Group} & \\textbf{Female share (\\%)} \\\\"
    ),
    "\\midrule",
    "\\endfirsthead",
    "\\multicolumn{4}{c}{\\tablename\\ \\thetable{} -- Continued}\\\\",
    "\\toprule",
    paste0(
      "\\textbf{Code} & \\textbf{Occupation} & ",
      "\\textbf{Group} & \\textbf{Female share (\\%)} \\\\"
    ),
    "\\midrule",
    "\\endhead",
    "\\midrule",
    "\\multicolumn{4}{r}{\\footnotesize Continued on next page}\\\\",
    "\\endfoot",
    "\\bottomrule",
    paste0(
      "\\multicolumn{4}{p{0.95\\textwidth}}{\\footnotesize ",
      nota, "}\\\\"
    ),
    "\\endlastfoot"
  )
  
  archivo <- file.path(
    resultado$carpeta,
    "tabla_ocupaciones_feminizadas.tex"
  )
  
  writeLines(
    enc2utf8(c(
      encabezado,
      filas,
      "\\end{longtable}",
      "\\endgroup"
    )),
    con = archivo,
    useBytes = TRUE
  )
  
  cat(
    "\nTabla LaTeX guardada en:",
    normalizePath(archivo, winslash = "/", mustWork = TRUE),
    "\nOcupaciones detalladas incluidas:", nrow(tabla),
    "\nGrupos clasificados como feminizados:", nrow(grupos),
    "\n"
  )
  
  invisible(tabla)
}

tabla_ocupaciones_feminizadas <- exportar_ocupaciones_feminizadas_tex(
  resultado_ocupaciones
)

