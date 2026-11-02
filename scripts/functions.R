library(dplyr)
library(tidyr)
library(rlang)
library(survey)

# =============================================================================
# FUNCTIONS - LGBT Gaps
# =============================================================================

# --- Funciones de procesamiento ----------------------------------------------

calcular_menores_hogar <- function(base_casen, edad_corte) {
  
  # Definimos los nombres de las columnas dinámicas
  col_hijos <- paste0("cantidad_menores_", edad_corte)
  col_todos <- paste0("men", edad_corte, "c")
  
  base_final <- base_casen %>%
    # 1. Creamos la variable auxiliar para identificar a los hijos
    mutate(
      hjh = as.numeric(pco1_b %in% c(4, 5, 6))
    ) %>%
    # Agrupamos por hogar
    group_by(id_vivienda, folio) %>%
    mutate(
      # 2. Sumamos solo los que son hijos del jefe de hogar (<= edad_corte)
      !!sym(col_hijos) := sum(edad <= edad_corte & hjh == 1, na.rm = TRUE),
      
      # 3. Sumamos a todas las personas en el hogar (<= edad_corte)
      !!sym(col_todos) := sum(edad <= edad_corte, na.rm = TRUE),
      
      # 4. Sumamos a todas las personas estrictamente menores de 18 años
      men18numper = sum(edad < 18, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    # 5. Eliminamos la variable auxiliar 'hjh'
    select(-hjh)
  
  return(base_final)
}

crear_variables_comunes <- function(df) {
  df %>%
    mutate(
      AMAB = ifelse(sexo == 1, 1, 0),
      AFAB = ifelse(sexo == 2, 1, 0),
      cis = ifelse(sexo == genero, 1, 0),
      hrs = ifelse(hrs < 0, NA, hrs),
      hrs_m = hrs * 4.33,
      w_cepal_hrs = ifelse(hrs_m == 0 | is.na(hrs_m), NA, w_cepal / hrs_m),
      w_mdsf_hrs = ifelse(hrs_m == 0 | is.na(hrs_m), NA, w_mdsf / hrs_m),
      ln_w_cepal = log(w_cepal / hrs_m),
      ln_w_cepal = ifelse(hrs_m == 0 | is.na(hrs_m) | is.infinite(ln_w_cepal), NA, ln_w_cepal),
      ln_w_mdsf = log(w_mdsf / hrs_m),
      ln_w_mdsf = ifelse(hrs_m == 0 | is.na(hrs_m) | is.infinite(ln_w_mdsf), NA, ln_w_mdsf),
      exp = pmax(edad - esc - 6, 0),
      exp2 = exp^2,
      edad2 = edad^2,
      extranjero = as.numeric(lugar_nac == 1),
      casado = ifelse(ecivil == 1, 1, 0),
      pareja = ifelse(ecivil == 2, 1, 0),
      separado = ifelse(ecivil == 5, 1, 0),
      divorciado = ifelse(ecivil == 6, 1, 0),
      viudo = ifelse(ecivil == 7, 1, 0),
      soltero = ifelse(ecivil == 8, 1, 0),
      salud = as.numeric(s28 != 22),
      y_cepal = ((ymonecorh - yautcorh) / tot_per_h),
      y_cepal = ifelse(is.na(ymonecorh), 0, y_cepal),
      y_cepal_1000 = y_cepal/1000,
      y_mdsf = ypchtotcor - ypchautcor,
      y_mdsf = ifelse(is.na(ypchtotcor), 0, y_mdsf),
      y_mdsf_1000 = y_mdsf/1000
    )
}

# --- Funciones de visualización ----------------------------------------------

# --- Funciones de diseño de muestreo -----------------------------------------
