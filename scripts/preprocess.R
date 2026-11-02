library(haven)
library(dplyr)
library(readr)
library(survey)
library(tidyr)
library(rlang)
library(knitr)
library(openxlsx)

source("scripts/functions.R", local = TRUE, encoding = "UTF-8")

edad_hijos <- 11
edad_corte <- 11

variables <- c(
  # 1. Ruta e Identificación
  "id_vivienda", "folio", "region", "area", 
  
  # 2. Muestreo (Diseño Complejo)
  "varstrat", "varunit", 
  
  # 3. Residentes (Demografía y Composición del Hogar)
  "tot_per_h", "men18c", "sexo", "edad", "ecivil",
  "lugar_nac", "pueblos_indigenas", "numper",
  paste0("cantidad_menores_", edad_hijos),
  paste0("men", edad_corte, "c"),
  "men18numper",
  
  # 4. Educación
  "analfabetismo", "esc", "desercion", "educc",
  
  # 5. Empleo y Fuerza de Trabajo
  "activ", "contrato", "o10", "o15",
  
  # 6. Ingresos (Individuales y del Hogar)
  # CEPAL         | MDSF 
  "y0101c",         "yoprcor",                 # <- Individuales
  "yautcorh",       "ypchautcor",              # <- Autónomos (Hogar y Per cápita)
  "ytotcorh",       "ymonecorh", "ypchtotcor", # <- Totales (Hogar y Per cápita)
  
  # 7. Salud
  "s28",
  
  # 8. Vivienda, Entorno y Pobreza
  "ind_hacina", "ind_san", "ind_estado", "pobreza", "pobreza_multi_4d"
)

# Bases



Casen_2015 <- read_sav("Data/casen_2015.sav")
Casen_2017 <- read_sav("Data/casen_2017.sav")
Casen_2022 <- read_sav("Data/casen_2022.sav")
Casen_2024 <- read_sav("Data/casen_2024.sav")
IPC <- read.xlsx("Data/IPC.xlsx")

# Inflacion 
Inflacion <- IPC %>% rename(year = X1,
                            IPC =`Índice.de.Precios.al.Consumidor.(IPC)`,
                            mes = X2) %>%
  filter(year %in% c(2015, 2017, 2022, 2024), mes == 12) %>%
  select(year, IPC) %>%
  mutate(IPC = as.numeric(IPC)) %>%
  pivot_wider(names_from = year, values_from = IPC)

# Aplicacion Funciones

Casen_2015 <- Casen_2015 %>%
  rename(pco1_b = pco1)

Casen_2017 <- Casen_2017 %>%
  rename(pco1_b = pco1)


Casen_2015 <- calcular_menores_hogar(Casen_2015, edad_hijos)
Casen_2017 <- calcular_menores_hogar(Casen_2017, edad_hijos)
Casen_2022 <- calcular_menores_hogar(Casen_2022, edad_hijos)
Casen_2024 <- calcular_menores_hogar(Casen_2024, edad_hijos)

# Limpiea y creacion de variables
Casen_2015 <- Casen_2015 %>% select(all_of(variables), expr_div, r21, r21_esp, r22, r22_esp, oficio4_88, o28) %>% 
  mutate(year = 2015,
         IPC = Inflacion$`2015`/Inflacion$`2022`) %>%
  filter(!is.na(expr_div)) %>%
  rename(os1 = r21,
         os1_esp = r21_esp,
         genero = r22,
         genero_esp = r22_esp,
         expr_osig = expr_div) %>%
  mutate(Hetero = ifelse(os1 == 1, 1, 0),
         LG = ifelse(os1 == 2, 1, 0),
         bisexual = ifelse(os1 == 3, 1, 0),
         Other = ifelse(os1 == 4, 1, 0),
         IDK = ifelse(os1 == 9, 1, 0),
         RNS = ifelse(os1 == 0, 1, 0),
         TRANS = ifelse(genero == 10, 1, 0),
         NB = ifelse(genero == 10, 1, 0),
         IDK_ig = ifelse(genero == 9, 1, 0),
         RNA_ig = ifelse(genero == 0, 1, 0),
         Other_ig = ifelse(genero == 3, 1, 0),
         GM = ifelse((genero != sexo | TRANS == 1 | NB == 1) & IDK_ig == 0 & RNA_ig == 0, 1, 0)) %>%
  rename(CIIU = oficio4_88, 
         Pension = o28,
         w_cepal = y0101c,
         w_mdsf = yoprcor,
         hrs = o10) %>%
  crear_variables_comunes()


Casen_2017 <- Casen_2017 %>% select(all_of(variables), expr_div, r23, r23_esp, r24, r24_esp, oficio4_88, o28) %>%
  mutate(year = 2017,
         IPC = Inflacion$`2017`/Inflacion$`2022`) %>%
  filter(!is.na(expr_div)) %>%
  rename(os1 = r23,
         os1_esp = r23_esp,
         genero = r24,
         genero_esp = r24_esp,
         expr_osig = expr_div) %>%
  mutate(Hetero = ifelse(os1 == 1, 1, 0),
         LG = ifelse(os1 == 2, 1, 0),
         bisexual = ifelse(os1 == 3, 1, 0),
         Other = ifelse(os1 == 4, 1, 0),
         IDK = ifelse(os1 > 4, 1, 0),
         RNS = ifelse(os1 == 0, 1, 0),
         TRANS = ifelse(genero == 3, 1, 0),
         NB = ifelse(genero == 10, 1, 0),
         IDK_ig = ifelse(genero == 9, 1, 0),
         RNA_ig = ifelse(genero == 4, 1, 0),
         Other_ig = ifelse(genero == 10, 1, 0),
         GM = ifelse((genero != sexo | TRANS == 1 | NB == 1) & IDK_ig == 0 & RNA_ig == 0, 1, 0)) %>%
  rename(CIIU = oficio4_88, 
         Pension = o28,
         w_cepal = y0101c,
         w_mdsf = yoprcor,
         hrs = o10) %>%
  crear_variables_comunes()

Casen_2022 <- Casen_2022 %>% select(all_of(variables), expr_osig, os1, os1_esp, genero, genero_esp, trans, oficio4_08, all_of(paste0("cantidad_menores_", edad_hijos)), o31, paste0("men", edad_corte, "c"),men18numper, pobreza_multi, disc_wg, o16) %>%
  mutate(year = 2022,
         IPC = Inflacion$`2022`/Inflacion$`2022`) %>%
  filter(!is.na(expr_osig)) %>%
  mutate(LG = ifelse(os1 == 2, 1, 0),
         Hetero = ifelse(os1 == 1, 1, 0),
         bisexual = ifelse(os1 == 3, 1, 0),
         Other = ifelse(os1 == 4, 1, 0),
         IDK = ifelse(os1 == -88, 1, 0),
         RNS = ifelse(os1 == -99, 1, 0),
         trans = ifelse(is.na(trans) == TRUE, 0, trans),
         TRANS = ifelse(genero == 3 | genero == 4 | genero == 5 | trans == 1, 1, 0),
         NB = ifelse(genero == 5, 1, 0),
         IDK_ig = ifelse(genero == -88, 1, 0),
         RNA_ig = ifelse(genero == -99, 1, 0),
         Other_ig = ifelse(genero == 6, 1, 0),
         GM = ifelse((genero != sexo | TRANS == 1 | NB == 1 | Other_ig == 1) & IDK_ig == 0 & RNA_ig == 0, 1, 0)) %>%
  rename(CIIU = oficio4_08, 
         Pension = o31,
         w_cepal = y0101c,
         w_mdsf = yoprcor,
         hrs = o10) %>%
  crear_variables_comunes()

Casen_2024 <- Casen_2024 %>% select(any_of(variables), expr_osig, os1, os1_esp, genero, genero_esp, trans, oficio4_08, all_of(paste0("cantidad_menores_", edad_hijos)), pobreza_multi_2015, o31, paste0("men", edad_corte, "c"), men18numper, pobreza_multi, disc_wg, o16) %>%
  mutate(year = 2024,
         IPC = Inflacion$`2024`/Inflacion$`2022`) %>%
  filter(!is.na(expr_osig)) %>%
  mutate(LG = ifelse(os1 == 2, 1, 0),
         Hetero = ifelse(os1 == 1, 1, 0),
         bisexual = ifelse(os1 == 3, 1, 0),
         Other = ifelse(os1 == 4, 1, 0),
         IDK = ifelse(os1 == -88, 1, 0),
         RNS = ifelse(os1 == -99, 1, 0),
         trans = ifelse(is.na(trans) == TRUE, 0, trans),
         TRANS = ifelse(genero == 3 | genero == 4 | genero == 5 | trans == 1, 1, 0),
         NB = ifelse(genero == 5, 1, 0),
         IDK_ig = ifelse(genero == -88, 1, 0),
         RNA_ig = ifelse(genero == -99, 1, 0),
         Other_ig = ifelse(genero == 6, 1, 0),
         GM = ifelse((genero != sexo | TRANS == 1 | NB == 1 | Other_ig == 1) & IDK_ig == 0 & RNA_ig == 0, 1, 0)) %>%
  rename(pobreza_multi_4d = pobreza_multi_2015,
         CIIU = oficio4_08, 
         Pension = o31,
         w_cepal = y0101c,
         w_mdsf = yoprcor,
         hrs = o10) %>%
  crear_variables_comunes()

## Creacion del panel
# Sacar las etiquetas
Casen_2015 <- zap_labels(Casen_2015)
Casen_2017 <- zap_labels(Casen_2017)
Casen_2022 <- zap_labels(Casen_2022)
Casen_2024 <- zap_labels(Casen_2024)

#Panel 
Panel_total <- bind_rows(Casen_2015, Casen_2017, Casen_2022, Casen_2024)

Panel_total <- Panel_total %>% 
  mutate(varstrat_panel = paste0(year, "_", varstrat),
         varunit_panel = paste0(year, "_", varunit))

Panel_post <- bind_rows(Casen_2022, Casen_2024) 

Panel_post <- Panel_post %>%
  mutate(varstrat_panel = paste0(year, "_", varstrat),
         varunit_panel = paste0(year, "_", varunit))

dir.create("Intermediates", showWarnings = FALSE)

# Exportar las bases
write_csv(Panel_total, "Intermediates/Panel_total.csv")
write_csv(Panel_post, "Intermediates/Panel_post.csv")