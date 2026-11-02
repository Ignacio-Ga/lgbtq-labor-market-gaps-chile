# LGBTQ+ Labor-Market Gaps in Chile

Código para estudiar brechas laborales y de ingresos por identidad de género y orientación sexual en Chile. Incluye análisis de CASEN, reconstrucción de Nettuno (2024) y estimaciones complementarias con la Encuesta Web sobre Diversidad Sexual y de Género del INE.

## Inicio rápido

1. Descargue o clone el repositorio.
2. Coloque los datos originales en las rutas indicadas abajo; no están incluidos en este paquete de código.
3. Instale los paquetes de R indicados en la siguiente sección.
4. Abra `main.R` y cambie `ruta_proyecto` por la ubicación de la carpeta descargada. La ruta inicial es:

   ```r
   ruta_proyecto <- "C:/Users/Ignac/OneDrive/Documentos/GitHub/lgbtq-labor-market-gaps-chile"
   ```

5. Para comprobar los archivos y visualizar el orden sin estimar, establezca `solo_verificar <- TRUE` y ejecute `main.R`. Esta comprobación no valida el contenido de las bases.
6. Para ejecutar, establezca `solo_verificar <- FALSE`. Desde la raíz del repositorio:

   ```r
   source("main.R", encoding = "UTF-8")
   ```

   Alternativamente, desde una terminal con R disponible: `Rscript main.R`.

Todos los módulos están activados inicialmente. Los modelos JKn pueden tardar varias horas y requerir mucha memoria. Puede desactivar bloques mediante los valores `TRUE`/`FALSE` de `ejecutar`. Si desactiva la preparación, debe disponer de los paneles ya generados y compatibles con los scripts actuales. Activar la sensibilidad ocupacional ejecuta también el modelo laboral principal porque necesita sus objetos.

## Requisitos de R

Se recomienda R 4.5.x para aproximarse al entorno de trabajo original. No se incluye un archivo de bloqueo de versiones; cada ejecución registra `sessionInfo()`.

```r
install.packages(c(
  "dplyr", "tidyr", "readr", "haven", "survey", "rlang",
  "knitr", "openxlsx", "ggplot2", "texreg", "sandwich", "tidyselect"
))
```

`main.R` ejecuta los bloques de código de los `.Rmd` mediante `knitr::purl()`. Genera los archivos que exportan esos bloques, sin compilar los cuadernos PDF. Para renderizar los cuadernos por separado necesita además `rmarkdown`, Pandoc y una distribución LaTeX, manteniendo la raíz del proyecto como directorio de trabajo.

## Datos de entrada

| Ruta | Contenido requerido |
|---|---|
| `Data/casen_2015.sav` | Base CASEN 2015 |
| `Data/casen_2017.sav` | Base CASEN 2017 |
| `Data/casen_2022.sav` | Base CASEN 2022 completa |
| `Data/casen_2024.sav` | Base CASEN 2024 completa |
| `Data/IPC.xlsx` | Archivo IPC utilizado en el procesamiento original |

Conserve las variables y etiquetas de las bases originales. El clasificador ocupacional necesita las encuestas completas, antes de seleccionar el módulo OSIG. `IPC.xlsx` debe mantener la estructura esperada por `preprocess.R`: columnas importadas como `X1`, `X2` e `Índice.de.Precios.al.Consumidor.(IPC)`, con observaciones de diciembre para los cuatro años. El programa no descarga ni reconstruye ese archivo.

La preparación genera `Intermediates/Panel_total.csv` y `Intermediates/Panel_post.csv`. Las bases CASEN en CSV o DTA de la carpeta MATLAB no sustituyen automáticamente los SAV requeridos por R.

## Organización y orden de ejecución

| Orden | Archivo dentro de `scripts/` | Función |
|---|---|---|
| 1 | `preprocess.R` | Cargar CASEN e IPC y construir los paneles; carga `functions.R` |
| 2 | `EEDD_os.Rmd`, `EEDD_ig.Rmd` | Descriptivos por orientación e identidad de género |
| 3 | `Cuestionarios.Rmd`, `Q_Improve.Rmd` | Comparaciones de instrumentos y modelos de autoidentificación |
| 4 | `labor_mpl_probit.R` | MPL principal, Probit de robustez y comparación de ocupaciones feminizadas |
| 5 | `sensibilidad_umbral_ocupacion_feminizada.R` | Umbrales 40%, 50%, 60% y 70%, en el mismo entorno del bloque anterior |
| 6 | `wages.R` | Ecuaciones de ingresos y corrección de selección |
| 7 | `nettuno_replica.R` | Reconstrucción de 2017 y aplicación a 2022–2024 |
| 8 | `labor_mpl_probit_expr.R`, `wages_expr.R` | Robustez con el factor general `expr` |
| 9 | `labor_nb_trans_jkn.R` | Resultados laborales desagregados con JKn |
| 10 | `wages_jkn.R` | Resultados salariales desagregados con JKn |

`ocupaciones_feminizadas_casen.R` es cargado automáticamente por los modelos laborales en un entorno separado. `comparar_sector_feminizado.R` se conserva como auxiliar histórico: no se ejecuta adicionalmente, porque los scripts laborales ya incorporan esa comparación. Ejecutarlo sobre los objetos actuales podría reemplazar las funciones de exportación por su versión anterior.

Cada bloque independiente usa su propio entorno para evitar colisiones entre objetos. Los resultados permanecen en los archivos exportados; `main.R` no conserva todos los modelos en el entorno global. No agrega checkpoints. Algunos scripts originales conservan sus exportaciones RDS finales; los JKn no incorporan nuevos guardados de matrices de réplicas. El registro de ejecución es un CSV pequeño en `logs/`.

## Salidas

| Carpeta | Resultados |
|---|---|
| `Tables/` y `Inter_Tables/` | Tablas descriptivas y de cuestionarios |
| `Tables/labor/` | MPL, Probit y comparación ocupacional |
| `Tables/labor/sensibilidad_umbral_feminizacion/` | Gráficos y tablas de sensibilidad |
| `Tables/ocupaciones-feminizadas/` | Clasificación y comparación de ocupaciones |
| `Tables/labor/robustez_expr/` | Resultados laborales con `expr` |
| `tables/wages/` | Resultados salariales principales |
| `tables/wages/robustez_expr/` | Resultados salariales con `expr` |
| `Tables/nettuno-2017/` | Reconstrucción y extensión de Nettuno |
| `Tables/labor-jkn/` | Resultados laborales trans/no binarios |
| `Tables/wages-jkn/` | Resultados salariales trans/no binarios |
| `Figures/` | Gráficos exportados por los scripts que utilizan esta ruta |

Se conservan las rutas originales de resultados. En sistemas sensibles a mayúsculas, `Tables` y `tables` son carpetas diferentes. Las tablas laborales principales omiten empleo en la población; la versión de ocho columnas se guarda como robustez. Los apéndices de coeficientes completos pueden conservar las ocho ecuaciones.

## MATLAB: INE y ENSSEX

El código MATLAB se ejecuta por separado, abriendo `matlab/principal.m` y pulsando **Run**. Su ruta se obtiene automáticamente desde la ubicación de ese archivo. `main.R` no inicia MATLAB.

El principal suministrado ejecuta tanto ENSSEX (incluidos mapas) como INE. Requiere los siguientes archivos en `matlab/Datos/`:

- `bbdd_ewd_2025v1.csv`.
- `ENSSEX_2022_2023.csv`.
- `Regional.shp` y los archivos complementarios del mismo shapefile, en particular `.dbf`, `.shx` y `.prj`.

Conserve estos datos de su carpeta original; no están incluidos en el paquete organizado. Las llamadas CASEN de `principal.m` están comentadas. Para ejecutar únicamente INE, comente la llamada `enssex_encuesta` y ejecute desde una sesión limpia.

El código usa funciones de **Statistics and Machine Learning Toolbox** (por ejemplo `tcdf`), **Optimization Toolbox** (`fminunc`) y, para los mapas, **Mapping Toolbox** (`readgeotable`). Las salidas se guardan en `matlab/Tablas/`. La encuesta web es voluntaria y no probabilística: sus estimaciones no deben interpretarse como representativas de la población nacional.

## Ajustes de integración y alcance de la comprobación

- Se trasladaron los scripts R/Rmd a `scripts/` y se actualizaron las llamadas auxiliares.
- `labor_nb_trans_jkn.R` ya no depende del archivo externo `03_pertenencia_muestras.rds`: reconstruye los universos con las reglas del modelo laboral actual, antes de excluir la categoría residual de género y aplicar los controles completos. Esto puede cambiar el N respecto de ejecuciones antiguas basadas en esas máscaras; revise `muestras_desagregadas.csv` y `registro_estimacion.csv` al reestimar.
- Se incorporó al JKn laboral la exportación principal de siete columnas y la robustez de ocho, previamente acordadas.
- Se hizo portable la ruta del principal MATLAB y se excluyeron datos y archivos temporales del paquete de código. No se borran ni modifican sus datos originales.

Se revisaron las rutas y dependencias del código. No se ejecutaron los modelos ni se validaron resultados numéricos al organizar este paquete: el entorno de edición no disponía de R/MATLAB ni de las entradas SAV e IPC requeridas. Esta organización no constituye una nueva auditoría de las definiciones econométricas o de las variables originales.

## Uso con GitHub

Conserve `main.R`, `README.md`, `scripts/` y `matlab/` en la raíz del repositorio. Mantenga los datos localmente en las carpetas indicadas. `.gitignore` excluye datos, sesiones, modelos RDS y temporales; no elimina archivos que Git ya estuviera siguiendo. No se ha asignado una licencia: acuerde con los coautores las condiciones de reutilización antes de publicarla.
