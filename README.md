# LGBTQ+ Labor-Market Gaps in Chile

Code for studying labor-market and earnings gaps by gender identity and sexual orientation in Chile. The repository includes CASEN analyses, a reconstruction of Nettuno (2024), and complementary estimates using the INE Web Survey on Sexual and Gender Diversity.

## Quick start

1. Download or clone the repository.
2. Place the original datasets in the locations listed below. They are not included in this code package.
3. Install the R packages listed in the next section.
4. Open `main.R` and change `project_path` to the location of the downloaded repository. The default path is:

   ```r
   project_path <- "C:/Users/Ignac/OneDrive/Documentos/GitHub/lgbtq-labor-market-gaps-chile"
   ```

5. Select the analyses to run using the `TRUE`/`FALSE` entries in `run_modules`.
6. From the repository root, execute:

   ```r
   source("main.R", encoding = "UTF-8")
   ```

   Alternatively, from a terminal with R available, run `Rscript main.R`.

Before estimation, the script displays the execution plan and checks the required files and packages. If any are missing, execution stops and lists the missing requirements. These checks do not validate the contents of the datasets.

All modules except `labor_jkn` and `wages_jkn` are enabled by default. Set those entries to `TRUE` to include the JKn analyses, which may take several hours and require substantial memory.

If `preprocessing` is disabled, the previously generated panel files must be available and compatible with the current scripts. Enabling `occupation_sensitivity` also runs the main labor-market script because the sensitivity analysis requires its objects.

## R requirements

R 4.5.x is recommended to approximate the original working environment. No package-version lockfile is included; each execution records `sessionInfo()`.

```r
install.packages(c(
  "dplyr", "tidyr", "readr", "haven", "survey", "rlang",
  "knitr", "openxlsx", "ggplot2", "texreg", "sandwich", "tidyselect"
))
```

`main.R` executes the R code chunks in `.Rmd` files using `knitr::purl()`. It generates the files exported by those chunks without compiling the notebooks into PDF documents. Rendering the notebooks separately additionally requires `rmarkdown`, Pandoc, and a LaTeX distribution, with the repository root used as the working directory.

## Input data

| Path | Required contents |
|---|---|
| `Data/casen_2015.sav` | CASEN 2015 dataset |
| `Data/casen_2017.sav` | CASEN 2017 dataset |
| `Data/casen_2022.sav` | Complete CASEN 2022 dataset |
| `Data/casen_2024.sav` | Complete CASEN 2024 dataset |
| `Data/IPC.xlsx` | Consumer Price Index file used in the original preprocessing |

Preserve the variables and labels in the original datasets. The occupational classifier requires the complete surveys, before restricting the sample to the OSIG module. `IPC.xlsx` must retain the structure expected by `preprocess.R`: columns imported as `X1`, `X2`, and `Índice.de.Precios.al.Consumidor.(IPC)`, with December observations for all four years. The program does not download or reconstruct this file.

Preprocessing generates `Intermediates/Panel_total.csv` and `Intermediates/Panel_post.csv`. CASEN datasets stored as CSV or DTA files in the MATLAB directory do not automatically replace the SAV files required by R.

## Organization and execution order

| Order | File within `scripts/` | Purpose |
|---|---|---|
| 1 | `preprocess.R` | Load CASEN and CPI data and construct the panels; loads `functions.R` |
| 2 | `EEDD_os.Rmd`, `EEDD_ig.Rmd` | Descriptive statistics by sexual orientation and gender identity |
| 3 | `Cuestionarios.Rmd`, `Q_Improve.Rmd` | Survey-instrument comparisons and self-identification models |
| 4 | `labor_mpl_probit.R` | Main linear probability models, Probit robustness estimates, and comparison of female-dominated occupation classifications |
| 5 | `sensibilidad_umbral_ocupacion_feminizada.R` | Thresholds of 40%, 50%, 60%, and 70%, using the same environment as the preceding block |
| 6 | `wages.R` | Earnings equations and selection correction |
| 7 | `nettuno_replica.R` | Reconstruction using 2017 data and application to 2022–2024 |
| 8 | `labor_mpl_probit_expr.R`, `wages_expr.R` | Robustness estimates using the general expansion factor `expr` |
| 9 | `labor_nb_trans_jkn.R` | Disaggregated labor-market results using JKn |
| 10 | `wages_jkn.R` | Disaggregated earnings results using JKn |

`ocupaciones_feminizadas_casen.R` is loaded automatically by the labor-market scripts in a separate environment. `comparar_sector_feminizado.R` is retained as a legacy auxiliary script and is not executed separately because the labor-market scripts already incorporate that comparison. Running it in an environment containing the current objects could replace the export functions with their earlier versions.

Each independent block uses its own environment to prevent object-name conflicts. Results remain available in the exported files; `main.R` does not retain all models in the global environment. It does not add checkpoints. Some original scripts retain their final RDS exports; no additional saving of replicate-weight matrices is introduced for JKn. Execution is recorded in a small CSV file in `logs/`.

## Outputs

| Directory | Results |
|---|---|
| `Tables/` and `Inter_Tables/` | Descriptive and survey-instrument tables |
| `Tables/labor/` | Linear probability models, Probit models, and occupational-classification comparisons |
| `Tables/labor/sensibilidad_umbral_feminizacion/` | Sensitivity figures and tables |
| `Tables/ocupaciones-feminizadas/` | Occupational classification and comparisons |
| `Tables/labor/robustez_expr/` | Labor-market results using `expr` |
| `tables/wages/` | Main earnings results |
| `tables/wages/robustez_expr/` | Earnings results using `expr` |
| `Tables/nettuno-2017/` | Nettuno reconstruction and later-wave application |
| `Tables/labor-jkn/` | Labor-market results for transgender and non-binary respondents |
| `Tables/wages-jkn/` | Earnings results for transgender and non-binary respondents |
| `Figures/` | Figures exported by scripts that use this directory |

The original output paths are preserved. On case-sensitive systems, `Tables` and `tables` are separate directories. The main labor-market tables omit employment in the population; the eight-column version is saved as a robustness table. Full-coefficient appendices may retain all eight equations.

## MATLAB: INE and ENSSEX

The MATLAB code runs separately. Open `matlab/principal.m` and click **Run**. Its working path is determined automatically from the file's location. `main.R` does not launch MATLAB.

The supplied MATLAB main script runs both ENSSEX analyses, including maps, and INE analyses. It requires the following files in `matlab/Datos/`:

- `bbdd_ewd_2025v1.csv`.
- `ENSSEX_2022_2023.csv`.
- `Regional.shp` and its accompanying shapefile components, particularly `.dbf`, `.shx`, and `.prj`.

Retain these datasets from your original directory; they are not included in the organized code package. The CASEN calls in `principal.m` are commented out. To run only the INE analysis, comment out the `enssex_encuesta` call and execute the script from a clean session.

The code uses functions from **Statistics and Machine Learning Toolbox**, such as `tcdf`; **Optimization Toolbox**, including `fminunc`; and, for maps, **Mapping Toolbox**, including `readgeotable`. Outputs are saved in `matlab/Tablas/`. The web survey is voluntary and non-probability-based; its estimates should not be interpreted as nationally representative.