# =============================================================================
# LGBTQ+ labor-market gaps in Chile: main execution script
# =============================================================================
# CHANGE this path to the repository location on your computer.
# Use / (or double backslashes), including on Windows.
project_path <- "C:/Users/Ignac/OneDrive/Documentos/GitHub/lgbtq-labor-market-gaps-chile"
setwd(project_path)

# TRUE runs the block; FALSE skips it. JKn may take several hours.
run_modules <- c(
  preprocessing = TRUE,
  descriptive_statistics = TRUE,
  questionnaires = TRUE,
  labor = TRUE,
  occupation_sensitivity = TRUE,
  wages = TRUE,
  nettuno = TRUE,
  expr_robustness = TRUE,
  labor_jkn = FALSE,
  wages_jkn = FALSE
)

run_project <- function(root, modules) {
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  previous_working_directory <- getwd()
  on.exit(setwd(previous_working_directory), add = TRUE)
  setwd(root)
  
  options(survey.lonely.psu = "adjust")
  set.seed(14062001)
  
  execution_plan <- list()
  
  add_block <- function(name, files) {
    execution_plan[[name]] <<- files
  }
  
  if (modules[["preprocessing"]]) {
    add_block("Preprocessing", "preprocess.R")
  }
  
  if (modules[["descriptive_statistics"]]) {
    add_block("Descriptive statistics: sexual orientation", "EEDD_os.Rmd")
    add_block("Descriptive statistics: gender identity", "EEDD_ig.Rmd")
  }
  
  if (modules[["questionnaires"]]) {
    add_block("Questionnaire comparisons", "Cuestionarios.Rmd")
    add_block("Questionnaire models", "Q_Improve.Rmd")
  }
  
  # The sensitivity analysis requires the main labor-model objects
  # in the SAME environment.
  if (modules[["labor"]] || modules[["occupation_sensitivity"]]) {
    block_files <- "labor_mpl_probit.R"
    
    if (modules[["occupation_sensitivity"]]) {
      block_files <- c(
        block_files,
        "sensibilidad_umbral_ocupacion_feminizada.R"
      )
    }
    
    add_block("Main labor models and sensitivity analysis", block_files)
  }
  
  if (modules[["wages"]]) {
    add_block("Main wage models", "wages.R")
  }
  
  if (modules[["nettuno"]]) {
    add_block("Nettuno: 2017 and 2022-2024", "nettuno_replica.R")
  }
  
  if (modules[["expr_robustness"]]) {
    add_block("Labor models using expr", "labor_mpl_probit_expr.R")
    add_block("Wage models using expr", "wages_expr.R")
  }
  
  if (modules[["labor_jkn"]]) {
    add_block(
      "Transgender and non-binary labor models: JKn",
      "labor_nb_trans_jkn.R"
    )
  }
  
  if (modules[["wages_jkn"]]) {
    add_block(
      "Transgender and non-binary wage models: JKn",
      "wages_jkn.R"
    )
  }
  
  required_packages <- c(
    "dplyr", "tidyr", "readr", "haven", "survey", "rlang",
    "knitr", "openxlsx", "ggplot2", "texreg", "sandwich", "tidyselect"
  )
  
  missing_packages <- required_packages[
    !vapply(
      required_packages,
      requireNamespace,
      logical(1),
      quietly = TRUE
    )
  ]
  
  required_inputs <- character()
  
  if (modules[["preprocessing"]]) {
    required_inputs <- c(
      required_inputs,
      paste0("Data/casen_", c(2015, 2017, 2022, 2024), ".sav"),
      "Data/IPC.xlsx"
    )
  }
  
  uses_post_panel <- any(
    modules[c(
      "descriptive_statistics",
      "labor",
      "occupation_sensitivity",
      "wages",
      "expr_robustness",
      "labor_jkn",
      "wages_jkn"
    )]
  )
  
  if (uses_post_panel) {
    required_inputs <- c(
      required_inputs,
      "Data/casen_2022.sav",
      "Data/casen_2024.sav"
    )
  }
  
  if (modules[["nettuno"]]) {
    required_inputs <- c(
      required_inputs,
      paste0("Data/casen_", c(2017, 2022, 2024), ".sav")
    )
  }
  
  if (!modules[["preprocessing"]]) {
    if (uses_post_panel) {
      required_inputs <- c(
        required_inputs,
        "Intermediates/Panel_post.csv"
      )
    }
    
    if (modules[["questionnaires"]]) {
      required_inputs <- c(
        required_inputs,
        "Intermediates/Panel_total.csv"
      )
    }
  }
  
  required_scripts <- c(
    file.path("scripts", unlist(execution_plan, use.names = FALSE)),
    "scripts/functions.R",
    "scripts/ocupaciones_feminizadas_casen.R"
  )
  
  missing_files <- unique(c(required_inputs, required_scripts))
  missing_files <- missing_files[!file.exists(missing_files)]
  
  cat("\nEXECUTION PLAN\n")
  
  for (i in seq_along(execution_plan)) {
    cat(sprintf(
      "%02d. %s: %s\n",
      i,
      names(execution_plan)[i],
      paste(execution_plan[[i]], collapse = " -> ")
    ))
  }
  
  if (length(missing_packages)) {
    cat(
      "\nMissing packages: ",
      paste(missing_packages, collapse = ", "),
      "\n",
      sep = ""
    )
  }
  
  if (length(missing_files)) {
    cat(
      "\nMissing files:\n",
      paste(missing_files, collapse = "\n"),
      "\n"
    )
  }
  
  if (length(missing_packages) || length(missing_files)) {
    stop(
      "Install the missing packages and provide the required files ",
      "before running the analyses. See README.md.",
      call. = FALSE
    )
  }
  
  for (directory in c(
    "Intermediates", "Inter_Tables", "Tables", "Figures", "logs"
  )) {
    dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  }
  
  execution_log <- data.frame(
    block = character(),
    status = character(),
    seconds = numeric(),
    details = character()
  )
  
  log_path <- file.path(
    "logs",
    paste0("execution_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
  )
  
  capture.output(
    sessionInfo(),
    file = "logs/sessionInfo_start.txt"
  )
  
  run_block <- function(files) {
    block_environment <- new.env(parent = globalenv())
    
    for (file_name in files) {
      setwd(root)
      script_path <- file.path("scripts", file_name)
      
      message("Running: ", script_path)
      
      if (grepl("\\.Rmd$", file_name, ignore.case = TRUE)) {
        temporary_script <- tempfile(fileext = ".R")
        on.exit(unlink(temporary_script), add = TRUE)
        
        # Execute the R chunks without compiling the notebook into a PDF.
        knitr::purl(
          script_path,
          output = temporary_script,
          documentation = 0L,
          quiet = TRUE
        )
        
        source(
          temporary_script,
          local = block_environment,
          chdir = FALSE,
          encoding = "UTF-8"
        )
      } else {
        source(
          script_path,
          local = block_environment,
          chdir = FALSE,
          encoding = "UTF-8"
        )
      }
    }
    
    invisible(NULL)
  }
  
  for (block_name in names(execution_plan)) {
    start_time <- proc.time()[[3]]
    
    block_error <- tryCatch(
      {
        run_block(execution_plan[[block_name]])
        NULL
      },
      error = identity
    )
    
    setwd(root)
    
    execution_log <- rbind(
      execution_log,
      data.frame(
        block = block_name,
        status = if (is.null(block_error)) "completed" else "error",
        seconds = proc.time()[[3]] - start_time,
        details = if (is.null(block_error)) {
          ""
        } else {
          conditionMessage(block_error)
        }
      )
    )
    
    write.csv(execution_log, log_path, row.names = FALSE)
    
    if (!is.null(block_error)) {
      stop(
        "Error in ", block_name, ": ",
        conditionMessage(block_error),
        "\nFiles exported by completed blocks are preserved.",
        call. = FALSE
      )
    }
    
    invisible(gc())
  }
  
  capture.output(
    sessionInfo(),
    file = "logs/sessionInfo_end.txt"
  )
  
  message(
    "R execution completed. Run MATLAB separately using matlab/principal.m."
  )
  
  invisible(execution_log)
}

execution_log <- run_project(project_path, run_modules)