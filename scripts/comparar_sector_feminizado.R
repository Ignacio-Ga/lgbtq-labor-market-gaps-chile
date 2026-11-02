# =============================================================================
# ADICION A labor_probit.R Y wages.R: OCUPACION FEMINIZADA ANTIGUA / NUEVA
# Ejecutar desde la raiz del proyecto. No ejecuta ni modifica los scripts base.
# No guarda modelos, checkpoints, matrices de replicas ni RDS.
# Requiere ocupaciones_feminizadas_casen.R en la raiz del proyecto.
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
  # En labor se conserva literalmente la variable que genero la tabla original.
  # En wages, si no existia, se aplica la lista historica proporcionada.
  x$SF_antigua <- if ("Feminized_sector" %in% names(x)) {
    x$Feminized_sector
  } else {
    ifelse(x$activ %in% 1,
           as.integer(x[[var_ocupacion]] %in% lista_antigua), NA_integer_)
  }
  x$SF_nueva <- x$Female_dominated
  # Solo columnas adicionales: se conservan filas, pesos, estratos y PSU.
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

sf_tabla <- function(ef, columnas, cabeceras, titulo, etiqueta, nota, ruta,
                     ajustes=NULL) {
  fin <- " \\\\"; nc <- length(columnas)+1L
  out <- c("\\begin{table}[htbp]", "\\centering",
    paste0("\\caption{",titulo,"}"), paste0("\\label{",etiqueta,"}"),
    "\\resizebox{\\textwidth}{!}{%",
    paste0("\\begin{tabular}{l",paste(rep("r",nc-1L),collapse=""),"}"),
    "\\toprule", paste0("Contrast & ",paste(cabeceras,collapse=" & "),fin),"\\midrule")
  for (pa in c("A","B")) {
    titulo_panel <- if (pa=="A") "Assigned Sex and Gender Minority Status" else
      "Sexual Orientation Within Assigned Sex"
    out <- c(out,paste0("\\multicolumn{",nc,"}{l}{\\textit{Panel ",pa,": ",titulo_panel,"}}",fin))
    for (od in sort(unique(ef$orden[ef$panel==pa]))) {
      r <- ef[ef$orden==od,]; r <- r[match(columnas,r$resultado),]
      out <- c(out,paste0(sf_escenarios[[od]][[2]]," & ",
        paste(mapply(sf_num,r$estimacion,r$p_value),collapse=" & "),fin),
        paste0(" & ",paste(ifelse(is.na(r$se),"--",sprintf("$(%.3f)$",r$se)),collapse=" & "),fin))
    }
    out <- c(out,"\\midrule")
  }
  if (!is.null(ajustes)) {
    z <- ajustes[match(columnas,ajustes$resultado),]
    out <- c(out,paste0("Inverse Mills ratio & ",
      paste(mapply(sf_num,z$lambda,z$p_lambda),collapse=" & "),fin),
      paste0(" & ",paste(ifelse(is.na(z$se_lambda),"--",sprintf("$(%.3f)$",z$se_lambda)),collapse=" & "),fin),
      paste0("R$^2$ & ",paste(sprintf("%.3f",z$r2),collapse=" & "),fin))
  }
  nn <- vapply(columnas,function(y) ef$n[match(y,ef$resultado)],numeric(1))
  out <- c(out,paste0("N & ",paste(format(nn,big.mark=",",trim=TRUE,scientific=FALSE),collapse=" & "),fin),
    "\\bottomrule","\\end{tabular}%","}","\\par\\smallskip",
    paste0("\\footnotesize\\textit{Note:} ",nota,
      " $^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$; $^{\\dagger}p<0.10$."),"\\end{table}")
  writeLines(out,ruta,useBytes=TRUE)
}

sf_apendice <- function(cf, ruta) {
  fin <- " \\\\"
  out <- c("% Requires booktabs and longtable.","\\begingroup\\scriptsize",
    "\\begin{longtable}{p{0.46\\textwidth}rrrr}",
    "\\toprule Term & Estimate & SE & Statistic & p-value \\\\ \\midrule",
    "\\endhead", "\\bottomrule\\endfoot")
  for (y in unique(cf$resultado)) {
    r <- cf[cf$resultado==y,]
    out <- c(out,paste0("\\multicolumn{5}{l}{",sf_tex(y),"; N=",r$n[1],"}",fin))
    for (i in seq_len(nrow(r))) out <- c(out,paste0(sf_tex(r$termino[i])," & ",
      paste(sprintf("%.4f",as.numeric(unlist(r[i,c("estimacion","se","estadistico","p_value")],use.names=FALSE))),collapse=" & "),fin))
    out <- c(out,"\\midrule")
  }
  writeLines(c(out,"\\end{longtable}","\\endgroup"),ruta,useBytes=TRUE)
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

# LABOR: ejecutar inmediatamente DESPUES de labor_probit.R.
# Solo reestima dos probit: medida antigua y medida nueva en muestra comun.
# Reutiliza los resultados ya calculados para las otras siete columnas.
comparar_sector_labor <- function(entorno=parent.frame(),
    salida="Tables/probit-labor/comparacion-sector-feminizado",
    var_ocupacion="CIIU", clasificador="scripts/ocupaciones_feminizadas_casen.R") {
  modulo <- sf_clasificador(clasificador)
  ds <- sf_agregar(get("diseno",entorno),modulo,var_ocupacion)
  base <- get("resultado_labor_probit",entorno)
  rhs_local <- get("rhs",entorno)
  mask <- get("mascaras",entorno)[["Feminized_sector"]] %in% TRUE
  f0 <- as.formula(paste("SF_antigua ~",rhs_local))
  f1 <- as.formula(paste("SF_nueva ~",rhs_local))
  original <- mask & sf_completos(f0,ds$variables)
  comun <- original & sf_completos(f1,ds$variables)
  dir.create(salida,recursive=TRUE,showWarnings=FALSE)
  cols <- c("Employed_population","Employed","Labor_force","Self_employed",
            "Public_sector","Feminized_sector","Monthly_hours","Informal_worker")
  heads <- c("Employment in population","Employment among participants","Labor-force participation",
             "Self-employed","Public sector","Female-dominated occupation","Log monthly hours","Informal worker")
  heads <- paste0("\\shortstack{",heads,"\\\\ (",seq_along(heads),")}")
  comparaciones <- completos <- list()
  for (v in c("antigua","nueva")) {
    t0 <- proc.time()[[3]]
    cat("\nLabor: medida",v,"| N comun =",sum(comun),"\n"); flush.console()
    f <- if(v=="antigua") f0 else f1
    m <- survey::svyglm(f,design=ds[comun,],family=quasibinomial(link="probit"),na.action=na.fail)
    ee <- sf_contrastes(m,"Feminized_sector",100)
    ef <- dplyr::bind_rows(base$contrastes[base$contrastes$resultado!="Feminized_sector",],ee)
    cf <- dplyr::bind_rows(base$coeficientes[base$coeficientes$resultado!="Feminized_sector",],
                          sf_coeficientes(m,"Feminized_sector"))
    nota <- paste("Columns (1), (2), and (3) report employment in the population, employment among participants, and participation, respectively. Columns (4)--(8) are conditional on employment. Binary outcomes are in percentage points; hours are in log points. The joint specification, controls and survey design are inherited from the main labor script. Only column (6) is reestimated: both definitions use identical respondents in that column; all other columns reproduce the main estimates.",
      sf_nota_medida(modulo,v),
      "Contrasts average over observed covariates. Taylor covariance and delta-method standard errors are used.")
    sf_tabla(ef,cols,heads,paste("Labor Outcomes: Female-Dominated Occupation -",v),
      paste0("tab:labor_fem_",v),nota,file.path(salida,paste0("tabla_labor_sector_",v,".tex")))
    sf_apendice(cf,file.path(salida,paste0("apendice_labor_sector_",v,".tex")))
    write.csv(ef,file.path(salida,paste0("contrastes_",v,".csv")),row.names=FALSE)
    write.csv(cf,file.path(salida,paste0("coeficientes_",v,".csv")),row.names=FALSE)
    comparaciones[[v]] <- ef; completos[[v]] <- cf
    cat(sprintf("Terminado en %.1f segundos\n",proc.time()[[3]]-t0)); flush.console()
  }
  nd <- data.frame(resultado="Feminized_sector",N_original=sum(original),
    N_comparacion_antigua=sum(comun),N_comparacion_nueva=sum(comun),
    excluidos_sin_nueva_clasificacion=sum(original & !comun))
  write.csv(nd,file.path(salida,"comparacion_N.csv"),row.names=FALSE)
  x <- ds$variables[comun,]; w <- as.numeric(weights(ds[comun,],type="sampling"))
  cruce <- aggregate(cbind(N=rep(1,nrow(x)),peso=w),
    by=list(antigua=x$SF_antigua,nueva=x$SF_nueva),FUN=sum)
  write.csv(cruce,file.path(salida,"cruce_clasificaciones.csv"),row.names=FALSE)
  writeLines(paste("RHS heredado:",rhs_local),file.path(salida,"formula.txt"))
  print(nd,row.names=FALSE)
  cat("\nTablas de comparacion:",normalizePath(salida),"\n")
  invisible(list(contrastes=comparaciones,coeficientes=completos,N=nd))
}

