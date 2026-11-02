# =============================================================================
# NETTUNO (2024): RECONSTRUCCION 2017 Y APLICACION AGRUPADA 2022-2024
# =============================================================================
library(haven)
library(dplyr)
library(sandwich)

carpeta <- 'Tables/nettuno-2017'
dir.create(carpeta, recursive=TRUE, showWarnings=FALSE)
edad_min <- 18L
edad_max <- 65L
peso_2017 <- 'expr'
peso_2224 <- 'expr_osig'
hc_tipo <- 'HC1'
tecnico_completo_como_terciario <- TRUE

# Mes de entrevista: fecha_mes (2017); fecha_entrev (2022 y 2024).
# haven suele importar fechas SPSS como POSIXct. Se contemplan tambien Date,
# texto dd/mm/aa o yyyy-mm-dd, y segundos SPSS desde 1582-10-14.
extraer_mes <- function(x) {
  if (inherits(x,'POSIXt')) return(as.integer(format(x,'%m',tz='UTC')))
  if (inherits(x,'Date')) return(as.integer(format(x,'%m')))
  if (is.numeric(x)) {
    fecha <- as.POSIXct(as.numeric(x),origin='1582-10-14',tz='UTC')
    return(as.integer(format(fecha,'%m',tz='UTC')))
  }
  s <- trimws(as.character(x))
  fecha <- as.Date(rep(NA_character_,length(s)))
  for (formato in c('%Y-%m-%d','%d/%m/%Y','%d/%m/%y',
                    '%d-%m-%Y','%d-%m-%y','%Y/%m/%d')) {
    nuevo <- as.Date(s,format=formato)
    usar <- is.na(fecha) & !is.na(nuevo)
    fecha[usar] <- nuevo[usar]
  }
  as.integer(format(fecha,'%m'))
}
binaria <- function(x) ifelse(x %in% 0:1,as.numeric(x),NA_real_)
positivo <- function(x) ifelse(is.finite(x) & x>0,as.numeric(x),NA_real_)

preparar <- function(anio) {
  cat('\nPreparando CASEN ',anio,'...\n',sep=''); flush.console()
  z <- read_sav(paste0('Data/casen_',anio,'.sav'))
  names(z) <- tolower(names(z))
  mes <- if (anio==2017) as.numeric(z$fecha_mes) else extraer_mes(z$fecha_entrev)
  if ('pobreza_multi_2015' %in% names(z)) {
    pob <- z$pobreza_multi_2015
  } else if ('pobreza_multi_5d_2015' %in% names(z)) {
    pob <- z$pobreza_multi_5d_2015
  } else if (anio==2022) {
    # Versión original 2022: pobreza_multi era el indicador 5d metodologia 2015.
    pob <- z$pobreza_multi
  } else {
    pob <- z$pobreza_multi_5d
  }
  d <- as.data.frame(zap_labels(z))
  d$year <- anio
  d$mes <- factor(ifelse(mes %in% 1:12,mes,NA))
  d$poverty <- binaria(pob)
  d$peso <- as.numeric(d[[if(anio==2017) peso_2017 else peso_2224]])
  # folio identifica el hogar dentro de cada ola. Contar antes de filtrar SOGI.
  d <- d %>% group_by(folio) %>% mutate(
    adultos_hogar=if(anyNA(edad)) NA_integer_ else sum(edad>=18),
    menores_hogar=if(anyNA(edad)) NA_integer_ else sum(edad<18)
  ) %>% ungroup()

  if (anio==2017) {
    d <- d %>% mutate(
      grupo=case_when(
        sexo==1 & r24==1 ~ 'AMAB_cis', sexo==2 & r24==2 ~ 'AFAB_cis',
        sexo==1 & r24 %in% c(2,3) ~ 'AMAB_noncis',
        sexo==2 & r24 %in% c(1,3) ~ 'AFAB_noncis',
        sexo %in% 1:2 & r24==9 ~ 'GI_dont_know',
        sexo %in% 1:2 & r24==4 ~ 'GI_prefer_not_answer',
        TRUE ~ NA_character_),
      orientacion=factor(ifelse(r23 %in% c(1:4,9),r23,NA),levels=c(1:4,9)),
      indigena=case_when(r3 %in% 1:9 ~ 1,r3==10 ~ 0,TRUE ~ NA_real_),
      inmigrante=case_when(r1b==3 ~ 1,r1b %in% 1:2 ~ 0,TRUE ~ NA_real_),
      educacion=case_when(
        educc==0 ~ 'None',educc %in% 1:3 ~ 'Less_HS',educc==4 ~ 'HS',
        educc %in% c(5,6) & e6a %in% c(16,17) ~ 'Graduate',
        educc==5 & e6a %in% 12:15 ~ 'Some_college',
        educc==6 & e6a==13 ~ ifelse(tecnico_completo_como_terciario,'Bachelors','Some_college'),
        educc==6 & e6a %in% c(12,14,15) ~ 'Bachelors',TRUE ~ NA_character_),
      informal=case_when(!(activ %in% 1) ~ NA_real_,
        o29 %in% 1:6 ~ 0,o29==7 ~ 1,o28==2 ~ 1,TRUE ~ NA_real_)
    )
  } else {
    d <- d %>% mutate(
      grupo=case_when(
        sexo %in% 1:2 & genero==-88 ~ 'GI_dont_know',
        sexo %in% 1:2 & genero==-99 ~ 'GI_prefer_not_answer',
        sexo==1 & genero %in% 1:6 & (genero!=sexo | trans %in% 1) ~ 'AMAB_noncis',
        sexo==2 & genero %in% 1:6 & (genero!=sexo | trans %in% 1) ~ 'AFAB_noncis',
        sexo==1 & genero==1 ~ 'AMAB_cis',sexo==2 & genero==2 ~ 'AFAB_cis',
        TRUE ~ NA_character_),
      orientacion=factor(ifelse(os1 %in% c(1:4,-88,-99),os1,NA),levels=c(1:4,-88,-99)),
      indigena=binaria(pueblos_indigenas),inmigrante=binaria(lugar_nac),
      educacion=case_when(
        educc==0 ~ 'None',educc %in% 1:3 ~ 'Less_HS',educc==4 ~ 'HS',
        educc %in% c(5,6) & e6a %in% c(14,15) ~ 'Graduate',
        educc==5 & e6a %in% c(12,13) ~ 'Some_college',
        educc==6 & e6a==12 ~ ifelse(tecnico_completo_como_terciario,'Bachelors','Some_college'),
        educc==6 & e6a==13 ~ 'Bachelors',TRUE ~ NA_character_),
      informal=case_when(!(activ %in% 1) ~ NA_real_,
        o32 %in% 1:5 ~ 0,o32==6 ~ 1,o31==2 ~ 1,TRUE ~ NA_real_)
    )
  }
  d <- d %>% mutate(
    grupo=factor(grupo,levels=c('AMAB_cis','AFAB_cis','AMAB_noncis','AFAB_noncis',
                                'GI_dont_know','GI_prefer_not_answer')),
    educacion=factor(educacion,levels=c('HS','None','Less_HS','Some_college','Bachelors','Graduate')),
    civil=case_when(ecivil==1 ~ 'Married',ecivil %in% c(2,3) ~ 'Partnered',
      ecivil %in% 4:6 ~ 'Divorced',ecivil==7 ~ 'Widowed',ecivil==8 ~ 'Single',TRUE ~ NA_character_),
    civil=factor(civil,levels=c('Single','Married','Partnered','Widowed','Divorced')),
    edad2=edad^2,region_fe=factor(region),
    urbano=case_when(area==1 ~ 1,area==2 ~ 0,TRUE ~ NA_real_),
    employed=case_when(activ==1 ~ 1,activ %in% c(2,3) ~ 0,TRUE ~ NA_real_),
    horas_semana=ifelse(activ %in% 1,positivo(o10),NA_real_),
    ingreso_laboral=ifelse(activ %in% 1,positivo(ytrabajocor),NA_real_),
    log_hours=log(horas_semana),log_income=log(ingreso_laboral)
  ) %>% filter(edad>=edad_min,edad<=edad_max,!is.na(grupo),is.finite(peso),peso>0) %>%
    select(
      year, edad, edad2, peso, grupo, orientacion, educacion, civil,
      indigena, inmigrante, adultos_hogar, menores_hogar,
      urbano, region_fe, mes, employed, horas_semana,
      ingreso_laboral, log_hours, log_income, informal, poverty,
      ytrabajocor, yoprcor, y0101c
    )
  cat('  Universo antes de controles completos: ',nrow(d),'\n',sep='')
  d
}

# Dos ejercicios: reconstruccion 2017 y aplicacion conjunta a 2022-2024.
datos_2017 <- preparar(2017L)
datos_2224 <- bind_rows(preparar(2022L),preparar(2024L))
ys <- c('employed','log_hours','log_income','informal','poverty')
titulos <- c('Employed','Log(Weekly Hrs)','Log(Labor Inc)','Informal Work','Multidim. Poverty')
medias_vars <- c('employed','horas_semana','ingreso_laboral','informal','poverty')
rhs <- paste('grupo + edad + edad2 + indigena + inmigrante + orientacion +',
             'educacion + civil + adultos_hogar + menores_hogar + urbano + region_fe + mes')
contrastes <- rbind(c(1,0,0),c(0,1,0),c(0,0,1),c(-1,1,0),c(-1,0,1),c(0,1,-1))
etiquetas <- c('AFAB, cisgender','AMAB, non-cisgender','AFAB, non-cisgender',
  'AMAB non-cisgender - AFAB cisgender','AFAB non-cisgender - AFAB cisgender',
  'AMAB non-cisgender - AFAB non-cisgender')
terminos <- c('grupoAFAB_cis','grupoAMAB_noncis','grupoAFAB_noncis')

estimar <- function(datos,periodo,agrupado=FALSE) {
  resultados <- completos <- resumen <- covarianzas <- list()
  for(j in seq_along(ys)) {
    inicio <- Sys.time()
    cat(sprintf('\n[%s: %d/5] %s...\n',periodo,j,titulos[j]));flush.console()
    f <- as.formula(paste(ys[j],'~',rhs,if(agrupado) '+ factor(year)' else ''))
    vars <- all.vars(f)
    ok <- complete.cases(datos[,vars,drop=FALSE])
    nums <- vars[vapply(datos[,vars,drop=FALSE],is.numeric,logical(1))]
    for(v in nums) ok <- ok & is.finite(datos[[v]])
    d <- droplevels(datos[ok,,drop=FALSE])
    m <- lm(f,data=d,weights=peso)
    V <- sandwich::vcovHC(m,type=hc_tipo)
    b <- coef(m)
    L <- matrix(0,6,length(b),dimnames=list(etiquetas,names(b)))
    L[,terminos] <- contrastes
    beta <- drop(L %*% b)
    se <- sqrt(diag(L %*% V %*% t(L)))
    resultados[[ys[j]]] <- data.frame(contraste=etiquetas,beta=beta,se=se,
                                     p=2*pt(-abs(beta/se),df.residual(m)))
    completos[[j]] <- data.frame(periodo=periodo,resultado=ys[j],termino=names(b),
      beta=unname(b),se=unname(sqrt(diag(V))),row.names=NULL)
    completos[[j]]$p <- 2*pt(-abs(completos[[j]]$beta/completos[[j]]$se),df.residual(m))
    resumen[[j]] <- data.frame(resultado=ys[j],N=nobs(m),
      media_ponderada=weighted.mean(d[[medias_vars[j]]],d$peso),
      media_simple=mean(d[[medias_vars[j]]]))
    covarianzas[[ys[j]]] <- V
    cat(sprintf('  N=%s; %.1f segundos\n',format(nobs(m),big.mark=','),
      as.numeric(difftime(Sys.time(),inicio,units='secs'))));flush.console()
  }
  list(contrastes=resultados,completos=bind_rows(completos),
       resumen=bind_rows(resumen),vcov=covarianzas)
}
replica_2017 <- estimar(datos_2017,'2017')
aplicacion_2224 <- estimar(datos_2224,'2022-2024',TRUE)

# Valores publicados: se preservan incluso las estrellas transcritas literalmente.
pub_b <- rbind(c(-.276,-.192,-.548,.091,.019),
               c(-.045,.031,-.039,.011,.042),
               c(-.218,-.166,-.454,.127,.020),
               c(.231,.223,.509,-.080,.023),
               c(.058,.026,.095,.036,.001),
               c(.173,.197,.415,-.116,.021))
pub_se <- rbind(c(.005,.008,.013,.006,.004),c(.020,.022,.048,.020,.018),
                c(.020,.031,.048,.027,.016),c(.020,.023,.048,.020,.018),
                c(.019,.032,.048,.027,.016),c(.027,.038,.067,.033,.024))
pub_st <- rbind(c('***','***','***','***','***'),c('**','','','','**'),
                c('***','***','***','***',''),c('***','***','***','***',''),
                c('***','','**','',''),c('***','','***','***',''))
pub_n <- c(70372,44303,46472,44359,67499)
pub_mean <- c(.647,42.08,553204,.278,.184)
stars <- function(p) ifelse(p<.01,'***',ifelse(p<.05,'**',ifelse(p<.10,'*','')))
cell <- function(x, s='') paste0('$',sprintf('%.3f',x),
                                ifelse(nzchar(s),paste0('^{',s,'}'),''),'$')
rowtex <- function(label, cells) paste0(label,' & ',paste(cells,collapse=' & '),' \\\\')
interleave <- function(a,b) as.vector(rbind(a,b))


# Comparacion de N; la diferencia 2022-2024 NO es un error de replicacion:
# representa otra poblacion temporal y dos olas agrupadas.
comparacion_N <- data.frame(
  resultado=titulos,N_Nettuno=pub_n,
  N_replica_2017=replica_2017$resumen$N,
  diferencia_2017=replica_2017$resumen$N-pub_n,
  N_aplicacion_2022_2024=aplicacion_2224$resumen$N,
  diferencia_2022_2024=aplicacion_2224$resumen$N-pub_n
)
print(comparacion_N,row.names=FALSE)
write.csv(comparacion_N,file.path(carpeta,'comparacion_N_limpia.csv'),row.names=FALSE)
write.csv(bind_rows(replica_2017$completos,aplicacion_2224$completos),
          file.path(carpeta,'regresiones_completas_limpias.csv'),row.names=FALSE)
comparacion_coeficientes <- bind_rows(lapply(seq_along(ys),function(j) {
  a <- replica_2017$contrastes[[j]]; b <- aplicacion_2224$contrastes[[j]]
  data.frame(resultado=titulos[j],contraste=etiquetas,
    beta_Nettuno=pub_b[,j],se_Nettuno=pub_se[,j],
    beta_2017=a$beta,se_2017=a$se,p_2017=a$p,
    beta_2224=b$beta,se_2224=b$se,p_2224=b$p)
}))
write.csv(comparacion_coeficientes,file.path(carpeta,'comparacion_coeficientes_limpia.csv'),row.names=FALSE)
# Solo coeficientes, covarianzas y resumenes; no guardar microdatos ni disenos gigantes.
saveRDS(list(replica_2017=replica_2017,aplicacion_2224=aplicacion_2224,
  N=comparacion_N,configuracion=list(peso_2017=peso_2017,peso_2224=peso_2224,
  edad_min=edad_min,edad_max=edad_max,hc=hc_tipo)),
  file.path(carpeta,'resultados_nettuno_limpios.rds'))

fmt_n <- function(x) format(x,big.mark=',',scientific=FALSE,trim=TRUE)
fmt_mean <- function(x) c(sprintf('%.3f',x[1]),sprintf('%.2f',x[2]),
                         format(round(x[3]),big.mark=',',trim=TRUE),sprintf('%.3f',x[4:5]))
nota <- paste0(
  'Published estimates refer to CASEN 2017 and are transcribed from Nettuno (2024), Table 3. ',
  'Our estimates use weighted OLS with HC1 heteroskedasticity-robust standard errors ',
  'and residual degrees of freedom. The configured age range is ',edad_min,'--',edad_max,'. ',
  'The 2017 reconstruction uses ',peso_2017,' weights; the pooled 2022--2024 application uses ',
  gsub('_','\\\\_',peso_2224), ' weights, without wave-specific normalization. ',
  'Controls: age and its square, Indigenous and immigrant status, sexual orientation, ',
  'education categories, marital-status categories, household adults and children, urban residence, ',
  'region and interview month. The pooled model additionally includes survey-year fixed effects. ',
  'Gender and sexual-orientation unknown/refusal categories are retained where available. ',
  '$^{***}p<0.01$; $^{**}p<0.05$; $^{*}p<0.10$.')

crear_tabla <- function(nombre,incluir=c('2017','2022-2024')) {
  series <- list()
  if('2017' %in% incluir) series[['Our 2017 reconstruction']] <- replica_2017
  if('2022-2024' %in% incluir) series[['Our 2022--2024 application']] <- aplicacion_2224
  k <- 1L+length(series)
  intercalar <- function(publicado,otras) as.vector(do.call(rbind,c(list(publicado),otras)))
  ncols <- 1L+5L*k
  tex <- c('\\begin{table}[htbp]','\\centering',
    '\\caption{Nettuno (2024): published estimates, reconstruction and later-wave application}',
    paste0('\\label{tab:',nombre,'}'),'\\resizebox{\\textwidth}{!}{%',
    paste0('\\begin{tabular}{l*{',5*k,'}{r}}'),'\\toprule',
    paste0(' & ',paste(paste0('\\multicolumn{',k,'}{c}{',titulos,'}'),collapse=' & '),' \\\\'),
    rowtex('',rep(c('Nettuno (2024)',names(series)),5)),'\\midrule',
    paste0('\\multicolumn{',ncols,'}{l}{\\textit{Panel A: reference is AMAB cisgender}} \\\\'))
  for(i in 1:6) {
    if(i==4) tex <- c(tex,'\\midrule',paste0('\\multicolumn{',ncols,
                      '}{l}{\\textit{Panel B: differences between groups}} \\\\'))
    bs <- lapply(series,function(s) vapply(s$contrastes,function(r) cell(r$beta[i],stars(r$p[i])),character(1)))
    ss <- lapply(series,function(s) vapply(s$contrastes,function(r) sprintf('$(%.3f)$',r$se[i]),character(1)))
    tex <- c(tex,rowtex(etiquetas[i],intercalar(cell(pub_b[i,],pub_st[i,]),bs)),
      rowtex('',intercalar(sprintf('$(%.3f)$',pub_se[i,]),ss)))
  }
  tex <- c(tex,'\\midrule',rowtex('N',intercalar(fmt_n(pub_n),lapply(series,function(s) fmt_n(s$resumen$N)))),
    rowtex('Mean of outcome',intercalar(fmt_mean(pub_mean),lapply(series,function(s) fmt_mean(s$resumen$media_ponderada)))),
    '\\bottomrule','\\end{tabular}%','}','\\par\\smallskip',
    paste0('\\footnotesize\\textit{Note:} ',nota),'\\end{table}')
  writeLines(tex,file.path(carpeta,paste0(nombre,'.tex')),useBytes=TRUE)
}
crear_tabla('tabla3_tres_estimaciones')
crear_tabla('tabla3_2017','2017')
crear_tabla('tabla3_2022_2024','2022-2024')
writeLines(c('\\documentclass{article}','\\usepackage[margin=1.5cm]{geometry}',
  '\\usepackage{booktabs,graphicx,pdflscape}','\\begin{document}','\\begin{landscape}',
  '\\input{tabla3_tres_estimaciones.tex}','\\clearpage',
  '\\input{tabla3_2017.tex}','\\clearpage','\\input{tabla3_2022_2024.tex}',
  '\\end{landscape}','\\end{document}'),file.path(carpeta,'comparacion_compilar.tex'))
cat('\nFinalizado. Resultados en: ',normalizePath(carpeta,winslash='/'),'\n',sep='')

# =============================================================================
# SENSIBILIDAD: VARIABLE DE INGRESO, CASEN 2017
# Mantiene controles, ponderador y rango de edad de nuestra reconstrucción.
# Ejercicio adicional para ver como reeplicar mejor a Nettuno
# =============================================================================

datos_ingresos_2017 <- preparar(2017L)

variables_ingreso <- c("ytrabajocor", "yoprcor", "y0101c")

controles_ingreso <- all.vars(
  as.formula(paste("~", rhs))
)

controles_ok <- complete.cases(
  datos_ingresos_2017[, controles_ingreso, drop = FALSE]
)

for (v in controles_ingreso) {
  if (is.numeric(datos_ingresos_2017[[v]])) {
    controles_ok <- controles_ok &
      is.finite(datos_ingresos_2017[[v]])
  }
}

ocupados <- datos_ingresos_2017$employed %in% 1

ingresos_validos <- lapply(
  variables_ingreso,
  function(v) {
    x <- datos_ingresos_2017[[v]]
    is.finite(x) & x > 0
  }
)
names(ingresos_validos) <- variables_ingreso


muestra_comun <- controles_ok &
  ocupados &
  Reduce(`&`, ingresos_validos)

terminos_ingreso <- c(
  "grupoAFAB_cis",
  "grupoAMAB_noncis",
  "grupoAFAB_noncis"
)

grupos_ingreso <- c(
  "AFAB cis",
  "AMAB no cis",
  "AFAB no cis"
)

beta_publicado <- c(-0.548, -0.039, -0.454)

comparaciones_ingreso <- list()
resumenes_ingreso <- list()

for (variable in variables_ingreso) {
  
  muestras <- list(
    ocupados_muestra_propia =
      controles_ok & ocupados & ingresos_validos[[variable]],
    
    ocupados_muestra_comun =
      muestra_comun
  )
  
  if (variable == "ytrabajocor") {
    muestras$sin_restriccion_actividad <-
      controles_ok & ingresos_validos[[variable]]
  }
  
  for (tipo_muestra in names(muestras)) {
    
    seleccion <- muestras[[tipo_muestra]]
    
    d <- droplevels(
      datos_ingresos_2017[seleccion, , drop = FALSE]
    )
    
    d$log_ingreso_prueba <- log(d[[variable]])
    
    modelo_prueba <- lm(
      as.formula(paste("log_ingreso_prueba ~", rhs)),
      data = d,
      weights = peso,
      na.action = na.fail
    )
    
    V_prueba <- sandwich::vcovHC(
      modelo_prueba,
      type = hc_tipo
    )
    
    beta <- coef(modelo_prueba)[terminos_ingreso]
    se <- sqrt(diag(V_prueba))[terminos_ingreso]
    
    p <- 2 * pt(
      abs(beta / se),
      df = df.residual(modelo_prueba),
      lower.tail = FALSE
    )
    
    identificador <- paste(variable, tipo_muestra, sep = "__")
    
    comparaciones_ingreso[[identificador]] <- data.frame(
      variable_ingreso = variable,
      muestra = tipo_muestra,
      grupo = grupos_ingreso,
      beta_Nettuno = beta_publicado,
      beta_replica = unname(beta),
      diferencia_beta = unname(beta) - beta_publicado,
      error_estandar = unname(se),
      p_value = unname(p),
      N = nobs(modelo_prueba)
    )
    
    resumenes_ingreso[[identificador]] <- data.frame(
      variable_ingreso = variable,
      muestra = tipo_muestra,
      N = nobs(modelo_prueba),
      N_Nettuno = 46472,
      diferencia_N = nobs(modelo_prueba) - 46472,
      media_ponderada = weighted.mean(d[[variable]], d$peso),
      media_no_ponderada = mean(d[[variable]]),
      media_Nettuno = 553204
    )
  }
}

comparacion_ingresos <- dplyr::bind_rows(comparaciones_ingreso)
resumen_ingresos <- dplyr::bind_rows(resumenes_ingreso)

cat("\nTAMAÑOS MUESTRALES Y MEDIAS:\n")
print(as.data.frame(resumen_ingresos), row.names = FALSE)

cat("\nCOEFICIENTES Y COMPARACIÓN CON NETTUNO:\n")
print(as.data.frame(comparacion_ingresos), row.names = FALSE)

dir.create(carpeta, recursive = TRUE, showWarnings = FALSE)

write.csv(
  resumen_ingresos,
  file.path(carpeta, "sensibilidad_ingresos_2017_N_medias.csv"),
  row.names = FALSE
)

write.csv(
  comparacion_ingresos,
  file.path(carpeta, "sensibilidad_ingresos_2017_coeficientes.csv"),
  row.names = FALSE
)

cat(
  "\nComparaciones guardadas en:",
  normalizePath(carpeta, winslash = "/"),
  "\n"
)
