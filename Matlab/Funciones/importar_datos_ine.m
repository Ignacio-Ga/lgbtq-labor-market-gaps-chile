function [datos_filtrados, datos_h, datos_m] = importar_datos_ine(nombre_archivo)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ignacio Gálvez
%
% importar_datos_ine
%
% Importa y armoniza la Encuesta Web Diversidades del INE con parte de las
% variables utilizadas en el análisis de CASEN.
%
% USO:
%   [datos_ine, datos_h, datos_m] = ...
%       importar_datos_ine("bbdd_ewd_2025v1.csv");
%
% INPUT:
%   nombre_archivo : ruta.
%
% OUTPUTS:
%   datos_filtrados : muestra armonizada y filtrada.
%   datos_h         : personas de identidad masculina cis, cuando puede
%                     identificarse.
%   datos_m         : personas de identidad femenina cis, cuando puede
%                     identificarse.
%
% ADVERTENCIAS:
%   1. La EWD no es una encuesta probabilística.
%   2. No existe edad continua, sino tramo de edad.
%   3. El ingreso corresponde a ingreso personal total por tramos, no al
%      salario de la ocupación principal.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% ------------------------------------------------------------------------
% Importación
% -------------------------------------------------------------------------

opts = detectImportOptions(nombre_archivo, ...
    'Delimiter', ',', ...
    'TextType', 'string');

% Tratar códigos textuales como missing
opts = setvaropts(opts, opts.VariableNames, ...
    'TreatAsMissing', {'NA','Na','na','N/A','n/a','.', ''});

ine = readtable(nombre_archivo, opts);

% Convertir columnas importadas como string a double cuando sea posible
nombres = ine.Properties.VariableNames;

for j = 1:numel(nombres)

    nombre_j = nombres{j};

    if isstring(ine.(nombre_j)) || iscellstr(ine.(nombre_j))
        x = str2double(string(ine.(nombre_j)));

        % Solo reemplazar si al menos algún dato pudo convertirse
        if any(~isnan(x))
            ine.(nombre_j) = x;
        end
    end
end

%% ------------------------------------------------------------------------
%  Estandarización de códigos especiales
% -------------------------------------------------------------------------

% Variables numéricas
esNumerica = varfun(@isnumeric, ine, 'OutputFormat', 'uniform');
varsNumericas = ine.Properties.VariableNames(esNumerica);

% Los códigos -77, -66, 88 y 99 representan no aplicación,
% dato perdido, no sabe o no responde.
for j = 1:numel(varsNumericas)

    v = varsNumericas{j};
    x = ine.(v);

    % No se reemplaza 99 en variables derivadas antes de crear las
    % variables armonizadas, porque allí 99 significa "sin información".
    x(x == -77 | x == -66 | x == 88) = NaN;

    ine.(v) = x;
end

%% ------------------------------------------------------------------------
%  Año de la encuesta
% -------------------------------------------------------------------------

ine.t = repmat(2025, height(ine), 1);

%% ------------------------------------------------------------------------
%  Edad
% -------------------------------------------------------------------------

% tr_edad:
% 1 = 14 a 17 años
% 2 = 18 a 29 años
% 3 = 30 a 59 años
% 4 = 60 años o más

ine.edad_tramo = ine.tr_edad;

% Punto medio aproximado, SOLO para análisis auxiliares.
% No debe interpretarse como edad observada.
ine.edad_aprox = NaN(height(ine),1);

ine.edad_aprox(ine.tr_edad == 1) = 15.5;
ine.edad_aprox(ine.tr_edad == 2) = 23.5;
ine.edad_aprox(ine.tr_edad == 3) = 44.5;

% Para el tramo abierto 60+, se adopta 65 como valor convencional.
ine.edad_aprox(ine.tr_edad == 4) = 65;

ine.edad2_aprox = ine.edad_aprox.^2;

%% ------------------------------------------------------------------------
%  Educación
% -------------------------------------------------------------------------

% ed_nivel:
% 1 = Sin primaria o primaria incompleta
% 2 = Primaria completa
% 3 = Secundaria completa
% 4 = Técnica
% 5 = Universitaria completa
% 99 = Sin información

ine.educacion = ine.ed_nivel;
ine.educacion(ine.educacion == 99) = NaN;

% Dummies educacionales
ine.edu_primaria_incompleta = double(ine.educacion == 1);
ine.edu_primaria            = double(ine.educacion == 2);
ine.edu_secundaria          = double(ine.educacion == 3);
ine.edu_tecnica             = double(ine.educacion == 4);
ine.edu_universitaria       = double(ine.educacion == 5);

% Estudia actualmente:
% m03_var04:
% 1 = No
% 2 a 6 = Sí, según nivel
ine.asiste = NaN(height(ine),1);

idxValidoEstudia = ismember(ine.m03_var04, 1:6);
ine.asiste(idxValidoEstudia) = ...
    double(ismember(ine.m03_var04(idxValidoEstudia), 2:6));

%% ------------------------------------------------------------------------
%  Situación laboral
% -------------------------------------------------------------------------

% ft:
% 1 = dentro de la fuerza de trabajo
% 0 = fuera de la fuerza de trabajo

ine.ft_ine = ine.ft;
ine.ft_ine(ine.ft_ine == 99) = NaN;

% Participación laboral, comparable conceptualmente con activ.
ine.participa = ine.ft_ine;

% ocupacion:
% 1 = ocupada
% 0 = desocupada
%
% Para las personas fuera de la fuerza laboral normalmente será missing.
ine.l = ine.ocupacion;
ine.l(ine.l == 99) = NaN;

% Resultado alternativo: estar ocupada respecto de toda la población
% válida en edad laboral.
ine.empleado_pet = zeros(height(ine),1);
ine.empleado_pet(:) = NaN;

idxFT = ine.ft_ine == 1;
idxFueraFT = ine.ft_ine == 0;

ine.empleado_pet(idxFT) = ine.l(idxFT);
ine.empleado_pet(idxFueraFT) = 0;

%% ------------------------------------------------------------------------
% Ingreso por tramos
% -------------------------------------------------------------------------

% m01_var17:
% 1 = No recibió ingreso
% 2 = $0 a $250.000
% 3 = $250.001 a $500.000
% 4 = $500.001 a $1.000.000
% 5 = $1.000.001 a $1.500.000
% 6 = $1.500.001 a $2.000.000
% 7 = $2.000.001 a $3.000.000
% 8 = $3.000.001 o más

ine.ingreso_tramo = ine.m01_var17;

ine.ingreso_tramo( ...
    ~ismember(ine.ingreso_tramo, 1:8)) = NaN;

% Límites inferior y superior de cada intervalo
limite_inferior = [ ...
          0; ...
          0; ...
     250001; ...
     500001; ...
    1000001; ...
    1500001; ...
    2000001; ...
    3000001];

limite_superior = [ ...
          0; ...
     250000; ...
     500000; ...
    1000000; ...
    1500000; ...
    2000000; ...
    3000000; ...
        Inf];

ine.ingreso_li = NaN(height(ine),1);
ine.ingreso_ls = NaN(height(ine),1);

for k = 1:8
    idx = ine.ingreso_tramo == k;
    ine.ingreso_li(idx) = limite_inferior(k);
    ine.ingreso_ls(idx) = limite_superior(k);
end

% Puntos medios para aproximación continua
puntos_medios = [ ...
          0; ...
     125000; ...
     375000; ...
     750000; ...
    1250000; ...
    1750000; ...
    2500000; ...
    3500000];

ine.ingreso_mid = NaN(height(ine),1);

for k = 1:8
    ine.ingreso_mid(ine.ingreso_tramo == k) = puntos_medios(k);
end

% Logaritmo del ingreso aproximado
% Se utiliza log(ingreso + 1) para incluir el tramo sin ingreso.
ine.ln_ingreso_mid = log(ine.ingreso_mid + 1);

% Alternativa binaria: percibe algún ingreso
ine.tiene_ingreso = NaN(height(ine),1);

idxIngresoValido = ismember(ine.ingreso_tramo, 1:8);
ine.tiene_ingreso(idxIngresoValido) = ...
    double(ine.ingreso_tramo(idxIngresoValido) >= 2);

%% ------------------------------------------------------------------------
% Migración y pueblos originarios
% -------------------------------------------------------------------------

ine.extranjero = ine.p_migrante;
ine.extranjero(ine.extranjero == 99) = NaN;

% Pertenencia a pueblo originario
ine.indigena = NaN(height(ine),1);

idxIndigena = ismember(ine.m01_var07, [1,2]);
ine.indigena(idxIndigena) = ...
    double(ine.m01_var07(idxIndigena) == 1);

%% ------------------------------------------------------------------------
% Estado civil
% -------------------------------------------------------------------------

% m01_var18:
% 1 Casada
% 2 Conviviente
% 3 Conviviente civil
% 4 Anulada
% 5 Separada
% 6 Divorciada
% 7 Viuda
% 8 Soltera

ine.civil = NaN(height(ine),1);

idxCivilValido = ismember(ine.m01_var18, 1:8);

ine.civil(idxCivilValido) = ...
    double(ismember(ine.m01_var18(idxCivilValido), [1,2,3]));

%% ------------------------------------------------------------------------
% Hijos
% -------------------------------------------------------------------------

% La encuesta permite identificar si tiene hijos, pero no la cantidad ni
% cuántos son menores de 11 años como en CASEN.

ine.tiene_hijos = NaN(height(ine),1);

varsHijos = {'m01_var19_c01', ...
             'm01_var19_c02', ...
             'm01_var19_c03', ...
             'm01_var19_c04', ...
             'm01_var19_c05'};

if all(ismember(varsHijos, ine.Properties.VariableNames))

    sinHijos = ine.m01_var19_c01 == 1;

    conHijos = ine.m01_var19_c02 == 1 | ...
               ine.m01_var19_c03 == 1 | ...
               ine.m01_var19_c04 == 1 | ...
               ine.m01_var19_c05 == 1;

    ine.tiene_hijos(sinHijos) = 0;
    ine.tiene_hijos(conHijos) = 1;
end

%% ------------------------------------------------------------------------
%  Discapacidad y salud
% -------------------------------------------------------------------------

% La variable más comparable como control general es discapacidad.
ine.salud = ine.disc_dicotomica;
ine.salud(ine.salud == 99) = NaN;

% En esta función:
% salud = 1 indica persona con discapacidad.
% Conviene cambiar el nombre en las regresiones a "discapacidad".

ine.discapacidad = ine.salud;

%% ------------------------------------------------------------------------
% Identidad de género y orientación sexual
% -------------------------------------------------------------------------

% Variables derivadas oficiales
ine.trans = ine.sgos_trans;
ine.trans(ine.trans == 99) = NaN;

ine.div_genero = ine.sgos_div_genero;
ine.div_genero(ine.div_genero == 99) = NaN;

ine.div_sexual = ine.sgos_div_sexual;
ine.div_sexual(ine.div_sexual == 99) = NaN;

% LGBT+: persona con orientación sexual diversa o diversidad de género
ine.lgbt = NaN(height(ine),1);

idxSGOS = ~isnan(ine.div_genero) & ~isnan(ine.div_sexual);

ine.lgbt(idxSGOS) = double( ...
    ine.div_genero(idxSGOS) == 1 | ...
    ine.div_sexual(idxSGOS) == 1);

ine.cis_hetero = NaN(height(ine),1);
ine.cis_hetero(idxSGOS) = double(ine.lgbt(idxSGOS) == 0);

% Orientación sexual original m02_var09:
% 1 Gay
% 2 Lesbiana
% 3 Bisexual
% 4 Pansexual
% 5 Asexual
% 6 Heterosexual
% 7 Otra

ine.gay       = double(ine.m02_var09 == 1);
ine.lesbiana  = double(ine.m02_var09 == 2);
ine.b         = double(ine.m02_var09 == 3);
ine.pansexual = double(ine.m02_var09 == 4);
ine.asexual   = double(ine.m02_var09 == 5);
ine.hetero    = double(ine.m02_var09 == 6);
ine.other     = double(ine.m02_var09 == 7);

% Gay o lesbiana, equivalente aproximado de lg en CASEN
ine.lg = double(ismember(ine.m02_var09, [1,2]));

% Grupos agregados
ine.lgb = double(ismember(ine.m02_var09, [1,2,3]));
ine.lgb_plus = double(ismember(ine.m02_var09, [1,2,3,4,5,7]));

%% ------------------------------------------------------------------------
%  Identidad masculina y femenina cis
% -------------------------------------------------------------------------

% m02_var01:
% 1 Masculino
% 2 Femenino
% 3 Transmasculino
% 4 Transfemenino
% 5 No binario
% 6 Otro

ine.identidad_genero = ine.m02_var01;

% Sexo/género binario armonizado:
% 1 = masculino cis
% 0 = femenino cis
% NaN = identidades trans, no binarias, otras o información insuficiente
%
% Esta variable se crea solo para reproducir la separación utilizada en
% CASEN. No debe usarse como clasificación general de toda la muestra.

ine.sexo = NaN(height(ine),1);

idxMasculinoCis = ine.m02_var01 == 1 & ine.trans == 0;
idxFemeninoCis  = ine.m02_var01 == 2 & ine.trans == 0;

ine.sexo(idxMasculinoCis) = 1;
ine.sexo(idxFemeninoCis)  = 0;

%% ------------------------------------------------------------------------
% Identidad trans: discordancia entre género y sexo asignado al nacer
% -------------------------------------------------------------------------

% m02_var01: identidad de género
%   1 = Masculino
%   2 = Femenino
%   3 = Transmasculino
%   4 = Transfemenino
%   5 = No binario
%   6 = Otro
%
% m02_var05: sexo asignado al nacer
%   1 = Masculino
%   2 = Femenino

% Inicialmente se deja como missing
ine.trans1 = NaN(height(ine), 1);

% Observaciones con respuestas válidas en ambas preguntas
idx_valido = ismember(ine.m02_var01, 1:6) & ...
             ismember(ine.m02_var05, 1:2);

% Cisgénero:
% masculino con sexo masculino al nacer, o
% femenino con sexo femenino al nacer
idx_cis = idx_valido & ...
          ((ine.m02_var05 == 1 & ine.m02_var01 == 1) | ...
           (ine.m02_var05 == 2 & ine.m02_var01 == 2));

% Trans o diversidad de género:
% cualquier otra combinación válida
idx_trans = idx_valido & ~idx_cis;

ine.trans1(idx_cis)   = 0;
ine.trans1(idx_trans) = 1;
%% ------------------------------------------------------------------------
% LGBTQ+: orientación no heterosexual o identidad trans
% -------------------------------------------------------------------------

% m02_var09:
% 1 = Gay
% 2 = Lesbiana
% 3 = Bisexual
% 4 = Pansexual
% 5 = Asexual
% 6 = Heterosexual
% 7 = Otra orientación
%
% trans1:
% 1 = identidad de género discordante con sexo asignado al nacer
% 0 = identidad cisgénero
% NaN = información insuficiente

ine.lgbtq_plus = NaN(height(ine), 1);

% Orientación sexual válida
idx_orientacion_valida = ismember(ine.m02_var09, 1:7);

% Orientación sexual distinta de heterosexual
idx_no_hetero = idx_orientacion_valida & ine.m02_var09 ~= 6;

% Personas trans según la definición construida
idx_trans = ine.trans1 == 1;

% LGBTQ+: no heterosexual o trans
ine.lgbtq_plus(idx_no_hetero | idx_trans) = 1;

% Grupo de comparación: heterosexual cis
idx_cis_hetero = ine.m02_var09 == 6 & ine.trans1 == 0;

ine.lgbtq_plus(idx_cis_hetero) = 0;

% Indicador complementario
ine.cis_hetero = NaN(height(ine), 1);
ine.cis_hetero(idx_cis_hetero) = 1;
ine.cis_hetero(idx_no_hetero | idx_trans) = 0;

%% ------------------------------------------------------------------------
%  Filtros de muestra
% -------------------------------------------------------------------------

% En CASEN se utiliza 25 a 65 años.
% En EWD no puede reproducirse exactamente.
%
% La aproximación más defendible es conservar:
%   tr_edad == 2 : 18 a 29
%   tr_edad == 3 : 30 a 59
%   tr_edad == 4 : 60 o más
%
% Pero esto incluye personas de 18 a 24 y mayores de 65.
%
% Para evitar una falsa equivalencia, aquí se filtra 18 años o más y se
% deja tr_edad como control categórico.

ine = ine(ismember(ine.tr_edad, [2,3,4]), :);

% Excluir información inválida en orientación e identidad
ine = ine(~isnan(ine.m02_var09), :);
ine = ine(~isnan(ine.m02_var01), :);

%% ------------------------------------------------------------------------
% Resultado
% -------------------------------------------------------------------------

datos_filtrados = ine;

% Submuestras cis binarias
datos_h = ine(ine.sexo == 1, :);
datos_m = ine(ine.sexo == 0, :);

end