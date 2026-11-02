function [datos_filtrados,datos_h,datos_m,reg_names] = importar_datos(nombre_archivo,casen2)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ignacio Gálvez
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% importar_datos Carga un archivo .csv y filtra la base para quedarse con
% los datos a usar para el análisis.
% 
% USO:
% datos_filtrados = importar_datos('casen.csv');
%
% INPUTS:
%   - nombre_archivo: nombre del archivo CSV 
%
% OUTPUT:
%   - datos_filtrados: tabla con observaciones filtradas según criterios
%   - datos_h: subconjunto de datos_filtrados que solo contiene los datos
%   para los hombres
%   - datos_m: subconjunto de datos_filtrados que solo contiene los datos
%   para las muejres

%% Carga de datos
   % Configuración inicial
    opts = detectImportOptions(nombre_archivo, ...
        'Delimiter', ',', ...
        'TextType', 'string');

    opts = setvaropts(opts, opts.VariableNames, ...
        'TreatAsMissing', {'NA','Na','na','N/A','n/a','.', ''});

    % Fuerza TODAS las columnas a double
    opts = setvartype(opts, opts.VariableNames, 'double');
    
    % Importar
    casen2022 = readtable(nombre_archivo, opts);

    casen2024 = readtable(casen2, opts);


%% ------------------------------------------------------------------------
%  Creación de variables
% -------------------------------------------------------------------------

% Hijos menores de 11
casen2022.menor_11 = double(casen2022.edad < 11);

casen2024.menor_11 = double(casen2024.edad < 11);

% Conteo de menores por hogar (group-by id_vivienda, folio)
grpVars = {'id_vivienda','folio'};
tMin = groupsummary(casen2022, grpVars, 'sum', 'menor_11'); % crea sum_menor_11
tMin.Properties.VariableNames{end} = 'cantidad_menores_11';

grpVars2 = {'id_vivienda','folio'};
tMin2 = groupsummary(casen2024, grpVars, 'sum', 'menor_11'); % crea sum_menor_11
tMin2.Properties.VariableNames{end} = 'cantidad_menores_11';

% Unir de vuelta (left join)
casen2022 = outerjoin(casen2022, tMin(:,[grpVars, "cantidad_menores_11"]), ...
    'Keys', grpVars, 'MergeKeys', true, 'Type','left');

casen2024 = outerjoin(casen2024, tMin2(:,[grpVars2, "cantidad_menores_11"]), ...
    'Keys', grpVars, 'MergeKeys', true, 'Type','left');

% Rellenar NaN en el conteo con 0 y limpiar columnas auxiliares si fuera necesario
casen2022.cantidad_menores_11 = fillmissing(casen2022.cantidad_menores_11,'constant',0);

casen2024.cantidad_menores_11 = fillmissing(casen2024.cantidad_menores_11,'constant',0);


%%
%Variables laborales
casen2022.y = casen2022.y0101c;
casen2022.yho = casen2022.ytotcorh;
casen2022.l = casen2022.activ;
%casen2022.l = casen2022.o1;

casen2024.y = casen2024.y0101c;
casen2024.yho = casen2024.ytotcorh;
casen2024.l = casen2024.activ;
%casen2024.l = casen2024.o1;

casen2022.yho_base1000 = casen2022.ytotcorh ./ 1000;

casen2024.yho_base1000 = casen2024.ytotcorh ./ 1000;

% Empleo binario y actividad

casen2022.l    = double(casen2022.l==1);
casen2024.l    = double(casen2024.l==1);

%%
% ============================================================
% Missing estructural en o15
% ============================================================

if any(ismember(casen2022.Properties.VariableNames,'o15'))
    casen2022.o15 = fillmissing(casen2022.o15,'constant',0);
end

if any(ismember(casen2024.Properties.VariableNames,'o15'))
    casen2024.o15 = fillmissing(casen2024.o15,'constant',0);
end


% ============================================================
% Horas trabajadas (o10)
%
% Si no trabaja y o10 es missing -> 0 horas
% Si trabaja y o10 es missing -> mantener NaN
% Códigos negativos -> NaN
% ============================================================

% Primero convertir códigos negativos en missing
casen2022.o10(casen2022.o10 < 0) = NaN;
casen2024.o10(casen2024.o10 < 0) = NaN;

% Missing estructural para quienes no trabajan
idx_no_trabaja22 = casen2022.l == 0 & isnan(casen2022.o10);
casen2022.o10(idx_no_trabaja22) = 0;

idx_no_trabaja24 = casen2024.l == 0 & isnan(casen2024.o10);
casen2024.o10(idx_no_trabaja24) = 0;

%  Esc >= 0
casen2022 = casen2022( casen2022.esc >= 0, : );

casen2024 = casen2024( casen2024.esc >= 0, : );

% Variables derivadas
casen2022.exp   = casen2022.edad - casen2022.esc - 6;
casen2022.exp(casen2022.exp < 0) = 0;
casen2022.exp2  = casen2022.exp.^2;
casen2022.edad2 = casen2022.edad.^2;

casen2024.exp   = casen2024.edad - casen2024.esc - 6;
casen2024.exp(casen2024.exp < 0) = 0;
casen2024.exp2  = casen2024.exp.^2;
casen2024.edad2 = casen2024.edad.^2;

% Extranjero: 0 si r1a ∈ {1,2}; 1 en otro caso
casen2022.extranjero = double(~(casen2022.r1a==1 | casen2022.r1a==2));

casen2024.extranjero = double(~(casen2024.r1a==1 | casen2024.r1a==2));

%Eliminar responde -88 o -99 en género
casen2022 = casen2022( casen2022.genero ~= -88, : );
casen2022 = casen2022( casen2022.genero ~= -99, : );

casen2024 = casen2024( casen2024.genero ~= -88, : );
casen2024 = casen2024( casen2024.genero ~= -99, : );


% Trans (desalineación sexo vs genero)
casen2022.trans1 = double( (casen2022.sexo==1 & casen2022.genero~=1) | ...
                           (casen2022.sexo==2 & casen2022.genero~=2) );

casen2024.trans1 = double( (casen2024.sexo==1 & casen2024.genero~=1) | ...
    (casen2024.sexo==2 & casen2024.genero~=2) );


%Orientación/agrupaciones
casen2022.lg       = double(casen2022.os1==2);
casen2022.b        = double(casen2022.os1==3);
casen2022.other    = double(casen2022.os1==4);
casen2022.b_plus   = double(casen2022.os1==3 | casen2022.os1==4);
casen2022.lg_plus  = double(casen2022.os1==2 | casen2022.os1==4);
casen2022.lgb      = double(casen2022.os1==2 | casen2022.os1==3);
casen2022.lgb_plus = double(casen2022.os1==2 | casen2022.os1==3 | casen2022.os1==4);

casen2024.lg       = double(casen2024.os1==2);
casen2024.b        = double(casen2024.os1==3 );
casen2024.other    = double(casen2024.os1==4);
casen2024.b_plus   = double(casen2024.os1==3 | casen2024.os1==4);
casen2024.lg_plus  = double(casen2024.os1==2 | casen2024.os1==4);
casen2024.lgb      = double(casen2024.os1==2 | casen2024.os1==3);
casen2024.lgb_plus = double(casen2024.os1==2 | casen2024.os1==3 | casen2024.os1==4);


% LGBT (0 si heterosexual y cis coherente; 1 en caso contrario)
casen2022.lgbt = double( ~ ( (casen2022.os1==1 & casen2022.sexo==1 & casen2022.genero==1) | ...
                             (casen2022.os1==1 & casen2022.sexo==2 & casen2022.genero==2) ) );
casen2022.cis_hetero = double(  (casen2022.os1==1 & casen2022.sexo==1 & casen2022.genero==1) | ...
                             (casen2022.os1==1 & casen2022.sexo==2 & casen2022.genero==2) );

casen2024.lgbt = double( ~ ( (casen2024.os1==1 & casen2024.sexo==1 & casen2024.genero==1) | ...
                       (casen2024.os1==1 & casen2024.sexo==2 & casen2024.genero==2) ) );
casen2024.cis_hetero = double(  (casen2024.os1==1 & casen2024.sexo==1 & casen2024.genero==1) | ...
    (casen2024.os1==1 & casen2024.sexo==2 & casen2024.genero==2) );



%Asiste establecimiento educacional
casen2022.asiste = double(casen2022.asiste==1);

casen2022.e3 = double(casen2022.e3==1);

casen2024.asiste = double(casen2024.asiste==1);

casen2024.e3 = double(casen2024.e3==1);

%Estado civil

casen2022.civil       = double(casen2022.ecivil==1 | casen2022.ecivil==2 | casen2022.ecivil==3);

casen2024.civil       = double(casen2024.ecivil==1 | casen2024.ecivil==2 | casen2024.ecivil==3);
% (2) Eliminar no sabe en y1 == -88
casen2022 = casen2022( casen2022.y ~= -88, : );

casen2024 = casen2024( casen2024.y ~= -88, : );
% Horas mensuales y salario por hora
casen2022.hrs_m = casen2022.o10 .* 4;
casen2022.y_hrs = casen2022.y ./ casen2022.hrs_m;

casen2024.hrs_m = casen2024.o10 .* 4;
casen2024.y_hrs = casen2024.y ./ casen2024.hrs_m;
% Evitar división por cero: donde hrs_m==0, poner NaN en y_hrs
idx0 = (casen2022.hrs_m==0 | isnan(casen2022.hrs_m));
casen2022.y_hrs(idx0) = NaN;

idx02 = (casen2024.hrs_m==0 | isnan(casen2024.hrs_m));
casen2024.y_hrs(idx02) = NaN;

% Presentes
if any(ismember(casen2022.Properties.VariableNames,'os_presente'))
    casen2022 = casen2022(casen2022.os_presente==1,:);
end

if any(ismember(casen2024.Properties.VariableNames,'os_presente'))
    casen2024 = casen2024(casen2024.os_presente==1,:);
end


% Filtrar registros con peso no-missing
if any(ismember(casen2022.Properties.VariableNames,'expr_osig'))
    casen2022 = casen2022(~isnan(casen2022.expr_osig),:);
end

if any(ismember(casen2024.Properties.VariableNames,'expr_osig'))
    casen2024 = casen2024(~isnan(casen2024.expr_osig),:);
end


% Sexo a binario 1/0 (1 si era 1; 0 en otro caso)
casen2022.sexo = double(casen2022.sexo==1);

casen2024.sexo = double(casen2024.sexo==1);

%% -----------------------------------------------------------
% Filtros
% -----------------------------------------------------------

% (1) Eliminar o15 en {1,2,8} -> Mantener solo 0,3,4,5,6,7,9
casen2022 = casen2022( ismember(casen2022.o15, [0,3,4,5,6,7,9]), : );

casen2024 = casen2024( ismember(casen2024.o15, [0,3,4,5,6,7,9]), : );

% % (2) Eliminar no responden en o10 == -88
% casen2022 = casen2022( casen2022.o10 ~= -88, : );
% 
% casen2024 = casen2024( casen2024.o10 ~= -88, : );

% (3) Eliminar no saben/no responden OS (os1 != -88 & != -99)
casen2022 = casen2022( casen2022.os1 ~= -88 & casen2022.os1 ~= -99, : );

casen2024 = casen2024( casen2024.os1 ~= -88 & casen2024.os1 ~= -99, : );

%Eliminar no sabe en s28 == -88
% casen2022 = casen2022( casen2022.s28 ~= -88, : );
% 
% casen2022.salud = double(casen2022.s28 ~= 22);
% 
% casen2024 = casen2024( casen2024.s28 ~= -88, : );
% 
% casen2024.salud = double(casen2024.s28 ~= 22);


%Hijo jefe del hogar
%casen2022.hjh = double(casen2022.pco1_b==4 | casen2022.pco1_b==5 | casen2022.pco1_b==6);

%% -----------------------------------------------------------
% (7) Variables derivadas: ingreso no laboral e ingreso por hora (ln)
% -----------------------------------------------------------

% ingreso no laboral: ytoth - y1, salvo casos con NA o y1 == -88
ing_nl = (casen2022.yho - casen2022.yautcorh) ./ casen2022.tot_per_h;

ing_nl24 = (casen2024.yho - casen2024.yautcorh) ./ casen2024.tot_per_h;
% Reemplazar por 0 cuando yho es NaN
ing_nl(isnan(casen2022.yho)) = 0;

ing_nl = log(ing_nl + 1);

ing_nl24(isnan(casen2024.yho)) = 0;

ing_nl24 = log(ing_nl24 + 1);



% ln_y: log(y1 / hrs_m), evitando infinito y división por cero
ln_y = log(casen2022.y ./ casen2022.hrs_m);

ln_y24 = log(casen2024.y ./ casen2024.hrs_m);
% Donde hrs_m = 0, hrs_m NaN o log = ±Inf, reemplazar por NaN
idxInvalid = casen2022.hrs_m == 0 | isnan(casen2022.hrs_m) | isinf(ln_y);
ln_y(idxInvalid) = NaN;

idxInvalid24 = casen2024.hrs_m == 0 | isnan(casen2024.hrs_m) | isinf(ln_y24);
ln_y24(idxInvalid24) = NaN;

% Añadir columnas a la tabla
casen2022.ing_nl = ing_nl;
casen2022.ln_y   = ln_y;

casen2024.ing_nl = ing_nl24;
casen2024.ln_y   = ln_y24;

%casen2022 = casen2022(~isnan(casen2022.ing_nl), :);

% % (8) Eliminar menores de 15
% casen2022 = casen2022( casen2022.edad >= 25, : );
% 
% casen2024 = casen2024( casen2024.edad >= 25, : );

%  Eliminar menores de 18
casen2022 = casen2022( casen2022.edad >= 18, : );

casen2024 = casen2024( casen2024.edad >= 18, : );

% % (5) Eliminar mayores de 65
% casen2022 = casen2022( casen2022.edad <= 65, : );
% 
% casen2024 = casen2024( casen2024.edad <= 65, : );

% Variable de interacción

casen2022.sx_menor11 = casen2022.sexo .* casen2022.cantidad_menores_11;

casen2024.sx_menor11 = casen2024.sexo .* casen2024.cantidad_menores_11;

% ==============================
% FE de región (dummies + nombres)
% ==============================
reg_cat = categorical(casen2022.region);
Dreg    = dummyvar(reg_cat);           % K columnas 
Dreg    = Dreg(:,2:end);               % quitamos la 1ª como base 

% Nombres de columnas
ureg = categories(reg_cat);            % celdas tipo {'1','2',...}
ureg = ureg(:);
reg_names = arrayfun(@(k) sprintf('reg_%s', string(ureg(k))), (2:numel(ureg))', 'UniformOutput', false);

reg_cat24 = categorical(casen2024.region);
Dreg24    = dummyvar(reg_cat24);           % K columnas 
Dreg24    = Dreg24(:,2:end);               % quitamos la 1ª como base 

% Nombres de columnas
ureg24 = categories(reg_cat24);            % celdas tipo {'1','2',...}
ureg24 = ureg24(:);
reg_names24 = arrayfun(@(k) sprintf('reg_%s', string(ureg24(k))), (2:numel(ureg24))', 'UniformOutput', false);

% Guardar en la tabla principal 
casen2022.D_region = Dreg;

casen2024.D_region = Dreg24;

%% -------------------------------------------------------------
% Periodo e indicador temporal
% -------------------------------------------------------------

% Variable con el año de la encuesta
casen2022.periodo = repmat(2022, height(casen2022), 1);
casen2024.periodo = repmat(2024, height(casen2024), 1);

% Indicador temporal:
% t = 0 para CASEN 2022
% t = 1 para CASEN 2024
casen2022.t = zeros(height(casen2022), 1);
casen2024.t = ones(height(casen2024), 1);

%%-------------------------------------------------------------
%Juntar ambas tablas

% Obtener los nombres de variables comunes a ambas tablas
columnasComunes = intersect( ...
    casen2022.Properties.VariableNames, ...
    casen2024.Properties.VariableNames, ...
    'stable');

% Conservar únicamente las columnas comunes
casen2022_comun = casen2022(:, columnasComunes);
casen2024_comun = casen2024(:, columnasComunes);

% Apilar verticalmente ambas tablas
casen = [casen2022_comun; casen2024_comun];
%% -----------------------------------------------------------
% (9) Separar por sexo
% -----------------------------------------------------------
casen_m = casen( casen.sexo == 0, : );  % mujeres
casen_h = casen( casen.sexo == 1, : );  % hombres

datos_filtrados = casen;
datos_h = casen_h;
datos_m = casen_m;
end