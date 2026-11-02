%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ignacio Gálvez
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Resultados originales

beta_h1 = heck_1.beta;
beta_h2 = heck_2.beta;

V_h1 = heck_1.V;
V_h2 = heck_2.V;

dof_h1 = heck_1.dof;
dof_h2 = heck_2.dof;

%% Mapas: nombre del coeficiente -> posición

idx_h1 = containers.Map(names3, num2cell(1:numel(names3)));
idx_h2 = containers.Map(names4, num2cell(1:numel(names4)));

%% Construcción de combinaciones lineales

Lh1   = {};
labh1 = {};
baseh1 = {};

Lh2   = {};
labh2 = {};
baseh2 = {};

%% Modelo 1

for j = 1:numel(names3)

    nm = names3{j};

    if endsWith(nm, '*sexo')

        base = extractBefore(nm, '*sexo');

        if isKey(idx_h1, base)

            k = numel(names3);

            % Efecto para sexo = 1:
            % beta_base + beta_interaccion
            e1 = zeros(1, k);

            e1(idx_h1(base)) = 1;
            e1(idx_h1(nm))   = 1;

            Lh1{end+1,1}    = e1;
            labh1{end+1,1}  = sprintf('%s | sexo=1', base);
            baseh1{end+1,1} = base;

        end
    end
end

%% Modelo 2

for j = 1:numel(names4)

    nm = names4{j};

    if endsWith(nm, '*sexo')

        base = extractBefore(nm, '*sexo');

        if isKey(idx_h2, base)

            k = numel(names4);

            % Efecto para sexo = 1:
            % beta_base + beta_interaccion
            e1 = zeros(1, k);

            e1(idx_h2(base)) = 1;
            e1(idx_h2(nm))   = 1;

            Lh2{end+1,1}    = e1;
            labh2{end+1,1}  = sprintf('%s | sexo=1', base);
            baseh2{end+1,1} = base;

        end
    end
end

%% Convertir a matrices

if isempty(Lh1)
    L_h1 = zeros(0, numel(names3));
else
    L_h1 = vertcat(Lh1{:});
end

if isempty(Lh2)
    L_h2 = zeros(0, numel(names4));
else
    L_h2 = vertcat(Lh2{:});
end

%% Calcular combinaciones lineales

T_h1 = lincom( ...
    beta_h1, ...
    V_h1, ...
    L_h1, ...
    dof_h1, ...
    labh1);

T_h2 = lincom( ...
    beta_h2, ...
    V_h2, ...
    L_h2, ...
    dof_h2, ...
    labh2);

%% Insertar efectos para sexo = 1 en copias de los modelos

heck_1_men = heck_1;
heck_2_men = heck_2;

%% Índices automáticos de los términos base

idx_heck1_men = zeros(numel(baseh1), 1);

for j = 1:numel(baseh1)
    idx_heck1_men(j) = idx_h1(baseh1{j});
end

idx_heck2_men = zeros(numel(baseh2), 1);

for j = 1:numel(baseh2)
    idx_heck2_men(j) = idx_h2(baseh2{j});
end

%% Reemplazar coeficientes del modelo 1

heck_1_men.beta(idx_heck1_men) = T_h1.coef;
heck_1_men.se(idx_heck1_men)   = T_h1.se;
heck_1_men.t(idx_heck1_men)    = T_h1.t;
heck_1_men.p(idx_heck1_men)    = T_h1.p;

%% Reemplazar coeficientes del modelo 2

heck_2_men.beta(idx_heck2_men) = T_h2.coef;
heck_2_men.se(idx_heck2_men)   = T_h2.se;
heck_2_men.t(idx_heck2_men)    = T_h2.t;
heck_2_men.p(idx_heck2_men)    = T_h2.p;

%% Mostrar correspondencia para comprobar

disp('Modelo 1: efectos calculados para sexo = 1');
disp(table( ...
    baseh1, ...
    idx_heck1_men, ...
    T_h1.coef, ...
    T_h1.se, ...
    'VariableNames', ...
    {'Variable','Indice','Coeficiente','ErrorEstandar'}));

disp('Modelo 2: efectos calculados para sexo = 1');
disp(table( ...
    baseh2, ...
    idx_heck2_men, ...
    T_h2.coef, ...
    T_h2.se, ...
    'VariableNames', ...
    {'Variable','Indice','Coeficiente','ErrorEstandar'}));
