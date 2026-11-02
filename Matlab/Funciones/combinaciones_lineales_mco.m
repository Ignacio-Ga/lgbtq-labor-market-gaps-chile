%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ignacio Gálvez
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% ========================================================================
% Nombres en el orden exacto de las matrices x1, x2 y x3s.
% ========================================================================

varnames1 = [ ...
    {'Intercepto', ...
     'esc', ...
     'esc*sexo', ...
     'exp', ...
     'exp2', ...
     'sexo', ...
     'pueblos_indigenas', ...
     'extranjero'}, ...
    reg_names', ...
    {'t'} ...
];

varnames2 = [ ...
    {'Intercepto', ...
     'esc', ...
     'esc*sexo', ...
     'exp', ...
     'exp2', ...
     'sexo', ...
     'pueblos_indigenas', ...
     'extranjero', ...
     'lgbt', ...
     'lgbt*sexo'}, ...
    reg_names', ...
    {'t'} ...
];

varnames3 = [ ...
    {'Intercepto', ...
     'esc', ...
     'esc*sexo', ...
     'exp', ...
     'exp2', ...
     'sexo', ...
     'pueblos_indigenas', ...
     'extranjero', ...
     'lg', ...
     'lg*sexo', ...
     'b', ...
     'b*sexo', ...
     'other', ...
     'other*sexo', ...
     'trans1', ...
     'trans1*sexo'}, ...
    reg_names', ...
    {'t'} ...
];

%% ========================================================================
% Crear copias para almacenar los efectos correspondientes a sexo = 1
% ========================================================================

res1_men = res1;
res2_men = res2;
res3_men = res3;

%% ========================================================================
% MODELO 1
% ========================================================================

beta_mco1 = res1.beta;
V_mco1    = res1.V;
dof_mco1  = res1.dof;

idx_mco1 = containers.Map( ...
    varnames1, ...
    num2cell(1:numel(varnames1)));

L1      = {};
labels1 = {};
bases1  = {};

for j = 1:numel(varnames1)

    nombre = varnames1{j};

    if endsWith(nombre, '*sexo')

        base = extractBefore(nombre, '*sexo');

        if isKey(idx_mco1, base)

            contraste = zeros(1, numel(varnames1));

            contraste(idx_mco1(base))   = 1;
            contraste(idx_mco1(nombre)) = 1;

            L1{end+1,1}      = contraste;
            labels1{end+1,1} = sprintf('%s | sexo=1', base);
            bases1{end+1,1}  = base;
        end
    end
end

L_mco1 = vertcat(L1{:});

T_mco1 = lincom( ...
    beta_mco1, ...
    V_mco1, ...
    L_mco1, ...
    dof_mco1, ...
    labels1);

idx_res1_men = zeros(numel(bases1),1);

for j = 1:numel(bases1)
    idx_res1_men(j) = idx_mco1(bases1{j});
end

res1_men.beta(idx_res1_men) = T_mco1.coef;
res1_men.se(idx_res1_men)   = T_mco1.se;
res1_men.t(idx_res1_men)    = T_mco1.t;
res1_men.p(idx_res1_men)    = T_mco1.p;

%% Constante para sexo = 1: intercepto + sexo

L_const1 = zeros(1,numel(varnames1));
L_const1(idx_mco1('Intercepto')) = 1;
L_const1(idx_mco1('sexo'))       = 1;

T_const1 = lincom( ...
    beta_mco1, ...
    V_mco1, ...
    L_const1, ...
    dof_mco1, ...
    {'Intercepto | sexo=1'});

iConst1 = idx_mco1('Intercepto');

res1_men.beta(iConst1) = T_const1.coef;
res1_men.se(iConst1)   = T_const1.se;
res1_men.t(iConst1)    = T_const1.t;
res1_men.p(iConst1)    = T_const1.p;

%% ========================================================================
% MODELO 2
% ========================================================================

beta_mco2 = res2.beta;
V_mco2    = res2.V;
dof_mco2  = res2.dof;

idx_mco2 = containers.Map( ...
    varnames2, ...
    num2cell(1:numel(varnames2)));

L2      = {};
labels2 = {};
bases2  = {};

for j = 1:numel(varnames2)

    nombre = varnames2{j};

    if endsWith(nombre, '*sexo')

        base = extractBefore(nombre, '*sexo');

        if isKey(idx_mco2, base)

            contraste = zeros(1,numel(varnames2));

            contraste(idx_mco2(base))   = 1;
            contraste(idx_mco2(nombre)) = 1;

            L2{end+1,1}      = contraste;
            labels2{end+1,1} = sprintf('%s | sexo=1',base);
            bases2{end+1,1}  = base;
        end
    end
end

L_mco2 = vertcat(L2{:});

T_mco2 = lincom( ...
    beta_mco2, ...
    V_mco2, ...
    L_mco2, ...
    dof_mco2, ...
    labels2);

idx_res2_men = zeros(numel(bases2),1);

for j = 1:numel(bases2)
    idx_res2_men(j) = idx_mco2(bases2{j});
end

res2_men.beta(idx_res2_men) = T_mco2.coef;
res2_men.se(idx_res2_men)   = T_mco2.se;
res2_men.t(idx_res2_men)    = T_mco2.t;
res2_men.p(idx_res2_men)    = T_mco2.p;

%% Constante para sexo = 1

L_const2 = zeros(1,numel(varnames2));
L_const2(idx_mco2('Intercepto')) = 1;
L_const2(idx_mco2('sexo'))       = 1;

T_const2 = lincom( ...
    beta_mco2, ...
    V_mco2, ...
    L_const2, ...
    dof_mco2, ...
    {'Intercepto | sexo=1'});

iConst2 = idx_mco2('Intercepto');

res2_men.beta(iConst2) = T_const2.coef;
res2_men.se(iConst2)   = T_const2.se;
res2_men.t(iConst2)    = T_const2.t;
res2_men.p(iConst2)    = T_const2.p;

%% ========================================================================
% MODELO 3
% ========================================================================

beta_mco3 = res3.beta;
V_mco3    = res3.V;
dof_mco3  = res3.dof;

idx_mco3 = containers.Map( ...
    varnames3, ...
    num2cell(1:numel(varnames3)));

L3      = {};
labels3 = {};
bases3  = {};

for j = 1:numel(varnames3)

    nombre = varnames3{j};

    if endsWith(nombre, '*sexo')

        base = extractBefore(nombre, '*sexo');

        if isKey(idx_mco3,base)

            contraste = zeros(1,numel(varnames3));

            contraste(idx_mco3(base))   = 1;
            contraste(idx_mco3(nombre)) = 1;

            L3{end+1,1}      = contraste;
            labels3{end+1,1} = sprintf('%s | sexo=1',base);
            bases3{end+1,1}  = base;
        end
    end
end

L_mco3 = vertcat(L3{:});

T_mco3 = lincom( ...
    beta_mco3, ...
    V_mco3, ...
    L_mco3, ...
    dof_mco3, ...
    labels3);

idx_res3_men = zeros(numel(bases3),1);

for j = 1:numel(bases3)
    idx_res3_men(j) = idx_mco3(bases3{j});
end

res3_men.beta(idx_res3_men) = T_mco3.coef;
res3_men.se(idx_res3_men)   = T_mco3.se;
res3_men.t(idx_res3_men)    = T_mco3.t;
res3_men.p(idx_res3_men)    = T_mco3.p;

%% Constante para sexo = 1

L_const3 = zeros(1,numel(varnames3));
L_const3(idx_mco3('Intercepto')) = 1;
L_const3(idx_mco3('sexo'))       = 1;

T_const3 = lincom( ...
    beta_mco3, ...
    V_mco3, ...
    L_const3, ...
    dof_mco3, ...
    {'Intercepto | sexo=1'});

iConst3 = idx_mco3('Intercepto');

res3_men.beta(iConst3) = T_const3.coef;
res3_men.se(iConst3)   = T_const3.se;
res3_men.t(iConst3)    = T_const3.t;
res3_men.p(iConst3)    = T_const3.p;

%% ========================================================================
% Mostrar resultados y comprobar cálculos
% ========================================================================

disp('==============================================================');
disp('MCO modelo 1: efectos para sexo = 1');
disp('==============================================================');

disp(table( ...
    bases1, ...
    idx_res1_men, ...
    T_mco1.coef, ...
    T_mco1.se, ...
    T_mco1.t, ...
    T_mco1.p, ...
    'VariableNames', ...
    {'Variable','Indice','Coeficiente','ErrorEstandar','EstadisticoT','PValue'}));

disp('==============================================================');
disp('MCO modelo 2: efectos para sexo = 1');
disp('==============================================================');

disp(table( ...
    bases2, ...
    idx_res2_men, ...
    T_mco2.coef, ...
    T_mco2.se, ...
    T_mco2.t, ...
    T_mco2.p, ...
    'VariableNames', ...
    {'Variable','Indice','Coeficiente','ErrorEstandar','EstadisticoT','PValue'}));

disp('==============================================================');
disp('MCO modelo 3: efectos para sexo = 1');
disp('==============================================================');

disp(table( ...
    bases3, ...
    idx_res3_men, ...
    T_mco3.coef, ...
    T_mco3.se, ...
    T_mco3.t, ...
    T_mco3.p, ...
    'VariableNames', ...
    {'Variable','Indice','Coeficiente','ErrorEstandar','EstadisticoT','PValue'}));