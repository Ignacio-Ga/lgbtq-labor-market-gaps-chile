function out = ordered_probit_mem(res, varnames, opts)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ignacio Gálvez
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% ORDERED_PROBIT_MEM
%
% Calcula efectos marginales evaluados en las medias para un modelo
% probit ordenado estimado mediante svy_ordered_probit.
%
% Para regresores binarios calcula cambios discretos:
%
%   P(y=j | x_k=1, X_-k=media)
% - P(y=j | x_k=0, X_-k=media)
%
% Para regresores continuos calcula:
%
%   dP(y=j|x) / dx_k
%
% También calcula errores estándar mediante método delta numérico,
% utilizando res.V, cuya parametrización debe ser:
%
%   [beta ; puntos de corte]
%
% USO:
%
%   efectos = ordered_probit_mem(res_oprobit1, varnames, opts);
%
% INPUTS:
%
%   res:
%       resultado de svy_ordered_probit. Debe contener:
%       .beta
%       .cutpoints
%       .V
%       .X
%       .categories
%       .dof
%
%   varnames:
%       nombres de los K regresores.
%
%   opts:
%       .dummyIdx
%           índices de regresores binarios.
%           Por defecto, detecta variables que solo contienen 0 y 1.
%
%       .categoryNames
%           nombres de las categorías del resultado.
%
%       .digitsME
%           decimales del efecto marginal, default 4.
%
%       .digitsSE
%           decimales del error estándar, default 4.
%
%       .digitsZ
%           decimales del estadístico, default 3.
%
%       .digitsP
%           decimales del p-valor, default 4.
%
%       .printToScreen
%           mostrar tablas, default true.
%
% OUTPUT:
%
%   out.effects:
%       matriz K x J de efectos marginales.
%
%   out.se:
%       matriz K x J de errores estándar.
%
%   out.z:
%       matriz K x J de estadísticos.
%
%   out.p:
%       matriz K x J de p-valores.
%
%   out.tables:
%       celda con una tabla para cada categoría.
%
%   out.xbar:
%       vector de medias de los regresores.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% ------------------------------------------------------------------------
%  Opciones
% -------------------------------------------------------------------------

if nargin < 3
    opts = struct();
end

if ~isfield(opts,'digitsME')
    opts.digitsME = 4;
end

if ~isfield(opts,'digitsSE')
    opts.digitsSE = 4;
end

if ~isfield(opts,'digitsZ')
    opts.digitsZ = 3;
end

if ~isfield(opts,'digitsP')
    opts.digitsP = 4;
end

if ~isfield(opts,'printToScreen')
    opts.printToScreen = true;
end

%% ------------------------------------------------------------------------
%  Verificaciones
% -------------------------------------------------------------------------

camposNecesarios = { ...
    'beta', ...
    'cutpoints', ...
    'V', ...
    'X', ...
    'categories'};

for c = 1:numel(camposNecesarios)

    if ~isfield(res, camposNecesarios{c})
        error('Falta el campo res.%s.', camposNecesarios{c});
    end

end

beta = res.beta(:);
cuts = res.cutpoints(:);
X    = res.X;

K = numel(beta);
J = numel(cuts) + 1;

assert(size(X,2) == K, ...
    'El número de columnas de res.X no coincide con res.beta.');

assert(numel(varnames) == K, ...
    'varnames debe contener un nombre por cada regresor.');

varnames = varnames(:);

%% ------------------------------------------------------------------------
%  Detectar regresores binarios
% -------------------------------------------------------------------------

if isfield(opts,'dummyIdx') && ~isempty(opts.dummyIdx)

    dummyIdx = opts.dummyIdx(:);

else

    dummyIdx = [];

    for k = 1:K

        valores = unique(X(~isnan(X(:,k)),k));

        if all(ismember(valores,[0,1])) && numel(valores) <= 2
            dummyIdx(end+1,1) = k; %#ok<AGROW>
        end

    end

end

esDummy = false(K,1);
esDummy(dummyIdx) = true;

%% ------------------------------------------------------------------------
% Nombres de categorías
% -------------------------------------------------------------------------

if isfield(opts,'categoryNames') && ~isempty(opts.categoryNames)

    categoryNames = opts.categoryNames(:);

    assert(numel(categoryNames) == J, ...
        'categoryNames debe contener J nombres.');

else

    categoryNames = arrayfun( ...
        @(j) sprintf('Categoría %g',res.categories(j)), ...
        (1:J)', ...
        'UniformOutput',false);

end

%% ------------------------------------------------------------------------
%  Punto de evaluación: medias de X
% -------------------------------------------------------------------------

xbar = mean(X,1,'omitnan');

psiHat = [beta; cuts];

nPar = numel(psiHat);

effects = zeros(K,J);
se      = zeros(K,J);
zstat   = zeros(K,J);
pval    = zeros(K,J);

%% ------------------------------------------------------------------------
%Efectos marginales y método delta
% -------------------------------------------------------------------------

for k = 1:K

    for j = 1:J

        % Función escalar del efecto marginal respecto de psi
        effectFunction = @(psi) marginalEffectScalar( ...
            psi, xbar, k, j, K, J, esDummy(k));

        % Estimación puntual
        effects(k,j) = effectFunction(psiHat);

        % Gradiente numérico respecto de [beta; cortes]
        grad = numericalGradient(effectFunction, psiHat);

        % Método delta
        varEffect = grad' * res.V * grad;

        se(k,j) = sqrt(max(varEffect,0));

        if se(k,j) > 0

            zstat(k,j) = effects(k,j) / se(k,j);

            if isfield(res,'dof') && res.dof > 0
                pval(k,j) = 2 * tcdf(-abs(zstat(k,j)),res.dof);
            else
                pval(k,j) = 2 * normcdf(-abs(zstat(k,j)));
            end

        else

            zstat(k,j) = NaN;
            pval(k,j)  = NaN;

        end

    end

end

%% ------------------------------------------------------------------------
%  Tablas por categoría
% -------------------------------------------------------------------------

tables = cell(J,1);

fmtME = sprintf('%%.%df',opts.digitsME);
fmtSE = sprintf('%%.%df',opts.digitsSE);
fmtZ  = sprintf('%%.%df',opts.digitsZ);
fmtP  = sprintf('%%.%df',opts.digitsP);

for j = 1:J

    MEcol = str2double(compose(fmtME,effects(:,j)));
    SEcol = str2double(compose(fmtSE,se(:,j)));
    Zcol  = str2double(compose(fmtZ,zstat(:,j)));
    Pcol  = str2double(compose(fmtP,pval(:,j)));

    Sig = arrayfun( ...
        @(p) pstars(p), ...
        pval(:,j), ...
        'UniformOutput',false);

    Tipo = repmat({''},K,1);

    for k = 1:K

        if esDummy(k)
            Tipo{k} = 'Cambio discreto 0->1';
        else
            Tipo{k} = 'Derivada';
        end

    end

    T = table( ...
        varnames, ...
        MEcol, ...
        SEcol, ...
        Zcol, ...
        Pcol, ...
        Sig, ...
        Tipo, ...
        'VariableNames', ...
        {'Variable','EfectoMarginal','SE','z','p','Sig','Tipo'});

    tables{j} = T;

    if opts.printToScreen

        fprintf('\n============================================================\n');
        fprintf('EFECTOS MARGINALES EN LAS MEDIAS: %s\n', ...
            upper(categoryNames{j}));
        fprintf('============================================================\n\n');

        disp(T);

        fprintf(['Signif.:  . p<0.10   * p<0.05   ', ...
                 '** p<0.01   *** p<0.001\n']);

        fprintf(['Nota: los efectos de variables binarias corresponden ', ...
                 'al cambio discreto de 0 a 1.\n']);

    end

end

%% ------------------------------------------------------------------------
%  Resultado
% -------------------------------------------------------------------------

out = struct();

out.effects       = effects;
out.se            = se;
out.z             = zstat;
out.p             = pval;
out.tables        = tables;
out.xbar          = xbar;
out.dummyIdx      = dummyIdx;
out.categoryNames = categoryNames;

end


%% =========================================================================
% Efecto marginal escalar
% =========================================================================

function effect = marginalEffectScalar( ...
    psi, xbar, k, j, K, J, isDummy)

beta = psi(1:K);
cuts = psi(K+1:end);

if isDummy

    % Cambio discreto de 0 a 1
    x0 = xbar;
    x1 = xbar;

    x0(k) = 0;
    x1(k) = 1;

    p0 = orderedProbabilities(x0 * beta,cuts);
    p1 = orderedProbabilities(x1 * beta,cuts);

    effect = p1(j) - p0(j);

else

    % Derivada analítica para una variable continua
    eta = xbar * beta;

    lowerCuts = [-Inf; cuts];
    upperCuts = [cuts; Inf];

    lower = lowerCuts(j) - eta;
    upper = upperCuts(j) - eta;

    phiLower = normpdf(lower);
    phiUpper = normpdf(upper);

    if ~isfinite(lower)
        phiLower = 0;
    end

    if ~isfinite(upper)
        phiUpper = 0;
    end

    effect = beta(k) * (phiLower - phiUpper);

end

end


%% =========================================================================
% Probabilidades ordenadas para una observación
% =========================================================================

function p = orderedProbabilities(eta,cuts)

J = numel(cuts) + 1;

p = zeros(1,J);

p(1) = normcdf(cuts(1) - eta);

for j = 2:J-1

    p(j) = normcdf(cuts(j) - eta) - ...
           normcdf(cuts(j-1) - eta);

end

p(J) = 1 - normcdf(cuts(end) - eta);

p = max(p,0);
p = p ./ sum(p);

end


%% =========================================================================
% Gradiente numérico central
% =========================================================================

function grad = numericalGradient(fun,theta)

theta = theta(:);

P = numel(theta);

grad = zeros(P,1);

% Paso relativo
h = eps^(1/3) .* max(abs(theta),1);

for q = 1:P

    eq = zeros(P,1);
    eq(q) = h(q);

    fPlus  = fun(theta + eq);
    fMinus = fun(theta - eq);

    grad(q) = (fPlus - fMinus) / (2*h(q));

end

end


%% =========================================================================
% Símbolos de significancia
% =========================================================================

function s = pstars(p)

if isnan(p)

    s = '';

elseif p < 0.001

    s = '***';

elseif p < 0.01

    s = '**';

elseif p < 0.05

    s = '*';

elseif p < 0.10

    s = '.';

else

    s = '';

end

end