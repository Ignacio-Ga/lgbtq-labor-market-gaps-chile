function out = svy_ordered_probit(y, X, w, strata, cluster, opts)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ignacio Gálvez
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% SVY_ORDERED_PROBIT
%
% Probit ordenado estimado por máxima verosimilitud ponderada, con
% matriz de varianza tipo sandwich que incorpora estratos y UPM.
%
% MODELO:
%
%   y_i* = X_i beta + epsilon_i
%   epsilon_i ~ N(0,1)
%
%   y_i = 1  si y_i* <= kappa_1
%   y_i = 2  si kappa_1 < y_i* <= kappa_2
%   ...
%   y_i = J  si y_i* > kappa_{J-1}
%
% PROBABILIDADES:
%
%   P(y_i=j|X_i) =
%       Phi(kappa_j - X_i beta)
%       - Phi(kappa_{j-1} - X_i beta)
%
% donde:
%
%   kappa_0 = -Inf
%   kappa_J = Inf
%
% USO:
%
%   out = svy_ordered_probit(y, X, w, strata, cluster);
%
%   out = svy_ordered_probit(y, X, w, strata, cluster, opts);
%
% ENTRADAS:
%
%   y       : Nx1 variable ordinal.
%             Las categorías deben poder ordenarse.
%
%   X       : NxK matriz de regresores.
%             NO debe incluir intercepto.
%
%   w       : Nx1 pesos.
%             Si se entrega [], se usan pesos iguales a 1.
%
%   strata  : Nx1 identificador de estrato.
%             Si se entrega [], se considera un único estrato.
%
%   cluster : Nx1 identificador de UPM.
%             Si se entrega [], cada observación es su propia UPM.
%
%   opts    : estructura opcional:
%
%       .weightNormalization
%           'none'  : no normaliza los pesos
%           'mean1' : pesos con media 1
%           'sumN'  : pesos que suman N
%
%       .maxIter
%           Máximo de iteraciones de fminunc.
%
%       .tol
%           Tolerancia del optimizador.
%
%       .verbose
%           true o false.
%
%       .theta0
%           Valor inicial del vector de parámetros transformados.
%
% SALIDAS:
%
%   out.beta          : coeficientes beta
%   out.cutpoints     : puntos de corte ordenados
%   out.se_beta       : EE de beta por diseño
%   out.se_cutpoints  : EE de los puntos de corte
%   out.V             : VCOV de [beta; puntos de corte]
%   out.V_theta       : VCOV en parametrización interna
%   out.z_beta        : estadísticos t de beta
%   out.p_beta        : p-valores de beta
%   out.z_cutpoints   : estadísticos t de puntos de corte
%   out.p_cutpoints   : p-valores de puntos de corte
%   out.logLik        : log-verosimilitud ponderada
%   out.n             : tamaño de muestra usado
%   out.K             : número de regresores
%   out.J             : número de categorías
%   out.G             : número total de UPM utilizadas
%   out.H             : número de estratos
%   out.dof           : grados de libertad G-H
%   out.categories    : categorías originales de y
%   out.p_hat         : probabilidades predichas NxJ
%   out.y_hat         : categoría predicha
%   out.converged     : indicador de convergencia
%   out.exitflag      : código de salida de fminunc
%   out.note          : advertencias
%
% REQUISITO:
%   Optimization Toolbox, debido al uso de fminunc.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% ------------------------------------------------------------------------
%  Opciones
% -------------------------------------------------------------------------

if nargin < 6
    opts = struct();
end

if ~isfield(opts, 'weightNormalization')
    opts.weightNormalization = 'none';
end

if ~isfield(opts, 'maxIter')
    opts.maxIter = 500;
end

if ~isfield(opts, 'tol')
    opts.tol = 1e-8;
end

if ~isfield(opts, 'verbose')
    opts.verbose = true;
end

%% ------------------------------------------------------------------------
%  Dimensiones y valores por defecto
% -------------------------------------------------------------------------

y = y(:);

[N, K] = size(X);

if isempty(w)
    w = ones(N,1);
else
    w = w(:);
end

if isempty(strata)
    strata = ones(N,1);
else
    strata = strata(:);
end

if isempty(cluster)
    cluster = (1:N)';
else
    cluster = cluster(:);
end

assert(length(y) == N, ...
    'La dimensión de y no coincide con el número de filas de X.');

assert(length(w) == N, ...
    'La dimensión de w no coincide con el número de filas de X.');

assert(length(strata) == N, ...
    'La dimensión de strata no coincide con el número de filas de X.');

assert(length(cluster) == N, ...
    'La dimensión de cluster no coincide con el número de filas de X.');

%% ------------------------------------------------------------------------
%  Eliminar observaciones incompletas
% -------------------------------------------------------------------------

miss = isnan(y) | ...
       any(isnan(X),2) | ...
       isnan(w) | ...
       isnan(strata) | ...
       isnan(cluster);

n_miss = sum(miss);

if n_miss > 0 && opts.verbose
    warning('svy_ordered_probit:MissingData', ...
        'Se eliminaron %d observaciones con datos faltantes.', n_miss);
end

y       = y(~miss);
X       = X(~miss,:);
w       = w(~miss);
strata  = strata(~miss);
cluster = cluster(~miss);

[N, K] = size(X);

%% ------------------------------------------------------------------------
% Reordenar las categorías como 1,...,J
% -------------------------------------------------------------------------

categories = unique(y);
categories = sort(categories);

J = numel(categories);

if J < 3
    error(['El probit ordenado requiere al menos tres categorías. ', ...
           'Para dos categorías utilice svy_probit.']);
end

% Recodificación interna como 1,...,J
y_internal = zeros(N,1);

for j = 1:J
    y_internal(y == categories(j)) = j;
end

if any(y_internal == 0)
    error('No fue posible recodificar todas las categorías de y.');
end

%% ------------------------------------------------------------------------
%  Normalización opcional de pesos
% -------------------------------------------------------------------------

note = {};

switch lower(opts.weightNormalization)

    case 'none'
        % Sin modificación

    case 'mean1'
        factor = mean(w);

        if factor <= 0 || ~isfinite(factor)
            error('Los pesos no pueden normalizarse: media inválida.');
        end

        w = w ./ factor;
        note{end+1} = 'Pesos normalizados a media 1.';

    case 'sumn'
        factor = sum(w) / N;

        if factor <= 0 || ~isfinite(factor)
            error('Los pesos no pueden normalizarse: suma inválida.');
        end

        w = w ./ factor;
        note{end+1} = 'Pesos reescalados para sumar N.';

    otherwise
        error('Opción weightNormalization no reconocida.');

end

if any(w < 0)
    error('Los pesos no pueden ser negativos.');
end

%% ------------------------------------------------------------------------
%  Valores iniciales
% -------------------------------------------------------------------------
%
% Parametrización interna:
%
% theta = [beta;
%          a1;
%          d2;
%          ...
%          d_{J-1}]
%
% Los cortes se construyen como:
%
% kappa_1 = a1
% kappa_2 = kappa_1 + exp(d2)
% ...
%
% Esto garantiza:
%
% kappa_1 < kappa_2 < ... < kappa_{J-1}
% -------------------------------------------------------------------------

nCuts = J - 1;
nParams = K + nCuts;

if isfield(opts, 'theta0') && ~isempty(opts.theta0)

    theta0 = opts.theta0(:);

    if numel(theta0) ~= nParams
        error('opts.theta0 debe tener K + J - 1 elementos.');
    end

else

    beta0 = zeros(K,1);

    % Cortes iniciales basados en proporciones acumuladas ponderadas
    p_acum = zeros(nCuts,1);

    for j = 1:nCuts
        p_acum(j) = sum(w(y_internal <= j)) / sum(w);
    end

    % Evitar cuantiles exactamente 0 o 1
    p_acum = min(max(p_acum, 1e-4), 1 - 1e-4);

    cuts0 = norminv(p_acum);

    % Asegurar separación mínima
    for j = 2:nCuts
        if cuts0(j) <= cuts0(j-1)
            cuts0(j) = cuts0(j-1) + 0.25;
        end
    end

    cutParam0 = zeros(nCuts,1);
    cutParam0(1) = cuts0(1);

    for j = 2:nCuts
        cutParam0(j) = log(max(cuts0(j) - cuts0(j-1), 1e-4));
    end

    theta0 = [beta0; cutParam0];

end

%% ------------------------------------------------------------------------
%  Optimización
% -------------------------------------------------------------------------

displayOption = 'off';

if opts.verbose
    displayOption = 'iter';
end

optimOptions = optimoptions('fminunc', ...
    'Algorithm', 'quasi-newton', ...
    'Display', displayOption, ...
    'MaxIterations', opts.maxIter, ...
    'MaxFunctionEvaluations', max(5000, 100*nParams), ...
    'OptimalityTolerance', opts.tol, ...
    'StepTolerance', opts.tol, ...
    'FunctionTolerance', opts.tol, ...
    'SpecifyObjectiveGradient', true);

objective = @(theta) orderedProbitObjective( ...
    theta, y_internal, X, w, J);

[thetaHat, fval, exitflag, output, ~, hessian] = ...
    fminunc(objective, theta0, optimOptions);

converged = exitflag > 0;

if ~converged && opts.verbose
    warning('svy_ordered_probit:NoConverge', ...
        'El probit ordenado no convergió. exitflag = %d.', exitflag);

    note{end+1} = sprintf( ...
        'El optimizador no convergió. exitflag = %d.', exitflag);
end

%% ------------------------------------------------------------------------
%  Recuperar beta y puntos de corte
% -------------------------------------------------------------------------

betaHat = thetaHat(1:K);
cutParamHat = thetaHat(K+1:end);

[cutpoints, Jcuts] = transformCutpoints(cutParamHat);

%% ------------------------------------------------------------------------
%  Scores individuales
% -------------------------------------------------------------------------

[~, ~, scoreIndividual] = orderedProbitObjective( ...
    thetaHat, y_internal, X, w, J);

% scoreIndividual es N x nParams y ya contiene el peso w_i.

%% ------------------------------------------------------------------------
%  Bread
% -------------------------------------------------------------------------

A = (hessian + hessian') / 2;

% Comprobar rango
if rcond(A) < 1e-12
    warning('svy_ordered_probit:NearSingularHessian', ...
        'La matriz Hessiana está mal condicionada.');

    note{end+1} = ...
        'La matriz Hessiana está mal condicionada; revise colinealidad.';
end

%% ------------------------------------------------------------------------
%  Meat por estratos y conglomerados
% -------------------------------------------------------------------------

[strata_ids, ~, strata_idx] = unique(strata);

H = numel(strata_ids);
B = zeros(nParams, nParams);

G = 0;
onePSU_warn = 0;

for h = 1:H

    in_h = strata_idx == h;

    clusters_h = cluster(in_h);
    scores_h = scoreIndividual(in_h,:);

    [cluster_ids_h, ~, cluster_idx_h] = unique(clusters_h);

    Ch = numel(cluster_ids_h);

    if Ch <= 1
        onePSU_warn = onePSU_warn + 1;
        continue
    end

    Shc = zeros(nParams, Ch);

    for c = 1:Ch

        in_hc = cluster_idx_h == c;

        % Suma de scores individuales dentro de la UPM
        Shc(:,c) = sum(scores_h(in_hc,:), 1)';

    end

    sbar_h = mean(Shc, 2);

    Shc_centered = Shc - sbar_h;

    Bh = (Ch / (Ch - 1)) * ...
         (Shc_centered * Shc_centered');

    B = B + Bh;
    G = G + Ch;

end

if onePSU_warn > 0

    warning('svy_ordered_probit:OnePSUStrata', ...
        ['Se detectaron %d estratos con una sola UPM. ', ...
         'Estos estratos no contribuyen a la varianza.'], ...
         onePSU_warn);

    note{end+1} = sprintf( ...
        '%d estratos con una sola UPM no contribuyeron a B.', ...
        onePSU_warn);

end

%% ------------------------------------------------------------------------
%  Varianza sándwich en parametrización interna
% -------------------------------------------------------------------------

Ainv = pinv(A);

V_theta = Ainv * B * Ainv;
V_theta = (V_theta + V_theta') / 2;

%% ------------------------------------------------------------------------
%  Delta method para obtener VCOV de beta y cortes reales
% -------------------------------------------------------------------------
%
% psi = [beta; kappa]
%
% Gtransform = d psi / d theta
% -------------------------------------------------------------------------

Gtransform = zeros(nParams, nParams);

% Beta no cambia
Gtransform(1:K, 1:K) = eye(K);

% Jacobiano de los puntos de corte
Gtransform(K+1:end, K+1:end) = Jcuts;

V = Gtransform * V_theta * Gtransform';
V = (V + V') / 2;

se_all = sqrt(max(diag(V), 0));

se_beta = se_all(1:K);
se_cutpoints = se_all(K+1:end);

%% ------------------------------------------------------------------------
%  Inferencia
% -------------------------------------------------------------------------

dof = G - H;

if dof > 0

    z_beta = betaHat ./ se_beta;
    p_beta = 2 * tcdf(-abs(z_beta), dof);

    z_cutpoints = cutpoints ./ se_cutpoints;
    p_cutpoints = 2 * tcdf(-abs(z_cutpoints), dof);

else

    z_beta = betaHat ./ se_beta;
    p_beta = NaN(size(betaHat));

    z_cutpoints = cutpoints ./ se_cutpoints;
    p_cutpoints = NaN(size(cutpoints));

    warning('svy_ordered_probit:NonPositiveDF', ...
        'Los grados de libertad por diseño no son positivos: G-H = %d.', ...
        dof);

    note{end+1} = ...
        sprintf('Grados de libertad no positivos: G-H = %d.', dof);

end

%% ------------------------------------------------------------------------
%  Probabilidades predichas
% -------------------------------------------------------------------------

eta = X * betaHat;

p_hat = orderedProbitProbabilities(eta, cutpoints);

[~, y_hat_internal] = max(p_hat, [], 2);

y_hat = categories(y_hat_internal);

%% ------------------------------------------------------------------------
% 16. Log-verosimilitud
% -------------------------------------------------------------------------

logLik = -fval;

%% ------------------------------------------------------------------------
%  Salida
% -------------------------------------------------------------------------

out = struct();

out.beta          = betaHat;
out.cutpoints     = cutpoints;

out.se_beta       = se_beta;
out.se_cutpoints  = se_cutpoints;

out.V             = V;
out.V_theta       = V_theta;

out.z_beta        = z_beta;
out.p_beta        = p_beta;

out.z_cutpoints   = z_cutpoints;
out.p_cutpoints   = p_cutpoints;

out.logLik        = logLik;

out.theta         = thetaHat;
out.hessian       = A;
out.B             = B;

out.eta           = eta;
out.p_hat         = p_hat;
out.y_hat         = y_hat;

out.categories    = categories;

out.n             = N;
out.K             = K;
out.J             = J;
out.G             = G;
out.H             = H;
out.dof           = dof;

out.exitflag      = exitflag;
out.output        = output;
out.converged     = converged;

out.note          = note;

% Guardar la matriz de regresores utilizada después de eliminar missing
out.X = X;
out.y = y_internal;
out.w = w;


end


%% =========================================================================
% FUNCIÓN AUXILIAR: objetivo, gradiente y scores individuales
% =========================================================================

function [nll, gradient, scoreIndividual] = ...
    orderedProbitObjective(theta, y, X, w, J)

[N, K] = size(X);

nCuts = J - 1;

beta = theta(1:K);
cutParam = theta(K+1:end);

[cutpoints, Jcuts] = transformCutpoints(cutParam);

eta = X * beta;

% Bordes:
% kappa_0 = -Inf
% kappa_J = Inf
lowerCuts = [-Inf; cutpoints];
upperCuts = [cutpoints; Inf];

lower = lowerCuts(y) - eta;
upper = upperCuts(y) - eta;

PhiLower = normcdf(lower);
PhiUpper = normcdf(upper);

prob = PhiUpper - PhiLower;

% Protección numérica
epsProb = 1e-14;
prob = max(prob, epsProb);

logProb = log(prob);

% Log-verosimilitud negativa ponderada
nll = -sum(w .* logProb);

%% ------------------------------------------------------------------------
% Scores respecto de beta
% -------------------------------------------------------------------------

phiLower = normpdf(lower);
phiUpper = normpdf(upper);

% En ±Inf, normpdf devuelve 0, pero se fuerza por claridad
phiLower(~isfinite(lower)) = 0;
phiUpper(~isfinite(upper)) = 0;

% d log P / d beta
%
% P = Phi(upperCut - eta) - Phi(lowerCut - eta)
%
% dP/d beta = X * (phi(lower) - phi(upper))
scoreBetaScalar = (phiLower - phiUpper) ./ prob;

scoreBeta = X .* scoreBetaScalar;

%% ------------------------------------------------------------------------
% Scores respecto de los puntos de corte reales
% -------------------------------------------------------------------------

scoreCuts = zeros(N, nCuts);

for j = 1:J

    idx = y == j;

    if ~any(idx)
        continue
    end

    % El corte superior kappa_j aparece positivamente si j <= J-1
    if j <= nCuts
        scoreCuts(idx, j) = ...
            scoreCuts(idx, j) + phiUpper(idx) ./ prob(idx);
    end

    % El corte inferior kappa_{j-1} aparece negativamente si j >= 2
    if j >= 2
        scoreCuts(idx, j-1) = ...
            scoreCuts(idx, j-1) - phiLower(idx) ./ prob(idx);
    end

end

%% ------------------------------------------------------------------------
% Transformación de score de cortes a parámetros internos
% -------------------------------------------------------------------------

% scoreCutParam = scoreCuts * d(kappa)/d(cutParam)
scoreCutParam = scoreCuts * Jcuts;

% Score total sin peso
scoreUnweighted = [scoreBeta, scoreCutParam];

% Score individual ponderado
scoreIndividual = scoreUnweighted .* w;

% Gradiente de la log-verosimilitud negativa
gradient = -sum(scoreIndividual, 1)';

end


%% =========================================================================
% FUNCIÓN AUXILIAR: transformación de puntos de corte
% =========================================================================

function [cutpoints, Jcuts] = transformCutpoints(cutParam)

nCuts = numel(cutParam);

cutpoints = zeros(nCuts,1);

% Primer corte libre
cutpoints(1) = cutParam(1);

% Cortes posteriores mediante incrementos positivos
for j = 2:nCuts
    cutpoints(j) = cutpoints(j-1) + exp(cutParam(j));
end

% Jacobiano d(kappa)/d(cutParam)
Jcuts = zeros(nCuts, nCuts);

% Todos los cortes dependen del primer parámetro
Jcuts(:,1) = 1;

for j = 2:nCuts
    for m = 2:j
        Jcuts(j,m) = exp(cutParam(m));
    end
end

end


%% =========================================================================
% FUNCIÓN AUXILIAR: probabilidades predichas
% =========================================================================

function probabilities = orderedProbitProbabilities(eta, cutpoints)

N = numel(eta);
J = numel(cutpoints) + 1;

probabilities = zeros(N, J);

% Primera categoría
probabilities(:,1) = normcdf(cutpoints(1) - eta);

% Categorías intermedias
for j = 2:J-1

    probabilities(:,j) = ...
        normcdf(cutpoints(j) - eta) - ...
        normcdf(cutpoints(j-1) - eta);

end

% Última categoría
probabilities(:,J) = 1 - normcdf(cutpoints(end) - eta);

% Protección ante pequeños errores numéricos
probabilities = max(probabilities, 0);

rowSums = sum(probabilities, 2);

probabilities = probabilities ./ rowSums;


end