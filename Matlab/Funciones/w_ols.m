function out = w_ols(y, X, w, opts)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ignacio Gálvez
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SVY_OLS: Regresión lineal ponderada .
%
%   out = svy_ols(y, X, w)
%   out = svy_ols(y, X, w, opts)
%
% ENTRADAS
%   y       : Nx1 vector de respuesta.
%   X       : NxK matriz de regresores (incluye intercepto si se desea).
%   w       : Nx1 pesos de expansión (p.ej., expr_osig).
%   opts    : struct opcional con campos:
%               .addIntercept (bool, default=false)
%               .weightNormalization ('none'|'mean1'|'sumN', default='none')
%               .verbose (bool, default=true)
%
% SALIDAS (estructura)
%   out.beta      : Kx1 coeficientes
%   out.V         : KxK matriz de varianzas-covarianzas (sandwich por diseño)
%   out.se        : Kx1 errores estándar
%   out.t         : Kx1 estadísticos t
%   out.p         : Kx1 p-valores (t bilateral con dof = N - K)
%   out.dof       : grados de libertad
%   out.resid     : Nx1 residuos ponderados del ajuste (y - X*beta)
%   out.fitted    : Nx1 valores ajustados
%   out.A         : X'WX (información observada para MCO)
%   out.B         : "meat" de sandwich agregado por diseño
%   out.note      : mensajes informativos 
%
% REQUISITOS
%   - Vectores columna de igual longitud N.
%   - Sin valores faltantes tras limpieza (la función eliminará filas con NaN).

    if nargin < 4, opts = struct; end
    if ~isfield(opts,'addIntercept'),        opts.addIntercept        = false; end
    if ~isfield(opts,'weightNormalization'), opts.weightNormalization = 'none'; end
    if ~isfield(opts,'verbose'),             opts.verbose             = true;  end

    % Asegurar columnas
    y       = y(:);
    w       = w(:);

    % Añadir intercepto si se solicita
    if opts.addIntercept
        X = [ones(size(X,1),1), X];
    end

    % Chequeos básicos
    [N,K] = size(X);
    assert(length(y)==N && length(w)==N, ...
        'Dimensiones incompatibles entre y, X, w, strata, cluster.');

    % Limpiar filas con NaN en cualquiera de las entradas
    miss = any([isnan(y), any(isnan(X),2), isnan(w)], 2);
    n_miss = sum(miss);
    if n_miss>0 && opts.verbose
        warning('svy_ols:MissingData', 'Se eliminaron %d filas con NaN.', n_miss);
    end
    y = y(~miss); X = X(~miss,:); w = w(~miss);
    [N,K] = size(X);

    % Normalización de pesos (opcional; por defecto none para mantener expansión)
    note = {};
    switch lower(opts.weightNormalization)
        case 'mean1'
            sf = mean(w);
            w  = w / sf;
            note{end+1} = 'Pesos normalizados a media 1 (global).';
        case 'sumn'
            sf = sum(w)/N;
            w  = w / sf;
            note{end+1} = 'Pesos re-escalados para sumar N (global).';
        case 'none'
            % do nothing
        otherwise
            error('Opción weightNormalization no reconocida.');
    end

    % Matrices básicas
    WX = X .* w;                 % Cada columna de X multiplicada por w (elementwise por filas)
    A  = X' * WX;                % X' W X
    Wy = w .* y;
    XWy = X' * Wy;
    K   = size(X,2);

    % --- Criterio de singularidad 
    % Caso regular 
    beta = A \ XWy;         % (X'WX)^(-1) X'Wy 
    fitted = X * beta;
    u = y - fitted;              % residuos

    %% Calculo de varianza
    Z = X .* (w .* u);          % n x K, cada fila es (w_i u_i) * x_i'
    B = Z' * Z;                    % K x K
    k = K;
    n  = size(X,1);
    B = (n/(n - k)) * B;
    % Varianza tipo "sandwich"
    V = A \ B / A';
        se = sqrt(max(diag(V),0));
        tstat = beta ./ se;
        % p-valor bilateral
        dof = N-K;
        pval = 2 * tcdf(-abs(tstat), dof);
    

        % --- Indicador de intercepto (true si la 1a columna de X es todo 1)
    has_intercept = all(abs(X(:,1) - 1) < 1e-12);
    p = K;                             % número de parámetros estimados
    q = p - double(has_intercept);     % # de restricciones para test global (excluye intercepto)

    % --- R^2 ponderado y R^2 ajustado (definición WLS con media ponderada)
    wsum   = sum(w);
    ybar_w = sum(w .* y) / wsum;
    SST    = sum(w .* (y - ybar_w).^2);     % total (alrededor de la media ponderada)
    SSR    = sum(w .* u.^2);                % suma de residuos ponderada
    if SST > 0
        r2 = 1 - SSR / SST;
    else
        r2 = NaN;
    end



      % --- Estadístico F (Wald de diseño) para H0: betas (sin intercepto) = 0
    if q > 0 && dof > 0
        % Matriz R: selecciona columnas no-intercepto
        if has_intercept
            R = [zeros(q,1), eye(q)];   % asume intercepto en la 1a columna
        else
            R = eye(q);                  % sin intercepto: prueba todos los coeficientes
        end
        wald_num = (R*beta);
        VR = R*V*R.';
        % uso pinv por estabilidad numérica
        W = wald_num' * pinv(VR) * wald_num;  % estadístico Wald (chi^2_q)
        Fstat = W / q;                         % F(q, dof) aproximado
        pF = 1 - fcdf(Fstat, q, dof);
    else
        Fstat = NaN; pF = NaN;
    end

    

    % Salidas
    out = struct();
    out.beta   = beta;
    out.V      = V;
    out.se     = se;
    out.t      = tstat;
    out.p      = pval;
    out.dof    = dof;
    out.resid  = u;
    out.fitted = fitted;
    out.A      = A;
    out.B      = B;
    out.n      = N;
    out.K      = K;
    out.note   = note;
    out.r2          = r2;
    out.has_intercept = has_intercept;
    out.q           = q;                 % #restricciones en el test global
    out.F = struct('F', Fstat, 'df1', q, 'df2', dof, 'p', pF);
end
