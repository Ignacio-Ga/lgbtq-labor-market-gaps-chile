function out = w_probit(y, X, w, opts)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ignacio Gálvez
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SVY_PROBIT: Probit ponderado (WML)).
%
%   out = w_probit(y, X, w)
%   out = w_probit(y, X, w, opts)
%
% ENTRADAS
%   y       : Nx1 binaria (0/1)
%   X       : NxK regresores (sin intercepto si opts.addIntercept=true)
%   w       : Nx1 pesos de expansión (p.ej. expr_osig)
%   opts    : struct opcional
%               .addIntercept (bool, default=true)   -> añade columna de 1s
%               .weightNormalization ('none'|'mean1'|'sumN', default='none')
%               .maxIter (int, default=100)
%               .tol    (double, default=1e-8)
%               .verbose (bool, default=true)
%               .beta0   (Kx1 inicial, default = zeros)
%
% SALIDAS
%   out.beta      : Kx1 coeficientes (Probit)
%   out.se        : Kx1 EE por diseño
%   out.V         : KxK VCOV (sandwich por diseño)
%   out.z         : Kx1 estadísticos t (usando df = G-H)
%   out.p         : Kx1 p-valores bilaterales
%   out.dof       : grados de libertad (N - K)
%   out.iter      : iteraciones IRLS realizadas
%   out.converged : bool
%   out.eta       : Nx1 predictor lineal X*beta
%   out.p_hat     : Nx1 probabilidad estimada Phi(eta)
%   out.resid     : Nx1 residuo (y - p_hat)
%   out.A         : KxK "bread" (Fisher information) = X' diag(w_i*Wi) X
%   out.B         : KxK "meat" agregado por diseño
%   out.n,out.K : tamaños
%   out.note      : celdas con notas/advertencias
%
% TEORÍA 
%   - LogL ponderada: sum_i w_i [ y_i log Phi(eta_i) + (1-y_i) log (1-Phi(eta_i)) ]
%   - IRLS (Fisher scoring):
%       mu_i = Phi(eta_i),  phi_i = N(0,1) pdf(eta_i)
%       W_i  = (d mu/d eta)^2 / Var(Y|X) = phi_i^2 / (mu_i (1-mu_i))
%       z_i  = eta_i + (y_i - mu_i) / phi_i
%       beta^{new} = (X' diag(w_i W_i) X)^{-1} X' diag(w_i W_i) z
%   - Varianza por diseño (Binder/sandwich):
%       A = X' diag(w_i W_i) X
%       B = Z' * Z;  
%       V = A^{-1} B A^{-1},  df = N - K
%


    if nargin < 4, opts = struct; end
    if ~isfield(opts,'addIntercept'),        opts.addIntercept        = true;  end
    if ~isfield(opts,'weightNormalization'), opts.weightNormalization = 'none'; end
    if ~isfield(opts,'maxIter'),             opts.maxIter             = 100;   end
    if ~isfield(opts,'tol'),                 opts.tol                 = 1e-8;   end
    if ~isfield(opts,'verbose'),             opts.verbose             = true;   end

    % Forzar columnas
    y       = y(:);
    w       = w(:);

    % Añadir intercepto si procede
    if opts.addIntercept
        X = [ones(size(X,1),1), X];
    end

    % Dimensiones y limpieza de NaN
    [N,K] = size(X);
    assert(length(y)==N && length(w)==N, ...
        'Dimensiones incompatibles entre y, X, w.');
    n = N;

    miss = any([isnan(y), any(isnan(X),2), isnan(w)], 2);
    n_miss = sum(miss);
    if n_miss>0 && opts.verbose
        warning('svy_probit:MissingData','Se eliminaron %d filas con NaN.', n_miss);
    end
    y = y(~miss); X = X(~miss,:); w = w(~miss);
    [N,K] = size(X);

    % Normalización de pesos (opcional)
    note = {};
    switch lower(opts.weightNormalization)
        case 'mean1'
            sf = mean(w); w = w / sf;
            note{end+1} = 'Pesos normalizados a media 1.';
        case 'sumn'
            sf = sum(w)/N; w = w / sf;
            note{end+1} = 'Pesos reescalados para sumar N.';
        case 'none'
            % nada
        otherwise
            error('Opción weightNormalization no reconocida.');
    end

    % Inicialización
    if isfield(opts,'beta0') && ~isempty(opts.beta0)
        beta = opts.beta0(:);
        if numel(beta) ~= K, error('beta0 debe tener dimensión K.'); end
    else
        beta = zeros(K,1);
    end

    converged = false;
    iter = 0;

    % --- IRLS (Fisher scoring) ---
    while iter < opts.maxIter
        iter = iter + 1;

        eta = X * beta;
        mu  = normcdf(eta);        % Phi(eta)
        phi = normpdf(eta);        % phi(eta)

        % Evitar extremos exactos (estabilidad numérica)
        epsmu = 1e-12;
        mu = min(max(mu, epsmu), 1 - epsmu);
        phi = max(phi, epsmu);

        % Working weights (GLM) y working response
        Wi = (phi.^2) ./ (mu .* (1 - mu));   % Kallenberg/GLM Probit
        zi = eta + (y - mu) ./ phi;

        % Resolver paso IRLS ponderado por w_i
        WIRLS = w .* Wi;                    % pesos efectivos
        XW = X .* WIRLS;                    % X .* (w_i*Wi) por filas
        A  = X' * XW;                       % bread provisional (Fisher info)
        XWz = X' * (WIRLS .* zi);

        % Paso de Newton/Fisher
        delta = A \ (XWz - A*beta);         % equivalente a (X' W X)^{-1} X' W (z - X beta)
        beta_new = beta + delta;

        % Criterio de convergencia
        if norm(delta, Inf) < opts.tol * max(1, norm(beta, Inf))
            beta = beta_new;
            converged = true;
            break
        end

        beta = beta_new;
    end

    if ~converged && opts.verbose
        warning('svy_probit:NoConverge','IRLS no convergió en %d iteraciones.', opts.maxIter);
        note{end+1} = sprintf('IRLS no convergió en %d iteraciones.', opts.maxIter);
    end

    % Estimaciones finales
    eta = X * beta;
    mu  = normcdf(eta);
    phi = normpdf(eta);
    mu  = min(max(mu, epsmu), 1 - epsmu);
    phi = max(phi, epsmu);

    % "Bread" A = X' diag(w_i * Wi) X con Wi en el punto final
    Wi = (phi.^2) ./ (mu .* (1 - mu));
    WIRLS = w .* Wi;
    A = X' * (X .* WIRLS);

    % Score individual (vector Kx1 por obs):
    % u_i = w_i * x_i * (y_i - mu_i) * lambda_i,  lambda_i = phi_i / (mu_i*(1-mu_i))
    tiny = 1e-12;
    denom = mu .* (1 - mu);
    resid  = (y - mu);
    s = w .* phi .* resid ./ max(denom, tiny);  % n x 1
    Z = X .* s;                               % n x K: g_i = x_i * s_i
    B = Z' * Z;                               % K x K
    B = (n/(n - K)) * B;


    % Varianza sandwich por diseño
    Ainv = inv(A);
    V = Ainv * B * Ainv;

    % EE 
    dof = N - K;
    se = sqrt(max(diag(V),0));
    if dof > 0
        zstat = beta ./ se;                   % "t" con df = N-K
        pval  = 2 * tcdf(-abs(zstat), dof);
    else
        zstat = beta ./ se;
        pval  = nan(size(zstat));
        if opts.verbose
            warning('svy_probit:NonPositiveDF','Grados de libertad no positivos (N-K=%d).', dof);
        end
    end

    % Salida
    out = struct();
    out.beta      = beta;
    out.se        = se;
    out.V         = V;
    out.z         = zstat;
    out.p         = pval;
    out.dof       = dof;
    out.iter      = iter;
    out.converged = converged;
    out.eta       = eta;
    out.p_hat     = mu;
    out.resid     = y - mu;
    out.A         = A;
    out.B         = B;
    out.n         = N;
    out.K         = K;
    out.note      = note;
end
