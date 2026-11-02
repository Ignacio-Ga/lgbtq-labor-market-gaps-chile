function out = svy_ols(y, X, w, strata, cluster, opts)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ignacio Gálvez
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SVY_OLS: Regresión lineal ponderada con varianza por diseño (estratos y conglomerados).
%
%   out = svy_ols(y, X, w, strata, cluster)
%   out = svy_ols(y, X, w, strata, cluster, opts)
%
% ENTRADAS
%   y       : Nx1 vector de respuesta.
%   X       : NxK matriz de regresores (incluye intercepto si se desea).
%   w       : Nx1 pesos de expansión (p.ej., expr_osig).
%   strata  : Nx1 identificador de estrato (varstrat).
%   cluster : Nx1 identificador de conglomerado/UPM (varunit).
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
%   out.p         : Kx1 p-valores (t bilateral con dof = G - H)
%   out.dof       : grados de libertad
%   out.resid     : Nx1 residuos ponderados del ajuste (y - X*beta)
%   out.fitted    : Nx1 valores ajustados
%   out.A         : X'WX (información observada para MCO)
%   out.B         : "meat" de sandwich agregado por diseño
%   out.n, out.K  : dimensiones
%   out.G, out.H  : #PSU totales y #estratos utilizados
%   out.note      : mensajes informativos (estratos con 1 PSU, normalización, etc.)
%
% REQUISITOS
%   - Vectores columna de igual longitud N.
%   - Sin valores faltantes tras limpieza (la función eliminará filas con NaN).

    if nargin < 6, opts = struct; end
    if ~isfield(opts,'addIntercept'),        opts.addIntercept        = false; end
    if ~isfield(opts,'weightNormalization'), opts.weightNormalization = 'none'; end
    if ~isfield(opts,'verbose'),             opts.verbose             = true;  end

    % Asegurar columnas
    y       = y(:);
    w       = w(:);
    strata  = strata(:);
    cluster = cluster(:);

    % Añadir intercepto si se solicita
    if opts.addIntercept
        X = [ones(size(X,1),1), X];
    end

    % Chequeos básicos
    [N,K] = size(X);
    assert(length(y)==N && length(w)==N && length(strata)==N && length(cluster)==N, ...
        'Dimensiones incompatibles entre y, X, w, strata, cluster.');

    % Limpiar filas con NaN en cualquiera de las entradas
    miss = any([isnan(y), any(isnan(X),2), isnan(w), isnan(strata), isnan(cluster)], 2);
    n_miss = sum(miss);
    if n_miss>0 && opts.verbose
        warning('svy_ols:MissingData', 'Se eliminaron %d filas con NaN.', n_miss);
    end
    y = y(~miss); X = X(~miss,:); w = w(~miss); strata = strata(~miss); cluster = cluster(~miss);
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

    % Estimador WLS
    beta = A \ XWy;              % (X'WX)^(-1) X'Wy 
    fitted = X * beta;
    u = y - fitted;              % residuos

    % Construcción del "meat" por diseño: sum_h [ C_h/(C_h-1) * sum_c (s_hc - sbar_h)(...)' ]
    % donde s_hc = sum_{i in (h,c)} w_i * x_i * u_i  (vector Kx1)
    [strata_ids,~,strat_idx] = unique(strata);
    H = numel(strata_ids);
    B = zeros(K,K);
    G = 0; onePSU_warn = 0;
    
    for h = 1:H
        in_h = (strat_idx==h);
    
        % Agrupar por PSU en el estrato h
        clus_h = cluster(in_h);
        [clus_ids_h,~,clus_idx_h] = unique(clus_h);
    
        % Aportes por PSU
        Xh = X(in_h,:); uh = u(in_h); wh = w(in_h);
        Ch_all = numel(clus_ids_h);
    
        Shc = zeros(K, Ch_all);
        Wsum_hc = zeros(Ch_all,1);
    
        for c = 1:Ch_all
            in_hc = (clus_idx_h==c);
            whc   = wh(in_hc);
            Xhc   = Xh(in_hc,:); uhc = uh(in_hc);
    
            Wsum_hc(c) = sum(whc);                       % peso total del PSU
            Shc(:,c)   = (Xhc' * (whc .* uhc));          % s_hc = ∑ w x u
        end
    
        % === (CLAVE) Usar sólo PSUs activos (peso total > 0) ===
        active = (Wsum_hc > 0);
        Shc = Shc(:, active);
        Ch  = size(Shc,2);
    
        if Ch <= 1
            onePSU_warn = onePSU_warn + 1;
            continue
        end
    
        sbar_h = mean(Shc, 2);                                 % promedio entre PSUs activos
        Shc_centered = Shc - sbar_h;
        Bh = (Shc_centered * Shc_centered.') * (Ch/(Ch-1));    % corrección finita intra-estrato
    
        B = B + Bh;
        G = G + Ch;                                            % df: total de PSUs ACTIVOS
    end


    if onePSU_warn>0 && opts.verbose
        warning('svy_ols:OnePSUStrata', ...
            'Se detectaron %d estrato(s) con un solo PSU (no aportan a la varianza).', onePSU_warn);
        note{end+1} = sprintf('%d estrato(s) con un solo PSU no contribuyeron a B.', onePSU_warn);
    end

    % Varianza tipo "sandwich"
    Ainv = inv(A);
    V = Ainv * B * Ainv;

    % Errores estándar, t, p con dof = G - H 
    dof = G - H;
    if dof <= 0
        % Respaldo conservador si no hay df positivos
        if opts.verbose
            warning('svy_ols:NonPositiveDF', ...
                'dof no positivo (G-H=%d). Se usarán p-valores NA.', dof);
        end
        se = sqrt(max(diag(V),0));
        tstat = beta ./ se;
        pval = nan(size(beta));
    else
        se = sqrt(max(diag(V),0));
        tstat = beta ./ se;
        % p-valor bilateral
        pval = 2 * tcdf(-abs(tstat), dof);
    end

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

    % Ajustado usando N y p (nota: en diseños complejos es una aproximación usual)
    N_eff = N;                              % puedes cambiarlo por sum(w>0) si lo prefieres
    df_tot = N_eff - double(has_intercept); % gl de SST
    df_res = N_eff - p;                     % gl residuales "de conteo"
    if df_res > 0 && df_tot > 0 && SST > 0
        r2_adj = 1 - (SSR/df_res) / (SST/df_tot);
    else
        r2_adj = NaN;
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
    out.G      = G;
    out.H      = H;
    out.note   = note;
    out.r2          = r2;
    out.r2_adj      = r2_adj;
    out.has_intercept = has_intercept;
    out.q           = q;                 % #restricciones en el test global
    out.F = struct('F', Fstat, 'df1', q, 'df2', dof, 'p', pF);
end
