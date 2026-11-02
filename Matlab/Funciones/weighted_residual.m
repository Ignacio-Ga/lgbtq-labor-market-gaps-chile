function z_res = weighted_residual(z, X, w)
% Devuelve el residuo de proyectar z en X con pesos w (normalizados).
% z: vector Nx1 
% X: matriz NxK  
% w: vector Nx1  
    z   = z(:); w = w(:);
    % Filtra filas completas
    ok = all(~isnan([z, w, X]), 2);
    z_ok = z(ok); X_ok = X(ok,:); w_ok = w(ok);
    % Normaliza pesos para estabilidad
    w_ok = w_ok ./ mean(w_ok, 'omitnan');
    % Bread ponderada
    W = diag(w_ok);
    % Coefs de la proyección
    b = (X_ok' * W * X_ok) \ (X_ok' * W * z_ok);
    % Residuo en todo el N (NA donde faltan datos)
    z_hat      = nan(size(z));
    z_hat(ok)  = X_ok * b;
    z_res      = z - z_hat;
end
