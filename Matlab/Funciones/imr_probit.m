function out = imr_probit(res, opts)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ignacio Gálvez
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IMR_PROBIT: calcula el ratio inverso de Mills a partir de svy_probit.
%
%   out = svy_imr_probit(res)
%   out = svy_imr_probit(res, opts)
%
% ENTRADA:
%   res  : estructura devuelta por svy_probit, debe contener:
%           .eta   (Nx1)  = X*beta
%           .p_hat (Nx1)  = Phi(eta)
%         (si no están, se recalcularán a partir de res.beta y res.X, si existieran)
%   opts : struct opcional con campos:
%           .eps (double, default = 1e-12)   -> recorte para estabilidad numérica
%           .return ('both'|'lambda1'|'lambda0', default = 'both')
%
% SALIDA:
%   out : struct con
%           .lambda1 = phi(eta) ./ Phi(eta)           (N x 1)
%           .lambda0 = phi(eta) ./ (1 - Phi(eta))     (N x 1)
%           .eta     = predictor lineal
%           .mu      = Phi(eta)
%           .phi     = phi(eta)

    if nargin < 2, opts = struct; end
    if ~isfield(opts,'eps'),     opts.eps = 1e-14; end
    if ~isfield(opts,'return'),  opts.return = 'both'; end

    % Obtener eta y mu (Phi(eta))
    if isfield(res,'eta') && isfield(res,'p_hat')
        eta = res.eta(:);
        mu  = res.p_hat(:);
    else
        error(['svy_imr_probit: el objeto res no tiene .eta/.p_hat. ', ...
               'Asegúrate de llamar svy_probit con salidas completas.']);
    end

    % Densidad normal estándar
    phi = normpdf(eta);

    % Recortes para evitar división por cero
    mu  = min(max(mu,  opts.eps), 1 - opts.eps);
    phi = max(phi, opts.eps);

    % IMR para y=1 y y=0
    lambda1 = phi ./ mu;           % phi(eta)/Phi(eta)
    lambda0 = phi ./ (1 - mu);     % phi(eta)/(1-Phi(eta))

    % Si se quiere devolver solo una
    switch lower(opts.return)
        case 'lambda1'
            lambda0_out = [];
        case 'lambda0'
            lambda1_out = [];
        case 'both'
            % devolver ambas
        otherwise
            error('opts.return debe ser ''both'', ''lambda1'' o ''lambda0''.');
    end

    % Armar salida
    out = struct();
    if exist('lambda1_out','var') 
        out.lambda1 = lambda1_out;
    else
        out.lambda1 = lambda1;
    end
    if exist('lambda0_out','var') 
        out.lambda0 = lambda0_out;
    else
        out.lambda0 = lambda0;
    end
    out.eta = eta;
    out.mu  = mu;
    out.phi = phi;
end
