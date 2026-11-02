function T = export_table(res, varnames, opts)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ignacio Gálvez
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXPORT_TABLE
%
% Resume coeficientes, errores estándar, estadísticos y p-valores.
%
% Compatible con:
%
%   1. MCO:
%      res.beta
%      res.se
%      res.t
%      res.p
%
%   2. Probit binario:
%      res.beta
%      res.se
%      res.z
%      res.p
%
%   3. Probit ordenado:
%      res.beta
%      res.se_beta
%      res.z_beta
%      res.p_beta
%      res.cutpoints
%      res.se_cutpoints
%      res.z_cutpoints
%      res.p_cutpoints
%
% USO:
%
%   T = export_table(res, varnames);
%
%   T = export_table(res, varnames, opts);
%
% ENTRADAS:
%
%   res      : estructura de resultados.
%
%   varnames : nombres de los regresores.
%              En probit ordenado deben corresponder únicamente a beta,
%              sin incluir los puntos de corte.
%
%   opts     : estructura opcional:
%
%       .digitsCoef       decimales coeficientes       default = 4
%       .digitsSE         decimales errores estándar   default = 4
%       .digitsT          decimales estadístico        default = 3
%       .digitsP          decimales p-valor            default = 4
%       .useBooktabs      opción reservada             default = true
%       .caption          título                       default = ''
%       .label            etiqueta                     default = ''
%       .printToScreen    mostrar resultados           default = true
%       .includeCutpoints incluir cortes en oprobit     default = true
%       .omit_idx         índices de filas a omitir
%       .omit_regex       patrón de nombres a omitir
%
% SALIDA:
%
%   T : tabla MATLAB con resultados.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% ------------------------------------------------------------------------
% 1. Opciones
% -------------------------------------------------------------------------

if nargin < 3
    opts = struct();
end

if ~isfield(opts,'digitsCoef')
    opts.digitsCoef = 4;
end

if ~isfield(opts,'digitsSE')
    opts.digitsSE = 4;
end

if ~isfield(opts,'digitsT')
    opts.digitsT = 3;
end

if ~isfield(opts,'digitsP')
    opts.digitsP = 4;
end

if ~isfield(opts,'useBooktabs')
    opts.useBooktabs = true;
end

if ~isfield(opts,'caption')
    opts.caption = '';
end

if ~isfield(opts,'label')
    opts.label = '';
end

if ~isfield(opts,'printToScreen')
    opts.printToScreen = true;
end

if ~isfield(opts,'includeCutpoints')
    opts.includeCutpoints = true;
end

%% ------------------------------------------------------------------------
% 2. Detectar tipo de modelo
% -------------------------------------------------------------------------

esOrderedProbit = ...
    isfield(res,'se_beta') && ...
    isfield(res,'z_beta') && ...
    isfield(res,'p_beta');

%% ------------------------------------------------------------------------
% 3. Extraer resultados
% -------------------------------------------------------------------------

if esOrderedProbit

    % Coeficientes del probit ordenado
    beta = res.beta(:);
    se   = res.se_beta(:);
    stat = res.z_beta(:);
    pval = res.p_beta(:);

    Kbeta = numel(beta);

    % Nombres de regresores
    if nargin < 2 || isempty(varnames)

        varnames = arrayfun( ...
            @(k) sprintf('x%d',k), ...
            (1:Kbeta)', ...
            'UniformOutput',false);

    else

        assert(numel(varnames) == Kbeta, ...
            ['En el probit ordenado, varnames debe contener solamente ', ...
             'los nombres de los regresores y tener longitud K.']);

        varnames = varnames(:);

    end

    % Incluir puntos de corte
    if opts.includeCutpoints

        camposCortes = { ...
            'cutpoints', ...
            'se_cutpoints', ...
            'z_cutpoints', ...
            'p_cutpoints'};

        if all(isfield(res, camposCortes))

            cuts    = res.cutpoints(:);
            seCuts  = res.se_cutpoints(:);
            zCuts   = res.z_cutpoints(:);
            pCuts   = res.p_cutpoints(:);

            nCuts = numel(cuts);

            nombresCortes = arrayfun( ...
                @(j) sprintf('Corte %d',j), ...
                (1:nCuts)', ...
                'UniformOutput',false);

            beta = [beta; cuts];
            se   = [se; seCuts];
            stat = [stat; zCuts];
            pval = [pval; pCuts];

            varnames = [varnames; nombresCortes];

        else

            warning('export_table:MissingCutpoints', ...
                ['Se solicitó incluir puntos de corte, pero faltan ', ...
                 'campos en la estructura de resultados.']);

        end

    end

    nombreEstadistico = 'z';

else

    % Modelos estándar: MCO o probit binario
    assert(isfield(res,'beta'), ...
        'La estructura res debe contener el campo beta.');

    assert(isfield(res,'se'), ...
        'La estructura res debe contener el campo se.');

    assert(isfield(res,'p'), ...
        'La estructura res debe contener el campo p.');

    beta = res.beta(:);
    se   = res.se(:);
    pval = res.p(:);

    if isfield(res,'t')

        stat = res.t(:);
        nombreEstadistico = 't';

    elseif isfield(res,'z')

        stat = res.z(:);
        nombreEstadistico = 'z';

    else

        error(['La estructura res debe contener un campo t o z ', ...
               'para el estadístico de prueba.']);

    end

    K = numel(beta);

    % Nombres de variables
    if nargin < 2 || isempty(varnames)

        varnames = arrayfun( ...
            @(k) sprintf('x%d',k), ...
            (1:K)', ...
            'UniformOutput',false);

    else

        assert(numel(varnames) == K, ...
            'varnames debe tener longitud K.');

        varnames = varnames(:);

    end

end

%% ------------------------------------------------------------------------
% 4. Comprobar dimensiones
% -------------------------------------------------------------------------

Ktotal = numel(beta);

assert(numel(se) == Ktotal, ...
    'beta y se tienen dimensiones incompatibles.');

assert(numel(stat) == Ktotal, ...
    'beta y el estadístico tienen dimensiones incompatibles.');

assert(numel(pval) == Ktotal, ...
    'beta y p tienen dimensiones incompatibles.');

assert(numel(varnames) == Ktotal, ...
    'El número de nombres no coincide con el número de resultados.');

%% ------------------------------------------------------------------------
% 5. Símbolos de significancia
% -------------------------------------------------------------------------

Sig = arrayfun( ...
    @(p) pstars(p), ...
    pval, ...
    'UniformOutput',false);

%% ------------------------------------------------------------------------
% 6. Redondeo
% -------------------------------------------------------------------------

fmtC = sprintf('%%.%df', opts.digitsCoef);
fmtS = sprintf('%%.%df', opts.digitsSE);
fmtT = sprintf('%%.%df', opts.digitsT);
fmtP = sprintf('%%.%df', opts.digitsP);

Coef = str2double(compose(fmtC, beta));
SE   = str2double(compose(fmtS, se));
Stat = str2double(compose(fmtT, stat));
Pcol = str2double(compose(fmtP, pval));

%% ------------------------------------------------------------------------
% 7. Construir tabla
% -------------------------------------------------------------------------

if strcmp(nombreEstadistico,'t')

    T = table( ...
        varnames, ...
        Coef, ...
        SE, ...
        Stat, ...
        Pcol, ...
        Sig, ...
        'VariableNames', ...
        {'Variable','Coef','SE','t','p','Sig'});

else

    T = table( ...
        varnames, ...
        Coef, ...
        SE, ...
        Stat, ...
        Pcol, ...
        Sig, ...
        'VariableNames', ...
        {'Variable','Coef','SE','z','p','Sig'});

end

%% ------------------------------------------------------------------------
% 8. Omitir filas por índice
% -------------------------------------------------------------------------

if isfield(opts,'omit_idx') && ~isempty(opts.omit_idx)

    omit = false(height(T),1);

    if islogical(opts.omit_idx)

        assert(numel(opts.omit_idx) == height(T), ...
            'omit_idx lógico debe tener la misma longitud que la tabla.');

        omit = opts.omit_idx(:);

    else

        idxOmit = opts.omit_idx(:);

        assert(all(idxOmit >= 1 & idxOmit <= height(T)), ...
            'omit_idx contiene índices fuera de rango.');

        omit(idxOmit) = true;

    end

    T = T(~omit,:);

end

%% ------------------------------------------------------------------------
% 9. Omitir filas por patrón
% -------------------------------------------------------------------------

if isfield(opts,'omit_regex') && ~isempty(opts.omit_regex)

    nombresString = string(T.Variable);

    mask = ~cellfun( ...
        @isempty, ...
        regexp(cellstr(nombresString), opts.omit_regex, 'once'));

    T = T(~mask,:);

end

%% ------------------------------------------------------------------------
% 10. Mostrar resultados
% -------------------------------------------------------------------------

if opts.printToScreen

    fprintf('\n====================================\n');
    fprintf('           %s\n', upper(opts.caption));
    fprintf('====================================\n\n');

    disp(T);

    fprintf( ...
        'Signif.:  . p<0.10   * p<0.05   ** p<0.01   *** p<0.001\n');

    %% Estadísticos globales

    if isfield(res,'r2') || ...
       isfield(res,'F') || ...
       isfield(res,'logLik')

        fprintf('\n--- Estadísticos del modelo ---\n');

        if isfield(res,'r2')
            fprintf('R^2            : %.4f\n', res.r2);
        end

        if isfield(res,'r2_adj')
            fprintf('R^2 ajustado   : %.4f\n', res.r2_adj);
        end

        if isfield(res,'F') && ...
           isstruct(res.F) && ...
           all(isfield(res.F, {'F','df1','df2','p'}))

            fprintf( ...
                'F(%d, %d)       : %.3f   p = %.4g\n', ...
                res.F.df1, ...
                res.F.df2, ...
                res.F.F, ...
                res.F.p);

        end

        if isfield(res,'logLik')
            fprintf('Log-verosimilitud: %.4f\n', res.logLik);
        end

        if isfield(res,'n')
            fprintf('Observaciones   : %d\n', res.n);
        end

        if isfield(res,'J')
            fprintf('Categorías      : %d\n', res.J);
        end

        if isfield(res,'dof')
            fprintf('Grados libertad : %d\n', res.dof);
        end

    end

    if isfield(opts,'label') && ~isempty(opts.label)
        fprintf('\n%s\n', opts.label);
    end

end

end


%% =========================================================================
% Helper: símbolos de significancia
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