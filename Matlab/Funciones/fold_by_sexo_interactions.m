function [model_base, model_sex1, names_base, names_sex1] = ...
    fold_by_sexo_interactions(model_full, names_full, varargin)
% Genera dos "modelos" transformados (por L*beta) con las mismas filas:
%  - model_base: coeficientes base (sexo=0)
%  - model_sex1: SOLO combinaciones (base + inter) para variables con *_sexo
%
% Opciones:
%   'OmitRegex'               (char)  e.g., '^reg_' para ocultar dummies región
%   'IncludeInterceptCombined' (bool) incluir Intercepto+sexo en la col. 2 (default=false)

    p = inputParser;
    addParameter(p, 'OmitRegex', '', @ischar);
    addParameter(p, 'IncludeInterceptCombined', false, @islogical);
    parse(p, varargin{:});
    opt = p.Results;

    beta = model_full.beta(:);  V = model_full.V;  k = numel(beta);
    idx  = containers.Map(names_full, num2cell(1:numel(names_full)));

    isInteraction = @(nm) endsWith(nm,'_sexo') || startsWith(nm,'sexo_') || endsWith(nm,'*sexo');

    % ---- lista base (mismas filas para ambas columnas) ----
    names_base = {};
    for j = 1:numel(names_full)
        nm = names_full{j};
        if isInteraction(nm), continue; end             % omitir interacciones
        if strcmp(nm,'sexo'),  continue; end            % omitir término 'sexo'
        if ~isempty(opt.OmitRegex) && ~isempty(regexp(nm,opt.OmitRegex,'once')), continue; end
        names_base{end+1,1} = nm; %#ok<AGROW>
    end

    % ---- pares base<->interacción ----
    base2inter = containers.Map('KeyType','char','ValueType','int32');
    for j = 1:numel(names_full)
        nm = names_full{j};
        if endsWith(nm,'_sexo')
            base = extractBefore(nm,'_sexo');
        elseif startsWith(nm,'sexo_')
            base = extractAfter(nm,'sexo_');
        elseif endsWith(nm,'*sexo')
            base = extractBefore(nm,'*sexo');
        else
            continue;
        end
        if isKey(idx,base)
            base2inter(base) = j;
        end
    end

    % ---- L para columna base (identidad en variables de names_base) ----
    L0 = zeros(numel(names_base), k);
    for r = 1:numel(names_base)
        L0(r, idx(names_base{r})) = 1;
    end

    % ---- L para columna Men: solo combinaciones disponibles ----
    names_sex1 = {};
    L1_rows = {};
    for r = 1:numel(names_base)
        base = names_base{r};
        if strcmp(base,'Intercepto') && ~opt.IncludeInterceptCombined
            % no repetir constante en col. 2
            continue;
        end
        if isKey(base2inter, base)
            e = zeros(1,k);
            e(idx(base))       = 1;
            e(base2inter(base))= 1;
            L1_rows{end+1,1} = e; %#ok<AGROW>
            names_sex1{end+1,1} = base; %#ok<AGROW> % misma etiqueta de fila
        end
    end
    L1 = vertcat(L1_rows{:});

    % ---- construir modelos transformados ----
    model_base          = model_full;
    model_base.beta     = L0 * beta;
    model_base.V        = L0 * V * L0.';
    model_base.se       = sqrt(diag(model_base.V));
    model_base.t        = model_base.beta ./ model_base.se;

    model_sex1          = model_full;
    model_sex1.beta     = L1 * beta;
    model_sex1.V        = L1 * V * L1.';
    model_sex1.se       = sqrt(diag(model_sex1.V));
    model_sex1.t        = model_sex1.beta ./ model_sex1.se;
end
