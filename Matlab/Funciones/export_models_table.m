function latex_code = export_models_table(models_in, layout, outpath, opts)
% EXPORT_MODELS_TABLE
% Exporta tablas LaTeX (tipo "stargazer") para uno o varios modelos.
% Devuelve el código LaTeX como string y opcionalmente escribe a 'outpath'.
%
% USO:
%   latex_code = export_models_table(models, layout, 'tabla.tex', opts);
%   latex_code = export_models_table({modelsA, modelsB}, layoutAB, 'AT1.tex', opts);

    if nargin < 4, opts = struct; end
    if ~isfield(opts,'digitsC'),  opts.digitsC = 2; end
    if ~isfield(opts,'digitsSE'), opts.digitsSE = 2; end
    if ~isfield(opts,'useBooktabs'), opts.useBooktabs = true; end
    if ~isfield(opts,'caption'), opts.caption = ''; end
    if ~isfield(opts,'label'),   opts.label = ''; end
    if ~isfield(opts,'notes'),   opts.notes = {}; end
    if ~isfield(opts,'fontsize'),opts.fontsize = 'footnotesize'; end
    if ~isfield(opts,'addThreePartTable'), opts.addThreePartTable = true; end
    if ~isfield(opts,'captionsetup'), opts.captionsetup = ''; end
    if ~isfield(opts,'varnames_by_model'), opts.varnames_by_model = {}; end
    if ~isfield(opts,'varnames_by_model_by_panel'), opts.varnames_by_model_by_panel = {}; end
    % --- NUEVO: opciones para ocultar filas ---
    if ~isfield(opts,'omit_regex'), opts.omit_regex = ''; end
    if ~isfield(opts,'omit_keys'),  opts.omit_keys  = {}; end

    if nargin < 3 || isempty(outpath), outpath = ''; end

    % Detectar modo: simple vs multi-panel
    isMultipanel = iscell(models_in) && ~isempty(models_in) && iscell(models_in{1});
    if isMultipanel
        models_by_panel = models_in;
        P = numel(models_by_panel);
        assert(isfield(layout,'panels') && numel(layout.panels)==P, ...
               'layout.panels debe tener una entrada por panel.');
        M = numel(models_by_panel{1});
    else
        models_by_panel = {models_in};
        P = 1;
        M = numel(models_in);
        assert(isfield(layout,'panels') && numel(layout.panels)==1, ...
               'Para tabla simple, layout.panels debe tener un solo panel.');
    end

    % Separadores por grupos de columnas
    groupSpacersAfter = false(1,M);
    if isfield(layout,'col_groups') && ~isempty(layout.col_groups)
        for g = 1:numel(layout.col_groups)-1
            lastcol = max(layout.col_groups(g).cols);
            groupSpacersAfter(lastcol) = true;
        end
    end
    colPos = zeros(1,M); pos = 0;
    for m=1:M
        pos = pos + 1; colPos(m) = pos;
        if groupSpacersAfter(m), pos = pos + 1; end
    end
    totalC = pos;

    % Colspec
    colspec = 'l';
    for c=1:totalC, colspec = [colspec, 'c']; end 

    % ==== buffer de líneas
    L = strings(0,1);
    app = @(s) assignin('caller','L',[evalin('caller','L'); string(s)]); 

    app('% Tabla generada por export_models_table (MATLAB)');
    app('\begin{table}[H]');
    app('\begin{center}');
    if opts.addThreePartTable, app('\begin{threeparttable}'); end
    if ~isempty(opts.captionsetup), app(opts.captionsetup); end
    if ~isempty(opts.caption),      app(['\caption{', opts.caption, '}']); end
    if ~isempty(opts.label),        app(['\label{',  opts.label,   '}']); end
    if ~isempty(opts.fontsize),     app(['\', opts.fontsize]); end
    app('\centering');

    if opts.useBooktabs
        app(['\begin{tabular}{', colspec, '}']);
        app('\toprule');
    else
        app(['\begin{tabular}{', colspec, '}']);
        app('\hline');
    end

    % Encabezado de grupos
    % Encabezado de grupos
if isfield(layout,'col_groups') && ~isempty(layout.col_groups)

    line = ' & ';

    for g = 1:numel(layout.col_groups)

        cols = layout.col_groups(g).cols;

        c1   = min(colPos(cols)) + 1;
        c2   = max(colPos(cols)) + 1;
        span = c2 - c1 + 1;

        line = [line, ...
            sprintf('\\multicolumn{%d}{c}{%s}', ...
            span, layout.col_groups(g).name)];

        if g < numel(layout.col_groups)

            % Última columna de modelos perteneciente al grupo actual
            lastModelCol = max(cols);

            % Si existe una columna separadora después del grupo,
            % agregar un & extra para dejarla vacía
            if groupSpacersAfter(lastModelCol)
                line = [line, ' & & '];
            else
                line = [line, ' & '];
            end

        else
            line = [line, ' \\ '];
        end
    end

    app(line);
    end

    % Encabezado de etiquetas
    line = ' ';
    if isfield(layout,'model_labels') && ~isempty(layout.model_labels)
        for m=1:M
            line = [line, '& ', layout.model_labels{m}, ' ']; 
            if groupSpacersAfter(m), line = [line, '& ']; end 
        end
    else
        for m=1:M
            line = [line, sprintf('& (%d) ', m)]; 
            if groupSpacersAfter(m), line = [line, '& ']; end 
        end
    end
    if opts.useBooktabs, app([line, '\\ \midrule']); else, app([line, '\\ \hline']); end

    % Formatos
    fmtC  = sprintf('%%.%df', opts.digitsC);
    fmtSE = sprintf('%%.%df', opts.digitsSE);

    % Recorrer paneles
    for p = 1:P
        % Título del panel
        if ~isempty(strtrim(layout.panels(p).title))
            line = ['\textit{', layout.panels(p).title, '}'];
            for m=1:M
                line = [line, ' & ']; 
                if groupSpacersAfter(m), line = [line, '& ']; end 
            end
            app([line, ' \\ ']);
        end

        % Mapas por panel
        name2idx = cell(1,M);
        for m=1:M
            modpm = models_by_panel{p}{m};
            % Nombres
            if isfield(modpm,'names') && ~isempty(modpm.names)
                nm = modpm.names(:);
            else
                if isMultipanel
                    assert(numel(opts.varnames_by_model_by_panel)>=p && ...
                           numel(opts.varnames_by_model_by_panel{p})>=m && ...
                           ~isempty(opts.varnames_by_model_by_panel{p}{m}), ...
                           'Faltan nombres: panel %d, modelo %d.', p, m);
                    nm = opts.varnames_by_model_by_panel{p}{m}(:);
                else
                    assert(numel(opts.varnames_by_model)>=m && ~isempty(opts.varnames_by_model{m}), ...
                           'Faltan nombres: modelo %d (tabla simple).', m);
                    nm = opts.varnames_by_model{m}(:);
                end
            end
            name2idx{m} = containers.Map();
            for k=1:numel(nm)
                name2idx{m}(char(nm{k})) = k;
            end
        end

        % --------- construir y filtrar la lista de filas ----------
        rows_all = layout.panels(p).rows(:);

        % a) Omitir por keys exactas
        if ~isempty(opts.omit_keys)
            mask_keep = true(numel(rows_all),1);
            omitSet = containers.Map('KeyType','char','ValueType','logical');
            for kk = 1:numel(opts.omit_keys)
                omitSet(char(opts.omit_keys{kk})) = true;
            end
            for rr = 1:numel(rows_all)
                krr = char(rows_all{rr});
                if isKey(omitSet, krr)
                    mask_keep(rr) = false;
                end
            end
            rows_all = rows_all(mask_keep);
        end

        % b) Omitir por patrón regex
        if ~isempty(opts.omit_regex)
            mask_keep = true(numel(rows_all),1);
            for rr = 1:numel(rows_all)
                krr = char(rows_all{rr});
                if ~isempty(regexp(krr, opts.omit_regex, 'once'))
                    mask_keep(rr) = false;
                end
            end
            rows_all = rows_all(mask_keep);
        end
        % -----------------------------------------------------------------

        % Filas (ya filtradas)
        for r = 1:numel(rows_all)
            key = rows_all{r};
            label = key;
            if isfield(layout,'display_map') && ~isempty(layout.display_map) && isKey(layout.display_map, key)
                label = layout.display_map(key);
            end

            % Coeficientes
            line = [char(label), ' '];
            for m=1:M
                [coefStr, sigStr] = getCoefStar(models_by_panel{p}{m}, name2idx{m}, key, fmtC);
                if isempty(coefStr)
                    line = [line, '& '];
                else
                    if isempty(sigStr)
                        line = [line, '& $', coefStr, '$'];
                    else
                        line = [line, '& $', coefStr, '^{', sigStr, '}$'];
                    end
                end
                if groupSpacersAfter(m), line = [line, ' & ']; end 
            end
            app([line, ' \\ ']);

            % Errores estándar
            line = ' ';
            for m=1:M
                seStr = getSE(models_by_panel{p}{m}, name2idx{m}, key, fmtSE);
                if isempty(seStr), line = [line, '& ']; 
                else,              line = [line, '& ($', seStr, '$)']; 
                end
                if groupSpacersAfter(m), line = [line, ' & ']; end 
            end
            app([line, ' \\ ']);
        end

        % Separador entre paneles
        if p < P
            if opts.useBooktabs, app('\midrule'); else, app('\hline'); end
        end
    end

    % Estadísticos al pie
    line = 'R$^2$ ';
    for m=1:M
        mod0 = models_by_panel{1}{m};
        if isfield(mod0,'r2') && ~isempty(mod0.r2) && ~isnan(mod0.r2)
            line = [line, '& $', num2str(mod0.r2,'%.2f'), '$']; 
        else
            line = [line, '& ']; 
        end
        if groupSpacersAfter(m), line = [line, ' & ']; end 
    end
    app([line, ' \\ ']);

    line = 'Num. obs. ';
    for m=1:M
        mod0 = models_by_panel{1}{m};
        if isfield(mod0,'n') && ~isempty(mod0.n) && ~isnan(mod0.n)
            line = [line, '& $', num2str(round(mod0.n)), '$']; 
        else
            line = [line, '& ']; 
        end
        if groupSpacersAfter(m), line = [line, ' & ']; end 
    end
    if opts.useBooktabs, app([line, ' \\ \bottomrule']); else, app([line, ' \\ \hline']); end

    app('\end{tabular}');
    if opts.addThreePartTable
        app('\begin{tablenotes}');
        % Nota automática si se ocultaron filas y no se pasó 'notes'
        if (~isempty(opts.omit_regex) || ~isempty(opts.omit_keys)) && isempty(opts.notes)
            app('\item \emph{Nota:} Controles (p.ej., dummies de región) incluidos en la estimación y omitidos en esta presentación.');
        end
        for j=1:numel(opts.notes), app(['\item ', opts.notes{j}]); end
        app('\end{tablenotes}');
        app('\end{threeparttable}');
    end
    app('\end{center}');
    app('\end{table}');

    latex_code = strjoin(L, newline);

    % Escritura a archivo si corresponde
    if ~isempty(outpath)
        fid = fopen(outpath,'w'); assert(fid>0, 'No se pudo abrir %s', outpath);
        fwrite(fid, latex_code, 'char');
        fclose(fid);
    end
end

% ------- helpers ----------
function [coefStr, sigStr] = getCoefStar(mod, map, key, fmtC)
    coefStr=''; sigStr='';
    if ~isKey(map,key), return; end
    k = map(key); if k > numel(mod.beta), return; end
    b = mod.beta(k); coefStr = num2str(b, fmtC);
    p = NaN; if isfield(mod,'p') && numel(mod.p)>=k, p = mod.p(k); end
    if isnan(p), sigStr=''; return; end
    if     p<0.001, sigStr='***';
    elseif p<0.01,  sigStr='**';
    elseif p<0.05,  sigStr='*';
    elseif p<0.10,  sigStr='o';
    else,           sigStr='';
    end
end

function seStr = getSE(mod, map, key, fmtSE)
    seStr=''; if ~isKey(map,key), return; end
    k = map(key); if k > numel(mod.se), return; end
    s = mod.se(k); seStr = num2str(s, fmtSE);
end
