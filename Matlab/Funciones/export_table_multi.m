function T = export_models_table(models_in, layout, outpath, opts)
% EXPORT_MODELS_TABLE
% Exporta tablas LaTeX (tipo "stargazer") para uno o varios modelos.
% Soporta ambos modos:
%   - Tabla simple (uno o varios modelos):      models_in = {model1, model2, ...}
%   - Tabla con múltiples paneles:              models_in = { {model1,...}, {model1,...}, ... }
%
% Donde cada "model" es una struct con al menos:
%   .beta (Kx1), .se (Kx1), .p (Kx1)
% Opcionales:
%   .n (scalar), .r2 (scalar), .names (cellstr Kx1)
%
% layout:
%   .display_map: containers.Map, mapea claves internas -> etiqueta a mostrar
%   .panels: struct array con .title y .rows (cellstr de claves internas)
%   .col_groups: struct array opcional con:
%       .name (char) y .cols (vector con índices de columna 1..M)
%   .model_labels: cellstr, etiquetas de columna, ej. {'(1)','(2)',...}
%
% opts:
%   .digitsC, .digitsSE  (default 2,2)
%   .useBooktabs         (default true)
%   .caption, .label, .notes (cellstr), .fontsize ('footnotesize' por defecto)
%   .addThreePartTable   (default true)
%   .captionsetup        (string LaTeX extra; ej. '\captionsetup{margin=0.5cm}')
%   .varnames_by_model               (tabla simple) : {M} cell, cada uno cellstr Kx1
%   .varnames_by_model_by_panel      (multi-panel) : {P} donde cada celda es {M} con cellstr
%
% Devuelve T (opcional; placeholder para inspección).

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

    % Detectar modo: simple vs multi-panel
    isMultipanel = iscell(models_in) && ~isempty(models_in) && iscell(models_in{1});

    if isMultipanel
        % models_by_panel: 1xP cell, cada celda es {M}
        models_by_panel = models_in;
        P = numel(models_by_panel);
        assert(isfield(layout,'panels') && numel(layout.panels)==P, ...
               'layout.panels debe tener una entrada por panel.');
        M = numel(models_by_panel{1}); % # columnas (modelos por panel)
    else
        % single panel (tabla simple)
        models = models_in;
        M = numel(models);
        % Si no hay panels en layout, creamos uno por defecto con todas las rows en .panels(1).rows
        if ~isfield(layout,'panels') || isempty(layout.panels)
            error('Para tabla simple, layout.panels(1).rows debe estar definido (orden de filas).');
        end
        P = numel(layout.panels);
        assert(P==1, 'Para tabla simple, use un solo panel en layout.panels.');
        models_by_panel = {models}; % unificar interfaz
    end

    % Separadores de grupos de columnas
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

    % Colspec para tabular
    colspec = 'l';
    for c=1:totalC, colspec = [colspec, 'c']; end %#ok<AGROW>

    % Abrir archivo
    fid = fopen(outpath,'w'); assert(fid>0, 'No se pudo abrir %s', outpath);
    fprintf(fid, '%% Tabla generada por export_models_table (MATLAB)\n');
    fprintf(fid, '\\begin{table}[H]\n\\begin{center}\n');
    if opts.addThreePartTable, fprintf(fid, '\\begin{threeparttable}\n'); end
    if ~isempty(opts.captionsetup), fprintf(fid, '%s\n', opts.captionsetup); end
    if ~isempty(opts.caption),      fprintf(fid, '\\caption{%s}\n', opts.caption); end
    if ~isempty(opts.label),        fprintf(fid, '\\label{%s}\n', opts.label); end
    if ~isempty(opts.fontsize),     fprintf(fid, '\\%s\n', opts.fontsize); end
    fprintf(fid, '\\centering\n');

    if opts.useBooktabs
        fprintf(fid, '\\begin{tabular}{%s}\n\\toprule\n', colspec);
    else
        fprintf(fid, '\\begin{tabular}{%s}\n\\hline\n', colspec);
    end

    % Encabezado de grupos
    if isfield(layout,'col_groups') && ~isempty(layout.col_groups)
        fprintf(fid, ' & ');
        for g = 1:numel(layout.col_groups)
            cols  = layout.col_groups(g).cols;
            c1    = min(colPos(cols)) + 1;
            c2    = max(colPos(cols)) + 1;
            span  = c2 - c1 + 1;
            fprintf(fid, '\\multicolumn{%d}{c}{%s}', span, layout.col_groups(g).name);
            if g < numel(layout.col_groups), fprintf(fid,' & '); else, fprintf(fid,' \\\\ '); end
        end
        fprintf(fid, '\n');
        for g = 1:numel(layout.col_groups)
            cols  = layout.col_groups(g).cols;
            c1    = min(colPos(cols)) + 1;
            c2    = max(colPos(cols)) + 1;
            fprintf(fid, '\\cline{%d-%d} ', c1, c2);
        end
        fprintf(fid, '\n');
    end

    % Encabezado de etiquetas de modelos
    fprintf(fid, ' ');
    if isfield(layout,'model_labels') && ~isempty(layout.model_labels)
        for m=1:M
            fprintf(fid, '& %s ', layout.model_labels{m});
            if groupSpacersAfter(m), fprintf(fid, '& '); end
        end
    else
        for m=1:M
            fprintf(fid, '& (%d) ', m);
            if groupSpacersAfter(m), fprintf(fid, '& '); end
        end
    end
    if opts.useBooktabs, fprintf(fid, '\\\\ \\midrule\n'); else, fprintf(fid, '\\\\ \\hline\n'); end

    % Formatos
    fmtC  = sprintf('%%.%df', opts.digitsC);
    fmtSE = sprintf('%%.%df', opts.digitsSE);

    % Recorrer paneles
    for p = 1:P
        % Título del panel
        if ~isempty(strtrim(layout.panels(p).title))
            fprintf(fid, '\\textit{%s}', layout.panels(p).title);
            for m=1:M, fprintf(fid, ' & '); if groupSpacersAfter(m), fprintf(fid, '& '); end; end
            fprintf(fid, '\\\\ \n');
        end

        % Mapas nombre->índice para este panel
        name2idx = cell(1,M);
        for m=1:M
            modpm = models_by_panel{p}{m};
            % Preferir .names si existe, si no usar opts.varnames_by_model / _by_panel
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

        % Filas (cada término: línea de coeficientes y línea de EE)
        for r = 1:numel(layout.panels(p).rows)
            key = layout.panels(p).rows{r};
            label = key;
            if isfield(layout,'display_map') && ~isempty(layout.display_map) && isKey(layout.display_map, key)
                label = layout.display_map(key);
            end

            % Línea de coeficientes
            fprintf(fid, '%s ', label);
            for m=1:M
                [coefStr, sigStr] = getCoefStar(models_by_panel{p}{m}, name2idx{m}, key, fmtC);
                if isempty(coefStr)
                    fprintf(fid, '& ');
                else
                    if isempty(sigStr), fprintf(fid, '& $%s$', coefStr);
                    else,               fprintf(fid, '& $%s^{%s}$', coefStr, sigStr);
                    end
                end
                if groupSpacersAfter(m), fprintf(fid,' & '); end
            end
            fprintf(fid, ' \\\\ \n');

            % Línea de EE
            fprintf(fid, ' ');
            for m=1:M
                seStr = getSE(models_by_panel{p}{m}, name2idx{m}, key, fmtSE);
                if isempty(seStr), fprintf(fid, '& ');
                else, fprintf(fid, '& $(%s)$', seStr);
                end
                if groupSpacersAfter(m), fprintf(fid,' & '); end
            end
            fprintf(fid, ' \\\\ \n');
        end

        if p < P
            if opts.useBooktabs, fprintf(fid, '\\midrule\n'); else, fprintf(fid, '\\hline\n'); end
        end
    end

    % Estadísticos al pie (tomados del PRIMER panel; cambia si prefieres otros)
    fprintf(fid, 'R$^2$ ');
    for m=1:M
        mod0 = models_by_panel{1}{m};
        if isfield(mod0,'r2') && ~isempty(mod0.r2) && ~isnan(mod0.r2)
            fprintf(fid, '& $%s$', num2str(mod0.r2,'%.2f'));
        else
            fprintf(fid, '& ');
        end
        if groupSpacersAfter(m), fprintf(fid,' & '); end
    end
    fprintf(fid, ' \\\\ \n');

    fprintf(fid, 'Num. obs. ');
    for m=1:M
        mod0 = models_by_panel{1}{m};
        if isfield(mod0,'n') && ~isempty(mod0.n) && ~isnan(mod0.n)
            fprintf(fid, '& $%d$', round(mod0.n));
        else
            fprintf(fid, '& ');
        end
        if groupSpacersAfter(m), fprintf(fid,' & '); end
    end
    if opts.useBooktabs, fprintf(fid, ' \\\\ \\bottomrule\n'); else, fprintf(fid, ' \\\\ \\hline\n'); end

    fprintf(fid, '\\end{tabular}\n');

    if opts.addThreePartTable
        fprintf(fid, '\\begin{tablenotes}\n');
        for j=1:numel(opts.notes), fprintf(fid, '\\item %s\n', opts.notes{j}); end
        fprintf(fid, '\\end{tablenotes}\n');
        fprintf(fid, '\\end{threeparttable}\n');
    end
    fprintf(fid, '\\end{center}\n\\end{table}\n');
    fclose(fid);

    T = table; % opcional
end

% ------- helpers ----------
function [coefStr, sigStr] = getCoefStar(mod, map, key, fmtC)
    coefStr=''; sigStr='';
    if ~isKey(map,key), return; end
    k = map(key);
    if k > numel(mod.beta), return; end
    b = mod.beta(k);
    coefStr = num2str(b, fmtC);
    p = NaN; if isfield(mod,'p') && numel(mod.p)>=k, p = mod.p(k); end
    if isnan(p), sigStr=''; return; end
    if     p<0.001, sigStr='***';
    elseif p<0.01,  sigStr='**';
    elseif p<0.05,  sigStr='*';
    elseif p<0.10,  sigStr='.';
    else,           sigStr='';
    end
end

function seStr = getSE(mod, map, key, fmtSE)
    seStr=''; if ~isKey(map,key), return; end
    k = map(key); if k > numel(mod.se), return; end
    s = mod.se(k); seStr = num2str(s, fmtSE);
end
