function S = preview_latex_as_html(latex_table_code)
% PREVIEW_LATEX_AS_HTML  (1 argumento)
% Compila una tabla LaTeX a HTML (make4ht/htlatex/pandoc) y la muestra en uihtml.
% Uso:
%   preview_latex_as_html(latex_code);

    if nargin<1 || isempty(latex_table_code)
        warning('Falta latex_table_code.'); if nargout, S=struct(); end; return;
    end
    if isstring(latex_table_code)
        latex_table_code = char(join(latex_table_code,newline));
    elseif ~ischar(latex_table_code)
        error('El argumento debe ser char o string con \\begin{table}...\\end{table}.');
    end
    % --- base dir: FolderData si existe; si no, pwd
    baseDir = pwd;
    try
        if evalin('base','exist(''FolderData'',''var'')')
            bd = evalin('base','FolderData'); if isstring(bd), bd=char(bd); end
            if ~isempty(bd), baseDir = bd; end
        end
    catch, end
    outdir = fullfile(baseDir,'_latex_preview');
    if ~exist(outdir,'dir'), mkdir(outdir); end

    % --- archivos
    % --- archivos
    hash = compute_hash(latex_table_code);              % md5 de latex_code
    bname = ['tbl_' hash(1:10)];                  % nombre compacto
    texfile = fullfile(outdir,[bname '.tex']);
    htmlOut = fullfile(outdir,[bname '.html']);
    logf    = fullfile(outdir,[bname '.log']);



    % --- documento LaTeX mínimo
    doc = [ ...
        '%% Auto-generated', newline, ...
        '\documentclass[11pt]{article}', newline, ...
        '\usepackage[margin=2cm]{geometry}', newline, ...
        '\usepackage{booktabs,threeparttable,hyperref,caption,float,makecell}', newline, ...
        '\captionsetup{margin=0.5cm}', newline, ...
        '\begin{document}', newline, ...
        latex_table_code, newline, ...
        '\end{document}', newline ];
    fid=fopen(texfile,'w'); assert(fid>0,'No se pudo escribir %s',texfile);
    fwrite(fid,doc,'char'); fclose(fid);

    % --- detectar herramientas
    make4ht = findexe({'make4ht.exe','make4ht'});
    htlatex = findexe({'htlatex.exe','htlatex'});
    pandoc  = findexe({'pandoc.exe','pandoc'});

    % --- compilar a HTML (preferimos make4ht → htlatex → pandoc)
    cwd = pwd; cd(outdir);
    try
        if ~isempty(make4ht)
            % HTML5, unicode
            cmd = sprintf('"%s" -u -f html5 "%s"', make4ht, [bname '.tex']);
            [~,log1] = system(cmd);
        elseif ~isempty(htlatex)
            % TeX4ht clásico
            cmd = sprintf('"%s" "%s" "xhtml,charset=utf-8"', htlatex, [bname '.tex']);
            [~,log1] = system(cmd);
        elseif ~isempty(pandoc)
            % Pandoc como último recurso
            cmd = sprintf('"%s" -s -f latex -t html5 --mathjax -o "%s" "%s"', ...
                          pandoc, [bname '.html'], [bname '.tex']);
            [~,log1] = system(cmd);
        else
            warning(['No se encontró make4ht/htlatex/pandoc en PATH. ', ...
                'Instala TeX4ht (viene con MiKTeX/TeX Live) o Pandoc.']);
            cd(cwd);
            if nargout, S=struct('dir',outdir,'tex',texfile,'html','','log',logf); end
            return;
        end
    catch ME
        cd(cwd); rethrow(ME);
    end
    cd(cwd);
    fid=fopen(logf,'w'); if fid>0, fwrite(fid,log1,'char'); fclose(fid); end

    % --- localizar el HTML generado
    htmlList = dir(fullfile(outdir,'*.html'));
    % elegimos el más reciente
    if ~isempty(htmlList)
        [~,idx] = max([htmlList.datenum]);
        htmlOut = fullfile(htmlList(idx).folder, htmlList(idx).name);
    end
    if isempty(htmlOut) || ~isfile(htmlOut)
        warning('No se encontró HTML generado. Revisa el log: %s', logf);
        if nargout, S=struct('dir',outdir,'tex',texfile,'html','','log',logf); end
        return;
    end

    % --- mostrar en un uifigure con uihtml (1 sola ventana para todas las llamadas)
    persistent H
    if isempty(H) || ~isstruct(H) || ~isfield(H,'fig') || ~isvalid(H.fig)
        H.fig = uifigure('Name','Vista previa (LaTeX → HTML)','Position',[120 120 1024 720],'Color','w');
        gl = uigridlayout(H.fig,[2 1]); gl.RowHeight = {40,'1x'}; gl.ColumnWidth = {'1x'};
        top = uigridlayout(gl,[1 3]); top.ColumnWidth = {60, '1x', 60};
        H.btnPrev = uibutton(top,'Text','⬅︎'); H.btnPrev.Layout.Column=1;
        H.status  = uilabel(top,'Text','1/1','HorizontalAlignment','center','FontWeight','bold'); H.status.Layout.Column=2;
        H.btnNext = uibutton(top,'Text','➡︎'); H.btnNext.Layout.Column=3;
        H.html    = uihtml(gl,'HTMLSource','about:blank'); H.html.Layout.Row=2;
        H.items   = strings(0,1); H.idx=0;
        H.btnPrev.ButtonPushedFcn=@(~,~)nav(-1);
        H.btnNext.ButtonPushedFcn=@(~,~)nav(+1);
    end
    H.items(end+1) = string(htmlOut);
    H.idx = numel(H.items);
    show(H);  % carga el html

    if nargout
        S = struct('dir',outdir,'tex',texfile,'html',htmlOut,'log',logf,'index',H.idx,'count',numel(H.items));
    end

    % ------- helpers -------
    function nav(step)
        if isempty(H.items), return; end
        H.idx = max(1,min(H.idx+step, numel(H.items)));
        show(H);
    end
    function show(H)
        if H.idx<1 || H.idx>numel(H.items), return; end
        f = char(H.items(H.idx));
        if isfile(f), H.html.HTMLSource = f; end
        H.status.Text = sprintf('Mostrando %d/%d', H.idx, numel(H.items));
        drawnow;
    end
end

function exe = findexe(cands)
    exe = '';
    for i=1:numel(cands)
        if ispc, [s,p]=system(sprintf('where %s',cands{i}));
        else,     [s,p]=system(sprintf('which %s',cands{i}));
        end
        if s==0
            lines = strtrim(strsplit(p,newline));
            exe = lines{1}; return;
        end
    end
    % intentos comunes en Windows (MiKTeX/TeX Live)
    if ispc && any(strcmpi(cands, 'make4ht.exe'))
        guess = {'C:\texlive\2025\bin\win32\make4ht.exe','C:\texlive\2024\bin\win32\make4ht.exe'};
        for g=guess, if exist(g{1},'file'), exe=g{1}; return; end, end
    end
    if ispc && any(strcmpi(cands, 'htlatex.exe'))
        guess = {'C:\Program Files\MiKTeX\miktex\bin\x64\htlatex.exe','C:\texlive\2025\bin\win32\htlatex.exe'};
        for g=guess, if exist(g{1},'file'), exe=g{1}; return; end, end
    end
    function h = compute_hash(txt)
    % md5 hash mediante Java (sin dependencias externas)
    md = java.security.MessageDigest.getInstance('MD5');
    md.update(uint8(txt));
    h  = typecast(md.digest,'uint8');
    h  = lower(reshape(dec2hex(h,2).',1,[])); % hex string
    end

end
