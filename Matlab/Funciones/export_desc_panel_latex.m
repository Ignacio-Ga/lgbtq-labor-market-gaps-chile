function latex_code = export_desc_panel_latex(rowlabels, C_men, C_women, outpath, caption, label)
    if nargin<5, caption='Weighted descriptive statistics by Sex × Group'; end
    if nargin<6, label='tab:desc_panel'; end

    % Encabezados de subcolumnas
    subhdr = {'Cis-hetero','Cis-gay/lesbian','Cis-bisexual','Trans'};

    L = strings(0,1);
    app = @(s) assignin('caller','L',[evalin('caller','L'); string(s)]);

    app("\begin{table}[H]");
    app("\begin{center}");
    app("\begin{threeparttable}");
    app("\captionsetup{margin=0.5cm}");
    app("\caption{" + caption + "}");
    app("\label{" + label + "}");
    app("\footnotesize");
    app("\centering");
    app("% Requires \\usepackage{makecell} in your preamble");
    app("\renewcommand\arraystretch{1.15}");

    % l para la columna de variable + 8 columnas centradas
    app("\begin{tabular}{lcccc|cccc}");
    app("\toprule");

    % Fila 1: títulos grandes
    app(" & \multicolumn{4}{c|}{Men at birth} & \multicolumn{4}{c}{Women at birth} \\");
    app("\cline{2-5} \cline{6-9} ");

    % Fila 2: subencabezados
    app("Variable & " + strjoin(subhdr, " & ") + " & " + strjoin(subhdr, " & ") + " \\ \midrule");

    % Filas con datos: usar \makecell{ media \\ (EE) }
    V = size(C_men,1);
    for i = 1:V
        row = string(rowlabels(i)) + " & " + ...
              "\makecell{" + strrep(C_men(i,1),  "\\", " \\ ") + "}" + " & " + ...
              "\makecell{" + strrep(C_men(i,2),  "\\", " \\ ") + "}" + " & " + ...
              "\makecell{" + strrep(C_men(i,3),  "\\", " \\ ") + "}" + " & " + ...
              "\makecell{" + strrep(C_men(i,4),  "\\", " \\ ") + "}" + " & " + ...
              "\makecell{" + strrep(C_women(i,1),"\\", " \\ ") + "}" + " & " + ...
              "\makecell{" + strrep(C_women(i,2),"\\", " \\ ") + "}" + " & " + ...
              "\makecell{" + strrep(C_women(i,3),"\\", " \\ ") + "}" + " & " + ...
              "\makecell{" + strrep(C_women(i,4),"\\", " \\ ") + "}" + " \\ ";
        app(row);
    end

    app("\bottomrule");
    app("\end{tabular}");
    app("\begin{tablenotes}");
    app("\item[*] Source: Own elaboration, based on the CASEN 2022 survey.");
    app("\item Notes: All statistics are computed using the individual survey expansion weights ($w_i$, variable \texttt{expr\_osig}). Reported values are weighted means with standard errors in parentheses. Standard errors are computed using the weighted variance and the effective sample size within each sex $\times$ group $\times$ variable cell, defined as $n_{\mathrm{eff}}=(\sum w)^2 / \sum w^2$.");
    app("\end{tablenotes}");
    app("\end{threeparttable}");
    app("\end{center}");
    app("\end{table}");

    latex_code = strjoin(L, newline);

    if nargin>=4 && ~isempty(outpath)
        fid = fopen(outpath,'w'); assert(fid>0, 'No se pudo abrir %s', outpath);
        fwrite(fid, latex_code, 'char'); 
        fclose(fid);
    end
end