%% ============================
%  Descriptivos: filas = variables, columnas = Men/Women por grupo
% ============================

T = casen;  

% --- Construir grupos mutuamente excluyentes
grp = strings(height(T),1);
isTrans = (T.trans1==1);
grp(isTrans) = "Trans";
isCis = ~isTrans;
grp(isCis & T.os1==1) = "Cis-heterosexual";
grp(isCis & T.os1==2) = "Cis-gay/lesbiana";
grp(isCis & T.os1==3) = "Cis-bisexual";
% Excluir 'otros cis' (os1==4)
grp(isCis & T.os1==4) = "";
T = T(~ismissing(grp),:);
T.grupo = categorical(grp);

% --- Construir grupos mutuamente excluyentes 2017
% grp = strings(height(T),1);
% isTrans = (T.trans1==1);
% grp(isTrans) = "Trans";
% isCis = ~isTrans;
% grp(isCis & T.r23==1) = "Cis-heterosexual";
% grp(isCis & T.r23==2) = "Cis-gay/lesbiana";
% grp(isCis & T.r23==3) = "Cis-bisexual";
% % Excluir 'otros cis' (os1==4)
% grp(isCis & T.r23==4) = "";
% T = T(~ismissing(grp),:);
% T.grupo = categorical(grp);

% --- Variables a reportar 
varlist   = {'ln_y','l','esc','edad','ing_nl','cantidad_menores_11','salud'};
varlabels = containers.Map;
varlabels('ln_y')   = 'Log hourly wage';
varlabels('l')      = 'Employment rate';
varlabels('esc')    = 'Years of schooling';
varlabels('edad')   = 'Age';
varlabels('ing_nl') = 'Log non-labor income';
varlabels('cantidad_menores_11') = 'Child $< 11$';
varlabels('salud') = 'Health status';

% --- Variables a reportar 
varlist2   = {'y_hrs','o10','yho_base1000','l','esc','edad','cantidad_menores_11','salud'};
varlabels2 = containers.Map;
varlabels2('y_hrs')   = 'Hourly wage';
varlabels2('o10') = 'Hours worked per week';
varlabels2('yho_base1000') = 'Total household income';
varlabels2('l')      = 'Employment rate';
varlabels2('esc')    = 'Years of schooling';
varlabels2('edad')   = 'Age';
varlabels2('cantidad_menores_11') = 'Number of children $< 11$';
varlabels2('salud') = 'Health status';


% --- Orden de columnas por sexo/grupo
groups_order = {'Cis-heterosexual','Cis-gay/lesbiana','Cis-bisexual','Trans'};

% --- utilidades: media/EE ponderadas y neff
wmean = @(x,w) sum(w.*x) / sum(w);
neff  = @(w) (sum(w).^2) / sum(w.^2);
% EE del promedio ponderado (basado en n_eff)
wse_mean = @(x,w) ( ...
    sqrt( max( wmean( (x - wmean(x,w)).^2, w ), 0 ) ) / sqrt(neff(w)) );

% --- separar por sexo
S_men   = T(T.sexo==1, :);
S_women = T(T.sexo==0, :);

% --- construir paneles
C_men   = panel_by_sex(S_men,   varlist, groups_order,wmean,wse_mean);  % V x 4
C_women = panel_by_sex(S_women, varlist, groups_order,wmean,wse_mean);  % V x 4

C_men2   = panel_by_sex(S_men,   varlist2, groups_order,wmean,wse_mean);  % V x 4
C_women2 = panel_by_sex(S_women, varlist2, groups_order,wmean,wse_mean);  % V x 4


%%
[N_men,Neff_men]   = count_by_group(S_men,   groups_order);
[N_women,Neff_women] = count_by_group(S_women, groups_order);
%%
% --- Tabla de strings para mostrar en consola 
rowlabels = strings(numel(varlist),1);
for i=1:numel(varlist)
    if isKey(varlabels, varlist{i})
        rowlabels(i) = varlabels(varlist{i});
    else
        rowlabels(i) = varlist{i};
    end
end

rowlabels2 = strings(numel(varlist2),1);
for i=1:numel(varlist2)
    if isKey(varlabels2, varlist2{i})
        rowlabels2(i) = varlabels2(varlist2{i});
    else
        rowlabels2(i) = varlist2{i};
    end
end

rowlabels = [rowlabels; "N (effective)";"N"];
rowlabels2 = [rowlabels2;"N (effective)";"N"];
C_men     = [C_men; N_men;Neff_men];
C_women   = [C_women; N_women;Neff_women];
C_men2     = [C_men2; N_men;Neff_men];
C_women2   = [C_women2; N_women;Neff_women];

disp('=== Descriptivos (media y EE entre paréntesis) ===');
disp('Filas = variables; Columnas = [Men|Women] por grupo');
colhdr = ["Men: Cis-hetero","Men: Cis-gay/lesb.", "Men: Cis-bisexual", "Men: Trans", ...
          "Women: Cis-hetero","Women: Cis-gay/lesb.", "Women: Cis-bisexual", "Women: Trans"];
ShowTbl = [rowlabels, [C_men, C_women]];
ShowTbl = array2table(ShowTbl, 'VariableNames', ['Variable', colhdr]);
disp(ShowTbl);

ShowTbl2 = [rowlabels2, [C_men2, C_women2]];
ShowTbl2 = array2table(ShowTbl2, 'VariableNames', ['Variable', colhdr]);
disp(ShowTbl2);

% --- Guardar CSV "plano" 
out_csv = fullfile(FolderData, 'descriptivos_panel.csv');
writetable(ShowTbl, out_csv);

%% ========= Exportar a LaTeX =========

out_tex = fullfile(FolderData, 'descriptivos_panel.tex');
latex_code = export_desc_panel_latex(rowlabels, C_men, C_women, out_tex, ...
    'Weighted descriptive statistics by Sex × Group (CASEN 2022)', ...
    'tab:desc_panel');

out_tex2 = fullfile(FolderData, 'descriptivos_panel2.tex');
latex_code2 = export_desc_panel_latex(rowlabels2, C_men2, C_women2, out_tex2, ...
    'Weighted descriptive statistics by Sex × Group (CASEN 2022)', ...
    'tab:desc_panel');

%Visualizar en matlab
preview_latex_as_html(latex_code);

preview_latex_as_html(latex_code2);
