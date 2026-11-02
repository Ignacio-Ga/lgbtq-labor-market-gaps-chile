%% ========================================================================
% ENSSEX 2022-2023: ACEPTACIÓN LGBTQ+ POR REGIÓN
% ========================================================================

%%  Opciones y rutas
fileENSSEX = fullfile(FolderData, 'ENSSEX_2022_2023.csv');
fileShape  = fullfile(FolderData, 'Regional.shp');
exportPDF = false;


%% Cargar ENSSEX
fprintf('Cargando ENSSEX...\n'); tic
opts = detectImportOptions(fileENSSEX,'VariableNamingRule','preserve');
enssex = readtable(fileENSSEX,opts);
enssex.Properties.VariableNames = cellstr(lower(string(enssex.Properties.VariableNames)));
fprintf('Base cargada en %.2f segundos.\n',toc);

requiredVariables = {'region';'p31';'p32';'w_personas_cal'};
missingVariables = setdiff(requiredVariables,enssex.Properties.VariableNames);


region = numericVariable(enssex.region);
p31    = numericVariable(enssex.p31);
p32    = numericVariable(enssex.p32);
weight = numericVariable(enssex.w_personas_cal);

%% Regiones
regionCode = (1:16)';
regionName = [
    "Tarapacá"; "Antofagasta"; "Atacama"; "Coquimbo"; "Valparaíso";
    "O'Higgins"; "Maule"; "Biobío"; "La Araucanía"; "Los Lagos";
    "Aysén"; "Magallanes"; "Metropolitana"; "Los Ríos";
    "Arica y Parinacota"; "Ñuble"];
regionalResults = table(regionCode,regionName,'VariableNames',{'region_code','region_name'});

%%  Aceptación ponderada
validCodes = [1 2 3];
acceptLGB = nan(16,1); acceptTrans = nan(16,1);
sampleLGB = zeros(16,1); sampleTrans = zeros(16,1);
expandedLGB = nan(16,1); expandedTrans = nan(16,1);

for r = 1:16
    validLGB = region==r & ismember(p31,validCodes) & isfinite(weight) & weight>0;
    sampleLGB(r) = sum(validLGB);
    expandedLGB(r) = sum(weight(validLGB),'omitnan');
    if expandedLGB(r)>0
        acceptLGB(r) = sum(weight(validLGB & p31==1),'omitnan')/expandedLGB(r);
    end

    validTrans = region==r & ismember(p32,validCodes) & isfinite(weight) & weight>0;
    sampleTrans(r) = sum(validTrans);
    expandedTrans(r) = sum(weight(validTrans),'omitnan');
    if expandedTrans(r)>0
        acceptTrans(r) = sum(weight(validTrans & p32==1),'omitnan')/expandedTrans(r);
    end
end

regionalResults.n_lgb = sampleLGB;
regionalResults.n_trans = sampleTrans;
regionalResults.expanded_lgb = expandedLGB;
regionalResults.expanded_trans = expandedTrans;
regionalResults.accept_lgb = acceptLGB;
regionalResults.accept_trans = acceptTrans;
regionalResults.accept_lgb_pct = 100*acceptLGB;
regionalResults.accept_trans_pct = 100*acceptTrans;

disp(regionalResults(:,{'region_code','region_name','n_lgb','accept_lgb_pct','n_trans','accept_trans_pct'}));

%% Nacional
validNationalLGB = ismember(p31,validCodes) & isfinite(weight) & weight>0;
validNationalTrans = ismember(p32,validCodes) & isfinite(weight) & weight>0;
nationalAcceptLGB = 100*sum(weight(validNationalLGB & p31==1),'omitnan')/sum(weight(validNationalLGB),'omitnan');
nationalAcceptTrans = 100*sum(weight(validNationalTrans & p32==1),'omitnan')/sum(weight(validNationalTrans),'omitnan');

nationalSampleLGB = sum(validNationalLGB);
nationalSampleTrans = sum(validNationalTrans);

fprintf('\nAceptación nacional ponderada:\n');
fprintf('Hijo/a LGB:   %.2f %%\n',nationalAcceptLGB);
fprintf('Hijo/a trans: %.2f %%\n\n',nationalAcceptTrans);



%% Exportar LaTeX
latexFile = fullfile(FolderTables,'ENSSEX_aceptacion_por_region.tex');
fid = fopen(latexFile,'w','n','UTF-8');
if fid==-1, error('No fue posible crear el archivo LaTeX: %s',latexFile); end
cleanupFile = onCleanup(@() closeFileSafely(fid));

fprintf(fid,'\\begin{table}[H]\n');
fprintf(fid,'\\begin{center}\n');
fprintf(fid,'\\begin{threeparttable}\n');
fprintf(fid,'\\captionsetup{margin=0.5cm}\n');
fprintf(fid,'\\caption{Regional acceptance of LGB and transgender children in Chile.}\n');
fprintf(fid,'\\label{tab:enssex_acceptance_region}\n');
fprintf(fid,'\\footnotesize\n\\centering\n');
fprintf(fid,'\\begin{tabular}{lcccc}\n\\toprule\n');
fprintf(fid,'Region & \\multicolumn{2}{c}{LGB child} & \\multicolumn{2}{c}{Trans child} \\\\\n');
fprintf(fid,'\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}\n');
fprintf(fid,' & Observations & Acceptance (\\%%) & Observations & Acceptance (\\%%) \\\\\n');
fprintf(fid,'\\midrule\n');
for r=1:height(regionalResults)
    fprintf(fid,'%s & %.0f & %.2f & %.0f & %.2f \\\\\n',...
        escapeLatex(regionalResults.region_name(r)),...
        regionalResults.n_lgb(r),regionalResults.accept_lgb_pct(r),...
        regionalResults.n_trans(r),regionalResults.accept_trans_pct(r));
end
fprintf(fid,'\\midrule\n');
fprintf(fid, ...
    'National & %.0f & %.2f & %.0f & %.2f \\\\\n', ...
    nationalSampleLGB, ...
    nationalAcceptLGB, ...
    nationalSampleTrans, ...
    nationalAcceptTrans);
fprintf(fid,'\\bottomrule\n\\end{tabular}\n');
fprintf(fid,'\\begin{tablenotes}\n');
fprintf(fid,'\\item Notes: Acceptance rates are weighted using the ENSSEX 2022--2023 calibrated person weight.');
fprintf(fid,'\\item The number of observations corresponds to the unweighted number of valid responses in each region.\n');
fprintf(fid,'\\item Source: Own elaboration based on ENSSEX 2022--2023.\n');
fprintf(fid,'\\end{tablenotes}\n\\end{threeparttable}\n\\end{center}\n\\end{table}\n');
fclose(fid); clear cleanupFile
fprintf('Tabla LaTeX exportada en:\n%s\n',latexFile);


%% Leer shapefile
fprintf('Cargando shapefile como geospatial table...\n'); tic
regionsMap = readgeotable(fileShape,'VariableNamingRule','preserve');
fprintf('Shapefile cargado en %.2f segundos.\n',toc);

requiredMapVariables = ["Shape","codregion","Region"];
missingMapVariables = setdiff(requiredMapVariables,string(regionsMap.Properties.VariableNames));
if ~isempty(missingMapVariables)
    error('Faltan variables requeridas en el shapefile: %s',strjoin(missingMapVariables,', '));
end

%% Quitar Zona sin demarcar y hacer merge
shapeCode = numericVariable(regionsMap.codregion);
validRegion = isfinite(shapeCode) & shapeCode>=1 & shapeCode<=16;
regionsMap = regionsMap(validRegion,:);
shapeCode = shapeCode(validRegion);
if height(regionsMap)~=16
    error('Deberían quedar 16 regiones, pero quedaron %d.',height(regionsMap));
end

regionsMap.accept_lgb_pct = nan(height(regionsMap),1);
regionsMap.accept_trans_pct = nan(height(regionsMap),1);
for j=1:height(regionsMap)
    idx = find(regionalResults.region_code==shapeCode(j),1);
    if isempty(idx), error('No se pudo emparejar la región %.0f.',shapeCode(j)); end
    regionsMap.accept_lgb_pct(j) = regionalResults.accept_lgb_pct(idx);
    regionsMap.accept_trans_pct(j) = regionalResults.accept_trans_pct(idx);
end

disp(table(string(regionsMap.Region),shapeCode,regionsMap.accept_lgb_pct,regionsMap.accept_trans_pct,...
    'VariableNames',{'region','codigo_region','aceptacion_lgb','aceptacion_trans'}));

%% Escala común
allAcceptance = [regionsMap.accept_lgb_pct; regionsMap.accept_trans_pct];
colorLowerLimit = floor(min(allAcceptance,[],'omitnan')/5)*5;
colorUpperLimit = ceil(max(allAcceptance,[],'omitnan')/5)*5;
if ~isfinite(colorLowerLimit) || ~isfinite(colorUpperLimit) || colorLowerLimit>=colorUpperLimit
    colorLowerLimit=0; colorUpperLimit=100;
end

%% Create LGB map

fprintf('Generating LGB map...\n');

tic

figureLGB = figure( ...
    'Color', 'w', ...
    'Position', [100 50 680 900]);

axLGB = createMapAxes(regionsMap);

geoplot( ...
    axLGB, ...
    regionsMap, ...
    'ColorVariable', 'accept_lgb_pct', ...
    'EdgeColor', [0.3 0.3 0.3], ...
    'LineWidth', 0.6);

geolimits(axLGB, [-56.5 -17.5], [-76.5 -66]);

% Hide map grid lines
axLGB.GraticuleLineStyle = 'none';

% Hide scale bar
axLGB.Scalebar.Visible = 'off';

colormap(axLGB, parula(256));

clim(axLGB, [colorLowerLimit colorUpperLimit]);

cbLGB = colorbar(axLGB);
cbLGB.Label.String = 'Acceptance (%)';

title( ...
    axLGB, ...
    { ...
        'Acceptance of a gay, lesbian, bisexual,' ...
        'or other sexual orientation child'
    }, ...
    'FontWeight', 'bold');

subtitle( ...
    axLGB, ...
    'Percentage by region, ENSSEX 2022-2023');

drawnow;

exportgraphics( ...
    axLGB, ...
    fullfile(FolderTables, 'Map_acceptance_child_LGB.png'), ...
    'Resolution', 300);

fprintf('LGB map generated in %.2f seconds.\n', toc);

%% Create trans map

fprintf('Generating trans map...\n');

tic

figureTrans = figure( ...
    'Color', 'w', ...
    'Position', [800 50 680 900]);

axTrans = createMapAxes(regionsMap);

geoplot( ...
    axTrans, ...
    regionsMap, ...
    'ColorVariable', 'accept_trans_pct', ...
    'EdgeColor', [0.3 0.3 0.3], ...
    'LineWidth', 0.6);

geolimits(axTrans, [-56.5 -17.5], [-76.5 -66]);

% Hide map grid lines
axTrans.GraticuleLineStyle = 'none';

% Hide scale bar
axTrans.Scalebar.Visible = 'off';

colormap(axTrans, parula(256));

clim(axTrans, [colorLowerLimit colorUpperLimit]);

cbTrans = colorbar(axTrans);
cbTrans.Label.String = 'Acceptance (%)';

title( ...
    axTrans, ...
    'Acceptance of a transgender child', ...
    'FontWeight', 'bold');

subtitle( ...
    axTrans, ...
    'Percentage by region, ENSSEX 2022-2023');

drawnow;

exportgraphics( ...
    axTrans, ...
    fullfile(FolderTables, 'Map_acceptance_child_trans.png'), ...
    'Resolution', 300);

fprintf('Trans map generated in %.2f seconds.\n', toc);