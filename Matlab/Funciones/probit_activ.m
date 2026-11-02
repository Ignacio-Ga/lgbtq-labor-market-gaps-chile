%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ignacio Gálvez
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Variables

% Mujeres

y_ma       = casen_m.activ;                

% Hombres

y_ha       = casen_h.activ;                


% Estimación
res_probitm1a = svy_probit(y_ma, x1_m, w_m, strata_m, cluster_m, opts_probit);

res_probitm2a = svy_probit(y_ma, x2_m, w_m, strata_m, cluster_m, opts_probit);

res_probith1a = svy_probit(y_ha, x1_h, w_h, strata_h, cluster_h, opts_probit);

res_probith2a = svy_probit(y_ha, x2_h, w_h, strata_h, cluster_h, opts_probit);

%% Tablas
names1 = {'esc','edad','edad2','ing_nl', ...
    'cantidad menores 11','salud ','pueblos\_indigenas','extranjero', ...
    'lgbt'};
names2 = {'esc','edad','edad2','ing_nl', ...
    'cantidad menores 11','salud ', 'pueblos\_indigenas','extranjero', ...
    'lg','b','trans'};
texpath12  = fullfile(FolderData,'tabla12.tex');
T12 = export_table(res_probitm1a, names1, texpath12, struct( ...
    'caption','Probit ponderado con varianza por diseño (CASEN)', ...
    'label','tab:probit'));
texpath13  = fullfile(FolderData,'tabla13.tex');
T13 = export_table(res_probitm2a, names2, texpath5, struct( ...
    'caption','Probit ponderado con varianza por diseño (CASEN)', ...
    'label','tab:probit'));
texpath14  = fullfile(FolderData,'tabla14.tex');
T14 = export_table(res_probith1a, names1, texpath14, struct( ...
    'caption','Probit ponderado con varianza por diseño (CASEN)', ...
    'label','tab:probit'));
texpath15  = fullfile(FolderData,'tabla15.tex');
T15 = export_table(res_probith2a, names2, texpath15, struct( ...
    'caption','Probit ponderado con varianza por diseño (CASEN)', ...
    'label','tab:probit'));

%% Ratios inverso de Mills

imr_m1a = imr_probit(res_probitm1a);
casen_m.lambda_m1a = imr_m1a.lambda1;  

imr_m2a = imr_probit(res_probitm2a);
casen_m.lambda_m2a = imr_m2a.lambda1;

imr_h1a = imr_probit(res_probith1a);
casen_h.lambda_h1a = imr_h1a.lambda1;  

imr_h2a = imr_probit(res_probith2a);
casen_h.lambda_h2a = imr_h2a.lambda1;  