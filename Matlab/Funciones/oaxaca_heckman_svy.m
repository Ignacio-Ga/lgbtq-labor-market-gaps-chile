function OUT = oaxaca_heckman_svy(y, X, xp, o, g, svy, opts)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ignacio Gálvez
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% OAXACA_HECKMAN_SVY  Oaxaca–Blinder con corrección de selección e
% varianzas para encuesta compleja (pesos, estratos y conglomerados).
%
%  - Ecuación:
%       R = (XbarA - XbarB)*betaA  +  XbarB*(betaA - betaB)  +  (lamA*betaLamA - lamB*betaLamB)
%
% Entradas:
%   y       [N x 1]   : resultado (p.ej., salario)
%   X       [N x K]   : covariables (educación, experiencia, regiones, etc.)
%   g       [N x 1]   : indicador de grupo (1=A, 0=B)
%   lambda  [N x 1]   : inverse Mills ratio (IMR) 
%   svy               : struct con diseño muestral
%       .w     [N x 1]  pesos muestrales 
%       .strata[N x 1]  id de estrato 
%       .psu   [N x 1]  id de conglomerado/PSU
% Salida:
%   OUT.main_table : tabla con: ybarA, ybarB, Delta, Seleccion(S),
%                    DeltaCorregida, NeumarkR, SE e IC (si opts.R>0).
%   OUT.byvar_table: contribuciones por variable (Dotaciones, Retornos) + SE/IC (sin IMR).
%   OUT.coefs      : coeficientes por grupo (bloque [1,X] y beta_lambda)
%   OUT.settings   : opciones efectivas

%% --- Validaciones & opciones


N = numel(y);
assert(size(X,1)==N, 'X debe ser N x K');
g = logical(g(:));
w = svy.expr_osig(:);      assert(numel(w)==N, 'svy.w tamaño N');
H = svy.varstrat(:); assert(numel(H)==N, 'svy.strata tamaño N');
C = svy.varunit(:);    assert(numel(C)==N, 'svy.psu tamaño N');


 % % Limpiar filas con NaN en cualquiera de las entradas
 %    miss = any([isnan(y), any(isnan(X),2), isnan(w), isnan(H), isnan(C)], 2);
 %    n_miss = sum(miss);
 %    if n_miss>0 
 %        warning('oaxaca_heckman_svy:MissingData', 'Se eliminaron %d filas con NaN.', n_miss);
 %    end
 %    y = y(~miss); X = X(~miss,:); w = w(~miss); H = H(~miss); C = C(~miss); g = g(~miss);


N = numel(y);

% nombres
K = size(X,2);
if isempty(opts.VarNames)
    vnames = "x"+string(1:K);
else
    vnames = string(opts.VarNames(:)).';
    assert(numel(vnames)==K,'VarNames debe tener K nombres');
end
vn_full = ["Constante", vnames, "IMR"];



% diseño: normalizar pesos a suma 
%w = w(:); w = w * (N/sum(w));

% --- Bloques por grupo
XA = X(g,:); XB = X(~g,:);
yA = y(g);   yB = y(~g);
wA = w(g);   wB = w(~g);
HA = H(g);   HB = H(~g);
CA = C(g);   CB = C(~g);


opts_mco = struct();
opts_mco.addIntercept        = false;                 % añade columna de 1s internamente
opts_mco.weightNormalization = 'none';               % no reescala pesos
opts_mco.verbose             = false;

opts_probit = struct('addIntercept', false, 'weightNormalization','none', ...
                     'maxIter',100,'tol',1e-8,'verbose',true);

oA = o(g); oB = o(~g);

xpA = xp(g,:); xpB = xp(~g,:);

idx_hA = find(XA(:,2) == 1);

idx_hB = find(XB(:,2) == 1);

idx_mA = find(XA(:,2) == 0);

idx_mB = find(XB(:,2) == 0);

xpB = [ones(size(xpB,1),1), xpB]; 

xpA = [ones(size(xpA,1),1), xpA];

probitA = svy_probit(oA,xpA,wA,HA,CA,opts_probit);

probitB = svy_probit(oB,xpB,wB,HB,CB,opts_probit);

phi_A = normpdf(xpA * probitA.beta);

mu_A = normcdf(xpA * probitA.beta);

mu_A  = min(max(mu_A,  1e-12), 1 - 1e-12);
phi_A = max(phi_A, 1e-12);

lamA = phi_A ./ mu_A;

lamBarA_h = sum(lamA(idx_hA) .* wA(idx_hA))/sum(wA(idx_hA));

lamBarA_m = sum(lamA(idx_mA) .* wA(idx_mA))/sum(wA(idx_mA));

phi_B = normpdf(xpB * probitB.beta);

mu_B = normcdf(xpB * probitB.beta);

mu_B  = min(max(mu_B,  1e-12), 1 - 1e-12);
phi_B = max(phi_B, 1e-12);

lamB = phi_B ./ mu_B;

lamBarB_h = sum(lamB(idx_hB) .* wB(idx_hB))/sum(wB(idx_hB));

lamBarB_m = sum(lamB(idx_mB) .* wB(idx_mB))/sum(wB(idx_mB));

lamA_sexo = lamA .* XA(:,2);

lamB_sexo = lamB .* XB(:,2);

% Matrices de diseño para 2ª etapa 
XAtil = [ones(size(XA,1),1) XA lamA lamA_sexo];
XBtil = [ones(size(XB,1),1) XB lamB lamB_sexo];

% Estimar coeficientes por grupo (WLS)
mcoA = svy_ols(yA,XAtil, wA, HA, CA, opts_mco);
mcoB = svy_ols(yB,XBtil, wB, HB, CB, opts_mco);

bA = mcoA.beta;

bB = mcoB.beta;

% Separar bloques: [1,X] y coeficiente de IMR
bA_X = bA(1:end-2); bB_X = bB(1:end-2);
bA_l = bA(end-1);     bB_l = bB(end-1);

bA_lh = bA_l + bA(end); bB_lh = bB_l + bB(end);


% Medias ponderadas por grupo
[X1barA_h, ybarA_h, ~] = means_block([ones(size(XA(idx_hA,:),1),1), XA(idx_hA,:)], yA(idx_hA), [lamA(idx_hA) lamA_sexo(idx_hA)], wA(idx_hA));
[X1barA_m, ybarA_m, ~] = means_block([ones(size(XA(idx_mA,:),1),1), XA(idx_mA,:)], yA(idx_mA), [lamA(idx_mA) lamA_sexo(idx_mA)], wA(idx_mA));
[X1barB_h, ybarB_h, ~] = means_block([ones(size(XB(idx_hB,:),1),1), XB(idx_hB,:)], yB(idx_hB), [lamB(idx_hB) lamB_sexo(idx_hB)], wB(idx_hB));
[X1barB_m, ybarB_m, ~] = means_block([ones(size(XB(idx_mB,:),1),1), XB(idx_mB,:)], yB(idx_mB), [lamB(idx_mB) lamB_sexo(idx_mB)], wB(idx_mB));

Delta_h    = ybarA_h - ybarB_h;
Delta_m    = ybarA_m - ybarB_m;

Selec_S_h  = lamBarA_h*bA_lh - lamBarB_h*bB_lh;                % término de selección (S)
Selec_S_m  = lamBarA_m*bA_l - lamBarB_m*bB_l;  

% --- FÓRMULA Oaxaca–Blinder
Dxb_h      = (X1barA_h - X1barB_h) * bA_X;                   % dotaciones con beta_A
Rb_h       =  X1barB_h * (bA_X - bB_X);                    % retornos con Xbar_B
R_main_h   = Dxb_h + Rb_h + Selec_S_h;                         % R total

Dxb_m      = (X1barA_m - X1barB_m) * bA_X;                   % dotaciones con beta_A
Rb_m       =  X1barB_m * (bA_X - bB_X);                    % retornos con Xbar_B
R_main_m   = Dxb_m + Rb_m + Selec_S_m;                         % R total

%  --- Neumark 

phi_AB = normpdf(xpB * probitA.beta);

mu_AB = normcdf(xpB * probitA.beta);

mu_AB  = min(max(mu_AB,  1e-12), 1 - 1e-12);
phi_AB = max(phi_AB, 1e-12);

lamAB = phi_AB ./ mu_AB;

lbarAB_h = sum(lamAB(idx_hB) .* wB(idx_hB))/sum(wB(idx_hB));

lbarAB_m = sum(lamAB(idx_mB) .* wB(idx_mB))/sum(wB(idx_mB));

DF_caracteristica_h   = ((X1barA_h - X1barB_h) * bA_X) + ((lamBarA_h - lbarAB_h) * bA_lh);
discriminacion_h    =  X1barB_h * (bA_X - bB_X) + (lbarAB_h - lamBarB_h) * bA_lh;
seleccion_h    =  lamBarB_h * (bA_lh - bB_lh);
R_neum_h  = DF_caracteristica_h + discriminacion_h + seleccion_h;                       % lado derecho de Neumark

DF_caracteristica_m   = ((X1barA_m - X1barB_m) * bA_X) + ((lamBarA_m - lbarAB_m) * bA_l);
discriminacion_m    =  X1barB_m * (bA_X - bB_X) + (lbarAB_m - lamBarB_m) * bA_l;
seleccion_m    =  lamBarB_m * (bA_l - bB_l);
R_neum_m  = DF_caracteristica_m + discriminacion_m + seleccion_m;                       % lado derecho de Neumark


% --- Contribuciones por variable 
dot_byvar = ( (X1barA_h - X1barB_h)' ) .* bA_X;           
ret_byvar = ( X1barB_h' ) .* (bA_X - bB_X);
var_labels = ["Constante", vnames];

% Tablas de salida 
main_hdr  = ["ybarA_h","ybarB_h","Delta_h","delta_obser_h","delta_noobser_h",...
             "Seleccion_S_h","R_formula_h","Delta_caracteristica_h","discriminación_h","seleccion_R_h","Neumark_R_h",...
             "ybarA_m","ybarB_m","Delta_m","delta_obser_m","delta_noobser_m",...
             "Seleccion_S_m","R_formula_m","Delta_caracteristica_m","discriminación_m","seleccion_R_m","Neumark_R_m"];
main_vals = [ ybarA_h,  ybarB_h,  Delta_h,Dxb_h,Rb_h,  Selec_S_h, R_main_h,...
              DF_caracteristica_h, discriminacion_h , seleccion_h  ,   R_neum_h,...
              ybarA_m,  ybarB_m,  Delta_m,Dxb_m,Rb_m,  Selec_S_m, R_main_m,...
              DF_caracteristica_m, discriminacion_m , seleccion_m  ,   R_neum_m];

BY = table(var_labels.', dot_byvar, ret_byvar, ...
           'VariableNames', {'Variable','Dotaciones','Retornos'});

%  λ del total a vectores N×1, alineados con el orden original 
lam_full_all   = NaN(N,1);   % λ "por observación": A y B juntos
lam_full_all(g)  = lamA;     % posiciones donde g==1 toman lamA
lam_full_all(~g) = lamB;     % posiciones donde g==0 toman lamB

lamAB_full_B  = NaN(N,1);    % λ_AB solo definido para B (Neumark), NaN en A
lamAB_full_B(~g) = lamAB;    % rellenar solo en B

% --- JACKKNIFE por PSU (JK1) ---

%  Índices por estrato y PSU
[Hlist,~,hcode] = unique(H);     % códigos de estrato 1..Hn
Hn = numel(Hlist);
psu_by_h = cell(Hn,1);
nh_vec   = zeros(Hn,1);
for h = 1:Hn
    psu_by_h{h} = unique(C(hcode==h));
    nh_vec(h)   = numel(psu_by_h{h});
end

%  Inicializar acumuladores de varianza JK1
S_main = zeros(1, numel(main_vals));          % acumula sum_h ( (nh-1)/nh * (θ_r - θ)^2 )
S_dot  = zeros(1, numel(var_labels));
S_ret  = zeros(1, numel(var_labels));

%  Contadores/df
rep_total     = sum(max(nh_vec-1,0) + (nh_vec>1));  % aprox nº de réplicas (solo informativo)
df_jk         = sum(max(nh_vec - 1, 0));            % grados de libertad JK1
onePSU_count  = sum(nh_vec==1);
rep_count     = 0;

if onePSU_count>0 && isfield(opts,'verbose') && opts.verbose
    warning('JK1:OnePSUStrata', ...
        'Hay %d estrato(s) con una sola PSU: tratados como certeza (no aportan varianza).', onePSU_count);
end

% --- Fallback domain-safe (valores base del total muestral)
X1barA_h0 = X1barA_h;  X1barB_h0 = X1barB_h;
X1barA_m0 = X1barA_m;  X1barB_m0 = X1barB_m;

ybarA_h0 = ybarA_h;    ybarB_h0 = ybarB_h;
ybarA_m0 = ybarA_m;    ybarB_m0 = ybarB_m;

lamBarA_h0 = lamBarA_h;  lamBarB_h0 = lamBarB_h;
lamBarA_m0 = lamBarA_m;  lamBarB_m0 = lamBarB_m;

% helpers de medias "domain-safe"
tolW = 1e-8;
wmean_safe = @(x,w,fallback) ...
    (sum(w)>tolW).*(sum(w.*x)/max(sum(w),eps)) + (sum(w)<=tolW).*fallback;

wbarcols_safe = @(M,w,fallback) ...
    (sum(w)>tolW).*((w'*M)/max(sum(w),eps)) + (sum(w)<=tolW).*fallback;  % devuelve 1xK

% Bucle JK1 por estrato y PSU eliminada
for h = 1:Hn
    psus = psu_by_h{h};
    nh   = nh_vec(h);
    if nh <= 1
        % Estrato de certeza: no genera réplicas
        continue
    end

    idxh   = (hcode==h);
    fac_h  = (nh - 1)/nh;                      % factor JK1 para varianza en estrato h
    scaleh =  nh / (nh - 1);                   % factor de re-escalamiento JK1 (resto de PSUs)

    for c = 1:nh
        rep_count = rep_count + 1;

        % --- Pesar réplica delete-1 (JK1) ---
        in_c = (C==psus(c)) & idxh;            % unidades de la PSU c en el estrato h
        w_r  = w;
        w_r(in_c) = 0;                         % eliminar PSU c
        w_r(idxh & ~in_c) = w(idxh & ~in_c) * scaleh;   % multiplicar pesos de PSUs restantes en h

        % (opcional) Subconjunto para acelerar: sólo unidades con peso > 0
        idx = (w_r > 0);

        yr = y(idx); Xr = X(idx,:); gr = g(idx); 
        Hr = H(idx); Cr = C(idx); wr = w_r(idx); or = o(idx);
        xpr = xp(idx,:);

     

        % --- Partición por grupos ---
        XA = Xr(gr,:);    XB = Xr(~gr,:);
        yA = yr(gr);      yB = yr(~gr);
        wA = wr(gr);      wB = wr(~gr);
        HA = Hr(gr);      HB = Hr(~gr);
        CA = Cr(gr);      CB = Cr(~gr);
        oA = or(gr); oB = or(~gr);
        xpA = xpr(gr,:); xpB = xpr(~gr,:);

        idx_hA = (XA(:,2)==1);  idx_mA = (XA(:,2)==0);
        idx_hB = (XB(:,2)==1);  idx_mB = (XB(:,2)==0);

        xpB = [ones(size(xpB,1),1), xpB]; 
        
        xpA = [ones(size(xpA,1),1), xpA];
        
        lam_r_all = lam_full_all(idx);    
        lamA = lam_r_all(gr);              % toma λ de grupo A en la réplica
        lamB = lam_r_all(~gr);             % toma λ de grupo B en la réplica
        
        lamAB_r_all = lamAB_full_B(idx);   % λ_AB (definido solo en B)
        lamAB = lamAB_r_all(~gr);          % en Neumark, solo se usa para B
        
        lamBarA_h = wmean_safe(lamA(idx_hA), wA(idx_hA), lamBarA_h0);
        lamBarA_m = wmean_safe(lamA(idx_mA), wA(idx_mA), lamBarA_m0);

        lamBarB_h = wmean_safe(lamB(idx_hB), wB(idx_hB), lamBarB_h0);
        lamBarB_m = wmean_safe(lamB(idx_mB), wB(idx_mB), lamBarB_m0);

        lamA_sexo = lamA .* XA(:,2);

        lamB_sexo = lamB .* XB(:,2);
        
        % Matrices de diseño para 2ª etapa 
        XAtil = [ones(size(XA,1),1) XA lamA lamA_sexo];
        XBtil = [ones(size(XB,1),1) XB lamB lamB_sexo];
        
        % Estimar coeficientes por grupo (WLS)
        mcoA = w_ols(yA,XAtil, wA, opts_mco);
        mcoB = w_ols(yB,XBtil, wB, opts_mco);
        
        bA = mcoA.beta;
        
        bB = mcoB.beta;
        
        % Separar bloques: [1,X] y coeficiente de IMR
        bA_X = bA(1:end-2); bB_X = bB(1:end-2);
        bA_l = bA(end-1);     bB_l = bB(end-1);

        bA_lh = bA_l + bA(end); bB_lh = bB_l + bB(end);
        
        % Medias ponderadas por grupo
        X1barA_h = wbarcols_safe([ones(sum(idx_hA),1) XA(idx_hA,:)], wA(idx_hA), X1barA_h0);
        X1barA_m = wbarcols_safe([ones(sum(idx_mA),1) XA(idx_mA,:)], wA(idx_mA), X1barA_m0);
        X1barB_h = wbarcols_safe([ones(sum(idx_hB),1) XB(idx_hB,:)], wB(idx_hB), X1barB_h0);
        X1barB_m = wbarcols_safe([ones(sum(idx_mB),1) XB(idx_mB,:)], wB(idx_mB), X1barB_m0);

        [~,ybarA_hr,~] = means_block([ones(size(XA(idx_hA,:),1),1), XA(idx_hA,:)], yA(idx_hA), lamA(idx_hA), wA(idx_hA));
        [~,ybarA_mr,~] = means_block([ones(size(XA(idx_mA,:),1),1), XA(idx_mA,:)], yA(idx_mA), lamA(idx_mA), wA(idx_mA));
        [~,ybarB_hr,~] = means_block([ones(size(XB(idx_hB,:),1),1), XB(idx_hB,:)], yB(idx_hB), lamB(idx_hB), wB(idx_hB));
        [~,ybarB_mr,~] = means_block([ones(size(XB(idx_mB,:),1),1), XB(idx_mB,:)], yB(idx_mB), lamB(idx_mB), wB(idx_mB));
        
        Delta_hr    = ybarA_hr - ybarB_hr;
        Delta_mr    = ybarA_mr - ybarB_mr;
        
        Selec_S_hr  = lamBarA_h*bA_lh - lamBarB_h*bB_lh;                % término de selección (S)
        Selec_S_mr  = lamBarA_m*bA_l - lamBarB_m*bB_l;  
        
        % --- FÓRMULA Oaxaca–Blinder
        Dxb_hr      = (X1barA_h - X1barB_h) * bA_X;                   % dotaciones con beta_A
        Rb_hr       =  X1barB_h * (bA_X - bB_X);                    % retornos con Xbar_B
        R_main_hr   = Dxb_hr + Rb_hr + Selec_S_hr;                         % R total
        
        Dxb_mr      = (X1barA_m - X1barB_m) * bA_X;                   % dotaciones con beta_A
        Rb_mr       =  X1barB_m * (bA_X - bB_X);                    % retornos con Xbar_B
        R_main_mr   = Dxb_mr + Rb_mr + Selec_S_mr;                         % R total
        
        %  --- Neumark 

        lbarAB_h = wmean_safe(lamAB(idx_hB), wB(idx_hB), lamBarB_h0);
        lbarAB_m = wmean_safe(lamAB(idx_mB), wB(idx_mB), lamBarB_m0);
        
        DF_caracteristica_hr   = ((X1barA_h - X1barB_h) * bA_X) + ((lamBarA_h - lbarAB_h) * bA_lh);
        discriminacion_hr    =  X1barB_h * (bA_X - bB_X) + (lbarAB_h - lamBarB_h) * bA_lh;
        seleccion_hr    =  lamBarB_h * (bA_lh - bB_lh);
        R_neum_hr  = DF_caracteristica_hr + discriminacion_hr + seleccion_hr;                       % lado derecho de Neumark
        
        DF_caracteristica_mr   = ((X1barA_m - X1barB_m) * bA_X) + ((lamBarA_m - lbarAB_m) * bA_l);
        discriminacion_mr    =  X1barB_m * (bA_X - bB_X) + (lbarAB_m - lamBarB_m) * bA_l;
        seleccion_mr    =  lamBarB_m * (bA_l - bB_l);
        R_neum_mr  = DF_caracteristica_mr + discriminacion_mr + seleccion_mr;                       % lado derecho de Neumark     


        main_r = [ybarA_hr, ybarB_hr, Delta_hr,Dxb_hr,Rb_hr, Selec_S_hr, R_main_hr,...
                  DF_caracteristica_hr,discriminacion_hr, seleccion_hr, R_neum_hr,...
                  ybarA_mr, ybarB_mr, Delta_mr,Dxb_mr,Rb_mr, Selec_S_mr, R_main_mr,...
                  DF_caracteristica_mr,discriminacion_mr, seleccion_mr, R_neum_mr];

        if any(isnan(main_r)), pause; end

        % --- Contribuciones por variable ---
        dot_byvar_r = ((X1barA_h - X1barB_h)').*bA_X;   % (Kx1)
        ret_byvar_r = (X1barB_h').*(bA_X - bB_X);     % (Kx1)

        % --- Acumular varianza JK1: sum_h ( (nh-1)/nh * (θ_r - θ)^2 ) ---
        d_main = main_r - main_vals(:).';              % 1 x M
        S_main = S_main + fac_h * (d_main.^2);

        d_dot  = (dot_byvar_r(:) - dot_byvar(:));      % K x 1
        S_dot  = S_dot  + fac_h * (d_dot(:).'.^2);

        d_ret  = (ret_byvar_r(:) - ret_byvar(:));      % K x 1
        S_ret  = S_ret  + fac_h * (d_ret(:).'.^2);

        
            fprintf('JK1 réplica %d de %d  (estrato %d de %d, PSU %d de %d)\n', ...
                    rep_count, rep_total, h, Hn, c, nh);
        
    end
end

%  Errores estándar JK1
se_main = sqrt(S_main);
se_dot  = sqrt(S_dot);
se_ret  = sqrt(S_ret);

%  Intervalos de confianza con t(df)
if df_jk > 0
    tcrit = tinv(1 - opts.alpha/2, df_jk);
else
    % Si df=0 (todos estratos certeza): sin varianza estimable
    tcrit = NaN;
end
lo_main = main_vals - tcrit*se_main;
hi_main = main_vals + tcrit*se_main;

%  Tablas de salida
main_table = table( ...
            ["ybarA_h","ybarB_h","Delta_h","delta_obser_h","delta_noobser_h",...
             "Seleccion_S_h","R_formula_h","Delta_caracteristica_h","discriminación_h","seleccion_R_h","Neumark_R_h",...
             "ybarA_m","ybarB_m","Delta_m","delta_obser_m","delta_noobser_m",...
             "Seleccion_S_m","R_formula_m","Delta_caracteristica_m","discriminación_m","seleccion_R_m","Neumark_R_m"]', ...
    main_vals.', se_main.', lo_main.', hi_main.', ...
    'VariableNames', {'Magnitud','Valor','SE','IC_lo','IC_hi'} );

byvar_table = table(var_labels.', dot_byvar, se_dot.', ret_byvar, se_ret.', ...
    'VariableNames', {'Variable','Dotaciones','SE_Dot','Retornos','SE_Ret'});


% Salida
OUT = struct();
OUT.main_table  = main_table;
OUT.byvar_table = byvar_table;
OUT.coefs = struct( ...
    'betaA_X', bA_X, 'betaB_X', bB_X, ...
    'betaA_lambda', bA_l, 'betaB_lambda', bB_l);
OUT.settings = struct('R',opts.R,'alpha',opts.alpha,'Reference',opts.Reference, ...
                      'VarNames',[ "Constante", vnames, "IMR" ]);

end