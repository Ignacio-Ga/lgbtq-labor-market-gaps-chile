function C = panel_by_sex(S, varlist, groups_order,wmean,wse_mean)
% --- función para obtener matriz (V x 4) de celdas con "media" y "(EE)" para un sexo
    G = numel(groups_order);
    V = numel(varlist);
    C = strings(V, G);
    for j = 1:V
        vname = varlist{j};
        for g = 1:G
            gname = groups_order{g};
            idx = (S.grupo==gname);
            x   = S.(vname)(idx);
            w   = S.expr_osig(idx);
            ok  = ~isnan(x) & ~isnan(w) & (w>0);
            if any(ok)
                ww = w(ok); ww = ww./sum(ww);
                m  = wmean(x(ok), ww);
                se = wse_mean(x(ok), ww);
                C(j,g) = sprintf('%.2f\\\\(%.2f)', m, se);  % doble línea: media y (EE)
            else
                C(j,g) = '';
            end
        end
    end
end