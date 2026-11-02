function [n_str, neff_str] = count_by_group(S, groups_order)
% --- Conteo N ponderado y tamaño efectivo por grupo
% Devuelve dos arreglos de strings: 
%   n_str     = N ponderado 
%   neff_str  = N efectivo 

    G = numel(groups_order);
    n_str    = strings(1,G);
    neff_str = strings(1,G);

    for g = 1:G
        gname = groups_order{g};
        idx   = (S.grupo==gname);
        w     = S.expr_osig(idx);  % factores de expansión
        ok    = ~isnan(w) & (w>0);

        if any(ok)
            % N ponderado (suma de los pesos de expansión)
            

            S.i = double(S.grupo==gname);

            N_eff = sum(S.i);
             
            % Tamaño muestral efectivo
            N_weighted = sum(S.i .* S.expr_osig);

            % Guardar como strings para tabla
            n_str(g)    = sprintf('%.0f', N_weighted);
            neff_str(g) = sprintf('%.0f', N_eff);
        else
            n_str(g)    = '';
            neff_str(g) = '';
        end
    end
end