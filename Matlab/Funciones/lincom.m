function T = lincom(beta, V, L, dof, row_labels)
% Calcula coef, EE, t y p para combinaciones lineales theta = L*beta.
% beta: (k x 1), V: (k x k), L: (m x k), dof: grados de libertad (escala t)
% row_labels: cellstr de longitud m 
    if nargin < 5 || isempty(row_labels)
        row_labels = arrayfun(@(i) sprintf('L%d',i), 1:size(L,1), 'UniformOutput', false);
    end
    theta  = L * beta(:);                % m x 1
    Var_th = diag(L * V * L.');          % m x 1
    se_th  = sqrt(Var_th);
    tstat  = theta ./ se_th;
    pval   = 2 * tcdf(-abs(tstat), dof);

    T = table(theta, se_th, tstat, pval, ...
              'VariableNames', {'coef','se','t','p'});
    T.Properties.RowNames = row_labels;
end
