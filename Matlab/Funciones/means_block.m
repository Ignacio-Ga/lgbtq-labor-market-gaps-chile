function [xbar, ybar, lbar] = means_block(X1, y, lambda, w)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ignacio Gálvez
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% medias ponderadas de [1,X] y de y y lambda
idx = ~isnan(y) & ~isnan(w);

xbar = sum(X1 .* w)/sum(w);
ybar = sum(y(idx) .* w(idx)) / sum(w(idx));
lbar = sum(lambda .* w)/sum(w);
end