function x = numericVariable(v)
    if isnumeric(v) || islogical(v)
        x = double(v);

    elseif iscategorical(v)
        x = str2double(string(v));

    elseif isstring(v) || iscellstr(v) || ischar(v)
        x = str2double(string(v));

    else
        error('No fue posible convertir una variable a formato numérico.');
    end
end