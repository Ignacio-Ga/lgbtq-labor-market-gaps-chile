function output = escapeLatex(input)
    output = char(string(input));

    output = strrep(output, '\', '\textbackslash{}');
    output = strrep(output, '&', '\&');
    output = strrep(output, '%', '\%');
    output = strrep(output, '$', '\$');
    output = strrep(output, '#', '\#');
    output = strrep(output, '_', '\_');
    output = strrep(output, '{', '\{');
    output = strrep(output, '}', '\}');
end
