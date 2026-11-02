function hHtml = openPDFInUI(fig, pdfPath, varargin)
% openPDFInUI(fig, pdfPath)
% Muestra un PDF dentro de un uifigure usando un componente uihtml.
% - No requiere Ghostscript.
% - Requiere que el archivo PDF exista.
%
% Opcionales (Name,Value):
%   'Position' : [x y w h] dentro de la figura (default: usar todo)
%   'Title'    : título opcional para la figura/panel

    ip = inputParser;
    addParameter(ip,'Position',[],@(x)isnumeric(x)&&numel(x)==4);
    addParameter(ip,'Title','',@(x)ischar(x)||isstring(x));
    parse(ip,varargin{:});
    pos   = ip.Results.Position;
    title = ip.Results.Title;

    if ~isvalid(fig) || ~isa(fig,'matlab.ui.Figure') && ~isa(fig,'matlab.ui.Figure')
        error('El primer argumento debe ser un uifigure válido.');
    end
    if ~isfile(pdfPath)
        error('No existe el PDF: %s', pdfPath);
    end

    % Convertir ruta a URI "file:///" robusta (maneja espacios y backslashes)
    uri = char(matlab.net.URI.fromFilename(pdfPath).string);

    % Layout simple
    if isempty(pos)
        gl = uigridlayout(fig,[1 1]); %#ok<NASGU>
        hHtml = uihtml(fig,'HTMLSource','<div>cargando...</div>');
        hHtml.Layout.Row = 1; hHtml.Layout.Column = 1;
    else
        hHtml = uihtml(fig,'Position',pos,'HTMLSource','<div>cargando...</div>');
    end

    % Opcional: título de la figura
    if ~isempty(title), fig.Name = char(title); end

    % Iframe para PDF (100% del contenedor)
    html = sprintf([ ...
        '<!doctype html><html><head><meta charset="utf-8">' ...
        '<style>html,body{margin:0;height:100%%;} iframe{border:0;width:100%%;height:100%%;}</style>' ...
        '</head><body>' ...
        '<iframe src="%s"></iframe>' ...
        '</body></html>'], uri);

    hHtml.HTMLSource = html;
end
