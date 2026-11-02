% --- Cierre de la barra de progreso y reporte enriquecido en caso de error
    % 1) Cerrar la barra si existe
    if exist('d','var') && ~isempty(d) && isvalid(d)
        close(d);
    end

    % 2) Construir un resumen claro del stack (rastro)
    stk = ME.stack;                         % array de struct con fields: file, name, line
    n   = numel(stk);
    lines = strings(n,1);

    for k = 1:n
        [~,f,ext] = fileparts(stk(k).file);
        nameStr = stk(k).name;
        if isempty(nameStr)
            nameStr = '(script)';           % cuando el origen es un script
        end
        % Etiquetas: Origen = lugar exacto del error; luego niveles de llamada
        prefix = tern(k==1, 'Origen', sprintf('Nivel %d',k-1));
        lines(k) = sprintf('%s: %s%s > %s (línea %d)', ...
            prefix, f, ext, nameStr, stk(k).line);
    end

    % 3) Destacar también el "principal" (último en el stack)
    if n >= 2
        [~,f0,e0] = fileparts(stk(end).file);
        principalInfo = sprintf('Principal: %s%s > %s (línea %d)', ...
            f0, e0, tern(isempty(stk(end).name),'(script)',stk(end).name), stk(end).line);
    else
        principalInfo = '';
    end

    % 4) Mensaje resumido para uialert
    resumenStack = strjoin(lines, newline);
    baseMsg = ME.message;
    if ~isempty(principalInfo)
        resumenStack = sprintf('%s\n%s', resumenStack, principalInfo);
    end
    alertMsg = sprintf('%s\n\n%s', baseMsg, resumenStack);

    % uialert no es ideal para mensajes muy largos; en tal caso, recorta y remite a consola
    MAX_CHARS = 2000;
    if strlength(alertMsg) > MAX_CHARS
        alertMsg = sprintf(['%s\n\n(El detalle completo del error y el rastro se ' ...
                            'imprimió en la consola.)'], baseMsg);
    end

    % 5) Mostrar alerta modal
    try
        uialert(fig, alertMsg, 'Error', 'Icon', 'error');
    catch
        % Si no hay UI disponible, imprime en rojo en consola
        fprintf(2, '\n[ERROR] %s\n\n%s\n', baseMsg, resumenStack);
    end

    % 6) Reporte extendido (con causas) a la consola, con hipervínculos
    %    (permite abrir el archivo/línea haciendo clic en el link).
    try
        rep = getReport(ME, 'extended', 'hyperlinks', 'on');
        fprintf(2, '\n===== REPORTE EXTENDIDO =====\n%s\n=============================\n', rep);
    catch
        % fallback mínimo
        fprintf(2, '\n[ERROR] %s\n', baseMsg);
    end

    % 7) Abrir en el Editor la línea exacta del origen del error
    try
        if ~isempty(stk) && usejava('desktop') && exist(stk(1).file,'file')
            opentoline(stk(1).file, stk(1).line);
        end
    catch
        % Ignorar si no hay Desktop 
    end


% --- 
function out = tern(cond, a, b)
    if cond, out = a; else, out = b; end
end
