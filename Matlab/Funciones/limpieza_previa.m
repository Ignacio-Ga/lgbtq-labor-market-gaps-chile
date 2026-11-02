%% ========================================================================
% Limpiar previews de ejecuciones anteriores
% ========================================================================

previewFolder = fullfile(FolderData, '_latex_preview');

if isfolder(previewFolder)
    try
        rmdir(previewFolder, 's');
    catch ME
        warning( ...
            'No se pudo limpiar la carpeta de previews anteriores: %s', ...
            ME.message);
    end
end

if ~isfolder(previewFolder)
    mkdir(previewFolder);
end