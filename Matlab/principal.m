%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ignacio Gálvez y Cristian Valenzuela
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Limpieza de variables y consola
clc
clear
close all
delete(findall(0, 'Type', 'figure'));
delete(findall(0, 'Type', 'uifigure'));
disp('Entorno limpio. Iniciando...');
%% Inicio del código
workRoute = [fileparts(mfilename('fullpath')), filesep];
FolderData = [fullfile(workRoute, 'Datos'), filesep];
FolderTables = [fullfile(workRoute, 'Tablas'), filesep];
if ~exist(FolderTables, 'dir'), mkdir(FolderTables); end
allFolders = genpath(fullfile(workRoute, 'Funciones'));
addpath(allFolders);
rng(14062001)

%% Se crea la barra de progreso
% fig = uifigure;

try
    % Crear barra de progreso
    % d = uiprogressdlg(fig, ...
    %     'Title', 'Estimando la brecha salarial', ...
    %     'Message', 'Cargando los datos', ...
    %     'Cancelable', 'on');
    % d.Interpreter = 'latex';
    % pause(0.5);
%% Encuesta ENSSEX

enssex_encuesta

%% Importación de datos
% [casen,casen_h,casen_m,reg_names] = importar_datos('casen2022.csv','casen2024.csv');

%[casen,casen_h,casen_m,reg_names] = importar_datos('casen2017.csv');

[ine, ine_h, ine_m] = importar_datos_ine([FolderData,'bbdd_ewd_2025v1']);

% %% Primeras regresiones
% mco1
% %% Probit
% probit
% %% Segunda etapa
% heckman
% %% Muestra completa
%   probit2 %Corresponde al probit con la muestra completa
%   heckman2 %Corresponde al heckman con la muestra completa
%%
ine_probit_mco
%% Oaxaca–Blinder
%oaxaca
%% Tablas
tablas

% d.Value = 1;
% d.Message = 'Proceso finalizado con exito';
%Cierre de la barra de progreso en caso de error
catch ME
error_mensaje
end
