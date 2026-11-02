function ax = createMapAxes(GT)
coordinateType = string(GT.Shape(1).CoordinateSystemType);
if coordinateType=="planar"
    projectedCRS = GT.Shape(1).ProjectedCRS;
    if isempty(projectedCRS)
        error('El shapefile es proyectado, pero no se encontró el CRS del archivo .prj.');
    end
    ax = newmap(projectedCRS);
elseif coordinateType=="geographic"
    ax = newmap;
else
    error('Tipo de sistema de coordenadas no reconocido: %s',coordinateType);
end
end