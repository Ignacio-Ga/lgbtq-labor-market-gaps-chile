function closeFileSafely(fid)
if fid ~= -1
    try
        fclose(fid);
    catch
    end
end
end