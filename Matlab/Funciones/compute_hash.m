function h = compute_hash(txt)
    % md5 hash mediante Java (sin dependencias externas)
    md = java.security.MessageDigest.getInstance('MD5');
    md.update(uint8(txt));
    h  = typecast(md.digest,'uint8');
    h  = lower(reshape(dec2hex(h,2).',1,[])); % hex string
end