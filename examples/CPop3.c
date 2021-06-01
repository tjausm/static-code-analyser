begin
    proc root(val n, r, res v) is
        if r < 1 then
            skip;
        else {
            call root(n*n, r-1, v);
        }
    end
    call root(x, 4, y);
end