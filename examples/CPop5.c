begin
    proc root(val n, r, res v) is
        if r < 1 then
            v := n;
        else {
            call root(n*n, r-1, v);
        }
    end
    proc fact(val l, res f) is
        call factRec(l, l, f);
    end
    proc factRec(val m, t, res b) is
        if t < 2 then
            b := m;
        else {
            call factRec(m*t, r-1, b);
        }
    end
    x := 32;
    call fact(32, y);
    call root(y, 4, z);
end