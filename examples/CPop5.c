begin
    proc conPass1(val n, res v) is
        p := n * n;
        call conPass2(n, v);
    end
    proc conPass2(val n1, res v1) is
        u := n1 + n1;
        call doubleUp(n1, v1);
    end
    proc doubleUp(val n2, res v2) is
        v2 := n2 + n2;
    end
    con := 7;
    call conPass1(con, r);
end