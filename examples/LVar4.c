begin
    x := 2;
    while y > x do {
        x := y + 1;
        g := z + 4;
    }
    x := 4;
    g := 2;
    if y > x then
        z := y;
    else
        z := y * g;
    x := z;
end