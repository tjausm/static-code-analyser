begin
    x := 2;
    y := 4;
    x := 1;
    z := y;
    if y > x then
        z := y;
    else
        z := y * y;
    x := z;
end