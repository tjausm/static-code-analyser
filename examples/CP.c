begin
    proc plus(val x1, res y1) is
        y1 := x1 + x1;
    end
    proc minus(val x2, res y2) is
        y2 := x2 - x2;
    end
    call plus(3, y);
    call minus(3, z);
    x := y + z;
end