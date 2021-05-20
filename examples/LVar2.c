begin
    proc deadProc(val x1, res y1) is
        y1 := 99;
    end
    proc liveProc(val x2, res y2) is
        y2 := 100;
    end
    call liveProc(3, y);
    x := y + 2;
end