begin
    deadExp1 := 1;
    deadExp2 := 3;
    liveExp1 := 2;
    liveExp2 := 4;
    while liveExp1 > 0 do{
        liveExp2 := liveExp2 - 1;
        liveExp1 := liveExp1 - 1;
    }
end