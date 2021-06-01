begin
    deadVar1 := 1;
    deadVar2 := 3;
    liveVar1 := 2;
    liveVar2 := 4;
    while liveVar1 > 0 do{
        deadVar1 := liveVar2 - 1;
        deadVar1 := liveVar1 - 1;
    }
end