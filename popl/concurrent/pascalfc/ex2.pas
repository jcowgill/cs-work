program exercise2;

var C : integer;
var flag1, flag2, p2Token : boolean;

process P1;
    var i : integer;
begin
    for i := 1 to 20 do
    begin
        flag1 := true;
        p2Token := false;

        while flag2 and (not p2Token) do
            null;

        C := C + 1;
        flag1 := false;
    end
end;

process P2;
    var i : integer;
begin
    for i := 1 to 20 do
    begin
        flag2 := true;
        p2Token := true;

        while flag1 and p2Token do
            null;

        C := C + 1;
        flag2 := false;
    end
end;

begin
    C := 0;
    cobegin
        P1;
        P2
    coend;
    write(C);
end.
