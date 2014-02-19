program exercise3;

(* Each flag says that that process can continue execution *)
var flagA, flagB, flagC : boolean;

process PA;
begin
    repeat
        while not flagA do
            null;

        write('AAAA');
        flagA := false;
        flagB := true;
    forever
end;

process PB;
begin
    repeat
        while not flagB do
            null;

        write('BBBB');
        flagB := false;
        flagC := true;
    forever
end;

process PC;
begin
    repeat
        while not flagC do
            null;

        write('CCCC');
        flagC := false;
        flagA := true;
    forever
end;

begin
    flagA := true;
    cobegin
        PA;
        PB;
        PC
    coend;
end.
