program exercise6;

monitor Text;
    export Print;

    procedure Print(id : integer; textId : integer);
    begin
        write(id);
        case textid of
            1:  writeln(' entering barrier');
            2:  writeln(' leaving barrier');
            3:  writeln(' entering barrier (try 2)');
            4:  writeln(' leaving barrier (try 2)');
        end;
    end;
end;

monitor Barrier;
    export Wait;

    var arrived : integer;
    var waitCondition : condition;

    procedure Wait;
    begin
        (*  increment number arrived *)
        arrived := arrived + 1;

        (* wait while the number arrived is less than 5 *)
        if arrived < 5 then
            delay(waitCondition);

        (* decrement number arrived and resume one more *)
        arrived := arrived - 1;
        resume(waitCondition);
    end;
begin
    arrived := 0;
end;

process type P(id : integer);
begin
    (* attempt one *)
    Text.Print(id, 1);
    Barrier.Wait;
    Text.Print(id, 2);

    (* attempt two *)
    Text.Print(id, 3);
    Barrier.Wait;
    Text.Print(id, 4);
end;

var i : integer;
var processes : array[1..5] of P;
begin
    cobegin
        for i := 1 to 5 do
            processes[i](i);
    coend
end.
