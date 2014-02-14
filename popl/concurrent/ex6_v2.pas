program exercise6;

(* this barrier uses version numbers -
however this is not required in pascalfc but i've left this here anyway *)

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

(*
The barrier handles multiple uses by using a version field.
Each time the barrier is released, version is incremented so the callers of
wait know that it was THEIR barrier that was released.
*)
monitor Barrier;
    export Wait;

    var version : integer;
    var arrived : integer;
    var waitCondition : condition;

    procedure Wait;
        var myVersion : integer;
    begin
        (* store my version + increment number arrived *)
        myVersion := version;
        arrived := arrived + 1;

        (* wait while the number arrived is less than 5 *)
        while (myVersion = version) and (arrived < 5) do
            delay(waitCondition);

        (* if we are the first, reset variables *)
        if myVersion = version then
        begin
            version := version + 1;
            arrived := 0;
        end;

        (* resume someone else *)
        resume(waitCondition);
    end;

begin
    version := 0;
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
