program exercise3new;

(* Maximum number of processes *)
const maxProcesses = 3;

(* Process ID whos turn it is *)
var turn : integer;

process type P(pNumber : integer);
    var c : char;
begin
    c := chr(pNumber + ord('A'));

    repeat
        while turn <> pNumber do
            null;

        write(c, c, c, c);
        turn := (turn + 1) mod maxProcesses;
    forever
end;

var i : integer;
var localP : array[1..maxProcesses] of P;
begin
    cobegin
        for i := 1 to maxProcesses do
            localP[i](i - 1);
    coend;
end.
