program exercise5;

const numPhilosophers = 3;

var msgLock : semaphore;
var forks : array[1..numPhilosophers] of semaphore;

(* Philosopher process *)
process type P(leftFork : integer);
    var fork1 : integer;
    var fork2 : integer;
    var i : integer;
begin
    (* Determine fork order *)
    if leftFork = numPhilosophers then
    begin
        fork1 := numPhilosophers - 1;
        fork2 := numPhilosophers;
    end
    else
    begin
        fork1 := leftFork;
        fork2 := leftFork + 1;
    end;

    (* Enter main loop *)
    for i := 1 to 5 do
    begin
        wait(msgLock);
            write(leftFork);
            writeln(': Thinking');
        signal(msgLock);
        wait(forks[fork1]);
        wait(forks[fork2]);

        wait(msgLock);
            write(leftFork);
            writeln(': Eating');
        signal(msgLock);
        signal(forks[fork1]);
        signal(forks[fork2]);
    end
end;

(* Start the philospher processes *)
var i : integer;
var philosophers : array[1..numPhilosophers] of P;
begin
    (* Initialize all semaphores to 1 *)
    signal(msgLock);
    for i := 1 to numPhilosophers do
        signal(forks[i]);

    (* Run philosophers *)
    cobegin
        for i := 1 to numPhilosophers do
            philosophers[i](i);
    coend
end.
