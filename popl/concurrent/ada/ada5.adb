with Ada.Numerics.Discrete_Random;

procedure Ada5 is
    -- Number of philosophers
    NumPhilosophers : Constant Integer := 5;

    -- Random integer generator
    type RandRange is range 0..3;
    package RandInt is new Ada.Numerics.Discrete_Random(RandRange);

    -- One task controls each fork on the table
    task type Fork is
        entry PickUp;
        entry PutDown;
    end;

    task body Fork is
        InUse : Boolean := False;
    begin
        loop
            select
                when not InUse =>
                    accept PickUp do
                        InUse := True;
                    end;
            or
                accept PutDown;
                InUse := False;
            or
                terminate;
            end select;
        end loop;
    end;

    -- List of forks
    Forks : array (1..NumPhilosophers) of Fork;

    -- Philosopher tasks
    task type Philosopher is
        entry Init(newId : in Integer);
    end;

    task body Philosopher is
        id, f1, f2 : Integer;
        seed : RandInt.Generator;
    begin
        -- Get philosopher id
        accept Init(newId : in Integer) do
            id := newId;
        end;

        -- Calculate fork ids to use
        if id = NumPhilosophers then
            f1 := id - 1;
            f2 := id;
        else
            f1 := id;
            f2 := id + 1;
        end if;

        -- Reset random number generator
        RandInt.Reset(seed);

        -- Start eating!
        for i in 1..10 loop
            -- Pick up forks
            Forks(f1).PickUp;
            Forks(f2).PickUp;

            -- Wait a bit
            delay Duration(RandInt.Random(seed));

            -- Put down forks
            Forks(f1).PutDown;
            Forks(f2).PutDown;
        end loop;
    end;

    -- Philosopher initialization
    Philosophers : array (1..NumPhilosophers) of Philosopher;
begin
    -- Initialize all philosophers
    for i in 1..NumPhilosophers loop
        Philosophers(i).Init(i);
    end loop;
end;
