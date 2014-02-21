with Ada.Text_IO;
with Ada.Integer_Text_IO;

procedure Ada2 is
    -- Barrier which stores an integer
    protected type BarrierInt is
        -- Initializes the barrier with the given number of tasks
        procedure Init(M : in Integer);

        -- Gets the integer from the barrier
        --  Only returns when release is called
        entry GetInt(V : out Integer);

        -- Places an integer into the barrier
        --  Only returns after all tasks have arrived + been released
        entry Release(V : in Integer);

        private
            MaxTasks : Integer;
            Value : Integer;
            CanRelease : Boolean;
    end;

    -- Barrier code
    protected body BarrierInt is
        procedure Init(M : in Integer) is
        begin
            MaxTasks := M;
            CanRelease := False;
        end;

        entry GetInt(V : out Integer) when CanRelease is
        begin
            V := Value;

            -- Reset CanRelease when we're the last task
            if GetInt'count = 0 then
                CanRelease := False;
            end if;
        end;

        entry Release(V : in Integer) when GetInt'count = MaxTasks is
        begin
            Value := V;
            CanRelease := True;
        end;
    end;

    -- Global barrier instance
    Barrier : BarrierInt;

    -- Consumer task type
    task type Consumer;
    task body Consumer is
        V : Integer;
    begin
        -- Get + print integer
        for I in 1..5 loop
            Barrier.GetInt(V);
            Ada.Integer_Text_IO.Put(V);
        end loop;
    end;

    -- Consumer instances
    C1, C2, C3, C4 : Consumer;
begin
    -- This task acts as the producer
    Barrier.Init(5);

    for I in 1..5 loop
        Barrier.Release(I);
    end loop;
end;
