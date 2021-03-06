with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;

procedure Ada2 is
    -- Barrier which stores an integer
    protected type BarrierInt(MaxTasks : Integer) is
        -- Gets the integer from the barrier
        --  Only returns when release is called
        entry Wait(V : out Integer);

        -- Places an integer into the barrier
        --  Only returns after all tasks have arrived + been released
        entry Release(V : in Integer);

        private
            Value : Integer;
            CanRelease : Boolean := False;
    end;

    -- Barrier code
    protected body BarrierInt is
        entry Wait(V : out Integer) when CanRelease is
        begin
            V := Value;

            -- Reset CanRelease when we're the last task
            if Wait'count = 0 then
                CanRelease := False;
            end if;
        end;

        entry Release(V : in Integer) when Wait'count = MaxTasks is
        begin
            Value := V;
            CanRelease := True;
        end;
    end;

    -- Global barrier instance
    Barrier : BarrierInt(5);

    -- Consumer task type
    task type Consumer;
    task body Consumer is
        V : Integer;
    begin
        -- Get + print integer
        for I in 1..5 loop
            Barrier.Wait(V);
            Ada.Integer_Text_IO.Put(V, 2);
        end loop;
    end;

    -- Consumer instances
    Consumers : array (1..5) of Consumer;
begin
    -- Release 5 times
    for I in 1..5 loop
        Barrier.Release(I);
    end loop;
end;
