with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;

procedure Ada3 is
    -- Array of integers
    type IntegerArray is array (Integer range <>) of Integer;

    -- Bounded buffer of integers
    protected type BoundedBuffer(Capacity : Positive) is
        -- Puts a value into the buffer
        --  Blocks if there is not enough space
        entry Put(Value : in Integer);

        -- Gets a value from the buffer (fifo)
        --  Blocks if there is nothing in the buffer
        entry Get(Result : out Integer);

    private
        Data : IntegerArray(1..Capacity);
        ReadPtr : Integer := 1;
        WritePtr : Integer := 1;
        Size : Integer := 0;
    end;

    protected body BoundedBuffer is
        entry Put(Value : in Integer) when Size < Capacity is
        begin
            Data(WritePtr) := Value;
            Size := Size + 1;

            if WritePtr = Capacity then
                WritePtr := 1;
            else
                WritePtr := WritePtr + 1;
            end if;
            Put(Value + 100, 4);
        end;

        entry Get(Result : out Integer) when Size > 0 is
        begin
            Result := Data(ReadPtr);
            Size := Size - 1;

            if ReadPtr = Capacity then
                ReadPtr := 1;
            else
                ReadPtr := ReadPtr + 1;
            end if;
            Put(Result + 200, 4);
        end;
    end;

    -- Global buffer
    Buffer : BoundedBuffer(10);

    -- Tasks for testing
    task type Consumer;
    task type Producer;

    task body Consumer is
        V : Integer;
    begin
        for i in 1..4 loop
            Buffer.Get(V);
            Put(V, 2);
        end loop;
    end;

    task body Producer is
    begin
        for i in 1..4 loop
            Buffer.Put(i);
        end loop;
    end;

    -- 10 instances of each task
    Consumers : array (1..10) of Consumer;
    Producers : array (1..10) of Producer;
begin
    null;
end;
