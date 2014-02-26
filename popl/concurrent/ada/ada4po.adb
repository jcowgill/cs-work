with Ada.Text_IO;   use Ada.Text_IO;

procedure Ada4Po is
    -- Object which controlls access to a readers / writers buffer
    --  Priority to waiting writers
    protected type ReadersWritersController is
        entry     EnterRead;
        entry     EnterWrite;
        procedure LeaveRead;
        procedure LeaveWrite;

    private
        NumReaders : Integer := 0;      -- Number of readers in lock
        Writing    : Boolean := False;  -- Someone if writing
    end;

    protected body ReadersWritersController is
        entry EnterRead when (not Writing and EnterWrite'count = 0) is
        begin
            NumReaders := NumReaders + 1;
        end;

        entry EnterWrite when (not Writing and NumReaders = 0) is
        begin
            Writing := True;
        end;

        procedure LeaveRead is
        begin
            NumReaders := NumReaders - 1;
        end;

        procedure LeaveWrite is
        begin
            Writing := False;
        end;
    end;

    -- Common controller
    Controller : ReadersWritersController;

    -- Testing tasks
    task type Reader;
    task type Writer;

    task body Reader is
    begin
        for I in 1..10 loop
            Put_Line("Before Read");
            Controller.EnterRead;
            Put_Line("In Read");
            Controller.LeaveRead;
        end loop;
    end;

    task body Writer is
    begin
        for I in 1..10 loop
            Put_Line("Before Write");
            Controller.EnterWrite;
            Put_Line("In Write");
            Controller.LeaveWrite;
            delay 0.0;
        end loop;
    end;

    -- Task instances
    Readers : array (1..5) of Reader;
    Writers : array (1..2) of Writer;
begin
    null;
end;
