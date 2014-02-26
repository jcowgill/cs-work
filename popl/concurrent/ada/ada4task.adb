with Ada.Text_IO;   use Ada.Text_IO;

procedure Ada4Task is
    -- Task which acts as the controller for a readers / writers buffer
    --  Priority to waiting writers
    task type ReadersWritersController is
        entry EnterRead;
        entry EnterWrite;
        entry LeaveRead;
        entry LeaveWrite;
    end;

    task body ReadersWritersController is
        NumReaders : Integer := 0;      -- Number of readers in lock
        Writing    : Boolean := False;  -- Someone if writing
    begin
        loop
            select
                when (not Writing and EnterWrite'count = 0) =>
                    accept EnterRead do
                        NumReaders := NumReaders + 1;
                    end;
            or
                when (not Writing and NumReaders = 0) =>
                    accept EnterWrite do
                        Writing := True;
                    end;
            or
                accept LeaveRead;
                NumReaders := NumReaders - 1;
            or
                accept LeaveWrite;
                Writing := False;
            or
                terminate;
            end select;
        end loop;
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
