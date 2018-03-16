package body Spawn_Manager.Server is

   -----------
   -- Spawn --
   -----------
   Wait_Options_Map : constant array (Wait_Options) of Integer := (8, 1, 2);

   function Spawn (Request : Spawn_Request) return Spawn_Response is
      Args   : GNAT.OS_Lib.Argument_List_Access;
      Cursor : Integer;
   begin
      return Ret : Spawn_Response do
         Args := new GNAT.OS_Lib.Argument_List (1 .. Integer (Request.Args.Length));
         Cursor := Args.all'First;

         for I of  Request.Args loop
            Args.all (Cursor) := new String'(To_String (I));
            Cursor := Cursor + 1;
         end loop;

         case Request.Spawn_Type is
            when Spawn_1 =>
               GNAT.OS_Lib.Spawn
                 (Program_Name => To_String (Request.Program_Name),
                  Args         => Args.all,
                  Success      => Ret.Success);
            when Spawn_2 =>
               Ret.Return_Code := GNAT.OS_Lib.Spawn
                 (Program_Name => To_String (Request.Program_Name),
                  Args         => Args.all);
            when Spawn_3 =>
               GNAT.OS_Lib.Spawn
                 (Program_Name => To_String (Request.Program_Name),
                  Args         => Args.all,
                  Output_File  => To_String (Request.Output_File),
                  Success      => Ret.Success,
                  Return_Code  => Ret.Return_Code,
                  Err_To_Out   => Request.Err_To_Out);
            when Non_Blocking_Spawn_1 =>
               Ret.Pid := GNAT.OS_Lib.Non_Blocking_Spawn
                 (Program_Name => To_String (Request.Program_Name),
                  Args         => Args.all);

            when Non_Blocking_Spawn_2 =>
               Ret.Pid := GNAT.OS_Lib.Non_Blocking_Spawn
                 (Program_Name => To_String (Request.Program_Name),
                  Args         => Args.all,
                  Output_File  => To_String (Request.Output_File),
                  Err_To_Out   => Request.Err_To_Out);
            when Wait_Process =>
               GNAT.OS_Lib.Wait_Process (Ret.Pid, Ret.Success);
            when Waitpid =>
               declare
                  function I_Waitpid
                    (Pid  : Process_Id;
                     Stat : not null access Integer;
                     Options : Integer) return Process_Id;
                  pragma Import (C, I_Waitpid, "waitpid");
               begin
                  Ret.Pid := I_Waitpid (Request.Pid, Ret.Return_Code'Access, Wait_Options_Map (Request.Options));
               end;
            when Terminate_Server =>
               raise Program_Error with  "Shall never happend";
         end case;
         Free (Args);
      end return;
   end Spawn;
end Spawn_Manager.Server;
