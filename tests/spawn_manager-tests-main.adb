with GNAT.Exception_Traces;
with GNAT.Traceback.Symbolic;
with Ada.Text_IO; use Ada.Text_IO;
with Spawn_Manager.Client; use Spawn_Manager.Client;
with GNAT.Source_Info;
procedure Spawn_Manager.Tests.Main is
   Args         : GNAT.OS_Lib.Argument_List (1 .. 1);
   Arg2         : GNAT.OS_Lib.Argument_List (1 .. 0);
   Ls           : GNAT.OS_Lib.String_Access;
   Proj_Version : constant String := $VERSION;
   Ok           : Boolean := False;
begin
   Debug := True;
   GNAT.Exception_Traces.Set_Trace_Decorator (GNAT.Traceback.Symbolic.Symbolic_Traceback'Access);
   GNAT.Exception_Traces.Trace_On (GNAT.Exception_Traces.Every_Raise);
   pragma Warnings (Off);
   if Proj_Version /= Version then
      raise Program_Error with "Version missmatch: Proj_Version=" & Proj_Version & ", Version=" & Version & ".";
   end if;
   pragma Warnings (On);
   Ls := GNAT.OS_Lib.Locate_Exec_On_Path ("ls");
   Args (1) := new String'("-l");
   Put_Line (GNAT.Source_Info.Source_Location);
   declare
      Return_Code  : Integer;
      Success      : Boolean;
      M            : Controler; pragma Unreferenced (M);
      P            : Process_Id; pragma Unreferenced (P);
      Status       : Status_Kind; pragma Warnings (Off, Status);
   begin
      Put_Line (GNAT.Source_Info.Source_Location);
      Client.Spawn (Ls.all, Args, Success);
      Put_Line (GNAT.Source_Info.Source_Location);
      Put_Line ("-----< Success:" & Success'Img & " >--------------"& GNAT.Source_Info.Source_Location);
      Ok := Success;

      Return_Code := Client.Spawn (Ls.all, Arg2);
      Put_Line ("-----< Return_Code:" & Return_Code'Img & " >--------------"& GNAT.Source_Info.Source_Location);
      Ok := Ok or (Return_Code = 0);

      Client.Spawn (Ls.all, Arg2, "test.out", Success, Return_Code);
      declare
         F : Ada.Text_IO.File_Type;
      begin
         Open (F, In_File, "test.out");
         while not End_Of_File (F) loop
            Put_Line ("!" & Get_Line (F));
         end loop;
      end;
      Put_Line ("-----< Success:" & Success'Img & " >--------------" & GNAT.Source_Info.Source_Location);
      Put_Line ("-----< Return_Code:" & Return_Code'Img & " >--------------" & GNAT.Source_Info.Source_Location);
      Ok := Ok or Success;
      Ok := Ok or (Return_Code = 0);


      P := Waitpid (Invalid_Pid, Status => Status, Options => WNOHANG);
   end;
   Put_Line (GNAT.Source_Info.Source_Location);

   if not Ok then
      raise Program_Error with "test failed";
   end if;
   Put_Line (GNAT.Source_Info.Source_Location);
end Spawn_Manager.Tests.Main;
