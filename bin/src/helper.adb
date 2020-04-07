with Ada.Text_IO;
with Spawn_Manager;
with Ada.Command_Line;
procedure Helper is
   use Ada.Text_IO;
begin
   if Spawn_Manager.Version /= $VERSION then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      Put_Line ("Version Missmatch : Code=""" & Spawn_Manager.Version & """ /=  Lib=""" & $VERSION & """");
   end if;
   for I in 1 .. Ada.Command_Line.Argument_Count loop
      if (Ada.Command_Line.Argument (I) = "-v") or else
        (Ada.Command_Line.Argument (I) = "--version")
      then
         Put_Line (Spawn_Manager.Version);
      end if;
   end loop;

end Helper;
