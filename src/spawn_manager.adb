package body Spawn_Manager is

   ---------------------
   -- Is_Exit_Message --
   ---------------------

   function Is_Exit_Message (Request : Spawn_Request) return Boolean is
   begin
      return Request.Spawn_Type = Terminate_Server;
   end Is_Exit_Message;


   function WIFEXITED (Stat_Val : Status_Kind) return Boolean is
   begin
      return (Stat_Val and 16#FF_FF_FF_00#) = 0;
   end WIFEXITED;

   function WEXITSTATUS (Stat_Val : Status_Kind) return Integer is
   begin
      return Integer (Shift_Right (Stat_Val and 16#0000_FF_00#, 8));
   end WEXITSTATUS;

--     function WIFSIGNALED (Stat_Val : Status_Kind) return Boolean is
--        pragma Unreferenced (Stat_Val);
--     begin
--        return False ; -- (((Stat_Val and 16#7F#) +1);
--     end WIFSIGNALED;

--     function WTERMSIG (Stat_Val : Status_Kind) return Boolean is
--        pragma Unreferenced (Stat_Val);
--     begin
--        return False;
--     end WTERMSIG;

--     function WIFSTOPPED (Stat_Val : Status_Kind) return Boolean is
--     begin
--        return (Stat_Val and 16#FF#) = 16#7F#;
--     end WIFSTOPPED;
--     pragma Unreferenced (WIFSTOPPED);

--     function WSTOPSIG (Stat_Val : Status_Kind) return Integer is
--        pragma Unreferenced (Stat_Val);
--     begin
--        return 0;
--     end WSTOPSIG;
--     pragma Unreferenced (WSTOPSIG);

   function WIFCONTINUED (Stat_Val : Status_Kind) return Boolean is
   begin
      return Stat_Val = 16#FF_FF#;
   end WIFCONTINUED;


end Spawn_Manager;
