pragma Ada_2012;

with GNAT.OS_Lib; use GNAT.OS_Lib;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
private with Interfaces;
package Spawn_Manager is
--  This package provides tasksafe variants of the GNAT.OS_Lib Spawn routines.

   Version : constant String := "1.2.2";

   Default_Server_Name : aliased constant String := "gnat-os_lib-spawn-manager-server";

   --  The default name of the spawn server.

   type Wait_Options is
     (WCONTINUED,
      --  The waitpid() function shall report the status of any continued
      --  child process specified by pid whose status has not been reported
      --  since it continued from a job control stop.
      WNOHANG,
      --  The waitpid() function shall not suspend execution of the calling
      --  thread if status is not immediately available for one of the
      --  child processes specified by pid.
      WUNTRACED
      --  The status of any child processes specified by pid
      --  that are stopped, and whose status has not yet been reported
      --  since they stopped, shall also be reported to the
      --  requesting process.
     );

   type Status_Kind is private;

   function WIFEXITED (Stat_Val : Status_Kind) return Boolean;
   --  True value if status was returned for a
   --  child process that terminated normally.

   function WEXITSTATUS (Stat_Val : Status_Kind) return Integer;
   --  If the value of WIFEXITED is True, this
   --  evaluates to the low-order 8 bits of the status argument that
   --  the child process passed to _exit() or exit(),
   --  or the value the child process returned from main().

   --# function WIFSIGNALED (Stat_Val : Status_Kind) return Boolean;
   --  True if status was returned for a child process
   --  that terminated due to the receipt of a signal that was not caught
   --  (see <signal.h>).

   --# function WTERMSIG (Stat_Val : Status_Kind) return Boolean;
   --  If  WIFSIGNALED(stat_val) is True, this evaluates to
   --  the number of the signal that caused
   --  the termination of the child process.

   --#  function WIFSTOPPED (Stat_Val : Status_Kind) return Boolean;
   --  True if status was returned for a child process that is
   --  currently stopped.

   --# function WSTOPSIG (Stat_Val : Status_Kind) return Integer;
   --  If the value of WIFSTOPPED(stat_val) is True, this evaluates
   --  to the number of the signal that caused the child process to stop.

   function WIFCONTINUED (Stat_Val : Status_Kind) return Boolean;
   --  True if status was returned for a child process that has
   --  continued from a job control stop.


private

   package Unbounded_String_Vectors is new
     Ada.Containers.Vectors (Natural, Unbounded_String);
   type Request_Kind is
     (Terminate_Server,
      Spawn_1, Spawn_2, Spawn_3,
      Non_Blocking_Spawn_1, Non_Blocking_Spawn_2,
      Wait_Process, Waitpid);

   type Spawn_Request is record
      Id           : Long_Integer := 0;
      Spawn_Type   : Request_Kind   := Spawn_1;
      Program_Name : Unbounded_String := Null_Unbounded_String;
      Args         : Unbounded_String_Vectors.Vector := Unbounded_String_Vectors.Empty_Vector;
      Output_File  : Unbounded_String := Null_Unbounded_String;
      Err_To_Out   : Boolean := True;
      Version      : Unbounded_String := To_Unbounded_String (Spawn_Manager.Version);
      Pid          : Process_Id;
      Options      : Wait_Options;
   end record;

   type Spawn_Response is record
      Success      : Boolean := False;
      Return_Code  : aliased Integer := 0;
      Pid          : Process_Id := Invalid_Pid;
      Message      : Unbounded_String := Null_Unbounded_String;
   end record;
   function Is_Exit_Message (Request : Spawn_Request) return Boolean;
   type Status_Kind is new Interfaces.Unsigned_32;
   Debug : Boolean := False;
end Spawn_Manager;
