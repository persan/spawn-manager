pragma Ada_2012;

with Ada.Finalization;
with GNAT.Semaphores;
package Spawn_Manager.Client is
--  This package provides tasksafe variants of the GNAT.OS_Lib Spawn routines.
--  Calls to methods in this package is strictly serialized and calling
--  tasks will be blocked if another task is performing an operation.

   type Controler
     (Server_Name : not null access constant String := Default_Server_Name'Access) is limited private with
     Unreferenced_Objects => True;
   --  A singleton providing initialization and finalisation of the link
   --  to the spawn server.
   --  An object of this type must be created before any routines in
   --  this package are called.
   --  and it may not be declared in such a way that the object creation occures
   --  during elaboration of a shared library.

   procedure Spawn
     (Program_Name : String;
      Args         : GNAT.OS_Lib.Argument_List;
      Success      : out Boolean);

   function Spawn
     (Program_Name : String;
      Args         : GNAT.OS_Lib.Argument_List) return Integer;

   procedure Spawn
     (Program_Name : String;
      Args         : GNAT.OS_Lib.Argument_List;
      Output_File  : String;
      Success      : out Boolean;
      Return_Code  : out Integer;
      Err_To_Out   : Boolean := True);

   function Non_Blocking_Spawn
     (Program_Name : String;
      Args         : Argument_List) return Process_Id;

   function Non_Blocking_Spawn
     (Program_Name : String;
      Args         : Argument_List;
      Output_File  : String;
      Err_To_Out   : Boolean := True) return Process_Id;

   procedure Wait_Process (Pid : out Process_Id; Success : out Boolean);

   --  ==================================================================
   --  Extra functions from sys/wait.h
   --  ==================================================================

   function Waitpid
     (Pid      : Process_Id;
      Status   : out Status_Kind;
      Options  : Wait_Options) return Process_Id;
   --  return status information pertaining to one of the caller's
   --  child processes. Various options permit status information
   --  to be obtained for child processes that have terminated or stopped.
   --  If status information is available for two or more child processes,
   --  the order in which their status is reported is unspecified.

private

   type Key_Type (Sema : not null access GNAT.Semaphores.Binary_Semaphore) is
     new Ada.Finalization.Limited_Controlled with null record;
   pragma Unreferenced_Objects (Key_Type);
   overriding procedure Initialize (Object : in out Key_Type);
   overriding procedure Finalize   (Object : in out Key_Type);

   type Controler
     (Server_Name : not null access constant String := Default_Server_Name'Access)
     is new Ada.Finalization.Limited_Controlled with null record;

   overriding procedure Initialize (Object : in out Controler);
   overriding procedure Finalize   (Object : in out Controler);
end Spawn_Manager.Client;
