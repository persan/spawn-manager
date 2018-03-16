with GNAT.Sockets; use GNAT.Sockets;
with GNAT.Random_Numbers;
with Ada.Directories; use Ada.Directories;
with Ada.Command_Line;
with GNAT.IO;
package body Spawn_Manager.Client is

   Initialized : Boolean := False;
   Address     : Sock_Addr_Type;
   Socket      : Socket_Type;
   Server      : Socket_Type;
   Channel     : Stream_Access;
   Id          : Long_Integer;
   Lock        : aliased GNAT.Semaphores.Binary_Semaphore (True, GNAT.Semaphores.Default_Ceiling);

   function Image (P : GNAT.Sockets.Port_Type) return String;
   function Image (P : GNAT.Sockets.Port_Type) return String is
      Ret : constant String := P'Img;
   begin
      return Ret (Ret'First + 1 .. Ret'Last);
   end Image;

   overriding procedure Initialize (Object : in out Controler) is
      Args     : GNAT.OS_Lib.Argument_List_Access := new GNAT.OS_Lib.Argument_List (1 .. 3);
      Pid      : GNAT.OS_Lib.Process_Id;
      Exe      : GNAT.OS_Lib.String_Access;
      Gen      : GNAT.Random_Numbers.Generator;
      Key      : Key_Type (Lock'Access); pragma Unreferenced (Key);
   begin
      if Initialized then
         raise Program_Error with "only one controler per executabel is allowed";
      end if;

      GNAT.Random_Numbers.Reset (Gen);
      Id := GNAT.Random_Numbers.Random (Gen);

      declare
         Cmd      : constant String := Ada.Command_Line.Command_Name;
         Dir      : constant String := Containing_Directory (Cmd);
         Tmp      : constant String := Current_Directory;
      begin

         Set_Directory (Dir);

         declare
            New_Name : constant String := Compose (Current_Directory, Object.Server_Name.all);

         begin
            GNAT.IO.Put_Line (New_Name);
            if Ada.Directories.Exists (New_Name) then
               Free (Exe);
               Exe := new String'(New_Name);
            end if;
         end;
         Set_Directory (Tmp);
      end;
      if Exe = null then
         Exe := GNAT.OS_Lib.Locate_Exec_On_Path (Object.Server_Name.all);
      end if;

      if Exe = null then
         raise Program_Error with "Unable to locate server '" & Object.Server_Name.all & "'";
      end if;
      Address.Addr := Loopback_Inet_Addr;
      Address.Port := Any_Port;
      Create_Socket (Server);
      Bind_Socket (Server, Address);
      Listen_Socket (Server);

      Args (1) := new String'(Image (Get_Socket_Name (Server).Addr));
      Args (2) := new String'(Image (Get_Socket_Name (Server).Port));
      Args (3) := new String'(Id'Img);
      if Debug then
         GNAT.IO.Put (Exe.all);
         for I of Args.all loop
            GNAT.IO.Put (" " & I.all);
         end loop;
         GNAT.IO.Put_Line ("");
      end if;
      Pid := GNAT.OS_Lib.Non_Blocking_Spawn (Exe.all, Args.all);
      if Pid = GNAT.OS_Lib.Invalid_Pid then
         Close_Socket (Server);
         raise Program_Error with "Unable to launch server '" & Exe.all & "'";
      end if;
      GNAT.OS_Lib.Free (Args);
      GNAT.OS_Lib.Free (Exe);
      Accept_Socket (Server, Socket, Address);
      Channel := Stream (Socket, Address);
      Initialized := True;
   end Initialize;

   overriding procedure Finalize   (Object : in out Controler) is
      pragma Unreferenced (Object);
      Command  : Spawn_Request;
      Key      : Key_Type (Lock'Access); pragma Unreferenced (Key);
   begin
      if Initialized then
         Command.Id := Id;
         Command.Spawn_Type := Terminate_Server;
         Command.Program_Name := Null_Unbounded_String;
         Spawn_Request'Write (Channel, Command);
         Initialized := False;
         Close_Socket (Socket);
         Close_Socket (Server);
         Initialized := False;
      end if;
   end Finalize;

   -----------
   -- Spawn --
   -----------

   procedure Spawn
     (Program_Name : String;
      Args         : GNAT.OS_Lib.Argument_List;
      Success      : out Boolean)
   is
      Command  : Spawn_Request;
      Response : Spawn_Response;
      Key      : Key_Type (Lock'Access); pragma Unreferenced (Key);
   begin
      if not Initialized then
         raise Program_Error with "Uninitialized server not running";
      end if;
      Command.Id := Id;
      Command.Spawn_Type := Spawn_1;
      Command.Program_Name := To_Unbounded_String (Program_Name);
      for I of Args loop
         Command.Args.Append (To_Unbounded_String (I.all));
      end loop;
      Spawn_Request'Write (Channel, Command);
      Spawn_Response'Read (Channel, Response);
      Success := Response.Success;
   end Spawn;

   -----------
   -- Spawn --
   -----------

   function Spawn
     (Program_Name : String;
      Args         : GNAT.OS_Lib.Argument_List)
      return Integer
   is
      Command  : Spawn_Request;
      Response : Spawn_Response;
      Key      : Key_Type (Lock'Access); pragma Unreferenced (Key);
   begin
      if not Initialized then
         raise Program_Error with "Uninitialized server not running";
      end if;
      Command.Id := Id;
      Command.Spawn_Type := Spawn_2;
      Command.Program_Name := To_Unbounded_String (Program_Name);
      for I of Args loop
         Command.Args.Append (To_Unbounded_String (I.all));
      end loop;
      Spawn_Request'Write (Channel, Command);
      Spawn_Response'Read (Channel, Response);
      return Response.Return_Code;
   end Spawn;

   -----------
   -- Spawn --
   -----------

   procedure Spawn
     (Program_Name : String;
      Args         : GNAT.OS_Lib.Argument_List;
      Output_File  : String;
      Success      : out Boolean;
      Return_Code  : out Integer;
      Err_To_Out   : Boolean := True)
   is
      Command  : Spawn_Request;
      Response : Spawn_Response;
      Key      : Key_Type (Lock'Access); pragma Unreferenced (Key);
   begin
      if not Initialized then
         raise Program_Error with "Uninitialized server not running";
      end if;
      Command.Id := Id;
      Command.Spawn_Type := Spawn_3;
      Command.Program_Name := To_Unbounded_String (Program_Name);
      for I of Args loop
         Command.Args.Append (To_Unbounded_String (I.all));
      end loop;
      Command.Output_File := To_Unbounded_String (Output_File);
      Command.Err_To_Out := Err_To_Out;

      Spawn_Request'Write (Channel, Command);
      Spawn_Response'Read (Channel, Response);

      Success := Response.Success;
      Return_Code := Response.Return_Code;
   end Spawn;

   function Non_Blocking_Spawn
     (Program_Name : String;
      Args         : Argument_List) return Process_Id is
      Command  : Spawn_Request;
      Response : Spawn_Response;
      Key      : Key_Type (Lock'Access); pragma Unreferenced (Key);
   begin
      if not Initialized then
         raise Program_Error with "Uninitialized server not running";
      end if;
      Command.Id := Id;
      Command.Spawn_Type := Non_Blocking_Spawn_1;
      Command.Program_Name := To_Unbounded_String (Program_Name);
      for I of Args loop
         Command.Args.Append (To_Unbounded_String (I.all));
      end loop;

      Spawn_Request'Write (Channel, Command);
      Spawn_Response'Read (Channel, Response);

      return  Response.Pid;
   end Non_Blocking_Spawn;

   function Non_Blocking_Spawn
     (Program_Name : String;
      Args         : Argument_List;
      Output_File  : String;
      Err_To_Out   : Boolean := True) return Process_Id is
      Command  : Spawn_Request;
      Response : Spawn_Response;
      Key      : Key_Type (Lock'Access); pragma Unreferenced (Key);
   begin
      if not Initialized then
         raise Program_Error with "Uninitialized server not running";
      end if;
      Command.Id := Id;
      Command.Spawn_Type := Non_Blocking_Spawn_2;
      Command.Program_Name := To_Unbounded_String (Program_Name);
      for I of Args loop
         Command.Args.Append (To_Unbounded_String (I.all));
      end loop;
      Command.Output_File := To_Unbounded_String (Output_File);
      Command.Err_To_Out := Err_To_Out;

      Spawn_Request'Write (Channel, Command);
      Spawn_Response'Read (Channel, Response);

      return Response.Pid;
   end Non_Blocking_Spawn;

   procedure Wait_Process (Pid : out Process_Id; Success : out Boolean) is
      Command  : Spawn_Request;
      Response : Spawn_Response;
      Key      : Key_Type (Lock'Access); pragma Unreferenced (Key);
   begin
      if not Initialized then
         raise Program_Error with "Uninitialized server not running";
      end if;
      Command.Id := Id;
      Command.Spawn_Type := Wait_Process;
      Spawn_Request'Write (Channel, Command);
      Spawn_Response'Read (Channel, Response);
      Success := Response.Success;
      Pid := Response.Pid;
   end Wait_Process;

   function Waitpid
     (Pid      : Process_Id;
      Status   : out Status_Kind;
      Options  : Wait_Options) return Process_Id is
      Command  : Spawn_Request;
      Response : Spawn_Response;
      Key      : Key_Type (Lock'Access); pragma Unreferenced (Key);
   begin
      if not Initialized then
         raise Program_Error with "Uninitialized server not running";
      end if;
      Command.Id := Id;
      Command.Spawn_Type := Waitpid;
      Command.Pid := Pid;
      Command.Options := Options;
      Spawn_Request'Write (Channel, Command);
      Spawn_Response'Read (Channel, Response);
      Status := Status_Kind (Response.Return_Code);
      return Response.Pid;
   end Waitpid;


   overriding procedure Initialize (Object : in out Key_Type) is
   begin
      Object.Sema.all.Seize;
   end Initialize;

   overriding procedure Finalize   (Object : in out Key_Type) is
   begin
      Object.Sema.all.Release;
   end Finalize;

end Spawn_Manager.Client;
