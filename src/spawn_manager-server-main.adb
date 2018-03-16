with GNAT.Sockets; use GNAT.Sockets;
with Ada.Command_Line;
with GNAT.Exception_Traces;
with GNAT.Traceback.Symbolic;
procedure Spawn_Manager.Server.Main is
   Address  : Sock_Addr_Type;
   Server   : Socket_Type;
   Channel  : Stream_Access;
   Id       : Long_Integer;

   Request  : Spawn_Request;
   Response : Spawn_Response;
begin
   GNAT.Exception_Traces.Set_Trace_Decorator (GNAT.Traceback.Symbolic.Symbolic_Traceback'Access);
   GNAT.Exception_Traces.Trace_On (GNAT.Exception_Traces.Every_Raise);

   Address.Addr := Inet_Addr (Ada.Command_Line.Argument (1));
   Address.Port := Port_Type'Value (Ada.Command_Line.Argument (2));
   Id := Long_Integer'Value (Ada.Command_Line.Argument (3));

   Create_Socket (Server);
   Connect_Socket (Server, Address);

   Channel := Stream (Server);

   loop
      Spawn_Request'Read (Channel, Request);

      exit when Is_Exit_Message (Request);

      if Request.Id /= Id then
         Response := (False, -1, Invalid_Pid, To_Unbounded_String ("Invalid id from client"));
         Spawn_Response'Write (Channel, Response);
         Close_Socket (Server);
         raise Program_Error with  "Invalid id from client";
      elsif Request.Version /= Version then
         Response := (False, -1, Invalid_Pid,
                      To_Unbounded_String ("Server and client versions missmatch Server:") &
                        Version & ", Client:" & Request.Version);
         Spawn_Response'Write (Channel, Response);
         Close_Socket (Server);
         raise Program_Error with "Server and client versions missmatch Server:" &
           Version & ", Client:" & To_String (Request.Version);
      else
         Response := Spawn (Request);
         Spawn_Response'Write (Channel, Response);
      end if;

   end loop;
   Close_Socket (Server);
end Spawn_Manager.Server.Main;
