-module(records).
-compile(export_all).
-include("records.hrl").

-record(robot, {  name
                , type=industiral
                , hobbies
                , details=[]
               }).

first_robot() ->
  #robot{  name="Mechatron"
         , type=handmade
         , details=["Moved by a small man inside"]
        }.

included() -> #included{some_field="Some Value"}.
