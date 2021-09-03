




let file = "example.cub"
let message = "Hello!"
  
let () =
  (* Write message to file *)
  let oc = open_out file in (* create or truncate file, return channel *)
    Printf.fprintf oc "%s\n" message; (* write something *)   
    close_out oc;                     (* flush and close the channel *)