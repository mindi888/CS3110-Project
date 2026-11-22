let save filename answers =
  let out_channel = open_out_bin filename in
  Marshal.to_channel out_channel answers [];
  close_out out_channel

let load filename =
  if Sys.file_exists filename then (
    let in_channel = open_in_bin filename in
    let answers = Marshal.from_channel in_channel in
    close_in in_channel;
    Some answers)
  else None
