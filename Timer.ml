let make () =
  Unix.(
    let t = (times ()).tms_utime in
    (fun () -> (times ()).tms_utime -. t)
  )

