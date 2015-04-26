open Music

module Example = struct
  let first_riff () =
    let zero = create_eighth 2 0 in
    let rest = create_rest `Eighth in
    (repeat 2 zero) @
     (repeat 3 rest) @
      [zero; create_eighth 2 7; zero]

  let output_example () =
    let instrument = std5_bass in
    let xml = Music_xml.create "try" "2015-25-04" "P0" instrument (first_riff ()) in
    Music_xml.to_string xml
end

let () =
  let fd = open_out "/Users/ionalberdi/konposaketak/test.xml" in
  let () = Printf.fprintf fd "%s" (Example.output_example ()) in
  close_out fd
