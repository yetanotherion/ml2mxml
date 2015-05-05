open Music

module Example = struct
  let first_bass_riff () =
    let zero = [create_eighth 2 0] in
    let rest = [create_rest `Eighth] in
    (repeat 2 zero) @
     (repeat 3 rest) @
      [zero; [create_eighth 2 7]; zero]

  let first_guitar_riff () =
    repeat (4 * 3) [create_eighth ~meter:`Triple 1 7]

  let second_guitar_riff () =
    let zero = [create_eighth 1 7;
                create_eighth 2 9;
                create_eighth 3 9] in
    let rest = [create_rest `Eighth] in
    (repeat 2 zero) @
     (repeat 3 rest) @
       (repeat 3 zero)


  let output_example () =
    let bass = Music_xml.create_instrument 0 Music_xml.MidiInstruments.std5_bass std5_bass [first_bass_riff (); first_bass_riff ()] in
    let guitar = Music_xml.create_instrument 1 Music_xml.MidiInstruments.std_guitar std_guitar [first_guitar_riff (); second_guitar_riff ()] in
    let xml = Music_xml.create "try" "2015-25-04" [bass; guitar] in
    Music_xml.to_string xml
end

let () =
  let fd = open_out "/Users/ionalberdi/konposaketak/test.xml" in
  let () = Printf.fprintf fd "%s" (Example.output_example ()) in
  close_out fd
