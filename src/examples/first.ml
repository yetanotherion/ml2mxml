open Music

module Example = struct
  let first_bass_riff () =
    let zero = [create_string_eighth 2 0] in
    let rest = [create_rest `Eighth] in
    (repeat_note 2 zero) @
     (repeat_note 3 rest) @
      [zero; [create_string_eighth 2 7]; zero]

  let first_guitar_riff () =
    repeat_note (4 * 3) [create_string_eighth ~meter:`Triple 1 7]

  let second_guitar_riff () =
    let zero = [create_string_eighth 1 7;
                create_string_eighth 2 9;
                create_string_eighth 3 9] in
    let rest = [create_rest `Eighth] in
    (repeat_note 2 zero) @
     (repeat_note 3 rest) @
       (repeat_note 3 zero)

  let drum_riff () =
    let kick = [create_drum_eighth `Kick] in
    let snare = [create_drum_eighth `Snare] in
    let first_time = [kick; snare] in
    repeat_notes 4 first_time

  let output_example () =
    let bass = Music_xml.create_instrument 0 Music_xml.MidiInstruments.std5_bass (`String (std5_bass, [first_bass_riff (); first_bass_riff ()])) in
    let guitar = Music_xml.create_instrument 1 Music_xml.MidiInstruments.std_guitar (`String (std_guitar, [first_guitar_riff (); second_guitar_riff ()])) in
    let drum = Music_xml.create_instrument 3 Music_xml.MidiInstruments.std_drum (`Drum [drum_riff (); drum_riff ()]) in
    let xml = Music_xml.create "try" "2015-25-04" 184 [bass; guitar; drum] in
    Music_xml.to_string xml
end

let () =
  let fd = open_out "/Users/ionalberdi/konposaketak/test.xml" in
  let () = Printf.fprintf fd "%s" (Example.output_example ()) in
  close_out fd
