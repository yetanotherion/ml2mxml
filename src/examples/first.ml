open Music

module Example = struct
    let eighth_rest = create_rest `Eighth
    let whole_rest = create_rest `Whole

    let bass_zero = create_string_eighth 2 0
    let bass_seven = create_string_eighth 2 7
    let bass_eight = create_string_eighth 2 8
    let bass_ten = create_string_eighth 2 10

    let first_bass_measure =
      [bass_zero; bass_zero;
       eighth_rest; eighth_rest;
       eighth_rest; bass_zero;
       bass_seven; bass_zero]

    let second_bass_measure =
      [eighth_rest; bass_zero;
       eighth_rest; bass_zero;
       eighth_rest; bass_seven;
       bass_seven; bass_seven]

    let third_bass_measure =
      [bass_zero; bass_zero;
       eighth_rest; eighth_rest;
       eighth_rest; bass_seven;
       bass_zero; bass_zero]

    let fourth_bass_measure =
      [eighth_rest] @ (repeat_note 7 bass_eight)

    let eighth_bass_measure =
      [eighth_rest] @ (repeat_note 7 bass_ten)

    let first_guitar_riff () =
      repeat_note (4 * 3) (create_string_eighth ~meter:`Triple 1 7)

    let second_guitar_riff () =
      let zero = create_string_chord_eighth [(1, 7);
                                             (2, 9);
                                             (3, 9)] in
      (repeat_note 2 zero) @
        (repeat_note 3 eighth_rest) @
          (repeat_note 3 zero)

    let drum_riff () =
      let kick = create_drum_eighth `Kick in
      let snare = create_drum_eighth `Snare in
      let first_time = [kick; snare] in
      repeat_note_patterns 4 first_time

    let output_example () =
      let a_bass_line = [first_bass_measure; second_bass_measure; third_bass_measure; fourth_bass_measure;
                         first_bass_measure; second_bass_measure; third_bass_measure; eighth_bass_measure] in
      let rest_line = repeat_measures 8 [whole_rest] in
      let bass = Music_xml.create_instrument 0 Music_xml.MidiInstruments.std5_bass (`String (std5_bass, a_bass_line)) in
      let guitar = Music_xml.create_instrument 1 Music_xml.MidiInstruments.std_guitar (`String (std_guitar, rest_line)) in
      let drum = Music_xml.create_instrument 3 Music_xml.MidiInstruments.std_drum (`Drum rest_line) in
      let xml = Music_xml.create "try" "2015-25-04" 184 [bass; guitar; drum] in
      Music_xml.to_string xml
  end

let () =
  let fd = open_out "/Users/ionalberdi/konposaketak/test.xml" in
  let () = Printf.fprintf fd "%s" (Example.output_example ()) in
  close_out fd
