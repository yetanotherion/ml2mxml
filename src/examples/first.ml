open Music

module Example = struct
    let sixteenth_rest = create_rest `Sixteenth
    let eighth_rest = create_rest `Eighth
    let quarter_rest = create_rest `Quarter
    let whole_rest = create_rest `Whole

    let bass_zero = create_string_eighth 2 0
    let bass_seven = create_string_eighth 2 7
    let bass_eight = create_string_eighth 2 8
    let bass_ten = create_string_eighth 2 10

    let kick = create_drum_quarter `Kick
    let snare_q = create_drum_quarter `Snare

    let snare = create_drum_eighth `Snare
    let kick_e = create_drum_eighth `Kick
    let tom1 = create_drum_sixteenth `Tom_01
    let tom2 = create_drum_sixteenth `Tom_02
    let tom2_e = create_drum_eighth `Tom_02

    let zero_chord = create_string_chord_eighth [(1, 7);
                                                 (2, 9);
                                                 (3, 9)]
    let eight_chord = create_string_chord_eighth [(1, 8);
                                                  (2, 10);
                                                  (3, 10)]
    let ten_chord = create_string_chord_eighth [(1, 10);
                                                (2, 12);
                                                (3, 12)]

    let first_bass_measure =
      create_measure
        [bass_zero; bass_zero;
         eighth_rest; eighth_rest;
         eighth_rest; bass_zero;
         bass_seven; bass_zero]

    let second_bass_measure =
      create_measure
        [eighth_rest; bass_zero;
         eighth_rest; bass_zero;
         eighth_rest; bass_seven;
         bass_seven; bass_seven]

    let third_bass_measure =
      create_measure
        [bass_zero; bass_zero;
         eighth_rest; eighth_rest;
         eighth_rest; bass_seven;
         bass_zero; bass_zero]

    let fourth_bass_measure =
      create_measure ([eighth_rest] @ (repeat_note 7 bass_eight))

    let eighth_bass_measure =
      create_measure ([eighth_rest] @ (repeat_note 7 bass_ten))

    let first_drum_measure =
      create_measure [kick; snare;
                      tom1; tom2; tom2;
                      sixteenth_rest; tom2;
                      sixteenth_rest; eighth_rest; tom2_e]

    let second_drum_measure =
      create_measure [quarter_rest; snare;
                      eighth_rest;
                      kick_e; kick_e;
                      snare_q]
    let fourth_drum_measure =
      create_measure [quarter_rest; snare;
                      eighth_rest;
                      kick_e; kick_e;
                      create_drum_chord_quarter [`Snare;
                                                 `Splash]]

    let first_guitar_measure =
      create_measure (repeat_note (4 * 3) (create_string_eighth ~meter:`Triple 1 7))

    let second_guitar_measure =
      create_measure (repeat_note (4 * 3) (create_string_eighth ~meter:`Triple 1 8))

    let third_guitar_measure =
      create_measure (repeat_note (4 * 3) (create_string_eighth ~meter:`Triple 1 10))

    let guitar_bis_first_measure =
      create_measure
        ((repeat_note 2 zero_chord) @
           (repeat_note 3 eighth_rest) @
             (repeat_note 3 zero_chord))

    let guitar_bis_second_measure =
      create_measure
        ([eighth_rest; zero_chord;
          eighth_rest; zero_chord;
          eighth_rest; zero_chord;
          zero_chord; zero_chord])

    let guitar_bis_third_measure =
      create_measure
        (eighth_rest:: (repeat_note 7 eight_chord))

    let guitar_bis_fourth_measure =
      create_measure
        (eighth_rest:: (repeat_note 7 ten_chord))


    let output_example () =
      let a_bass_line = [first_bass_measure; second_bass_measure; third_bass_measure; fourth_bass_measure;
                         first_bass_measure; second_bass_measure; third_bass_measure; eighth_bass_measure] in
      let a_drum_line = [first_drum_measure; second_drum_measure; first_drum_measure; second_drum_measure;
                         first_drum_measure; second_drum_measure; first_drum_measure; fourth_drum_measure] in
      let a_guitar_line = [first_guitar_measure; first_guitar_measure; first_guitar_measure; second_guitar_measure;
                           first_guitar_measure; first_guitar_measure; first_guitar_measure; third_guitar_measure] in
      let b_guitar_line = [guitar_bis_first_measure; guitar_bis_second_measure; guitar_bis_first_measure; guitar_bis_third_measure;
                           guitar_bis_first_measure; guitar_bis_second_measure; guitar_bis_first_measure; guitar_bis_fourth_measure] in

      let rest_line = repeat_measures 8 [whole_rest] in
      let bass = Music_xml.create_instrument 0 Music_xml.MidiInstruments.std5_bass (`String (std5_bass, a_bass_line @ a_bass_line @ a_bass_line @ a_bass_line @ a_bass_line @ a_bass_line)) in
      let guitar = Music_xml.create_instrument 1 Music_xml.MidiInstruments.std_guitar (`String (std_guitar, rest_line @ rest_line @ a_guitar_line @ b_guitar_line @ a_guitar_line @ a_guitar_line)) in
      let drum = Music_xml.create_instrument 3 Music_xml.MidiInstruments.std_drum (`Drum (rest_line @ a_drum_line @ a_drum_line @ a_drum_line @ a_drum_line @ a_drum_line)) in
      let xml = Music_xml.create "try" "2015-25-04" 184 [bass; guitar; drum] in
      Music_xml.to_string xml
  end

let () =
  let fd = open_out "/Users/ionalberdi/konposaketak/test.xml" in
  let () = Printf.fprintf fd "%s" (Example.output_example ()) in
  close_out fd
