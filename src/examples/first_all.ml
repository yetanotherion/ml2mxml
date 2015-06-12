open Music
open Utils
open Verse


module Example = struct
    module BDGAll = struct

        let generate_tonal tonal_string tonal_note semi_tone_nb =
          let new_string = tonal_string + 1 in
          let new_note = tonal_note + semi_tone_nb - 5 in
          create_string_chord_eighth [(tonal_string, tonal_note);
                                      (new_string, new_note);
                                      (tonal_string + 2, tonal_note + 2)]
        (*(tonal_string + 2, tonal_note + 2)]*)

        let create_first first second third fb =
          let first_measure f =
            create_measure
              ((repeat_note 2 f) @
                 (repeat_note 3 eighth_rest) @
                   (repeat_note 3 f))
          in
          let second_measure f =
            create_measure
              ([eighth_rest; f;
                eighth_rest; f;
                eighth_rest; f;
                f; f])
          in
          let last_measure last =
            create_measure
              (eighth_rest :: (repeat_note 7 last))
          in
          [first_measure first; second_measure first; first_measure first; last_measure second;
           first_measure fb; second_measure fb; first_measure fb; last_measure third]

        let generate_t semi_tone_one semi_tone_two semi_tone_three semi_tone_one_bis =
          let first = generate_tonal 1 7 semi_tone_one in
          let second = generate_tonal 1 8 semi_tone_two in
          let third = generate_tonal 1 10 semi_tone_three in
          let first_bis = generate_tonal 1 7 semi_tone_one_bis in
          create_part a_bass_line (create_first first second third first_bis) (a_drum_line ())

        (*let make_all_riffs xrange yrange zrange = List.map (
                            fun x ->
                            let res =
                              List.map
                                (fun y ->
                                 let res =
                                   List.map (fun z ->
                                             generate_t x y z x) zrange
                                 in
                                 flatten res) yrange
                            in
                            flatten res) xrange
           let all_riffs = make_all_riffs [7] [6] [8]*)
        let all_riffs = [generate_t 7 6 7 7]

      end

    let song_to_mxml song =
      let bass = Music_xml.create_instrument 0 Music_xml.MidiInstruments.std5_bass (`String (std5_bass, song.bass)) in
      let guitar = Music_xml.create_instrument 1 Music_xml.MidiInstruments.std_guitar (`String (std_guitar, song.guitar)) in
      let drum = Music_xml.create_instrument 3 Music_xml.MidiInstruments.std_drum (`Drum (song.drum)) in
      let xml = Music_xml.create "try" "2015-25-04" 184 [bass; guitar; drum] in
      Music_xml.to_string xml

    let output_example () =
      let song = flatten (BDGAll.all_riffs) in
      song_to_mxml song

  end

let () =
  let fd = open_out "/Users/ionalberdi/konposaketak/test_all.xml" in
  let () = Printf.fprintf fd "%s" (Example.output_example ()) in
  close_out fd
