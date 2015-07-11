open Music
open Utils
open Verse


module Example = struct
    module Groove = struct
        let guitar_groovy_end =
          let first = create_string_eighth 3 5 in
          let second = create_string_eighth 3 3 in
          let third = create_string_eighth 1 4 in
          let fourth = create_string_eighth 1 3 in
          let fifth = create_string_eighth 0 4 in
          let guitar_groove_fifth =
            [first; second;
             third; fourth;
             fifth; fourth;
             fifth; Music.transpose_string_note 1 first]
          in
          let guitar_groove_sixth = Music.transpose_measure 1
            [second; third;
             fourth; fifth;
             fourth; fifth;
             fourth; fifth]
          in
          let guitar_groove_seventh = Music.transpose_measure 2
            [first; second;
             third; fourth;
             fifth; fourth;
             fifth; fourth]
          in
          let f = create_string_eighth in
          let guitar_groove_eighth =
            [
              f 0 6; f 1 5;
              f 2 5; f 3 5;
              f 3 7; f 2 5;
              f 1 5; f 0 6
            ]
          in
          let guitar_groove_last =
            [
              eighth_rest;
              f 1 5; f 1 5;
              eighth_rest;
              create_string_note `Half 1 5;
            ]
          in
          reduce [[guitar_groove_fifth;
                   guitar_groove_sixth;
                   guitar_groove_seventh;
                   guitar_groove_eighth];
                  [guitar_groove_fifth;
                   guitar_groove_sixth;
                   guitar_groove_seventh;
                   guitar_groove_last;]]

        let guitar = reduce [Chorus.Guitar.guitar_groovy_start;
                             guitar_groovy_end]
        let t_bis = create_part Chorus.bass_groovy guitar Chorus.drum
        let create () = flatten [                                 t_bis]
        let all_riffs = create ()
        let lower_tone note semi_tone_nb =
          let s, n = note in
          if n >= semi_tone_nb then s, n - semi_tone_nb
          else (s - 1, n - semi_tone_nb + 5)

        (*let generate_all note =
          let riffs =
            List.map (fun semi_tone_nb ->
                      let second_note = lower_tone note semi_tone_nb in
                      let l = List.map (fun semi_tone_nb ->
                                 let third_note = lower_tone note semi_tone_nb in
                                 let l = List.map (fun semi_tone_nb ->
                                                   let fourth_note = lower_tone note semi_tone_nb in
                                                   let l = List.map (fun semi_tone_nb ->
                                                                     let fifth_note = lower_tone note semi_tone_nb in
                                                                     let f (s, n) = Printf.sprintf "(%d, %d)" s n in
                                                                     let () = Printf.printf "%s %s %s %s %s (%d)\n" (f note) (f second_note)
                                                                                            (f third_note) (f fourth_note) (f fifth_note) (semi_tone_nb) in
                                                                     create note second_note third_note fourth_note fifth_note)
                                                                    (range (semi_tone_nb + 1) 12)
                                                   in
                                                   flatten l) (range (semi_tone_nb + 1) 12) in
                                 flatten l)
                                       (range (semi_tone_nb + 1) 12)
                      in
                      flatten l) (range 2 3)
          in
          flatten riffs*)
                  (*let all_riffs = create (2, 7) (2, 5) (2, 3) (1, 1) (1, 0)*)
          (* let all_riffs = generate_all (3, 7)*)
      end
    let song_to_mxml song =
      let bass = Music_xml.create_instrument 0 Music_xml.MidiInstruments.std5_bass (`String (std5_bass, song.bass)) in
      let guitar = Music_xml.create_instrument 1 Music_xml.MidiInstruments.std_guitar (`String (std_guitar, song.guitar)) in
      let drum = Music_xml.create_instrument 3 Music_xml.MidiInstruments.std_drum (`Drum (song.drum)) in
      let xml = Music_xml.create "try" "2015-25-04" 184 [bass; guitar; drum] in
      Music_xml.to_string xml

    let output_example () =
      let song = Groove.all_riffs in
      song_to_mxml song

  end

let () =
  let fd = open_out "/Users/ionalberdi/konposaketak/test_all.xml" in
  let () = Printf.fprintf fd "%s" (Example.output_example ()) in
  close_out fd
