open Music
open Utils
open Verse


module Example = struct
    module Groove = struct
        let create () =
          let bass_grooves = reduce [Chorus.bass_groovy_one;
                                     Chorus.bass_groovy_bis] in
          let guitar_groovy_start = Chorus.create_guitar bass_grooves in

          let guitar_groovy_end =
            let first = create_string_eighth 3 5 in
            let second = create_string_eighth 3 3 in
            let third = create_string_eighth 1 4 in
            let fourth = create_string_eighth 1 3 in
            let fifth = create_string_eighth 0 4 in
            let end_one = create_string_eighth 1 4 in
            let end_two = create_string_eighth 2 1 in

            let guitar_groove_first =
              [first; second;
               third; fourth;
               fifth; fourth;
               fifth; first]
            in
            let guitar_groove_two =
              [second; third;
               fourth; fifth;
               fourth; fifth;
               fourth; fifth]
            in
            let guitar_groove_third =
              [first; second;
               third; fourth;
               fifth; fourth;
               fifth; fourth]
            in
            reduce [[guitar_groove_first;
                     guitar_groove_two;
                     guitar_groove_third;
                     create_measure (repeat_note 8 end_one)];
                    [guitar_groove_first;
                     guitar_groove_two;
                     guitar_groove_third;
                     create_measure (repeat_note 8 end_two)]]
          in
          let guitar = (reduce [guitar_groovy_start;
                                guitar_groovy_end])
          in
          let t = create_part Chorus.bass_groovy
                              guitar
                              Chorus.drum in
          flatten [Verse.BDg.t_after_chorus_two;
                   t]

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
        let all_riffs = create ()
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
