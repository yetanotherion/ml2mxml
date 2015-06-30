open Music
open Utils
open Verse


module Example = struct
    module Groove = struct
        let var_guitar_line =
          let fn = create_string_eighth in
          let f =
            create_measure
              [fn 1 1; fn 2 2;
               fn 1 1; fn 0 0;
               fn 1 1; fn 2 2;
               fn 1 1; fn 0 0]
          in
          let s =
            create_measure
              [fn 1 1; fn 2 2;
               fn 1 1; fn 2 2;
               fn 1 1; fn 2 2;
               fn 1 1; fn 0 0]
          in
          let seven =
            create_measure
              [fn 1 1; fn 2 2;
               fn 2 2; fn 0 0;
               fn 1 1; fn 2 2;
               fn 2 2; fn 0 0]
          in
          let eight =
            let last = fn 2 2 in
            create_measure
              [fn 1 1; last;
               last; last;
               last; last;
               last; last]
          in

          [f; s; f; s; f; s; seven; eight]
        let t_before_chorus_two = create_part a_bass_line var_guitar_line (a_drum_line ~variation:`FstOnly ~hihat:false ~break:`Second ())

        let create () = flatten [BDg.t_before_chorus_one;
                                 t_before_chorus_two;
                                 Chorus.t_bis]
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
