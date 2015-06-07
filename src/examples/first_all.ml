open Music
open Utils
open Verse


module Example = struct
    module BDgAll = struct
        let var_guitar_line (xs, xn) (ys, yn) (zs, zn) (ls, ln) =
          let f = BDg.create_guitar_measures (xs, xn) (ys, yn) in
          let s = BDg.create_guitar_measures (zs, zn) (ls, ln) in
          f @ s

        let other_guitar_line (xs, xn)  =
          let f = BDg.create_guitar_measures (1, 7) (1, 3) in
          let s = BDg.create_guitar_measures (0, 0) (xs, xn) in
          f @ s

        let t_after_chorus_one arg = create_part a_bass_line (other_guitar_line arg) (a_drum_line ~hihat:true ~variation:`All ())
        let t_after_chorus_two x y z l = create_part a_bass_line (var_guitar_line x y z l) (a_drum_line ~hihat:true ~variation:`All ())
        let to_try =
          let f1 =
            [t_after_chorus_one (0, 1);
             t_after_chorus_two (0, 0) (1, 5) (1, 7) (1, 10)]
          in
          let f2 =
            [t_after_chorus_one (0, 1);
             t_after_chorus_two (0, 0) (1, 5) (1, 6) (1, 10)]
          in
          [f1; f2]

        let all_riffs = List.map (
                            fun arg ->
                            flatten arg) to_try
      end
    (* (1, 0) -> (1, 2)
       (1, 0) -> (0, 0)
       (0, 0) -> (1, 1) *)

    let song_to_mxml song =
      let bass = Music_xml.create_instrument 0 Music_xml.MidiInstruments.std5_bass (`String (std5_bass, song.bass)) in
      let guitar = Music_xml.create_instrument 1 Music_xml.MidiInstruments.std_guitar (`String (std_guitar, song.guitar)) in
      let drum = Music_xml.create_instrument 3 Music_xml.MidiInstruments.std_drum (`Drum (song.drum)) in
      let xml = Music_xml.create "try" "2015-25-04" 184 [bass; guitar; drum] in
      Music_xml.to_string xml

    let output_example () =
      let song = flatten (BDgAll.all_riffs) in
      song_to_mxml song

  end

let () =
  let fd = open_out "/Users/ionalberdi/konposaketak/test_all.xml" in
  let () = Printf.fprintf fd "%s" (Example.output_example ()) in
  close_out fd
