open Music
open Utils
open Verse


module Example = struct
    module BDgAll = struct
        let other_guitar_line (xs, xn) (ys, yn)  =
          let f = BDg.create_guitar_measures (1, 7) (xs, xn) in
          let s = BDg.create_guitar_measures (ys, yn) (1, 5) in
          f @ s
        (* 0 3 *)

        let t_after_chorus_one x y = create_part a_bass_line (other_guitar_line x y) (a_drum_line ~hihat:true ~variation:`All ())
        let to_try xo =
          let res = (List.map (fun x -> (1, x)) (List.rev (range 0 7))) @ (List.map (fun x -> (0, x)) (List.rev (range 0 5))) in
          match xo with
          | None -> res
          | Some x -> List.filter (fun elt -> Pervasives.compare elt x == -1) res

        let all_riffs = List.map2 (
                            fun x y ->
                            t_after_chorus_one x y)
                                  (*[(1, 6); (1, 6); (1, 6); (1, 5); (1, 5); (1, 3); (1, 3); (1, 0); (0, 1)]
                                    [(1, 6); (1, 5); (1, 0); (1, 0); (0, 0); (1, 0); (0, 0); (0, 0); (0, 0)]*)
                                  [(1, 3); (1, 0); (0, 1)]
                                  [(0, 0); (0, 0); (0, 0)]
      end
    (*
     - (1, 6), (1, 0)
     - (1, 6), (0, 0)
     - (1, 5), (1, 0)
     - (1, 5), (0, 0)
     - (1, 3), (1, 0)
     - (1, 3), (0, 0)
     - (1, 0), (0, 0)
     - (0, 1), (0, 0)

     *)
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
