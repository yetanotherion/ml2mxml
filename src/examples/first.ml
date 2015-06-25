open Music
open Utils

module Example = struct

    let song_to_mxml song =
      let bass = Music_xml.create_instrument 0 Music_xml.MidiInstruments.std5_bass (`String (std5_bass, song.bass)) in
      let guitar = Music_xml.create_instrument 1 Music_xml.MidiInstruments.std_guitar (`String (std_guitar, song.guitar)) in
      let drum = Music_xml.create_instrument 3 Music_xml.MidiInstruments.std_drum (`Drum (song.drum)) in
      let xml = Music_xml.create "try" "2015-25-04" 184 [bass; guitar; drum] in
      Music_xml.to_string xml

    let output_example () =
      let open Verse in
      let song = flatten [B.t; BD.t; BDg.t_before_chorus_one;
                          BDG.t;
                          BDg.t_before_chorus_one_prime;
                          BDg.t_before_chorus_two;
                          Chorus.t_bis;
                          BDG.t_without_drum_in_beggining;
                          BDg.t_after_chorus_one;
                          BDg.t_after_chorus_two;
                          Chorus.t_bis; Bridge.t;
                          BDG.t_without_drum_in_beggining;
                          BDg.t_after_chorus_one;
                          BDg.t_after_chorus_two; Bridge.t
                         ] in
      song_to_mxml song

  end

let () =
  let fd = open_out "/Users/ionalberdi/konposaketak/test.xml" in
  let () = Printf.fprintf fd "%s" (Example.output_example ()) in
  close_out fd
