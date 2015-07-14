open Music
open Utils
open Verse


module Example = struct
    module Groove = struct
        let create () = flatten [Chorus.t_bis; Bridge.t]
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
