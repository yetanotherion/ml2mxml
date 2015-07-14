module Example = struct
    module Bass = struct
        let t =
          let open Music in
          let f = create_string_quarter in
          let b = f 2 2 in
          let b_dot = f ~dot:true 2 2 in
          let c = f 2 3 in
          let d = f 2 5 in
          let a = f 1 5 in
          let a_dot = f ~dot:true 1 5 in
          let a_half = create_string_half 1 5 in
          let a_eighth = create_string_eighth 1 5 in
          let g = f 1 3 in
          let g_half = create_string_half 1 3 in
          let g_eighth = create_string_eighth 1 3 in
          let first =
            create_measure [b; b;
                            c; d]
          in
          let second =
            create_measure [d; c;
                            b; a]
          in
          let third =
            create_measure [g; g;
                            a; b]
          in
          let fourth =
            create_measure [b_dot; a_eighth;
                            a_half]
          in
          let eight =
            create_measure [a_dot; g_eighth;
                            g_half]
          in
          [first; second; third; fourth;
           first; second; third; eight]
      end
    module Guitar = struct
        let t =
          let open Music in
          let rest = create_measure [create_rest `Whole] in
          let f = create_string_sixteenth in
          let b = f 1 2 in
          let c = f 1 3 in
          let d = f 1 5 in
          let a = f 0 5 in
          let a_dot = create_string_quarter ~dot:true 0 5 in
          let g = f 0 3 in
          let g_half = create_string_half 0 3 in
          let g_eighth = create_string_eighth 0 3 in
          let first =
            create_measure [b; b; b; b;
                            b; b; b; b;
                            c; c; c; c;
                            d; d; d; d]
          in
          let second =
            create_measure [d; d; d; d;
                            c; c; c; c;
                            b; b; b; b;
                            a; a; a; a]
          in
          let third =
            create_measure [g; g; g; g;
                            g; g; g; g;
                            a; a; a; a;
                            b; b; b; b]
          in
          let fourth =
            create_measure [a_dot; g_eighth;
                            g_half]
          in
          [rest; rest; rest; rest;
           first; second; third; fourth]
      end
    module Drum = struct
        let t =
          let open Music in
          let kick_q = create_drum_quarter `Kick in
          let snare_q = create_drum_quarter `Snare in
          let snare_and_splash_q = create_drum_chord `Quarter [`Splash;
                                                               `Snare]
          in
          let first = create_measure [kick_q; snare_q; kick_q; snare_q] in
          let last = create_measure [kick_q; snare_q; snare_and_splash_q; create_rest `Quarter] in
          [first; first; first; first;
           first; first; first; last]
      end
    let song_to_mxml bass drum guitar =
      let bass = Music_xml.create_instrument 0 Music_xml.MidiInstruments.std5_bass (`String (Music.std5_bass, bass)) in
      let drum = Music_xml.create_instrument 1 Music_xml.MidiInstruments.std_drum (`Drum drum) in
      let guitar = Music_xml.create_instrument 2 Music_xml.MidiInstruments.std_guitar (`String (Music.std_guitar, guitar)) in
      let xml = Music_xml.create "try" "2015-25-04" 180 [bass; drum; guitar] in
      Music_xml.to_string xml

    let output_example () =
      let open Music in
      song_to_mxml Bass.t Drum.t Guitar.t
  end

let () =
  let fd = open_out "./hello_ode_to_joy.xml" in
  let () = Printf.fprintf fd "%s" (Example.output_example ()) in
  close_out fd
