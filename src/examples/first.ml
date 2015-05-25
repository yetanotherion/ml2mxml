open Music
type parts = {
    bass: string_note measures;
    guitar: string_note measures;
    drum: drum_note measures;
  }

let create_part b g d =
  let lb, lg, ld = List.length b, List.length g, List.length d in
  let () =
    if not (lb = lg && lg = ld) then failwith("invalid length")
  in
  {
    bass = b;
    guitar = g;
    drum = d;
  }

let append_parts a b =
  {
    bass = a.bass @ b.bass;
    guitar = a.guitar @ b.guitar;
    drum = a.drum @ b.drum;
  }

let flatten l =
  match l with
  | [] -> {bass = [];
           guitar = [];
           drum = []}
  | hd :: tl ->
     List.fold_left (fun accum x ->
                     append_parts accum x) hd tl

let reduce l = List.fold_left (fun accum r ->
                               accum @ r) [] l


module Example = struct
    let sixteenth_rest = create_rest `Sixteenth
    let eighth_rest = create_rest `Eighth
    let quarter_rest = create_rest `Quarter
    let whole_rest = create_rest `Whole

    let bass_zero = create_string_eighth 2 0
    let bass_seven = create_string_eighth 2 7
    let bass_seven_quarter = create_string_quarter 2 7
    let bass_eight = create_string_eighth 2 8

    let bass_ten = create_string_eighth 2 10
    let bass_high_nine = create_string_eighth 3 9
    let bass_high_seven = create_string_eighth 3 7
    let bass_high_ten = create_string_eighth 3 10
    let bass_high_eleven = create_string_eighth 3 11
    let bass_low_ten = create_string_eighth 1 10

    let kick_q = create_drum_quarter `Kick
    let snare_q = create_drum_quarter `Snare
    let snare_e = create_drum_eighth `Snare
    let snare_s = create_drum_sixteenth `Snare
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

    let first_drum_measure ?(crash=false) ?(hihat=true) () =
      let ending = [snare_e; kick_e] in
      let first_kick =
        if crash then
          create_drum_chord `Quarter [`Crash;
                                      `Kick]
        else
          kick_q
      in
      let beggining = [first_kick;
                       snare_q]
      in
      let middle =
        if hihat then
             let kick_hi_hat = create_drum_chord `Sixteenth [`Kick;
                                                             `Hihat] in
             [kick_hi_hat;
              create_drum_sixteenth `Hihat;
              kick_hi_hat;
              create_rest `Sixteenth]
        else
          [kick_e;
           kick_e]
      in
      create_measure (beggining @ middle @ ending)

    let second_drum_measure ?(variation=false) () =
      let start = [quarter_rest;
                   snare_q] in
      let ending =
        if variation then
          [create_drum_chord `Sixteenth [`Kick;
                                         `Hihat];
           create_drum_sixteenth `Hihat;
           create_drum_chord `Sixteenth [`Kick;
                                         `Hihat];
           create_rest `Sixteenth;
           create_drum_chord `Sixteenth [`Snare;
                                         `Hihat];
           create_drum_sixteenth `Hihat;
           create_drum_sixteenth `Hihat;
           create_rest `Sixteenth]
        else [kick_e; kick_e; snare_e; eighth_rest]
      in
      create_measure (start @ ending)


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
        (eighth_rest :: (repeat_note 7 eight_chord))

    let guitar_bis_fourth_measure =
      create_measure
        (eighth_rest :: (repeat_note 7 ten_chord))

    let a_bass_line = [first_bass_measure; second_bass_measure; third_bass_measure; fourth_bass_measure;
                       first_bass_measure; second_bass_measure; third_bass_measure; eighth_bass_measure]

    let first_drum_line ?(variation=`None) ?(hihat=true) () =
      let second =
        match variation with
        | `All | `FstOnly -> second_drum_measure ~variation:true ()
        | `None -> second_drum_measure ~variation:false ()
      in
      let last =
        match variation with
        | `All -> second_drum_measure ~variation:true ()
        | `None | `FstOnly -> second_drum_measure ~variation:false ()
      in
      [first_drum_measure ~hihat (); second; first_drum_measure ~hihat () ; last]

    let create_drum_line_with_break ?(break=`First) ?(hihat=true) () =
      let break_measure =
        match break with
        | `First ->
           create_measure [eighth_rest;
                           snare_q;
                           eighth_rest;
                           kick_e; kick_e;
                           snare_e;
                           create_drum_chord `Eighth [`Crash;
                                                      `Splash;
                                                      `Kick]]


        | `Second ->
           create_measure [eighth_rest; snare_e;
                           snare_e; kick_e;
                           snare_e; eighth_rest;
                           create_drum_chord `Eighth [`Splash;
                                                      `Crash;
                                                      `Snare];
                           eighth_rest]
      in
      [first_drum_measure ~crash:true ~hihat () ; second_drum_measure (); first_drum_measure ~hihat (); break_measure]

    let a_drum_line ?(variation=`None) ?(break=`First) ?(hihat=true) () =
      first_drum_line ~variation ~hihat () @ (create_drum_line_with_break ~break ~hihat ())

    let rest_line = repeat_measures 8 [whole_rest]

    module B = struct
        let t = create_part a_bass_line rest_line rest_line
      end

    module BD = struct
        let t = create_part a_bass_line rest_line (a_drum_line ~hihat:false ())
      end

    module BDg = struct

        let create_triple_measure note =
          let ns, nf = note in
          create_measure (repeat_note (4 * 3) (create_string_eighth ~meter:`Triple ns nf))

        let create_guitar_measures first_note second_note =
          let first_three = create_triple_measure first_note in
          let last = create_triple_measure second_note in
          [first_three; first_three; first_three; last]

        let std_guitar_line =
          let first_note, second_note, third_note = (1, 7), (1, 8), (1, 10) in
          (create_guitar_measures first_note second_note) @ (create_guitar_measures first_note third_note)

        let std_guitar_line = std_guitar_line
        let var_guitar_line =
          let f = create_guitar_measures (1, 12) (1, 10) in
          let s = create_guitar_measures (1, 7) (1, 10) in
          f @ s
        let t_before_chorus_one = create_part a_bass_line std_guitar_line (a_drum_line ~hihat:false ())
        let t_before_chorus_two = create_part a_bass_line var_guitar_line (a_drum_line ~variation:`FstOnly ~hihat:false ~break:`Second ())
        let t_after_chorus_one = create_part a_bass_line std_guitar_line (a_drum_line ~hihat:true ~variation:`All ())
        let t_after_chorus_two = create_part a_bass_line var_guitar_line (a_drum_line ~hihat:true ~variation:`All ~break:`Second ())
      end

    module BDG = struct
        let create_b_guitar_line last =
          [guitar_bis_first_measure; guitar_bis_second_measure; guitar_bis_first_measure; last]

        let b_guitar_line = (create_b_guitar_line guitar_bis_third_measure) @ (create_b_guitar_line guitar_bis_fourth_measure)

        let t = create_part a_bass_line b_guitar_line (a_drum_line ())
        let second_drum_measure_bis =
          create_measure [quarter_rest;
                          eighth_rest; eighth_rest;
                          eighth_rest; kick_e;
                          snare_q]

        let a_drum_line_bis =
          [create_measure [whole_rest];
           second_drum_measure_bis;
           first_drum_measure ();
           second_drum_measure ()] @ (create_drum_line_with_break ())

        let t_without_drum_in_beggining =
          create_part a_bass_line b_guitar_line a_drum_line_bis

      end

    module Chorus = struct
        let bass_groovy_first_measure =
          create_measure
            [bass_high_nine; bass_high_seven;
             bass_ten; bass_seven;
             bass_low_ten; bass_seven;
             eighth_rest; bass_high_nine]

        let bass_groovy_second_measure =
          create_measure
            [bass_high_seven; bass_ten;
             bass_seven; bass_low_ten;
             bass_seven; bass_low_ten;
             bass_seven; bass_low_ten]

        let bass_groovy_third_measure =
          create_measure
            [bass_high_nine; bass_high_seven;
             bass_ten; bass_seven;
             bass_low_ten; bass_seven;
             bass_low_ten; bass_seven]

        let bass_groovy_fourth_measure =
          create_measure (repeat_note 8 bass_eight)

        let bass_groovy_eighth_measure =
          create_measure (repeat_note 8 bass_high_ten)

        let bass_groovy_ninth_measure =
          create_measure
            [bass_high_nine; bass_high_seven;
             bass_ten; bass_seven;
             bass_low_ten; bass_seven;
             eighth_rest;
             create_string_eighth ~tied:(Some `Start) 3 9;
            ]

        let bass_groovy_tenth_measure =
          create_measure
            [create_string_eighth ~tied:(Some `Stop) 3 9;
             bass_high_seven;
             bass_ten; bass_low_ten;
             bass_seven; bass_low_ten;
             bass_seven; bass_low_ten]

        let bass_groovy_sixteenth_measure =
          create_measure (repeat_note 8 bass_high_eleven)

        let drum_groovy_first_measure =
          create_measure
            [kick_q;
             snare_e; eighth_rest;
             kick_e; snare_e;
             eighth_rest;
             kick_e]

        let drum_groovy_second_measure =
          create_measure
            [eighth_rest; snare_e;
             quarter_rest;
             kick_q;
             snare_e; eighth_rest]

        let drum_groovy_fourth_measure =
          create_measure
            [eighth_rest; snare_e;
             quarter_rest;
             kick_e; kick_e;
             snare_e; create_drum_chord `Eighth [`Kick;
                                                 `Crash;
                                                 `Splash]]

        let drum_groovy_eighth_measure =
          create_measure
            ([eighth_rest; snare_e;
              quarter_rest] @ (repeat_note 8 snare_s))

        let create_std_string_measure last_measure =
          [bass_groovy_first_measure;
           bass_groovy_second_measure;
           bass_groovy_third_measure;
           last_measure]

        let strings_measure_1 = create_std_string_measure bass_groovy_fourth_measure
        let strings_measure_2 = create_std_string_measure bass_groovy_eighth_measure
        let strings_measure_3 = [bass_groovy_ninth_measure; bass_groovy_tenth_measure;
                                 bass_groovy_third_measure; bass_groovy_fourth_measure]
        let strings_measure_4 = create_std_string_measure bass_groovy_sixteenth_measure

        let strings = reduce [strings_measure_1;
                              strings_measure_2;
                              strings_measure_3;
                              strings_measure_4]
        let create_std_drum_measure last_measure =
          [drum_groovy_first_measure;
           drum_groovy_second_measure;
           drum_groovy_first_measure;
           last_measure]

        let drum_measure_1 = create_std_drum_measure drum_groovy_fourth_measure
        let drum_measure_2 = create_std_drum_measure drum_groovy_eighth_measure
        let drum = reduce [drum_measure_1;
                           drum_measure_2;
                           drum_measure_1;
                           drum_measure_2]

        let t = create_part strings
                            strings
                            drum
      end
    module Bridge = struct
        let strings_first_measure =
          create_measure [bass_high_nine; bass_high_seven;
                          bass_low_ten; bass_seven_quarter;
                          bass_low_ten;
                          bass_low_ten; bass_low_ten]

        let strings_second_measure =
          create_measure [bass_seven; bass_low_ten;
                          bass_low_ten; bass_low_ten;
                          bass_seven; bass_low_ten;
                          bass_low_ten; bass_low_ten]
        let create_break ls lf =
          let create_quarter_eighth s f =
            create_string_quarter s f, create_string_eighth s f
          in
          let quarter, eigth = create_quarter_eighth 2 8 in
          let l_q, l_e = create_quarter_eighth ls lf in
          let f =
            create_measure [bass_high_nine; bass_high_seven;
                            bass_low_ten; quarter;
                            eigth; eigth; eigth]
          in
          let s =
            create_measure [eigth; eigth; eigth;
                            l_q;
                            l_e; l_e;
                            l_e]
          in
          let beggining =
            reduce (repeat_measures 3 [strings_first_measure;
                                       strings_second_measure])
          in
          let res = beggining @ [f; s] in
          res

        let strings =
          reduce ([create_break 2 10; create_break 1 5])

        let drum_parts ?(beggining=None) ?(ending=None) () =
          let create_first opt =
            match opt with
            | None ->
               create_drum_half `Kick
            | Some x ->
               match x with
               | `Splash ->
                  create_drum_chord `Half [`Kick;
                                           `Crash;
                                           `Splash]
               | `China ->
                  create_drum_chord `Half [`Kick;
                                           `China]
          in
          let first_one = create_first beggining in
          let beg = create_measure [first_one;
                                    create_drum_half `Snare]
          in
          let first_one = create_first ending in
          let ending = create_measure [first_one;
                                       create_drum_half `Snare] in
          [beg; ending]
        let create_drum_part first_beg =
          let first = drum_parts ~beggining:first_beg () in
          let second = drum_parts ~beggining:(Some `Splash) () in
          let third = drum_parts ~beggining:(Some `China) () in
          let fourth = drum_parts ~beggining:(Some `Splash) ~ending:(Some `China) () in
          reduce [first; second; third; fourth]

        let drums = (create_drum_part None) @ (create_drum_part (Some `Splash))

        let t = create_part strings
                            strings
                            drums

      end
    let song_to_mxml song =
      let bass = Music_xml.create_instrument 0 Music_xml.MidiInstruments.std5_bass (`String (std5_bass, song.bass)) in
      let guitar = Music_xml.create_instrument 1 Music_xml.MidiInstruments.std_guitar (`String (std_guitar, song.guitar)) in
      let drum = Music_xml.create_instrument 3 Music_xml.MidiInstruments.std_drum (`Drum (song.drum)) in
      let xml = Music_xml.create "try" "2015-25-04" 184 [bass; guitar; drum] in
      Music_xml.to_string xml

    let output_example () =
      let song = flatten [B.t; BD.t; BDg.t_before_chorus_one; BDG.t;
                          BDg.t_before_chorus_one; BDg.t_before_chorus_two; Chorus.t;
                          BDG.t_without_drum_in_beggining;
                          BDg.t_after_chorus_one; BDg.t_after_chorus_two; Chorus.t; Bridge.t;
                          BDG.t_without_drum_in_beggining;
                          BDg.t_after_chorus_one; BDg.t_after_chorus_two; Bridge.t
                         ] in
      song_to_mxml song

  end

let () =
  let fd = open_out "/Users/ionalberdi/konposaketak/test.xml" in
  let () = Printf.fprintf fd "%s" (Example.output_example ()) in
  close_out fd
