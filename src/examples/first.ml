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

    let first_drum_measure =
      create_measure [kick_q; snare_q;
                      kick_e; kick_e;
                      snare_e; kick_e]

    let second_drum_measure =
      create_measure [quarter_rest;
                      snare_q;
                      kick_e; kick_e;
                      snare_e; eighth_rest]

    let fourth_drum_measure =
      create_measure [eighth_rest;
                      snare_q;
                      eighth_rest;
                      kick_e; kick_e;
                      snare_e;
                      create_drum_chord `Eighth [`Crash;
                                                 `Splash;
                                                 `Kick]]

    let first_guitar_measure =
      create_measure (repeat_note (4 * 3) (create_string_eighth ~meter:`Triple 1 7))

    let second_guitar_measure =
      create_measure (repeat_note (4 * 3) (create_string_eighth ~meter:`Triple 1 8))

    let third_guitar_measure =
      create_measure (repeat_note (4 * 3) (create_string_eighth ~meter:`Triple 1 10))

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
    let first_drum_line = [first_drum_measure; second_drum_measure; first_drum_measure; second_drum_measure]
    let second_drum_line = [first_drum_measure; second_drum_measure; first_drum_measure; fourth_drum_measure]

    let a_drum_line = first_drum_line @ second_drum_line

    let create_a_guitar_line last =
      [first_guitar_measure; first_guitar_measure; first_guitar_measure; last]

    let a_guitar_line = (create_a_guitar_line second_guitar_measure) @ (create_a_guitar_line third_guitar_measure)
    let create_b_guitar_line last =
      [guitar_bis_first_measure; guitar_bis_second_measure; guitar_bis_first_measure; last]

    let b_guitar_line = (create_b_guitar_line guitar_bis_third_measure) @ (create_b_guitar_line guitar_bis_fourth_measure)

    let rest_line = repeat_measures 8 [whole_rest]

    module B = struct
        let t = create_part a_bass_line rest_line rest_line
      end

    module BD = struct
        let t = create_part a_bass_line rest_line a_drum_line
      end

    module BDg = struct
        let t = create_part a_bass_line a_guitar_line a_drum_line
      end

    module BDG = struct
        let t = create_part a_bass_line b_guitar_line a_drum_line
        let second_drum_measure_bis =
          create_measure [quarter_rest;
                          eighth_rest; eighth_rest;
                          eighth_rest; kick_e;
                          snare_q]

        let a_drum_line_bis =
          [create_measure [whole_rest];
           second_drum_measure_bis;
           first_drum_measure;
           second_drum_measure] @ second_drum_line

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

        let drum_parts ?(break=None) () =
          let first_one =
            match break with
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
          [create_measure [first_one;
                           create_drum_half `Snare];
           create_measure [create_drum_half `Kick;
                           create_drum_half `Snare]]
        let first = [drum_parts (); drum_parts ~break:(Some `Splash) ()]
        let second = reduce ([drum_parts ~break:(Some `China) ();
                              drum_parts ~break:(Some `Splash) ()])
        let drums =
          reduce (first @
                    (repeat_measures 3 second))

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
      let song = flatten [B.t; BD.t; BDg.t; BDG.t;
                          BDg.t; BDg.t; Chorus.t;
                          BDG.t_without_drum_in_beggining;
                          BDg.t; BDg.t; Chorus.t; Bridge.t;
                          BDG.t_without_drum_in_beggining;
                          BDg.t; BDg.t; Bridge.t
                         ] in
      song_to_mxml song

  end

let () =
  let fd = open_out "/Users/ionalberdi/konposaketak/test.xml" in
  let () = Printf.fprintf fd "%s" (Example.output_example ()) in
  close_out fd
