open Music
open Utils

let bass =
  let first = create_string_note `Quarter 2 0 in
  let second_eighth = create_string_note ~tied:(Some `Start) `Eighth 2 3 in
  let second = create_string_note ~tied:(Some `Stop) `Quarter 2 3 in
  let third_eighth = create_string_note ~tied:(Some `Start) `Eighth 1 4 in
  let third = create_string_note ~tied:(Some `Stop) `Quarter 1 4 in
  let bass_first =
    let curr = create_string_note `Eighth 2 0 in
    create_measure
      [first;
       curr; curr;
       curr; curr;
       curr; second_eighth;
      ]
  in
  let bass_second =
    let curr = create_string_note `Eighth 2 3 in
    create_measure
      [second;
       curr; curr;
       curr; curr;
       curr; third_eighth;
      ]
  in
  let bass_third =
    let curr = create_string_note `Eighth 1 4 in
    create_measure
      [third;
       curr; curr;
       curr; curr;
       curr; curr]
  in
  let bass_fourth =
    let f = create_string_eighth in
    create_measure
      [f 1 5; f 1 8;
       f 2 5; f 2 7;
       f 3 7; f 3 5;
       f 2 7; f 1 8;]
  in
  let third_bis = create_string_note ~tied:(Some `Stop)
                                     `Quarter 2 6 in

  let third_eighth_bis = create_string_note ~tied:(Some `Start)
                                            `Eighth 2 6 in
  let bass_second_bis =
    let curr = create_string_note `Eighth 2 3 in
    create_measure
      [second;
       curr; curr;
       curr; curr;
       curr; third_eighth_bis;
      ]
  in
  let bass_third_bis =
    let curr = create_string_note `Eighth 2 6 in
    create_measure
      [third_bis;
       curr; curr;
       curr; curr;
       curr; curr]
  in
  let bass_fourth_bis =
    let curr = create_string_note `Eighth 4 15 in
    create_measure
      [curr; curr;
       eighth_rest; curr;
       curr; eighth_rest;
       curr; curr]
  in
  let bass_eight =
    let curr = create_string_eighth 2 3 in
    create_measure
      [eighth_rest; curr;
       curr; eighth_rest;
       curr; curr;
       eighth_rest; curr]
  in
  [bass_first;
   bass_second;
   bass_third;
   bass_fourth;
   bass_first;
   bass_second;
   bass_third;
   bass_eight] @
    [bass_first;
     bass_second_bis;
     bass_third_bis;
     bass_fourth_bis;
     bass_first;
     bass_second;
     bass_third;
     bass_fourth]

let guitar =
  let first = create_string_chord `Quarter [(1, 0);
                                            (2, 0);
                                            (3, 2)] in

  let second_eighth = create_string_chord ~tied:(Some `Start)
                                          `Eighth [(1, 3);
                                                   (2, 3);
                                                   (3, 5)] in
  let second = create_string_chord ~tied:(Some `Stop) `Quarter [(1, 3);
                                                                (2, 3);
                                                                (3, 5)] in
  let third_eighth = create_string_chord ~tied:(Some `Start)
                                         `Eighth [(0, 4);
                                                  (1, 4);
                                                  (2, 6)] in
  let third = create_string_chord ~tied:(Some `Stop)
                                  `Quarter [(0, 4);
                                            (1, 4);
                                            (2, 6)] in

  let guitar_first =
    create_measure
      [first;
       eighth_rest; eighth_rest;
       eighth_rest; eighth_rest;
       eighth_rest; second_eighth;
      ]
  in
  let guitar_second =
    create_measure
      [second;
       eighth_rest; eighth_rest;
       eighth_rest; eighth_rest;
       eighth_rest; third_eighth;
      ]
  in
  let guitar_third =
    create_measure
      [third;
       eighth_rest; eighth_rest;
       eighth_rest; eighth_rest;
       eighth_rest; eighth_rest;]
  in
  let guitar_fourth =
    let f = create_string_eighth in
    create_measure
      [f 0 5; f 0 8;
       f 1 5; f 1 7;
       f 2 7; f 2 5;
       f 1 7; f 0 8]
  in
  let guitar_eight =
    let curr = create_string_chord `Eighth [(3, 7);
                                            (4, 8);
                                            (5, 10)] in
    create_measure
      [eighth_rest; curr;
       curr; eighth_rest;
       curr; curr;
       eighth_rest; curr]
  in
  let third_bis = create_string_chord ~tied:(Some `Stop)
                                      `Quarter [(1, 6);
                                                (2, 6);
                                                (3, 8)] in

  let third_eighth_bis = create_string_chord ~tied:(Some `Start)
                                             `Eighth [(1, 6);
                                                      (2, 6);
                                                      (3, 8)] in
  let guitar_second_bis =
    create_measure
      [second;
       eighth_rest; eighth_rest;
       eighth_rest; eighth_rest;
       eighth_rest; third_eighth_bis;
      ]
  in

  let guitar_third_bis =
    create_measure
      [third_bis;
       third_eighth_bis; third_eighth_bis;
       third_eighth_bis; third_eighth_bis;
       third_eighth_bis; third_eighth_bis;]
  in
  let fourth_bis = create_string_chord `Eighth [(3, 15);
                                                (4, 16);
                                                (5, 16)] in
  let guitar_fourth_bis =
    create_measure
      [fourth_bis; fourth_bis;
       eighth_rest; fourth_bis;
       fourth_bis; eighth_rest;
       fourth_bis; fourth_bis]
  in
  [guitar_first;
   guitar_second;
   guitar_third;
   guitar_fourth;
   guitar_first;
   guitar_second;
   guitar_third;
   guitar_eight;
  ] @ [guitar_first;
       guitar_second_bis;
       guitar_third_bis;
       guitar_fourth_bis;
       guitar_first;
       guitar_second;
       guitar_third;
       guitar_fourth;
      ]
let guitar_loop =
  let f = create_string_eighth in
  let guitar_loop =
    create_measure
      [f 0 5; f 0 8;
       f 1 5; f 1 7;
       f 2 7; f 2 5;
       f 1 7; f ~tied:(Some `Start) 0 5]
  in
  let guitar_loop_2 =
    create_measure
      [f ~tied:(Some `Stop) 0 5; f 0 8;
       f 1 5; f 1 7;
       f 2 7; f 2 5;
       f 1 7; f ~tied:(Some `Start) 0 5]
  in
  let guitar_loop_3 =
    create_measure
      [f ~tied:(Some `Stop) 0 5; f 0 8;
       f 1 5; f 1 7;
       f 2 7; f 2 5;
       f 1 7; f 2 7]

  in
  let guitar_loop_4 =
    create_measure
      [f 2 5; f 1 7;
       f 2 7; f 2 5;
       f 1 7; f 2 7;
       f 2 5; f ~tied:(Some `Start) 0 5]
  in
  let guitar_loop_5 =
    create_measure
      [f ~tied:(Some `Start) 0 5; f 0 8;
       f 1 5; f 1 7;
       f 2 7; f 2 5;
       f 1 7; f ~tied:(Some `Start) 0 5]
  in
  let guitar_loop_8 =
    create_measure
      [f 2 5; f 1 7;
       f 2 7; f 2 5;
       f 1 7; f 2 7;
       f 2 5; eighth_rest]
  in
  let first_half = [guitar_loop;
                    guitar_loop_2;
                    guitar_loop_3;
                    guitar_loop_4 ] in
  let second_half =  [guitar_loop_5;
                      guitar_loop_2;
                      guitar_loop_3;
                      guitar_loop_8]
  in
  let second_half_bis = [guitar_loop_5;
                         guitar_loop_2;
                         guitar_loop_3;
                         create_measure [eighth_rest; eighth_rest;
                                         eighth_rest; eighth_rest;
                                         eighth_rest; eighth_rest;
                                         eighth_rest; eighth_rest;]] in
  let first = first_half @ second_half in
  let second_first_half = transpose_measures 5 [guitar_loop;
                                                guitar_loop_2] in
  let guitar_loop_3_other =
    create_measure
      [f ~tied:(Some `Stop) 0 10; f 0 13;
       f 1 10; f 1 12;
       f 2 12; f 2 10;
       f 2 12; f ~tied:(Some `Start) 2 14]

  in
  let guitar_loop_4_other =
    create_measure
      [f ~tied:(Some `Stop) 2 14; f 2 12;
       f 2 10; f 1 12;
       f 2 12; f 2 10;
       f 1 10; f ~tied:(Some `Start) 0 5]
  in
  let second_first_half = second_first_half @ [guitar_loop_3_other; guitar_loop_4_other] in
  first @ (second_first_half @ second_half_bis)

let bass_loop = transpose_measures 5 guitar_loop

let t = create_part (bass @ bass_loop) (guitar @ guitar_loop) (a_drum_line () @ a_drum_line () @ a_drum_line () @ a_drum_line ())
