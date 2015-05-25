open Music
open Utils

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
