open Music
open Utils


let bass_groovy_first_measure =
  create_measure
    [bass_high_seven; bass_high_five;
     bass_one; bass_zero;
     eighth_rest; bass_zero;
     eighth_rest; bass_high_seven]

let bass_groovy_second_measure =
  create_measure
    [bass_high_five; bass_one;
     bass_zero; eighth_rest;
     bass_zero; eighth_rest;
     bass_zero; eighth_rest]

let bass_groovy_third_measure =
  create_measure
    [bass_high_seven; bass_high_five;
     bass_one; bass_zero;
     eighth_rest; bass_zero;
     eighth_rest; bass_zero]

let bass_groovy_fourth_measure =
  create_measure (repeat_note 8 bass_one)

let bass_groovy_eighth_measure =
  create_measure (repeat_note 8 bass_three)

let bass_groovy_one = [bass_groovy_first_measure;
                       bass_groovy_second_measure;
                       bass_groovy_third_measure;
                       bass_groovy_fourth_measure]

let bass_groovy_bis = [bass_groovy_first_measure;
                       bass_groovy_second_measure;
                       bass_groovy_third_measure;
                       bass_groovy_eighth_measure]

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

let bass_groovy = reduce [bass_groovy_one;
                          bass_groovy_bis;
                          bass_groovy_one;
                          bass_groovy_bis;]
let create_guitar bass_r =
  List.map (fun measure ->
            create_measure (List.map lower_string_note_by_fifth measure)) bass_r

let bass_grooves = reduce [bass_groovy_one;
                           bass_groovy_bis]

let guitar_groovy_start = create_guitar bass_grooves

let guitar_groovy_end =
  let first = create_string_eighth 3 5 in
  let second = create_string_eighth 3 3 in
  let third = create_string_eighth 1 4 in
  let fourth = create_string_eighth 1 3 in
  let fifth = create_string_eighth 0 4 in
  let end_one = create_string_eighth 1 4 in
  let end_two = create_string_eighth 2 1 in

  let guitar_groove_first =
    [first; second;
     third; fourth;
     fifth; fourth;
     fifth; first]
  in
  let guitar_groove_two =
    [second; third;
     fourth; fifth;
     fourth; fifth;
     fourth; fifth]
  in
  let guitar_groove_third =
    [first; second;
     third; fourth;
     fifth; fourth;
     fifth; fourth]
  in
  reduce [[guitar_groove_first;
           guitar_groove_two;
           guitar_groove_third;
           create_measure (repeat_note 8 end_one)];
          [guitar_groove_first;
           guitar_groove_two;
           guitar_groove_third;
           create_measure (repeat_note 8 end_two)]]

let guitar = (reduce [guitar_groovy_start;
                      guitar_groovy_end])


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

let t_bis = create_part bass_groovy
                        guitar
                        drum
