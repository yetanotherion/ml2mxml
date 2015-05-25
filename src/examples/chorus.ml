open Music
open Utils

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
