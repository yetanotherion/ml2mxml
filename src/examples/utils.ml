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


let sixteenth_rest = create_rest `Sixteenth
let eighth_rest = create_rest `Eighth
let quarter_rest = create_rest `Quarter
let whole_rest = create_rest `Whole

let bass_seven = create_string_eighth 2 7
let bass_seven_quarter = create_string_quarter 2 7
let bass_eight = create_string_eighth 2 8

let bass_ten = create_string_eighth 2 10
let bass_high_nine = create_string_eighth 3 9
let bass_high_seven = create_string_eighth 3 7
let bass_high_ten = create_string_eighth 3 10
let bass_high_eleven = create_string_eighth 3 11
let bass_high_five = create_string_eighth 3 5
let bass_high_zero = create_string_eighth 3 0
let bass_one = create_string_eighth 2 1
let bass_zero = create_string_eighth 2 0
let bass_two = create_string_eighth 2 2
let bass_three = create_string_eighth 2 3
let bass_low_ten = create_string_eighth 1 10

let kick_q = create_drum_quarter `Kick
let snare_q = create_drum_quarter `Snare
let snare_e = create_drum_eighth `Snare
let snare_s = create_drum_sixteenth `Snare
let kick_e = create_drum_eighth `Kick
let tom1 = create_drum_sixteenth `Tom_01
let tom2 = create_drum_sixteenth `Tom_02
let tom2_e = create_drum_eighth `Tom_02

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

let a_bass_line = [first_bass_measure; second_bass_measure;
                   third_bass_measure; fourth_bass_measure;
                   first_bass_measure; second_bass_measure;
                   third_bass_measure; eighth_bass_measure]

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
