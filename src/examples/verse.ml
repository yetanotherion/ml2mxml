open Music
open Utils

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
    let other_guitar_line =
      let f = create_guitar_measures (1, 7) (1, 3) in
      let s = create_guitar_measures (0, 0) (1, 5) in
      f @ s

    let t_before_chorus_one = create_part a_bass_line std_guitar_line (a_drum_line ~hihat:false ())
    let t_before_chorus_two = create_part a_bass_line var_guitar_line (a_drum_line ~variation:`FstOnly ~hihat:false ~break:`Second ())
    let t_before_chorus_one_prime = create_part a_bass_line other_guitar_line (a_drum_line ~variation:`FstOnly ~hihat:false ~break:`Second ())
    let t_after_chorus_one = create_part a_bass_line other_guitar_line (a_drum_line ~hihat:true ~variation:`All ())
    let t_after_chorus_two = create_part a_bass_line var_guitar_line (a_drum_line ~hihat:true ~variation:`All ~break:`Second ())
  end

module BDG = struct
    let zero_chord = create_string_chord_eighth [(1, 7);
                                                 (2, 9);
                                                 (3, 9)]
    let eight_chord = create_string_chord_eighth [(1, 8);
                                                  (2, 10);
                                                  (3, 10)]
    let ten_chord = create_string_chord_eighth [(1, 10);
                                                (2, 12);
                                                (3, 12)]

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
