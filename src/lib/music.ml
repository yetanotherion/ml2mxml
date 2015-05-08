type note_duration = [`Eighth]

let range ?step:(s=1) start_idx end_idx =
  (* range 0 3 == [0; 1; 2] *)
  let rec _range cidx eidx accum =
    if cidx + s >= eidx then List.rev (cidx :: accum)
    else _range (cidx + s) eidx (cidx :: accum)
  in
  _range start_idx end_idx []

let enumerate l =
  let _, r = List.fold_left (fun (curr_idx, res) elt -> (curr_idx + 1, (curr_idx, elt) :: res)) (0, []) l in
  List.rev r

type played_note = {
  string: int;
  fret: int;
}

module Diatonic = struct

  type diatonic_scale = [
  | `A | `ASharp | `B | `C
  | `CSharp | `D | `DSharp
  | `E | `F | `FSharp
  | `G | `GSharp ]

  let diatonic_to_string d =
    match d with
      | `A -> "A"
      | `ASharp -> "A#"
      | `B ->  "B"
      | `C -> "C"
      | `CSharp  -> "C#"
      | `D -> "D"
      | `DSharp -> "D#"
      | `E -> "E"
      | `F -> "F"
      | `FSharp -> "F#"
      | `G -> "G"
      | `GSharp -> "G#"

  let notes_per_semitone = [`C; `CSharp; `D; `DSharp; `E;
                            `F; `FSharp; `G; `GSharp; `A;
                            `ASharp; `B ]
  let number_notes = 12

  type t = {
    note: diatonic_scale;
    octave: int;
  }

  let note_idx n =
    let r, idx = List.fold_left (fun accum elt ->
      let found, res = accum in
      if found then (true, res)
      else
        if elt = n then (true, res)
        else (false, res + 1)) (false, 0) notes_per_semitone
    in
    let () = if not r then raise Not_found in
    idx

  let shift_n_semitone t n =
    let current_note_idx = note_idx t.note in
    let new_idx = current_note_idx + n in
    let new_tone_idx = new_idx mod number_notes in
    {
      octave = t.octave + (new_idx - new_tone_idx) / 12;
      note = List.nth notes_per_semitone new_tone_idx;
    }

  let to_string t =
    Printf.sprintf "note: %s, octave: %d" (diatonic_to_string t.note) t.octave

end

type string_clef = [`F | `G ]

type string_instrument = {
    strings: Diatonic.t array;
    string_clef: string_clef;
}

type drum_element = [
  | `Kick
  | `Snare
  | `Cowbell
  | `Tom
  | `Tom_01
  | `Tom_02
  | `Tom_03
  | `Tom_04
  | `Hihat
  | `PedalHiHat
  | `Crash
  | `Splash
  | `Ride
  | `China ]

type string_note = [
    | `Played of played_note
    | `Rest ]

type drum_note = [
    | `Played of drum_element
    | `Rest ]

type meter = [`Duple | `Triple ]

type 'a measure_elt = {
  note: 'a;
  duration: note_duration;
  meter: meter;
}


let create_note ?(meter=`Duple) dur n =
  {note=n;
   duration=dur;
   meter=meter}


let create_string_note ?(meter=`Duple) dur s v =
  create_note ~meter dur (`Played ({string=s;
                                    fret=v}))

let create_drum_note ?(meter=`Duple) dur h =
  create_note ~meter dur (`Played h)

let create_rest ?(meter=`Duple) dur = create_note ~meter dur `Rest

let create_string_eighth ?(meter=`Duple) = create_string_note ~meter `Eighth
let create_drum_eighth ?(meter=`Duple) = create_drum_note ~meter `Eighth


let make_standard_bass_shift note =
  Diatonic.shift_n_semitone note 5

let generate_bass nb_string first_string_note =
  List.rev (List.fold_left (fun accum _ ->
    make_standard_bass_shift (List.hd accum) :: accum)
    [first_string_note] (range 0 (nb_string - 1)))

let std5_bass = {
  strings=Array.of_list (generate_bass 5 {Diatonic.note=`B;
                                          Diatonic.octave=0});
  string_clef=`F;
}

let std_guitar =
  let first_four = generate_bass 4 {Diatonic.note=`E;
                                    Diatonic.octave = 2} in
  let last_two = generate_bass 2 {Diatonic.note=`B;
                                  Diatonic.octave = 3} in
  {
    strings=Array.of_list (first_four @ last_two);
    string_clef=`G;
  }

let repeat_note n note = List.map (fun _ -> note) (range 0 n)

let repeat_notes n notes = List.fold_left (fun accum i ->
                                           accum @ notes) [] (range 0 n)
