(**************************************************************************)
(*  Copyright 2015, Ion Alberdi <nolaridebi at gmail.com>                 *)
(*                                                                        *)
(*  Licensed under the Apache License, Version 2.0 (the "License");       *)
(*  you may not use this file except in compliance with the License.      *)
(*  You may obtain a copy of the License at                               *)
(*                                                                        *)
(*      http://www.apache.org/licenses/LICENSE-2.0                        *)
(*                                                                        *)
(*  Unless required by applicable law or agreed to in writing, software   *)
(*  distributed under the License is distributed on an "AS IS" BASIS,     *)
(*  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or       *)
(*  implied.  See the License for the specific language governing         *)
(*  permissions and limitations under the License.                        *)
(**************************************************************************)
type note_duration = [`Sixteenth | `Eighth | `Quarter | `Half | `Whole ]

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
  | `Snare_muted
  | `Cowbell
  | `Tom
  | `Tom_01
  | `Tom_02
  | `Tom_03
  | `Tom_04
  | `Hihat
  | `Hihat_02
  | `PedalHiHat
  | `Crash
  | `Crash_01
  | `Splash
  | `Ride
  | `Ride_01
  | `Ride_bell
  | `China ]

type string_note = [
  | `Single of played_note
  | `Chord of played_note list
  | `Rest ]

type drum_note = [
  | `Single of drum_element
  | `Chord of drum_element list
  | `Rest ]

type meter = [`Duple | `Triple ]
type tied = [`Start | `Stop]

type 'a measure_elt = {
  note: 'a;
  duration: note_duration;
  tied: tied option;
  meter: meter;
  dot: bool;
}

type 'a measure = 'a measure_elt list
type 'a measures = 'a measure list

let duration_to_float n =
  let res =
    match n.duration with
    | `Sixteenth -> begin
        match n.meter with
        | `Duple -> 1.0 /. 16.0
        | `Triple -> 1.0 /. 24.0
      end
    | `Eighth -> begin
        match n.meter with
        | `Duple -> 1.0 /. 8.0
        | `Triple -> 1.0 /. 12.0
      end
    | `Quarter -> begin
        match n.meter with
        | `Duple -> 1.0 /. 4.0
        | `Triple -> failwith ("not supported for now")
      end
    | `Half -> begin
        match n.meter with
        | `Duple -> 1.0 /. 2.0
        | `Triple -> failwith ("not supported for now")
      end
    | `Whole -> begin
        match n.meter with
        | `Duple -> 1.0
        | `Triple -> failwith ("not supported for now")
      end
  in
  if n.dot then res +. (res /. 2.0)
  else res

let create_single_note ?(tied=None) ?(meter=`Duple) ?(dot=false) dur n =
  {
    note=`Single n;
    duration=dur;
    meter=meter;
    tied=tied;
    dot=dot;
  }

let create_chord ?(tied=None) ?(meter=`Duple) ?(dot=false) dur played_notes =
  {note=`Chord played_notes;
   duration=dur;
   meter=meter;
   tied=tied;
   dot=dot;
  }

let create_rest ?(meter=`Duple) ?(dot=false) dur =
  {note=`Rest;
   duration=dur;
   meter=meter;
   tied=None;
   dot=dot;
  }

let rec find_note_in_guitar n =
  if n.fret < 0 then (
    if n.string = 0 then failwith("could not go lower")
    else
      find_note_in_guitar {string=n.string - 1;
                           fret=n.fret + 5}
  )
  else
    if n.fret > 22 then (
      if n.string = 4 then failwith("could not go higher")
      else find_note_in_guitar {string=n.string + 1;
                                fret=n.fret - 5})
    else n

let transpose_note semi_tone n =
  find_note_in_guitar {string=n.string;
                       fret=n.fret + semi_tone}


let transpose_string_note semi_tone n =
  let f = transpose_note semi_tone in
  let new_note =
    match n.note with
    | `Rest -> `Rest
    | `Single x -> `Single (f x)
    | `Chord l -> `Chord (List.map f l)
  in
  {note=new_note;
   duration=n.duration;
   tied=n.tied;
   meter=n.meter;
   dot=n.dot;
  }

let lower_string_note_by_fifth n = transpose_string_note (-5) n

let repeat_note_patterns n notes = List.fold_left (fun accum i ->
                                                   accum @ notes) [] (range 0 n)
let repeat_note n note =
  List.rev (
      List.fold_left (fun accum _ ->
                      note :: accum) [] (range 0 n))


let create_measure notes =
  let sum = List.fold_left
              (fun accum note ->
               accum +. (duration_to_float note)) 0.0 notes in
  if sum <> 1.0 then failwith (Printf.sprintf "not correct number of notes, missing %f units" sum)
  else notes

let repeat_measures n m =
  List.map (fun _ -> m) (range 0 n)

let transpose_measure semi_tone_nb measure =
  create_measure (List.map (transpose_string_note semi_tone_nb)
                           measure)

let transpose_measures semi_tone_nb measures =
  List.map (transpose_measure semi_tone_nb) measures


let create_string_note ?(tied=None) ?(meter=`Duple) ?(dot=false) dur s v =
  create_single_note ~tied ~meter ~dot dur ({string=s;
                                  fret=v})
let create_string_chord ?(tied=None) ?(meter=`Duple) ?(dot=false) dur l =
  create_chord ~tied ~meter ~dot dur (List.map (fun x ->
                                           let s, v = x in
                                           {string=s;
                                            fret=v}) l)

let create_drum_note ?(tied=None) ?(meter=`Duple) ?(dot=false) dur h =
  create_single_note ~tied ~meter ~dot dur h

let create_drum_chord ?(tied=None) ?(meter=`Duple) ?(dot=false) dur l =
  create_chord ~tied ~meter ~dot dur l

let create_string_sixteenth ?(tied=None) ?(meter=`Duple) ?(dot=false) = create_string_note ~tied ~meter ~dot `Sixteenth
let create_string_eighth ?(tied=None) ?(meter=`Duple) ?(dot=false) = create_string_note ~tied ~meter ~dot `Eighth
let create_string_chord_eighth ?(tied=None) ?(meter=`Duple) ?(dot=false) = create_string_chord ~tied ~meter ~dot `Eighth
let create_string_quarter ?(tied=None) ?(meter=`Duple) ?(dot=false) = create_string_note ~tied ~meter ~dot `Quarter
let create_string_half ?(tied=None) ?(meter=`Duple) ?(dot=false) = create_string_note ~tied ~meter ~dot `Half
let create_drum_sixteenth ?(tied=None) ?(meter=`Duple) ?(dot=false) = create_drum_note ~tied ~meter ~dot `Sixteenth
let create_drum_eighth ?(tied=None) ?(meter=`Duple) ?(dot=false) = create_drum_note ~tied ~meter ~dot `Eighth
let create_drum_quarter ?(tied=None) ?(meter=`Duple) ?(dot=false) = create_drum_note ~tied ~meter ~dot `Quarter
let create_drum_half ?(tied=None) ?(meter=`Duple) ?(dot=false) = create_drum_note ~tied ~meter ~dot `Half
let create_drum_chord_quarter ?(tied=None) ?(meter=`Duple) ?(dot=false) = create_drum_chord ~tied ~meter ~dot `Quarter

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
