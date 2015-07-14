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
open OUnit
open Music.Diatonic

module SemiTone = struct
    let suite name e1 e2 =
      let error = Printf.sprintf "%s == %s" (to_string e1) (to_string e2) in
      let test () = error @? (e1 = e2) in
      name >:: test

    let test = "Test_music" >:::
                 List.map (fun (name, e1, e2) ->
                           suite name e1 e2)
                          [
                            ("init", {note=`ASharp;
                                      octave=0},
                             shift_n_semitone {note=`A;
                                               octave=0} 1);
                            ("one_octave", {note=`A;
                                            octave=1},
                             shift_n_semitone {note=`A;
                                               octave=0} 12);
                            ("next_do", {note=`C;
                                         octave=2},
                             shift_n_semitone {note=`A;
                                               octave=0} 15);
                            ("next_mi", {note=`E;
                                         octave=1},
                             shift_n_semitone {note=`B;
                                               octave=0} 5)]
  end

module Measure = struct
    let suite name test =
      name >:: test

    let test = "Test_music" >:::
                 List.map (fun (name, f) ->
                           suite name f)
                          [
                            ("correct",
                             fun () ->
                             let rest = Music.create_rest `Eighth in
                             let eight_rest = Music.repeat_note 8 rest in
                             let _ = Music.create_measure eight_rest in
                             "no_error" @? true);
                            ("non_correct",
                             fun () ->
                             let rest = Music.create_rest `Eighth in
                             let seven_rest = Music.repeat_note 7 rest in
                             try
                               let _ = Music.create_measure seven_rest in
                               "exception not raised" @? false
                             with Failure s ->
                               let expected_msg = "not correct number of notes, missing 0.875000 units" in
                               "exception not raised" @? (s = expected_msg))
                          ]
  end

let _ =
  List.iter (fun t ->
             let _ = run_test_tt ~verbose:true t in
             ()
            ) [SemiTone.test;
               Measure.test]
