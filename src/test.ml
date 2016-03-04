(* In this test, we measure the error between the true quantiles of
   data, and the quantiles as computed through the [Bentov] histogram
   approximation module. The data is drawn from a mixture of two
   Gaussians, N(2,1) and N(5,1), where the mixing coefficient is
   1/2. We compute the approximate quantiles in two ways: In the
   frist, we simply add each sample into a [Bentov.histogram]. We call
   this histogram [mixed].  In the second, we add a datum to one of
   two [Bentov.histogram]'s, one associated with each of the
   Guassians..  We then merge these two histograms using
   [Bentov.merge].  We call the result of merging the two
   sub-histograms [merged].  Finally, we compute and print the
   mean-square-error between the true quantiles and [mixed], and the
   true quantiles and [merged]. *)

open Bentov
open Gsl.Randist

(* draw a sample from N([mu],[sigma]^2) *)
let normal rng ~mu ~sigma =
  mu +. (gaussian rng ~sigma)

(* [quantiles list num_intervals] returns the quantiles (true, not
   approximated) of list [list] at [num_intervals + 1] points, including
   the minimum and maximum values (which are the first and last values of
   the result, resp. *)
let quantiles list num_intervals =
  let num_intervals_f = float num_intervals in
  let array = Array.of_list list in
  Array.sort Pervasives.compare array;
  let rec loop i accu =
    if i > num_intervals then
      List.rev accu
    else
      let q = (float i) /. num_intervals_f in
      let x = Gsl.Stats.quantile_from_sorted_data array q in
      loop (i+1) (x :: accu)
  in
  loop 0 []


let _ =
  (* the number of data to draw *)
  let n = int_of_string Sys.argv.(1) in

  (* the size of the approximate histograms *)
  let q = int_of_string Sys.argv.(2) in

  let rng = Gsl.Rng.(make (default ())) in

  (* draw [x] from one of two Gaussians, adding it to the [mixed]
     approximate histogram, and one of [n1_h] and [n2_h] approximate
     histograms *)
  let rec loop i n1_h n2_h mixed_h data =
    if i < n then
      if Gsl.Rng.uniform rng > 0.5 then
        let x = normal rng ~mu:2.0 ~sigma:1.0 in (* N(2,1) *)
        let n1_h = Bentov.add x n1_h in
        let mixed_h = Bentov.add x mixed_h in
        loop (i+1) n1_h n2_h mixed_h (x :: data)
      else
        let x = normal rng ~mu:5.0 ~sigma:1.0 in (* N(5,1) *)
        let n2_h = Bentov.add x n2_h in
        let mixed_h = Bentov.add x mixed_h in
        loop (i+1) n1_h n2_h mixed_h (x :: data)
    else
      n1_h, n2_h, mixed_h, data
  in

  let open Bentov in

  let n1_h = create q in
  let n2_h = create q in
  let mixed_h = create q in
  let n1_h, n2_h, mixed_h, data = loop 0 n1_h n2_h mixed_h [] in

  (* merge the two sub-histograms *)
  let merged_h = merge [n1_h; n2_h] q in

  assert (total_count mixed_h = n );
  assert (total_count merged_h = n );

  (* measure the error between the true quantiles and approximations
     on a grid half the size of our approximate histograms *)
  let num_intervals = q/2 in

  (* compute mean-squared-error *)
  let rec errors actual mixed merged sum_se_mixed sum_se_merged =
    match actual, mixed, merged with
      | actual_h :: actual_t , mixed_h :: mixed_t, merged_h :: merged_t ->

        let err_mixed  = mixed_h  -. actual_h in
        let err_merged = merged_h -. actual_h in

        let sum_se_mixed  = sum_se_mixed  +. err_mixed  *. err_mixed in
        let sum_se_merged = sum_se_merged +. err_merged *. err_merged in

        errors actual_t mixed_t merged_t sum_se_mixed sum_se_merged

      | [], [], [] ->

        let num_intervals_1 = float (num_intervals + 1) in
        sqrt (sum_se_mixed  /. num_intervals_1),
        sqrt (sum_se_merged /. num_intervals_1)

      | _ -> assert false
  in

  let mixed_q  = uniform mixed_h num_intervals in
  let merged_q = uniform merged_h num_intervals in
  let actual_q = quantiles data num_intervals in

  let err_mixed, err_merged = errors actual_q mixed_q merged_q 0.0 0.0 in
  Printf.printf "err_mixed=%e err_merged=%e\n" err_mixed err_merged

