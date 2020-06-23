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
  Array.sort Stdlib.compare array;
  let rec loop i accu =
    if i > num_intervals then
      List.rev accu
    else
      let q = (float i) /. num_intervals_f in
      let x = Gsl.Stats.quantile_from_sorted_data array q in
      loop (i+1) ((i,x) :: accu)
  in
  loop 0 []

module IntMap = Map.Make(Int)

let map_of_assoc assoc =
  List.fold_left (
    fun k_to_v (k, v) ->
      IntMap.add k v k_to_v
  ) IntMap.empty assoc

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

  let error i actual mixed merged =
    match IntMap.find_opt i actual with
    | None -> None, None
    | Some actual ->
      (match IntMap.find_opt i mixed with
       | Some mixed -> Some (actual -. mixed)
       | None -> None
      ),
      (match IntMap.find_opt i merged with
       | Some merged -> Some (actual -. merged)
       | None -> None
      )
  in

  (* compute sum of squared-errors *)
  let rec stats i actual mixed merged mixed_stats merged_stats =
    if i < num_intervals then
      let mixed_err, merged_err = error i actual mixed merged in
      let mixed_stats =
        match mixed_err with
        | Some err ->
          let sum_se_mixed, n_mixed = mixed_stats in
          sum_se_mixed +. err *. err, n_mixed + 1
        | None -> mixed_stats
      in
      let merged_stats =
        match merged_err with
        | Some err ->
          let sum_se_merged, n_merged = merged_stats in
          sum_se_merged +. err *. err, n_merged + 1
        | None -> merged_stats
      in
      stats (i+1) actual mixed merged mixed_stats merged_stats
    else
      mixed_stats, merged_stats
  in

  let mixed_q  = map_of_assoc (uniform mixed_h num_intervals) in
  let merged_q = map_of_assoc (uniform merged_h num_intervals) in
  let actual_q = map_of_assoc (quantiles data num_intervals) in

  let (sum_se_mixed, n_mixed), (sum_se_merged, n_merged) =
    stats 0 actual_q mixed_q merged_q (0., 0) (0., 0) in

  let err_mixed = sqrt ((sum_se_mixed) /. (float n_mixed)) in
  let err_merged = sqrt ((sum_se_merged) /. (float n_merged)) in
  Printf.printf "err_mixed=%e (n=%d)\nerr_merged=%e (n=%d)\n"
    err_mixed n_mixed err_merged n_merged

