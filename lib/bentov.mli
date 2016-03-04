type bin = {
  center : float;
  (* the center of the bin *)

  count : int;
  (* the number of values in the bin *)
}

type histogram

val bins : histogram -> bin list
(* [bins h] returns the list of bins, sorted by the bin center,
   comprising histogram [h] *)

val num_bins : histogram -> int
(* [num_bins h] returns the size of the histogram [h] in terms of the
   number of bins; equivalent to [List.length (bins h)] *)

val max_bins : histogram -> int
(* [max_bins h] returns the maximum number of bins of this histogram;
   when the number of unique values added to the histogram exceeds
   this [max_bins h], [h] becomes an approximation. *)

val total_count : histogram -> int
(* [total_count h] returns the number of values added to histogram [h] *)

val range : histogram -> (float * float) option
(* [range h] returns the minimum and maximum values seen in the
   construction of histogram h, or [None] if no values have yet been
   added *)

val create : int -> histogram
(* [create max_bins] creates a histogram with up to [max_bins] bins *)

val add : float -> histogram -> histogram
(* [add v h] adds a value to [v] to histogram [h], returning the
   updated histogram *)

val merge : histogram list -> int -> histogram
(* [merge h_list max_bins] creates a new histogram from the histograms
   in [h_list], whose size is no bigger than [max_bins] *)

exception TooDense
exception Empty

val uniform : histogram -> int -> float list

val mean : histogram -> float
val mean_variance : histogram -> float * float
