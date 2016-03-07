type bin = {
  center : float;
  count : int
}

type histogram = {
  max_bins : int;
  num_bins : int;
  bins : bin list;
  range : (float * float) option; (* (min * max) *)
  total_count : int;
}

let max_bins h =
  h.max_bins

let num_bins h =
  h.num_bins

let bins h =
  h.bins

let range h =
  h.range

let total_count h =
  h.total_count

(* not tail rec! *)
let rec insert value = function
  | [] -> [{ center = value ; count = 1 }], true
  | h :: t ->
    if value < h.center then
      { center = value ; count = 1 } :: h :: t, true
    else if value = h.center then
      { h with count = h.count + 1 } :: t, false
    else
      let t, num_bins_is_incr = insert value t in
      h :: t, num_bins_is_incr

let pr = Printf.printf

let rec min_diff_index i index min_diff = function
  | a :: b :: t ->
    let diff = b.center -. a.center in
    assert ( diff > 0. );
    if diff < min_diff then
      min_diff_index (i+1) i diff (b :: t)
    else
      (* no change *)
      min_diff_index (i+1) index min_diff (b :: t)

  | [ _ ] -> index
  | [] -> assert false

let min_diff_index = function
  | a :: b :: t ->
    let diff = b.center -. a.center in
    assert ( diff > 0. );
    min_diff_index 1 0 diff (b :: t)

  | [ _ ]
  | [] -> assert false

let merge_bins lo hi =
  assert (lo.center < hi.center);
  let sum_count = lo.count + hi.count in
  let center =
    (* average of centers, weighted by their height *)
    (lo.center *. (float lo.count) +. hi.center *. (float hi.count)) /.
    (float sum_count)
  in
  { center; count = sum_count }

(* not tail rec! *)
let merge_bins_at_index =
  let rec loop i index = function
    | a :: b :: t ->
      if i = index then
        let bin = merge_bins a b in
        bin :: t
      else
        a :: (loop (i + 1) index (b :: t))

    | [ _ ]
    | [] -> assert false
  in
  fun index bins ->
    loop 0 index bins

let create max_bins =
  if max_bins < 2 then
    raise (Invalid_argument (Printf.sprintf "max_bins: %d" max_bins))
  else {
    max_bins;
    num_bins = 0;
    bins = [];
    total_count = 0;
    range = None
  }

let add value histogram =
  let range =
    match histogram.range with
      | Some (mn, mx) -> Some (min value mn, max value mx)
      | None -> Some (value, value)
  in
  let total_count = histogram.total_count + 1 in
  let bins, is_augmented = insert value histogram.bins in
  if histogram.num_bins = histogram.max_bins then
    if is_augmented then
      (* merge bins, so as to keep their number at [max_bins] *)
      let index = min_diff_index bins in
      let bins = merge_bins_at_index index bins in
      { histogram with bins; range; total_count }
    else
      { histogram with bins; range; total_count }
  else
    if is_augmented then
      { histogram with bins; range; total_count;
                       num_bins = histogram.num_bins + 1; }
    else
      { histogram with bins; range; total_count }

(* merge two sorted bin lists; not tail rec! *)
let rec binary_merge a b =
  match a, b with
    | a_h :: a_t, b_h :: b_t ->
      if a_h.center < b_h.center then
        a_h :: (binary_merge a_t b)
      else if a_h.center > b_h.center then
        b_h :: (binary_merge a b_t)
      else
        (* a_h.center = b_h.center: merge the two cells into one *)
        let merged = { a_h with count = a_h.count + b_h.count } in
        merged :: (binary_merge a_t b_t)

    | [], _ :: _ -> b
    | _ :: _, [] -> a
    | [], [] -> []

let rec k_ary_merge_half accu = function
  | a :: b :: t ->
    let ab = binary_merge a b in
    k_ary_merge_half  (ab :: accu) t

  | [a] -> (a :: accu)
  | [] -> accu

let rec k_ary_merge t =
  match k_ary_merge_half [] t with
    | [a] -> a
    | t -> k_ary_merge t


let rec reduce bins ~num_bins ~max_bins =
  if num_bins > max_bins then
    let index = min_diff_index bins in
    let bins = merge_bins_at_index index bins in
    reduce bins ~num_bins:(num_bins - 1) ~max_bins
  else
    bins



let merge h_list max_bins =
  let bins, num_bins, total_count, range = List.fold_left
      (fun (t_bins, t_num_bins, t_total_count, t_range)
        { bins; num_bins; total_count; range} ->
        let t_range =
          match t_range, range with
            | Some (t_mn, t_mx), Some (mn, mx) ->
              Some ((min t_mn mn), (max t_mx mx))
            | None, Some _ -> range
            | Some _, None -> t_range
            | None, None -> None
        in
        bins :: t_bins,
        t_num_bins + num_bins,
        t_total_count + total_count,
        t_range
      ) ([], 0, 0, None) h_list in

  (* even if [num_bins <= output_max_bins], we have to apply
     [k_ary_merge] to combine indentical bin centers *)
  let merged_bins = k_ary_merge bins in
  let num_bins = List.length merged_bins in
  let bins = reduce merged_bins ~num_bins ~max_bins in
  let num_bins = List.length bins in
  { bins;
    num_bins;
    max_bins;
    total_count;
    range }

let pos_quadratic_root ~a ~b ~c =
  if a = 0.0 then
    -.c /. b
  else
    let discriminant = b *. b -. 4. *. a *. c in
    ((sqrt discriminant) -. b) /. (2. *. a)

exception TooDense
exception Empty

let linear_interp ~x0 ~x1 ~y0 ~y1 x =
  let delta_y = float (y1 - y0) in
  let delta_x = x1 -. x0 in
  let slope = delta_y /. delta_x in
  (float y0) +. slope *. (x -. x0)

(* sample the pdf represented by the [histogram] at value [x] *)
let count_at =
  (* linearly interpoloate between the centers, if necessary *)
  let rec loop x max = function
    | bin_0 :: bin_1 :: rest ->
      if bin_0.center <= x && x < bin_1.center then
        linear_interp
          ~x0:bin_0.center ~x1:bin_1.center
          ~y0:bin_0.count ~y1:bin_1.count
          x
      else
        loop x max (bin_1 :: rest)
    | [ bin_0 ] ->
      if max = bin_0.center then
        if x = max then
          float bin_0.count
        else
          0.0
      else
        linear_interp
          ~x0:bin_0.center ~x1:max
          ~y0:bin_0.count ~y1:0
          x
    | [] -> assert false
  in
  fun histogram x ->
    match histogram.bins, histogram.range with
      | bin_0 :: _, Some (min, max) ->
        if x < min || max < x then
          0.0
        else
          (* prepend left bondary *)
          let aug_bins =
            if bin_0.center = min then
              histogram.bins
            else
              { center = min; count = 0 } :: histogram.bins
          in
          loop x max aug_bins

      | [], None -> raise Empty
      | _ -> assert false

let pdf_at histogram x =
  (count_at histogram x) /. (float histogram.total_count)

let uniform =
  let rec loop span j accu cdf =
    match cdf with
      | cdf_i :: cdf_i1 :: rest ->
        let { center = p_i ; count = m_i }, sum_m_i  = cdf_i  in
        let { center = p_i1; count = m_i1}, sum_m_i1 = cdf_i1 in

        let s = (float j) *. span in
        if s > sum_m_i1 then
          loop span j accu (cdf_i1 :: rest)

        else (
          (* interpolate *)
          assert ( sum_m_i <= s );
          let d = s -. sum_m_i in
          let a = float (m_i1 - m_i) in
          let b = float (2 * m_i) in
          let c = -2. *. d in
          let z = pos_quadratic_root ~a ~b ~c in
          let u = p_i +. (p_i1 -. p_i) *. z in
          match accu with
            | u_prev :: _ when u_prev = u -> raise TooDense
            | _ ->
              let accu = u :: accu in
              loop span (j + 1) accu cdf
        )

      | _ -> List.rev accu

  in

  let rec cdf max prev_m sum_m accu = function
    | bin :: rest ->
      let { center = p; count = m } = bin in
      let sum_m = sum_m +. 0.5 *. (float (m + prev_m)) in
      let accu = (bin, sum_m) :: accu in
      cdf max m sum_m accu rest
    | [] ->
      let sum_m = sum_m +. 0.5 *. (float prev_m) in
      let accu = ({ center = max; count = 0 }, sum_m) :: accu in
      List.rev accu
  in
  fun histogram b ->
    match histogram.bins, histogram.range with
      | { center = p; count = m } :: rest, Some (min, max) ->
        let accu = [{center = min; count = 0}, 0.0] in
        let cdf = cdf max 0 0.0 accu histogram.bins in
        let span = (float histogram.total_count) /. (float b) in
        loop span 0 [] cdf
      (* , List.map (fun ({ center }, c) -> center, c) cdf *)

      | [], None -> raise Empty
      | _ -> assert false


let mean { bins; total_count } =
  if total_count = 0 then
    raise Empty
  else
    let m = List.fold_left (
      fun sum { center; count } ->
        sum +. center *. (float count)
    ) 0.0 bins in
    m /. (float total_count)

let mean_stdev histogram =
  let mean = mean histogram in
  let v = List.fold_left (
      fun sum { center; count } ->
        let diff = center -. mean in
        sum +. diff *. diff *. (float count)
    ) 0.0 histogram.bins
  in
  let stdev = sqrt (v /. (float histogram.total_count)) in
  mean, stdev

