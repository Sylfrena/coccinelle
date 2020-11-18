open Printf
open Str

(*Read file contents*)
let read_whole_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

(*Breakdown split_result_list type to primtive type*)
let rec decompose_list (l: Str.split_result list)  =
  match l with
  | [] -> []
  | Text hd :: tl ->  decompose_list tl
  | Delim hd :: tl -> hd ::  decompose_list tl
;;

(*Fetch diff for specified path*)
let bash_read_help fpath =
  let command = "cd " ^ fpath ^ " && git diff | egrep '^+++|^@'" in
  let ic = Unix.open_process_in command in
  let all_input = ref [] in
  try
    while true do
      all_input := input_line ic :: !all_input
    done;
    !all_input
  with
    End_of_file -> close_in ic;
    List.rev !all_input;;

let extract_numbers line =
  let sep = String.split_on_char ',' line in
  match sep with
  | [l] -> (int_of_string l, int_of_string l)
  | [f ; e] -> (int_of_string f, int_of_string e)
  | _ -> failwith "no line numbers found, sorry"
;;

let fetch_file_name line =
  let pat_filename = Str.regexp "\\(+++ b\\)/\\(.+\\)\\.[a-z]+" in
  let s = Str.full_split pat_filename line in
  decompose_list s
;;

let fetch_line_number line  =
  let pat_line_num = Str.regexp "\\(\\+\\([0-9]+,[0-9]+\\)\\)" in
  let s = Str.full_split pat_line_num line in
  let fa = List.hd (decompose_list s) in
  let (b, e) = extract_numbers fa in
  (b, (b + e)) (*start of diff, end of diff *)
;;

type diff_info =
{
    file_name: string;
    line_no: (int * int) list;
}

type patch_info = No_info | File_info of string list | Line_info of (int * int)

(*check line and extract file name or file number*)
let extract_info line =
  if Str.string_match (Str.regexp_string "+++") line 0 then
    File_info (fetch_file_name line)
  else if Str.string_match (Str.regexp_string "@@ ") line 0 then
    Line_info (fetch_line_number line)
  else
    No_info

let rec reorg_helper new_list = function
[] -> (List.rev new_list, [])
| No_info :: tl -> reorg_helper new_list tl
| File_info file_list :: tl -> (List.rev new_list, File_info file_list :: tl)
| Line_info line_list :: tl ->  reorg_helper (line_list :: new_list) tl
;;

let rec reorg fpath = function
[] -> []
| No_info :: tl -> reorg fpath tl
| File_info [file] :: tl ->
  let lines, rest = reorg_helper [] tl in
  {
    file_name = Str.replace_first (Str.regexp "+++ b") fpath file;
    line_no = lines;
  } :: reorg fpath rest
| File_info file_list :: tl ->
    let lines, rest = reorg_helper [] tl in
    reorg fpath rest
| Line_info line_list :: tl ->  failwith "bad case"

let rec mlines lines =
  match lines with
  | [] -> []
  | hd :: tl -> extract_info hd :: mlines tl

let final_info dir =
  let list_diff = reorg dir (mlines (bash_read_help dir)) in
  list_diff;;

let rec print_tuple_list l =
  match l with
  | [] -> ()
  | (a,b) :: tl -> printf "\n%d, %d" a b; print_tuple_list tl

let getpatchdiff dir = final_info dir

