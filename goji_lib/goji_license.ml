(****************************************************************************)
(* GOJI (JavaScript Interface Generator for OCaml)                          *)
(* This file is published under the CeCILL licence                          *)
(* (C) 2013 Benjamin Canou                                                  *)
(****************************************************************************)

(** Type for licenses and collection of predefined ones. *)

(** The type of licenses. *)
type t = {
  short_name : string ; (** The acronym. *)
  long_name : string ; (** The spoken expansion of the acronym. *)
  url : string ; (** An URL to access the text of the license. *)
}

(** GNU General Public License (version 3 or superior) *)
let gpl_v3_plus =
  { short_name = "GNU GPL v3+" ;
    long_name = "GNU General Public License (version 3 or superior)" ;
    url = "https://gnu.org/licenses/gpl-3.0.txt" }

(** GNU General Public License (version 2 or superior) *)
let gpl_v2_plus =
  { short_name = "GNU GPL v2+" ;
    long_name = "GNU General Public License (version 2 or superior)" ;
    url = "https://gnu.org/licenses/gpl-2.0.txt" }

(** GNU General Public License (version 3) *)
let gpl_v3 =
  { short_name = "GNU GPL v3" ;
    long_name = "GNU General Public License (version 3)" ;
    url = "https://gnu.org/licenses/gpl-3.0.txt" }

(** GNU General Public License (version 2) *)
let gpl_v2 =
  { short_name = "GNU GPL v2" ;
    long_name = "GNU General Public License (version 2)" ;
    url = "https://gnu.org/licenses/gpl-2.0.txt" }

(** GNU Lesser General Public License (version 3 or superior) *)
let lgpl_v3_plus =
  { short_name = "GNU LGPL v3+" ;
    long_name = "GNU Lesser General Public License (version 3 or superior)" ;
    url = "https://gnu.org/licenses/lgpl-3.0.txt" }

(** GNU Library General Public License (version 2 or superior) *)
let lgpl_v2_plus =
  { short_name = "GNU LGPL v2+" ;
    long_name = "GNU Library General Public License (version 2 or superior)" ;
    url = "https://gnu.org/licenses/lgpl-2.0.txt" }

(** GNU Lesser General Public License (version 3) *)
let lgpl_v3 =
  { short_name = "GNU LGPL v3" ;
    long_name = "GNU Lesser General Public License (version 3)" ;
    url = "https://gnu.org/licenses/lgpl-3.0.txt" }

(** GNU Library General Public License (version 2) *)
let lgpl_v2 =
  { short_name = "GNU LGPL v2" ;
    long_name = "GNU Library General Public License (version 2)" ;
    url = "https://gnu.org/licenses/lgpl-2.0.txt" }

(** MIT License *)
let mit =
  { short_name = "MIT" ;
    long_name = "MIT License" ;
    url = "http://opensource.org/licenses/MIT" }

(** Do What The Fuck You Want to Public License *)
let wtfpl =
  { short_name = "WTFPL" ;
    long_name = "Do What The Fuck You Want to Public License" ;
    url = "http://www.wtfpl.net/txt/copying/" }

(** Zlib License *)
let zlib =
  { short_name = "ZLIB" ;
    long_name = "Zlib License" ;
    url = "http://zlib.net/zlib_license.html" }
