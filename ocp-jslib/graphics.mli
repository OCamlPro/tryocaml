(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../../LICENSE.  *)
(*                                                                     *)
(***********************************************************************)

(* $Id: graphics.mli 11156 2011-07-27 14:17:02Z doligez $ *)

(** Machine-independent graphics primitives. *)

exception Graphic_failure of string
(** Raised by the functions below when they encounter an error. *)


(** {6 Initializations} *)

val open_graph : string -> unit
(** Show the graphics window or switch the screen to graphic mode.
   The graphics window is cleared and the current point is set
   to (0, 0). The string argument is used to pass optional
   information on the desired graphics mode, the graphics window
   size, and so on. Its interpretation is implementation-dependent.
   If the empty string is given, a sensible default is selected. *)

val close_graph : unit -> unit
(** Delete the graphics window or switch the screen back to text mode. *)

val set_window_title : string -> unit
(** Set the title of the graphics window. *)

val resize_window : int -> int -> unit
(** Resize and erase the graphics window. *)

val clear_graph : unit -> unit
(** Erase the graphics window. *)

val size_x : unit -> int
(** See {!Graphics.size_y}. *)

val size_y : unit -> int
(** Return the size of the graphics window. Coordinates of the screen
   pixels range over [0 .. size_x()-1] and [0 .. size_y()-1].
   Drawings outside of this rectangle are clipped, without causing
   an error. The origin (0,0) is at the lower left corner. *)

(** {6 Colors} *)

type color = int
(** A color is specified by its R, G, B components. Each component
   is in the range [0..255]. The three components are packed in
   an [int]: [0xRRGGBB], where [RR] are the two hexadecimal digits for
   the red component, [GG] for the green component, [BB] for the
   blue component. *)

val rgb : int -> int -> int -> color
(** [rgb r g b] returns the integer encoding the color with red
   component [r], green component [g], and blue component [b].
   [r], [g] and [b] are in the range [0..255]. *)

val set_color : color -> unit
(** Set the current drawing color. *)

val background : color
(** See {!Graphics.foreground}.*)

val foreground : color
(** Default background and foreground colors (usually, either black
   foreground on a white background or white foreground on a
   black background).
   {!Graphics.clear_graph} fills the screen with the [background] color.
   The initial drawing color is [foreground]. *)


(** {7 Some predefined colors} *)

val black : color
val white : color
val red : color
val green : color
val blue : color
val yellow : color
val cyan : color
val magenta : color


(** {6 Point and line drawing} *)

val plot : int -> int -> unit
(** Plot the given point with the current drawing color. *)

val plots : (int * int) array -> unit
(** Plot the given points with the current drawing color. *)

val point_color : int -> int -> color
(** Return the color of the given point in the backing store
   (see "Double buffering" below). *)

val moveto : int -> int -> unit
(** Position the current point. *)

val rmoveto : int -> int -> unit
(** [rmoveto dx dy] translates the current point by the given vector. *)

val current_x : unit -> int
(** Return the abscissa of the current point. *)

val current_y : unit -> int
(** Return the ordinate of the current point. *)

val current_point : unit -> int * int
(** Return the position of the current point. *)

val lineto : int -> int -> unit
(** Draw a line with endpoints the current point and the given point,
   and move the current point to the given point. *)

val rlineto : int -> int -> unit
(** Draw a line with endpoints the current point and the
   current point translated of the given vector,
   and move the current point to this point. *)

val curveto : int * int -> int * int -> int * int -> unit
(** [curveto b c d] draws a cubic Bezier curve starting from
   the current point to point [d], with control points [b] and
   [c], and moves the current point to [d]. *)

val draw_rect : int -> int -> int -> int -> unit
(** [draw_rect x y w h] draws the rectangle with lower left corner
   at [x,y], width [w] and height [h].
   The current point is unchanged.
   Raise [Invalid_argument] if [w] or [h] is negative. *)

val draw_poly_line : (int * int) array -> unit
(** [draw_poly_line points] draws the line that joins the
   points given by the array argument.
   The array contains the coordinates of the vertices of the
   polygonal line, which need not be closed.
   The current point is unchanged. *)

val draw_poly : (int * int) array -> unit
(** [draw_poly polygon] draws the given polygon.
   The array contains the coordinates of the vertices of the
   polygon.
   The current point is unchanged. *)

val draw_segments : (int * int * int * int) array -> unit
(** [draw_segments segments] draws the segments given in the array
   argument. Each segment is specified as a quadruple
   [(x0, y0, x1, y1)] where [(x0, y0)] and [(x1, y1)] are
   the coordinates of the end points of the segment.
   The current point is unchanged. *)

val draw_arc : int -> int -> int -> int -> int -> int -> unit
(** [draw_arc x y rx ry a1 a2] draws an elliptical arc with center
   [x,y], horizontal radius [rx], vertical radius [ry], from angle
   [a1] to angle [a2] (in degrees). The current point is unchanged.
   Raise [Invalid_argument] if [rx] or [ry] is negative. *)

val draw_ellipse : int -> int -> int -> int -> unit
(** [draw_ellipse x y rx ry] draws an ellipse with center
   [x,y], horizontal radius [rx] and vertical radius [ry].
   The current point is unchanged.
   Raise [Invalid_argument] if [rx] or [ry] is negative. *)

val draw_circle : int -> int -> int -> unit
(** [draw_circle x y r] draws a circle with center [x,y] and
   radius [r]. The current point is unchanged.
   Raise [Invalid_argument] if [r] is negative. *)

val set_line_width : int -> unit
(** Set the width of points and lines drawn with the functions above.
   Under X Windows, [set_line_width 0] selects a width of 1 pixel
   and a faster, but less precise drawing algorithm than the one
   used when [set_line_width 1] is specified.
   Raise [Invalid_argument] if the argument is negative. *)

(** {6 Text drawing} *)

val draw_char : char -> unit
(** See {!Graphics.draw_string}.*)

val draw_string : string -> unit
(** Draw a character or a character string with lower left corner
   at current position. After drawing, the current position is set
   to the lower right corner of the text drawn. *)

val set_font : string -> unit
(** Set the font used for drawing text.
   The interpretation of the argument to [set_font]
   is implementation-dependent. *)

val set_text_size : int -> unit
(** Set the character size used for drawing text.
   The interpretation of the argument to [set_text_size]
   is implementation-dependent. *)

val text_size : string -> int * int
(** Return the dimensions of the given text, if it were drawn with
   the current font and size. *)


(** {6 Filling} *)

val fill_rect : int -> int -> int -> int -> unit
(** [fill_rect x y w h] fills the rectangle with lower left corner
   at [x,y], width [w] and height [h], with the current color.
   Raise [Invalid_argument] if [w] or [h] is negative. *)

val fill_poly : (int * int) array -> unit
(** Fill the given polygon with the current color. The array
   contains the coordinates of the vertices of the polygon. *)

val fill_arc : int -> int -> int -> int -> int -> int -> unit
(** Fill an elliptical pie slice with the current color. The
   parameters are the same as for {!Graphics.draw_arc}. *)

val fill_ellipse : int -> int -> int -> int -> unit
(** Fill an ellipse with the current color. The
   parameters are the same as for {!Graphics.draw_ellipse}. *)

val fill_circle : int -> int -> int -> unit
(** Fill a circle with the current color. The
   parameters are the same as for {!Graphics.draw_circle}. *)


(** {6 Images} *)

type image
(** The abstract type for images, in internal representation.
   Vally, images are represented as matrices of colors. *)

val transp : color
(** In matrices of colors, this color represent a ``transparent''
   point: when drawing the corresponding image, all pixels on the
   screen corresponding to a transparent pixel in the image will
   not be modified, while other points will be set to the color
   of the corresponding point in the image. This allows superimposing
   an image over an existing background. *)

val make_image : color array array -> image
(** Convert the given color matrix to an image.
   Each sub-array represents one horizontal line. All sub-arrays
   must have the same length; otherwise, exception [Graphic_failure]
   is raised. *)

val dump_image : image -> color array array
(** Convert an image to a color matrix. *)

val draw_image : image -> int -> int -> unit
(** Draw the given image with lower left corner at the given point. *)

val get_image : int -> int -> int -> int -> image
(** Capture the contents of a rectangle on the screen as an image.
   The parameters are the same as for {!Graphics.fill_rect}. *)

val create_image : int -> int -> image
(** [create_image w h] returns a new image [w] pixels wide and [h]
   pixels tall, to be used in conjunction with [blit_image].
   The initial image contents are random, except that no point
   is transparent. *)

val blit_image : image -> int -> int -> unit
(** [blit_image img x y] copies screen pixels into the image [img],
   modifying [img] in-place. The pixels copied are those inside the
   rectangle with lower left corner at [x,y], and width and height
   equal to those of the image. Pixels that were transparent in
   [img] are left unchanged. *)


(** {6 Mouse and keyboard events} *)

type status =
  { mouse_x : int;              (** X coordinate of the mouse *)
    mouse_y : int;              (** Y coordinate of the mouse *)
    button : bool;              (** true if a mouse button is pressed *)
    keypressed : bool;          (** true if a key has been pressed *)
    key : char;                 (** the character for the key pressed *)
  }
(** To report events. *)


type event =
    Button_down                 (** A mouse button is pressed *)
  | Button_up                   (** A mouse button is released *)
  | Key_pressed                 (** A key is pressed *)
  | Mouse_motion                (** The mouse is moved *)
  | Poll                        (** Don't wait; return immediately *)
(** To specify events to wait for. *)


val wait_next_event : event list -> status
(** Wait until one of the events specified in the given event list
   occurs, and return the status of the mouse and keyboard at
   that time. If [Poll] is given in the event list, return immediately
   with the current status. If the mouse cursor is outside of the
   graphics window, the [mouse_x] and [mouse_y] fields of the event are
   outside the range [0..size_x()-1, 0..size_y()-1]. Keypresses
   are queued, and dequeued one by one when the [Key_pressed]
   event is specified. *)

val loop_on_exit : event list -> (status -> unit) -> unit
(** Loops until exit and listen to the given events. Those events automatically
    returns a status record, which is used by the function given in argument. *)

(** {6 Mouse and keyboard polling} *)

val mouse_pos : unit -> int * int
(** Return the position of the mouse cursor, relative to the
   graphics window. If the mouse cursor is outside of the graphics
   window, [mouse_pos()] returns a point outside of the range
   [0..size_x()-1, 0..size_y()-1]. *)

val button_down : unit -> bool
(** Return [true] if the mouse button is pressed, [false] otherwise. *)

val read_key : unit -> char
(** Wait for a key to be pressed, and return the corresponding
   character. Keypresses are queued. *)

val key_pressed : unit -> bool
(** Return [true] if a keypress is available; that is, if [read_key]
   would not block. *)


(** {6 Sound} *)

val sound : int -> int -> unit
(** [sound freq dur] plays a sound at frequency [freq] (in hertz)
   for a duration [dur] (in milliseconds). *)

(** {6 Double buffering} *)

val auto_synchronize : bool -> unit
(** By default, drawing takes place both on the window displayed
   on screen, and in a memory area (the ``backing store'').
   The backing store image is used to re-paint the on-screen
   window when necessary.

   To avoid flicker during animations, it is possible to turn
   off on-screen drawing, perform a number of drawing operations
   in the backing store only, then refresh the on-screen window
   explicitly.

   [auto_synchronize false] turns on-screen drawing off.  All
   subsequent drawing commands are performed on the backing store
   only.

   [auto_synchronize true] refreshes the on-screen window from
   the backing store (as per [synchronize]), then turns on-screen
   drawing back on.  All subsequent drawing commands are performed
   both on screen and in the backing store.

   The default drawing mode corresponds to [auto_synchronize true]. *)

val synchronize : unit -> unit
(** Synchronize the backing store and the on-screen window, by
   copying the contents of the backing store onto the graphics
   window. *)


val display_mode : bool -> unit
(** Set display mode on or off. When turned on, drawings are done
   in the graphics window; when turned off, drawings do not affect
   the graphics window.  This occurs independently of
   drawing into the backing store (see the function {!Graphics.remember_mode}
   below). Default display mode is on. *)


val remember_mode : bool -> unit
(** Set remember mode on or off. When turned on, drawings are done
   in the backing store; when turned off, the backing store is
   unaffected by drawings.  This occurs independently of drawing
   onto the graphics window (see the function {!Graphics.display_mode} above).
   Default remember mode is on.  *)
