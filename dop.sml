(* blank file *)

fun chop str = String.substring (str, 0, (String.size str) - 1)

signature Dop =
sig
  exception DopException of string

  datatype command =
      CmdAdd of string * string
    | CmdRemove of string
    | CmdList of string option
    | CmdChDir of string

  val empty : (string * string) list
  val add : (string * string) * (string * string) list -> (string * string) list
  val remove : string * (string * string) list -> (string * string) list
  val replace : (string * string) * (string * string) list -> (string * string) list
  val printEntry : string * string -> unit
  val get : string * (string * string) list -> (string * string) option
  val getDir : string * (string * string) list -> string option
  val getByDir : string * (string * string) list -> string option
  val exists : string * (string * string) list -> bool
  val save : string * (string * string) list -> unit
  val load : string -> (string * string) list
  val eval : command -> unit
  val run  : string -> string list -> unit
  val getFileLocation : unit -> string
end

structure Dop :> Dop =
struct
  exception DopException of string

  datatype command =
      CmdAdd of string * string
    | CmdRemove of string
    | CmdList of string option
    | CmdChDir of string

  val empty = []

  fun getFileLocation _ = let
    val home = case Posix.ProcEnv.getenv "HOME" of
                    SOME x => x
                  | NONE   => raise DopException "$HOME not defined"
  in
    home ^ "/.dop"
  end

  fun add(entry, entries) = entry :: entries

  fun remove (key, []) = []
    | remove (key, (k,p)::xs) = if key = (k:string) then remove (key, xs)
                                else (k,p)::remove (key, xs)

  fun printEntry (a,b) = print (a ^ "\t" ^ b ^ "\n")

  fun get (key, []) = NONE
    | get (key, ((k,p)::xs)) = if key = (k:string) then SOME (k,p)
                               else get (key, xs)

  fun getDir (key, []) = NONE
    | getDir (key, ((k,p)::xs)) = if key = (k:string) then SOME p
                                  else getDir (key, xs)

  fun getByDir (dir, []) = NONE
    | getByDir (dir, ((k,p)::xs)) = if dir = (p:string) then SOME k
                                    else getByDir (dir, xs)

  fun exists (key, entries) = 
    case getDir (key, entries) of
         SOME x => true
       | NONE   => false

  fun replace ((key, dir), []) = []
    | replace ((key, dir), ((k,p)::xs)) = if key = (k:string)
                                          then (k,dir)::replace ((key, dir), xs)
                                          else (k,p)::replace ((key, dir), xs)

  fun save(filename, entries) = let
    val fh = TextIO.openOut filename
    fun writeEntry (f, (k,p)::xs) = (
      TextIO.output (f, k ^ "\t" ^ p ^ "\n");
      writeEntry (f, xs))
    | writeEntry (f, []) = ()
  in
    writeEntry (fh, entries);
    TextIO.closeOut fh
  end

  fun load filename = let
    val fh = TextIO.openIn filename
    fun readEntry f = let
      val line = case TextIO.inputLine f of
                      SOME x => x
                    | NONE   => raise DopException ("Invalid input in " ^ filename)
      val parts = String.tokens (fn x => x = #"\t") line
      val key = hd parts
      val dir = chop (List.last parts)
    in
      if TextIO.endOfStream fh then (key, dir)::[]
      else (key, dir)::(readEntry fh)
    end
  in
    if TextIO.endOfStream fh then [] else readEntry fh
  end

  fun eval cmd = let
    val path = getFileLocation ()
    val entries = load path
  in
    case cmd of
         CmdAdd (key, dir) => let
           val e = if exists (key, entries) then replace((key, dir), entries)
                   else add ((key, dir), entries)
         in
           save (path, e)
         end
       | CmdRemove key => let
           val realKey = if exists (key, entries) then key
                         else case getByDir (key, entries) of
                                   SOME dir => dir
                                 | NONE     => raise DopException ("Entry not found: " ^ key)
         in
           save (path, remove(realKey, entries))
         end
       | CmdList (SOME key) => (case getDir(key, entries) of
                                     SOME dir => let
                                       val entry = (case get (key, entries) of
                                                         SOME k => k
                                                       | NONE   => raise DopException ("Entry not found: " ^ key))
                                       val (_,jim) = entry
                                     in
                                       print (dir ^ "\n")
                                     end
                                   | NONE     => raise DopException ("Entry not found: " ^ key))
        | CmdList NONE => app printEntry entries
        | CmdChDir key => (case getDir (key, entries) of
                                SOME dir => OS.FileSys.chDir dir
                              | NONE     => raise DopException ("Entry not found :" ^ key))
  end

  fun cmdStrToCmd str params = let
    fun param num = List.nth (params, num)
    val nparams = List.length params
  in
    case (str, params) of
         ("dpush", [key])      => CmdAdd(key, OS.FileSys.getDir())
       | ("dpush", [key, dir]) => CmdAdd(key, dir)
       | ("dpush", _)          => raise DopException ("dpush <key> [dir]")
       | ("dpop", [key])       => CmdRemove key
       | ("dpop", _)           => CmdRemove (OS.FileSys.getDir())
       | ("dlist", [key])      => CmdList (SOME key)
       | ("dlist", _)          => CmdList NONE
       | ("dchange", [key])    => CmdChDir key
       | ("dchange", _)        => raise DopException ("dchange <key>")
       | _                     => raise DopException ("Invalid command: " ^ str)
  end

  fun run name params = eval (cmdStrToCmd name params)
end

fun main (name, args) = let
  val base = OS.Path.file name
  val (cmd, params) = case (base, args) of
                           ("dop", nil)    => ("dlist", nil)
                         | ("dop", x::xs)  => (x, xs)
                         | _               => (base, args)
  fun createIfMissing file = let
    fun createEmpty f = TextIO.closeOut (TextIO.openOut f)
    val fh = TextIO.openIn file handle SysErr => (createEmpty file; TextIO.openIn file)
  in
    TextIO.closeIn fh
  end
in
  ( createIfMissing (Dop.getFileLocation ());
    Dop.run cmd params;
    0 )
end

val _ = main(CommandLine.name(), CommandLine.arguments())
    handle Dop.DopException msg => (print ("error: " ^ msg ^ "\n"); 1)
