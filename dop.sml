(* blank file *)

fun chop str = String.substring (str, 0, (String.size str) - 1)

signature Dop =
sig
  exception DopException of string

  datatype command =
      CmdAdd of string * string
    | CmdRemove of string
    | CmdList of string
    | CmdChDir of string
    | CmdPrint of string

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
    | CmdList of string
    | CmdChDir of string
    | CmdPrint of string

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
    readEntry fh
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
       | CmdList key => (case getDir(key, entries) of 
                              SOME dir => let
                                val entry = (case get (key, entries) of
                                                  SOME k => k
                                                | NONE   => raise DopException ("Entry not found: " ^ key))
                              in
                                printEntry entry
                              end
                            | NONE     => app printEntry entries)
        | CmdChDir key => (case getDir (key, entries) of
                                SOME dir => OS.FileSys.chDir dir
                              | NONE     => raise DopException ("Entry not found :" ^ key))
        | CmdPrint key => (case getDir (key, entries) of
                                SOME dir => print (dir ^ "\n")
                              | NONE     => raise DopException ("Entry not found: " ^ key))

  end

  fun cmdStrToCmd str params = let
    fun param num = List.nth (params, num)
    val nparams = List.length params
  in
    case str of
         "dpush"      => let
           val key = param 0
           val dir = if nparams > 1 then param 1 else OS.FileSys.getDir()
         in
           CmdAdd (key, dir)
         end
       | "dpop"       => let
           val key = if nparams > 0 then param 0
                     else OS.FileSys.getDir()
         in
           CmdRemove (key)
         end
       | "dlist"     => if nparams > 0 then CmdList (param 0) else CmdList ("")
       | "dchange"   => CmdChDir (param 0)
       | "dprint"    => CmdPrint (param 0)
       | _           => raise DopException ("Invalid command: " ^ str)
  end

  fun run name params = eval (cmdStrToCmd name params)

end

fun main (name, args) = let
  val base = OS.Path.file name
  val cmd = if base = "dop"
            then if length args = 0 then "dlist" else List.hd args
            else base
  val params = if base = "dop"
               then if length args = 0 then [] else List.tl args
               else args
  fun createIfMissing file = let
    fun createEmpty f = let
      val fh = TextIO.openOut f
    in
      TextIO.closeOut fh
    end
    val fh = TextIO.openIn file handle SysErr => (createEmpty file; TextIO.openIn file)
  in
    TextIO.closeIn fh
  end
in
  ( createIfMissing (Dop.getFileLocation ()); Dop.run cmd params; 0 )
end

val _ = main(CommandLine.name(), CommandLine.arguments())
    handle Dop.DopException msg => (print ("error: " ^ msg ^ "\n"); 1)
