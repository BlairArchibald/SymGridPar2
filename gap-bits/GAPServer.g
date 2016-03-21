## Text-based GAP server.
## * Receiving commands on stdin.
##   * Commands consist of 3 parts, only the 3rd of which is payload:
##     (1) a natural number n (in decimal representation)
##     (2) a newline, and
##     (3) an n-character string.
##         The string must either be a valid GAP command (including the
##         trailing ';'), possibly prefixed by '!' (to ignore the result).
##   * Failure to respect the input format (eg. non-decimal rep of n, or
##     providing a string of fewer than n characters) will either result in
##     the server crashing or locking up (waiting for further input).
## * Responding on stdout.
##   * Responses consist of 3 parts, only the 3rd of which is payload:
##     (1) a reply prefix string,
##     (2) a natural number n (64-bit wide, in little endian format), and
##     (3) an n-character string (encoding the result of the command).
##   * The reply prefix string may not appear in any GAP output or error
##     messages, not even if quoted or escaped.
## * Encoding (see function ToHaskell for details).
##   * Each GAP object is encoded as a string consisting of one ore three fields
##     (in this order):
##     * type tag (discrimiating Booleans, integers, rationals, strings, lists,
##       opaque objects and failure)
##     * number of content items (string of bytes representing a natural number
##       in little endian format, 8- or 64-bit wide, depending on type tag), and
##     * contents (string).
##   * Booleans are encoded via the type tag;
##     #items == 0 (ie. there are no #items and content fields)
##   * The absolute value of Integers is encoded as a string of bytes in
##     little endian format, the sign is encoded via the type tag;
##     #items == length of string (which is 0, 1, 2, 4, or a multiple of 8)
##   * Rationals are encoded as 2 integers (numerator followed by denominator);
##     #items == 0 (ie. there is no explicit #items fields)
##   * Strings are encoded as sequences of characters;
##     #items == length of string
##   * Lists are encoded as sequences of encodings of elements;
##     #items == length of list
##   * Opaque objects are encoded as strings obtained by printing them in GAP;
##     #items == length of string representation
##   * Failure is encoded via the type tag; #items == 0
## * Startup.
##   * First thing, the server writes its own version string to stdout.
##   * Second, the server reads the reply prefix string from stdin;
##     the format is the same as for commands.
##   * The server then replies with the reply prefix string and one extra line
##     (with debug info) to indicate that it is ready to take commands.
## * Shutdown.
##   * The server terminates when it receives an empty string as command.


# version string (must not contain characters ^ and $)
version@HaskellGAP := "2b";


# load IO package
LoadPackage("io");


################################################################################
###
### utils copied from "pkg/scscp/lib/utils.g" (see doc there)
###

IO_PickleToString := function(obj)
  local rb, wb, s;
  rb := "";
  wb := "";
  s := IO_WrapFD(-1, rb, wb);
  IO_Pickle(s, obj);
  IO_Close(s);
  return wb;
end;

IO_UnpickleFromString := function(str)
  local rb, wb, s, r;
  rb := str;
  wb := "";
  s := IO_WrapFD(-1, rb, wb);
  r := IO_Unpickle(s);
  IO_Close(s);
  return r;
end;

###
### end code copied from "pkg/scscp/lib/utils.g"
###
#
###############################################################################


# set input and output streams (stdin buffered because IO_ReadLine needs that)
stdin@HaskellGAP  := IO_WrapFD(0, 512,   false);
stdout@HaskellGAP := IO_WrapFD(1, false, false);
stderr@HaskellGAP := IO_WrapFD(2, false, false);

# record process IDs (for debugging)
pids@HaskellGAP :=
  Concatenation(String(IO_getppid()), ".", String(IO_getpid()));

# uncomment these lines (and all marked '## DEBUG') to debug
#logfile@HaskellGAP := Concatenation("log-", pids@HaskellGAP);  ## DEBUG
#log@HaskellGAP := IO_File(logfile@HaskellGAP, "w", false);     ## DEBUG


# supply version information; other party expected to detect mismatch
IO_WriteFlush(stdout@HaskellGAP, "^", version@HaskellGAP, "$");


# Converts an integer i into a string of k bytes encoding the absolute value
# of i in little endian format such that
# * k = 0 if i = 0,
# * k = 1 if |i| < 2^8,
# * k = 2 if |i| < 2^16,
# * k = 4 if |i| < 2^32, or
# * otherwise k is a multiple of 8.
# The type of i is not checked.
LittleEndianStringAbsInt@HaskellGAP := function(i)
  local n, s, k;
  n := AbsInt(i);
  s := List(CoefficientsQadic(n, 256), CharInt);
  k := Length(s);
  if k <= 2 then
    return s;  ## NOTE: Length(s) = 0, 1, or 2
  elif k <= 4 then
    Append(s, ListWithIdenticalEntries(4 - k, CharInt(0)));
    return s;  ## NOTE: Length(s) = 4
  else
    Append(s, ListWithIdenticalEntries((-k) mod 8, CharInt(0)));
    return s;  ## NOTE: Length(s) = multiple of 8
  fi;
end;

# Converts an integer n (in the range 0 <= n < 2^64) into a string of 8 bytes
# encoding n in little endian format.
# The type and range of n are not checked; type and range should be ok
# if n is the size of an object in main memory.
LittleEndianStringSize8@HaskellGAP := function(n)
  local s, k;
  s := List(CoefficientsQadic(n, 256), CharInt);
  k := Length(s);
  Append(s, ListWithIdenticalEntries(8 - k, CharInt(0)));
  return s;
end;

# Converts an integer n (in the range 0 <= n < 2^64) into a string of either
# 1 or 8 bytes encoding n in little endian format.
# The type and range of n is not checked; type and range should be ok
# if n is the size of an object in main memory.
LittleEndianStringSize@HaskellGAP := function(n)
  if n < 256 then
    return [CharInt(n)];
  else
    return LittleEndianStringSize8@HaskellGAP(n);
  fi;
end;


# Appends a string encoding of the given object to the given output buffer.
# The encoding is described in detail in the preamble of this file.
ToHaskell@HaskellGAP := function(out, obj)
  local s, n, elt;
  if IsBool(obj) then
    if obj = fail then
      Append(out, "!");    # tag: fail
    elif obj = true then
      Append(out, "1");    # tag: Boolean true
    else
      Append(out, "0");    # tag: Boolean false
    fi;
  elif IsInt(obj) then
    s := LittleEndianStringAbsInt@HaskellGAP(obj);
    n := LittleEndianStringSize@HaskellGAP(Length(s));
    if Length(n) = 1 then
      if IsNegInt(obj) then
        Append(out, "n");  # tag: negative integer (< 2^8 bytes)
      else
        Append(out, "p");  # tag: zero or positive integer (< 2^8 bytes)
      fi;
    else
      if IsNegInt(obj) then
        Append(out, "N");  # tag: negative integer (>= 2^8 bytes)
      else
        Append(out, "P");  # tag: positive integer (>= 2^8 bytes)
      fi;
    fi;
    Append(out, n);
    Append(out, s);
  elif IsRat(obj) then
    Append(out, "/");      # tag: rational number (consisting of 2 integers)
    ToHaskell@HaskellGAP(out, NumeratorRat(obj));
    ToHaskell@HaskellGAP(out, DenominatorRat(obj));
  elif obj = [] then
    Append(out, "l");      # tag: list (< 2^8 elements)
    Append(out, [CharInt(0)]);  # 0 elements, ie. empty list or empty string
  elif IsString(obj) then
    n := LittleEndianStringSize@HaskellGAP(Length(obj));
    if Length(n) = 1 then
      Append(out, "s");    # tag: non-empty string (< 2^8 bytes)
    else
      Append(out, "S");    # tag: string (>= 2^8 bytes)
    fi;
    Append(out, n);
    Append(out, obj);
  elif IsDenseList(obj) then
    n := LittleEndianStringSize@HaskellGAP(Length(obj));
    if Length(n) = 1 then
      Append(out, "l");    # tag: non-empty list (< 2^8 elements)
    else
      Append(out, "L");    # tag: list (>= 2^8 elements)
    fi;
    Append(out, n);
    for elt in obj do
      ToHaskell@HaskellGAP(out, elt);
    od;
  else
    s := String(obj);
    n := LittleEndianStringSize@HaskellGAP(Length(s));
    if Length(n) = 1 then
      Append(out, "o");    # tag: opaque object (< 2^8 bytes string encoding)
    else
      Append(out, "O");    # tag: opaque object (>= 2^8 bytes string encoding)
    fi;
    Append(out, n);
    Append(out, s);
  fi;
end;


# Reads a string of length n from stdin, where the number n (in decimal ASCII
# encoding) precedes the string on a separate line.
ReadInput@HaskellGAP := function()
  local data, data_size_str, bytes_to_read, chunk, chunk_size;
  data := "";
  data_size_str := IO_ReadLine(stdin@HaskellGAP);
  if data_size_str = fail then IO_exit(1); fi;
  NormalizeWhitespace(data_size_str);
  if Length(data_size_str) = 0 then
    bytes_to_read := 0;
  else
    bytes_to_read := Int(data_size_str);
  fi;
  if bytes_to_read = fail then IO_exit(2); fi;
  while bytes_to_read > 0 do
    chunk := IO_Read(stdin@HaskellGAP, bytes_to_read);
    if chunk = fail then IO_exit(3); fi;
    chunk_size := Length(chunk);
    if chunk_size = 0 then IO_exit(4); fi;
    Append(data, chunk);
    bytes_to_read := bytes_to_read - chunk_size;
  od;
#  IO_WriteFlush(log@HaskellGAP, data, "\n");  ## DEBUG
  return data;
end;


# read reply prefix
rprefix@HaskellGAP := ReadInput@HaskellGAP();

# signal server ready
IO_WriteFlush(stdout@HaskellGAP, rprefix@HaskellGAP,
              "GAPServer ", pids@HaskellGAP, " ready\n");


# Writes a string encoding of the given object to stdout.
# The string encoding is computed by ToHaskell; note that the special value
# SuPeRfail is encoded by the empty string. The data string is preceded by
# the reply prefix and by its length (encoded as an 8 byte string in little
# endian format).
WriteOutput@HaskellGAP := function(obj)
  local data, bytes_to_write;
  data := "";
  if obj <> SuPeRfail then
    ToHaskell@HaskellGAP(data, obj);
    ConvertToStringRep(data);
  fi;
  bytes_to_write := LittleEndianStringSize8@HaskellGAP(Length(data));
  ConvertToStringRep(bytes_to_write);
  IO_WriteFlush(stdout@HaskellGAP, rprefix@HaskellGAP,
                bytes_to_write, data);
end;


# Reads and executes commands.
# Terminates on reading an empty command.
ServerLoop@HaskellGAP := function()
  local cmd, input, suppress_result, result;

  # read first command
  cmd := ReadInput@HaskellGAP();

  # as long as command is non-empty try to execute it
  while cmd <> "" do

    # check whether to suppress result
    suppress_result := false;
    if cmd[1] = '!' then
      suppress_result := true;
      Remove(cmd, 1);
    fi;

    # execute cmd (w/o echoing)
    input := InputTextString(cmd);
    result := READ_COMMAND(input, false);

    # encode result (or 'true' if suppressed) and write to stdout
    if suppress_result then
      WriteOutput@HaskellGAP(true);
    else
      WriteOutput@HaskellGAP(result);
    fi;

    # read next command
    cmd := ReadInput@HaskellGAP();
  od;
end;


# drop into server loop
ServerLoop@HaskellGAP();


# signal server termination and quit
IO_WriteFlush(stdout@HaskellGAP, rprefix@HaskellGAP,
              "GAPServer ", pids@HaskellGAP, " terminated\n");
quit;
