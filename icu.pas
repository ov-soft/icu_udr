unit ICU;

{$mode delphi}{$H+}

interface

uses
  Classes, dynlibs;

const
  U_ZERO_ERROR = 0;

const

  {$IFDEF ICU_VER74}
    {$DEFINE ICU_VERSION}
    ICU_VERSION = '74';
  {$ENDIF}
  {$IFDEF ICU_VER72}
    {$DEFINE ICU_VERSION}
    ICU_VERSION = '72';
  {$ENDIF}
  {$IFNDEF ICU_VERSION}
    ICU_VERSION = '70';
  {$ENDIF}

  {$IFDEF WINDOWS}
    ICU1_DLL = 'icuin'+ICU_VERSION+'.dll';
    ICU2_DLL = 'icuuc'+ICU_VERSION+'.dll';
    ICU3_DLL = 'icudt'+ICU_VERSION+'.dll';
  {$ENDIF}
  {$IFDEF UNIX}
    ICU1_DLL = 'libicui18n.so.'+ICU_VERSION;
    ICU2_DLL = 'libicuuc.so.'+ICU_VERSION;
    ICU3_DLL = '';
  {$ENDIF}

type
  UChar = WideChar;
  PUChar = PWideChar;

  UTransDirection = (UTRANS_FORWARD, UTRANS_REVERSE);

  PUEnumeration = Pointer;
  PURegularExpression = Pointer;

  UErrorCode = Integer;
  PUErrorCode = ^UErrorCode;

  UTransliterator = Integer;
  PUTransliterator = ^UTransliterator;

  {$IFNDEF UDR_ICU}
    //Tu_strFromUTF8 = function from ICU2_DLL
    Tuenum_close = procedure (pEnumeration: PUEnumeration); cdecl;
    Tutrans_openIDs = function (var Status: UErrorCode): PUEnumeration; cdecl;
    Tuenum_count = function(pEnumeration: PUEnumeration; var Status: UErrorCode): Integer; cdecl;
    Tuenum_next = function (pEnumeration: PUEnumeration; var ResultLength:Integer; var Status: UErrorCode): PChar; cdecl;
    Tuenum_unext = function (pEnumeration: PUEnumeration; var ResultLength:Integer; var Status: UErrorCode): PUChar;
    Tuenum_reset = procedure (pEnumeration: PUEnumeration; var Status: UErrorCode); cdecl;
  {$ENDIF}
  Tutrans_close = procedure (pTransliterator: PUTransliterator); cdecl;
  Tutrans_openU = function (const Id:PUChar; IdLength:Integer; TransDirection:UTransDirection; pRules:PUChar;
                                         RulesLength:Integer; pParseError:Pointer; var Status: UErrorCode):PUTransliterator; cdecl;
  Tutrans_transUChars = procedure (pTransliterator:PUTransliterator; pBuffer: PUChar; pInpLength: PInteger;
                                         TextCapacity: Integer; start:Integer;var limit: Integer; var Status: UErrorCode); cdecl;

  Turegex_open = function(pattern: PUChar; patternLenghth:Integer; flags: Integer; pParseError:Pointer; var Status: UErrorCode): PURegularExpression; cdecl;
  Turegex_setText = procedure(regex: PURegularExpression; text: PUChar; textLength:Integer; var Status: UErrorCode); cdecl;
  Turegex_close = procedure(regex: PURegularExpression); cdecl;
  Turegex_replace = function(regex: PURegularExpression; replaceText: PUChar; replaceLength: Integer; dest: PUChar; destLength: Integer; var Status: UErrorCode): Integer; cdecl;
  Turegex_find = function(regex: PURegularExpression; StartIndex: Integer; var Status: UErrorCode): Boolean; cdecl;
  Turegex_findNext = function(regex: PURegularExpression; var Status: UErrorCode): Boolean; cdecl;
  Turegex_matches = function(regex: PURegularExpression; StartIndex: Integer; var Status: UErrorCode): Boolean; cdecl;
  Turegex_start = function(regex: PURegularExpression; groupIndex: Integer; var Status: UErrorCode): Integer; cdecl;
  Turegex_end = function(regex: PURegularExpression; groupIndex: Integer; var Status: UErrorCode): Integer; cdecl;
  Turegex_groupCount = function(regex: PURegularExpression; var Status: UErrorCode): Integer; cdecl;
  Turegex_setTimeLimit = procedure(regex: PURegularExpression; limit: Integer; var Status: UErrorCode); cdecl;
  Turegex_reset = procedure(regex: PURegularExpression; index: Integer; var Status: UErrorCode); cdecl;

  TLibFunctions = record
    LibHandle1: TLibHandle;
    LibHandle2: TLibHandle;
    //translation function
    utrans_close: Tutrans_close;
    utrans_openU: Tutrans_openU;
    utrans_transUChars: Tutrans_transUChars;
    //regex functions
    uregex_open: Turegex_open;
    uregex_close: Turegex_close;
    uregex_replaceAll: Turegex_replace;
    uregex_replaceFirst: Turegex_replace;
    uregex_find: Turegex_find;
    uregex_matches: Turegex_matches;
    uregex_start: Turegex_start;
    uregex_end: Turegex_end;
    uregex_groupCount: Turegex_groupCount;
    uregex_setText: Turegex_setText;
    uregex_findNext: Turegex_findNext;
    uregex_reset: Turegex_reset;
    uregex_setTimeLimit: Turegex_setTimeLimit;


    //enum functions
    {$IFNDEF UDR_ICU}
      uenum_close: Tuenum_close;
      uenum_count: Tuenum_count;
      uenum_next: Tuenum_next;
      uenum_unext: Tuenum_unext;
      uenum_reset: Tuenum_reset;
      utrans_openIDs: Tutrans_openIDs;
    {$ENDIF}
    icuLoaded: Boolean;
  end;

  function InitICU:Boolean;
  procedure CloseICU;
  function ICU_CheckStatus(Status:Integer):Boolean; inline;


  function DecodeError(ErrorCode: Integer):AnsiString;

var
  ICUFunc:TLibFunctions;

implementation
uses
  Sysutils
  {$IFDEF DEBUG_LOG}
     ,log
  {$ENDIF};
function InitICU: Boolean;
begin
  ICUFunc.LibHandle1:=NilHandle;
  {$IFDEF DEBUG_LOG}
    LogStr('ICU1 path: '+ICU1_DLL);
  {$ENDIF}
  ICUFunc.LibHandle1:=LoadLibrary(ICU1_DLL);
  {$IFDEF DEBUG_LOG}
    LogStr('DLL1 Handle: '+GetHexQWord(ICUFunc.LibHandle1));
    //LogInt('LastError: ',GetLastOSError);
  {$ENDIF}
  Result:=NilHandle<>ICUFunc.LibHandle1;
  {$IFNDEF UDR_ICU}
    if Result then begin
      ICUFunc.LibHandle2:=NilHandle;
      {$IFDEF DEBUG_LOG}
        LogStr('ICU2 path: '+ICU2_DLL);
      {$ENDIF}
      ICUFunc.LibHandle2:=LoadLibrary(ICU2_DLL);
      {$IFDEF DEBUG_LOG}
        LogStr('DLL2 Handle: '+GetHexQWord(ICUFunc.LibHandle2));
        //LogInt('LastError: ',GetLastOSError);
      {$ENDIF}
      Result:=NilHandle<>ICUFunc.LibHandle2;
    end;
  {$ENDIF}
  if Result then begin
    ICUFunc.utrans_close:=GetProcAddress(ICUFunc.LibHandle1,'utrans_close_'+ICU_VERSION);
    ICUFunc.utrans_openU:=GetProcAddress(ICUFunc.LibHandle1,'utrans_openU_'+ICU_VERSION);
    ICUFunc.utrans_transUChars:=GetProcAddress(ICUFunc.LibHandle1,'utrans_transUChars_'+ICU_VERSION);

    ICUFunc.uregex_open:=GetProcAddress(ICUFunc.LibHandle1,'uregex_open_'+ICU_VERSION);
    ICUFunc.uregex_close:=GetProcAddress(ICUFunc.LibHandle1,'uregex_close_'+ICU_VERSION);
    ICUFunc.uregex_replaceFirst:=GetProcAddress(ICUFunc.LibHandle1,'uregex_replaceFirst_'+ICU_VERSION);
    ICUFunc.uregex_replaceAll:=GetProcAddress(ICUFunc.LibHandle1,'uregex_replaceAll_'+ICU_VERSION);
    ICUFunc.uregex_find:=GetProcAddress(ICUFunc.LibHandle1,'uregex_find_'+ICU_VERSION);
    ICUFunc.uregex_findNext:=GetProcAddress(ICUFunc.LibHandle1,'uregex_findNext_'+ICU_VERSION);
    ICUFunc.uregex_matches:=GetProcAddress(ICUFunc.LibHandle1,'uregex_matches_'+ICU_VERSION);
    ICUFunc.uregex_start:=GetProcAddress(ICUFunc.LibHandle1,'uregex_start_'+ICU_VERSION);
    ICUFunc.uregex_end:=GetProcAddress(ICUFunc.LibHandle1,'uregex_end_'+ICU_VERSION);
    ICUFunc.uregex_groupCount:=GetProcAddress(ICUFunc.LibHandle1,'uregex_groupCount_'+ICU_VERSION);
    ICUFunc.uregex_setText:=GetProcAddress(ICUFunc.LibHandle1,'uregex_setText_'+ICU_VERSION);
    ICUFunc.uregex_reset:=GetProcAddress(ICUFunc.LibHandle1,'uregex_reset_'+ICU_VERSION);
    ICUFunc.uregex_setTimeLimit:=GetProcAddress(ICUFunc.LibHandle1,'uregex_setTimeLimit_'+ICU_VERSION);
    {$IFNDEF UDR_ICU}
      ICUFunc.utrans_openIDs:=GetProcAddress(ICUFunc.LibHandle1,'utrans_openIDs_'+ICU_VERSION);
      ICUFunc.uenum_close:=GetProcAddress(ICUFunc.LibHandle2,'uenum_close_'+ICU_VERSION);
      ICUFunc.uenum_count:=GetProcAddress(ICUFunc.LibHandle2,'uenum_count_'+ICU_VERSION);
      ICUFunc.uenum_next:=GetProcAddress(ICUFunc.LibHandle2,'uenum_next_'+ICU_VERSION);
      ICUFunc.uenum_unext:=GetProcAddress(ICUFunc.LibHandle2,'uenum_unext_'+ICU_VERSION);
      ICUFunc.uenum_reset:=GetProcAddress(ICUFunc.LibHandle2,'uenum_reset_'+ICU_VERSION);
    {$ENDIF}
    ICUFunc.icuLoaded:=Assigned(ICUFunc.utrans_close) and Assigned(ICUFunc.utrans_openU) and Assigned(ICUFunc.utrans_transUChars) and
                       Assigned(ICUFunc.uregex_open) and Assigned(ICUFunc.uregex_close) and Assigned(ICUFunc.uregex_replaceFirst) and
                       Assigned(ICUFunc.uregex_replaceAll) and Assigned(ICUFunc.uregex_find) and Assigned(ICUFunc.uregex_findNext) and
                       Assigned(ICUFunc.uregex_matches) and Assigned(ICUFunc.uregex_start) and Assigned(ICUFunc.uregex_end) and
                       Assigned(ICUFunc.uregex_setText) and Assigned(ICUFunc.uregex_reset) and Assigned(ICUFunc.uregex_groupCount) and
                       Assigned(ICUFunc.uregex_setTimeLimit)
                       {$IFNDEF UDR_ICU} and
                         Assigned(ICUFunc.uenum_reset) and Assigned(ICUFunc.uenum_unext) and Assigned(ICUFunc.uenum_next) and
                         Assigned(ICUFunc.uenum_count) and Assigned(ICUFunc.uenum_close) and Assigned(ICUFunc.utrans_openIDs) and
                         Assigned(ICUFunc.uenum_count)
                       {$ENDIF};
    Result:=ICUFunc.icuLoaded;
    if Result then exit;
  end;
  CloseICU;
end;

procedure CloseICU;
begin
  if NilHandle<>ICUFunc.LibHandle1 then UnloadLibrary(ICUFunc.LibHandle1);
  ICUFunc.LibHandle1:=NilHandle;
  {$IFNDEF UDR_ICU}
    if NilHandle<>ICUFunc.LibHandle2 then UnloadLibrary(ICUFunc.LibHandle2);
    ICUFunc.LibHandle2:=NilHandle;
  {$ENDIF}
end;

function DecodeError(ErrorCode: Integer):AnsiString;
begin
  case ErrorCode of
    0: Result:='U_ZERO_ERROR - No Errors';
    //Warnings
    -128: Result:='U_USING_FALLBACK_WARNING - A resource bundle lookup returned a fallback result (not an error)';
    -127: Result:='U_USING_DEFAULT_WARNING - resource bundle lookup returned a result from the root locale (not an error)';
    -126: Result:='U_SAFECLONE_ALLOCATED_WARNING - A SafeClone operation required allocating memory (informational only)';
    -125: Result:='U_STATE_OLD_WARNING - ICU has to use compatibility layer to construct the service. Expect performance/memory usage degradation. Consider upgrading';
    -124: Result:='U_STRING_NOT_TERMINATED_WARNING - An output string could not be NUL-terminated because output length==destCapacity';
    -123: Result:='U_SORT_KEY_TOO_SHORT_WARNING - Number of levels requested in getBound is higher than the number of levels in the sort key';
    -122: Result:='U_AMBIGUOUS_ALIAS_WARNING - This converter alias can go to different converter implementations';
    -121: Result:='U_DIFFERENT_UCA_VERSION - ucol_open encountered a mismatch between UCA version and collator image version, so the collator was constructed from rules. No impact to further function';
    -120: Result:='U_PLUGIN_CHANGED_LEVEL_WARNING - A plugin caused a level change. May not be an error, but later plugins may not load.';
    //Errors
    1: Result:='U_ILLEGAL_ARGUMENT_ERROR - Start of codes indicating failure';
    2: Result:='U_MISSING_RESOURCE_ERROR - The requested resource cannot be found';
    3: Result:='U_INVALID_FORMAT_ERROR - Data format is not what is expected';
    4: Result:='U_FILE_ACCESS_ERROR - The requested file cannot be found';
    5: Result:='U_INTERNAL_PROGRAM_ERROR - Indicates a bug in the library code';
    6: Result:='U_MESSAGE_PARSE_ERROR - Unable to parse a message (message format)';
    7: Result:='U_MEMORY_ALLOCATION_ERROR - Memory allocation error';
    8: Result:='U_INDEX_OUTOFBOUNDS_ERROR - Trying to access the index that is out of bounds';
    9: Result:='U_PARSE_ERROR - Equivalent to Java ParseException';
    10: Result:='U_INVALID_CHAR_FOUND - Character conversion: Unmappable input sequence. In other APIs: Invalid character';
    11: Result:='U_TRUNCATED_CHAR_FOUND - Character conversion: Incomplete input sequence';
    12: Result:='U_ILLEGAL_CHAR_FOUND - Character conversion: Illegal input sequence/combination of input units';
    13: Result:='U_INVALID_TABLE_FORMAT - Conversion table file found, but corrupted';
    14: Result:='U_INVALID_TABLE_FILE - Conversion table file not found';
    15: Result:='U_BUFFER_OVERFLOW_ERROR - A result would not fit in the supplied buffer';
    16: Result:='U_UNSUPPORTED_ERROR - Requested operation not supported in current context';
    17: Result:='U_RESOURCE_TYPE_MISMATCH - an operation is requested over a resource that does not support it';
    18: Result:='U_ILLEGAL_ESCAPE_SEQUENCE - ISO-2022 illegal escape sequence';
    19: Result:='U_UNSUPPORTED_ESCAPE_SEQUENCE - ISO-2022 unsupported escape sequence';
    20: Result:='U_NO_SPACE_AVAILABLE - No space available for in-buffer expansion for Arabic shaping';
    21: Result:='U_CE_NOT_FOUND_ERROR - Currently used only while setting variable top, but can be used generally';
    22: Result:='U_PRIMARY_TOO_LONG_ERROR - User tried to set variable top to a primary that is longer than two bytes';
    23: Result:='U_STATE_TOO_OLD_ERROR - ICU cannot construct a service from this state, as it is no longer supported';
    24: Result:='U_TOO_MANY_ALIASES_ERROR - There are too many aliases in the path to the requested resource. It is very possible that a circular alias definition has occurred';
    25: Result:='U_ENUM_OUT_OF_SYNC_ERROR - UEnumeration out of sync with underlying collection';
    26: Result:='U_INVARIANT_CONVERSION_ERROR - Unable to convert a UChar* string to char* with the invariant converter';
    27: Result:='U_INVALID_STATE_ERROR - Requested operation can not be completed with ICU in its current state';
    28: Result:='U_COLLATOR_VERSION_MISMATCH - Collator version is not compatible with the base version';
    29: Result:='U_USELESS_COLLATOR_ERROR - Collator is options only and no base is specified';
    30: Result:='U_NO_WRITE_PERMISSION - Attempt to modify read-only or constant data';
    31: Result:='U_INPUT_TOO_LONG_ERROR - The input is impractically long for an operation. It is rejected because it may lead to problems';

    $10000: Result:='U_BAD_VARIABLE_DEFINITION - Missing ''$'' or duplicate variable name';
    $10001: Result:='U_MALFORMED_RULE - Elements of a rule are misplaced';
    $10002: Result:='U_MALFORMED_SET - A UnicodeSet pattern is invalid';
    $10003: Result:='U_MALFORMED_SYMBOL_REFERENCE - UNUSED as of ICU 2.4';
    $10004: Result:='U_MALFORMED_UNICODE_ESCAPE - A Unicode escape pattern is invalid';
    $10005: Result:='U_MALFORMED_VARIABLE_DEFINITION - A variable definition is invalid';
    $10006: Result:='U_MALFORMED_VARIABLE_REFERENCE - A variable reference is invalid';
    $10007: Result:='U_MISMATCHED_SEGMENT_DELIMITERS - UNUSED as of ICU 2.4';
    $10008: Result:='U_MISPLACED_ANCHOR_START - A start anchor appears at an illegal position';
    $10009: Result:='U_MISPLACED_CURSOR_OFFSET - A cursor offset occurs at an illegal position';
    $1000A: Result:='U_MISPLACED_QUANTIFIER - A quantifier appears after a segment close delimiter';
    $1000B: Result:='U_MISSING_OPERATOR - A rule contains no operator';
    $1000C: Result:='U_MISSING_SEGMENT_CLOSE - UNUSED as of ICU 2.4';
    $1000D: Result:='U_MULTIPLE_ANTE_CONTEXTS - More than one ante context';
    $1000E: Result:='U_MULTIPLE_CURSORS - More than one cursor';
    $1000F: Result:='U_MULTIPLE_POST_CONTEXTS - More than one post context';
    $10010: Result:='U_TRAILING_BACKSLASH - A dangling backslash';
    $10011: Result:='U_UNDEFINED_SEGMENT_REFERENCE - A segment reference does not correspond to a defined segment';
    $10012: Result:='U_UNDEFINED_VARIABLE - A variable reference does not correspond to a defined variable';
    $10013: Result:='U_UNQUOTED_SPECIAL - A special character was not quoted or escaped';
    $10014: Result:='U_UNTERMINATED_QUOTE - A closing single quote is missing';
    $10015: Result:='U_RULE_MASK_ERROR - A rule is hidden by an earlier more general rule';
    $10016: Result:='U_MISPLACED_COMPOUND_FILTER - A compound filter is in an invalid location';
    $10017: Result:='U_MULTIPLE_COMPOUND_FILTERS - More than one compound filter';
    $10018: Result:='U_INVALID_RBT_SYNTAX - A "::id" rule was passed to the RuleBasedTransliterator parser';
    $10019: Result:='U_INVALID_PROPERTY_PATTERN - UNUSED as of ICU 2.4';
    $1001A: Result:='U_MALFORMED_PRAGMA - A "use" pragma is invalid';
    $1001B: Result:='U_UNCLOSED_SEGMENT - A closing ")" is missing';
    $1001C: Result:='U_ILLEGAL_CHAR_IN_SEGMENT - UNUSED as of ICU 2.4';
    $1001D: Result:='U_VARIABLE_RANGE_EXHAUSTED - Too many stand-ins generated for the given variable range';
    $1001E: Result:='U_VARIABLE_RANGE_OVERLAP - The variable range overlaps characters used in rules';
    $1001F: Result:='U_ILLEGAL_CHARACTER - A special character is outside its allowed context';
    $10020: Result:='U_INTERNAL_TRANSLITERATOR_ERROR - Internal transliterator system error';
    $10021: Result:='U_INVALID_ID - A "::id" rule specifies an unknown transliterator';
    $10022: Result:='U_INVALID_FUNCTION - A "&fn()" rule specifies an unknown transliterator';
    //Formatting API
    $10100: Result:='U_UNEXPECTED_TOKEN - Syntax error in format pattern';
    $10101: Result:='U_MULTIPLE_DECIMAL_SEPARATORS - More than one decimal separator in number pattern';
    $10102: Result:='U_MULTIPLE_EXPONENTIAL_SYMBOLS - More than one exponent symbol in number pattern';
    $10103: Result:='U_MALFORMED_EXPONENTIAL_PATTERN - Grouping symbol in exponent pattern';
    $10104: Result:='U_MULTIPLE_PERCENT_SYMBOLS - More than one percent symbol in number pattern';
    $10105: Result:='U_MULTIPLE_PERMILL_SYMBOLS - More than one permill symbol in number pattern';
    $10106: Result:='U_MULTIPLE_PAD_SPECIFIERS - More than one pad symbol in number pattern';
    $10107: Result:='U_PATTERN_SYNTAX_ERROR - Syntax error in format pattern';
    $10108: Result:='U_ILLEGAL_PAD_POSITION - Pad symbol misplaced in number pattern';
    $10109: Result:='U_UNMATCHED_BRACES - Braces do not match in message pattern';
    $1010A: Result:='U_UNSUPPORTED_PROPERTY';
    $1010B: Result:='U_UNSUPPORTED_ATTRIBUTE';
    $1010C: Result:='U_ARGUMENT_TYPE_MISMATCH - Argument name and argument index mismatch in MessageFormat functions';
    $1010D: Result:='U_DUPLICATE_KEYWORD - Duplicate keyword in PluralFormat';
    $1010E: Result:='U_UNDEFINED_KEYWORD - Undefined Plural keyword';
    $1010F: Result:='U_DEFAULT_KEYWORD_MISSING - Missing DEFAULT rule in plural rules';
    $10110: Result:='U_DECIMAL_NUMBER_SYNTAX_ERROR - Decimal number syntax error';
    $10111: Result:='U_FORMAT_INEXACT_ERROR - Cannot format a number exactly and rounding mode is ROUND_UNNECESSARY';
    $10112: Result:='U_NUMBER_ARG_OUTOFBOUNDS_ERROR - The argument to a NumberFormatter helper method was out of bounds; the bounds are usually 0 to 999';
    $10113: Result:='U_NUMBER_SKELETON_SYNTAX_ERROR - The number skeleton passed to C++ NumberFormatter or C UNumberFormatter was invalid or contained a syntax error';
    //regexp
    $10300: Result:='U_REGEX_INTERNAL_ERROR - An internal error (bug) was detected';
    $10301: Result:='U_REGEX_RULE_SYNTAX - Syntax error in regexp pattern';
    $10302: Result:='U_REGEX_INVALID_STATE - RegexMatcher in invalid state for requested operation';
    $10303: Result:='U_REGEX_BAD_ESCAPE_SEQUENCE - Unrecognized backslash escape sequence in pattern';
    $10304: Result:='U_REGEX_PROPERTY_SYNTAX - Incorrect Unicode property';
    $10305: Result:='U_REGEX_UNIMPLEMENTED - Use of regexp feature that is not yet implemented';
    $10306: Result:='U_REGEX_MISMATCHED_PAREN - Incorrectly nested parentheses in regexp pattern';
    $10307: Result:='U_REGEX_NUMBER_TOO_BIG - Decimal number is too large';
    $10308: Result:='U_REGEX_BAD_INTERVAL - Error in {min,max} interval';
    $10309: Result:='U_REGEX_MAX_LT_MIN - In {min,max}, max is less than min';
    $1030A: Result:='U_REGEX_INVALID_BACK_REF - Back-reference to a non-existent capture group';
    $1030B: Result:='U_REGEX_INVALID_FLAG - Invalid value for match mode flags';
    $1030C: Result:='U_REGEX_LOOK_BEHIND_LIMIT - Look-Behind pattern matches must have a bounded maximum length';
    $1030D: Result:='U_REGEX_SET_CONTAINS_STRING - Regexps cannot have UnicodeSets containing strings';
    $1030F: Result:='U_REGEX_MISSING_CLOSE_BRACKET - Missing closing bracket on a bracket expression';
    $10310: Result:='U_REGEX_INVALID_RANGE - In a character range [x-y], x is greater than y';
    $10311: Result:='U_REGEX_STACK_OVERFLOW - Regular expression backtrack stack overflow';
    $10312: Result:='U_REGEX_TIME_OUT - Maximum allowed match time exceeded';
    $10313: Result:='U_REGEX_STOPPED_BY_CALLER - Matching operation aborted by user callback fn';
    $10314: Result:='U_REGEX_PATTERN_TOO_BIG - Pattern exceeds limits on size or complexity';
    $10315: Result:='U_REGEX_INVALID_CAPTURE_GROUP_NAME - Invalid capture group name';
    //
    $10500: Result:='U_PLUGIN_TOO_HIGH - The plugin''s level is too high to be loaded right now';
    $10501: Result:='U_PLUGIN_DIDNT_SET_LEVEL - The plugin didn''t call uplug_setPlugLevel in response to a QUERY';
    //
    else Result:='Unknown error HexCode='+IntToHex(ErrorCode,8);
  end;
end;

function ICU_CheckStatus(Status:Integer):Boolean;
begin
  Result:=Status {$IFNDEF DISABLE_ICU_WARNINGS}={$ELSE}<={$ENDIF} U_ZERO_ERROR;
end;

end.

