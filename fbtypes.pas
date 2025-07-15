unit fbtypes;

{$IFDEF FPC}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface
uses Firebird;

const
  // типы Firebird
  SQL_VARYING = 448; // VARCHAR
  SQL_TEXT = 452; // CHAR
  SQL_DOUBLE = 480; // DOUBLE PRECISION
  SQL_FLOAT = 482; // FLOAT
  SQL_LONG = 496; // INTEGER
  SQL_SHORT = 500; // SMALLINT
  SQL_TIMESTAMP = 510; // TIMESTAMP
  SQL_BLOB = 520; // BLOB
  SQL_D_FLOAT = 530; // DOUBLE PRECISION
  SQL_ARRAY = 540; // ARRAY
  SQL_QUAD = 550; // BLOB_ID (QUAD)
  SQL_TIME = 560; // TIME
  SQL_DATE = 570; // DATE
  SQL_INT64 = 580; // BIGINT
  SQL_INT128 = 32752; // INT128
  SQL_TIMESTAMP_TZ = 32754; // TIMESTAMP WITH TIME ZONE
  SQL_TIME_TZ = 32756; // TIME WITH TIME ZONE
  SQL_DEC16 = 32760; // DECFLOAT(16)
  SQL_DEC34 = 32762; // DECFLOAT(34)
  SQL_BOOLEAN = 32764; // BOOLEAN
  SQL_NULL = 32766; // NULL

type
  // указатели на специальные типы
  PISC_DATE = ^ISC_DATE;
  PISC_TIME = ^ISC_TIME;
  PISC_TIMESTAMP = ^ISC_TIMESTAMP;
  PISC_QUAD = ^ISC_QUAD;

  TVarChar<T> = record
    Length: Smallint;
    value: T
  end;

  TFBCharSet = (
    CS_NONE = 0, // No Character Set
    CS_BINARY = 1, // BINARY BYTES
    CS_ASCII = 2, // ASCII
    CS_UNICODE_FSS = 3, // UNICODE in FSS format
    CS_UTF8 = 4, // UTF-8
    CS_SJIS = 5, // SJIS
    CS_EUCJ = 6, // EUC-J

    CS_JIS_0208 = 7, // JIS 0208; 1990
    CS_UNICODE_UCS2 = 8, // UNICODE v 1.10

    CS_DOS_737 = 9,
    CS_DOS_437 = 10, // DOS CP 437
    CS_DOS_850 = 11, // DOS CP 850
    CS_DOS_865 = 12, // DOS CP 865
    CS_DOS_860 = 13, // DOS CP 860
    CS_DOS_863 = 14, // DOS CP 863

    CS_DOS_775 = 15,
    CS_DOS_858 = 16,
    CS_DOS_862 = 17,
    CS_DOS_864 = 18,

    CS_NEXT = 19, // NeXTSTEP OS native charset

    CS_ISO8859_1 = 21, // ISO-8859.1
    CS_ISO8859_2 = 22, // ISO-8859.2
    CS_ISO8859_3 = 23, // ISO-8859.3
    CS_ISO8859_4 = 34, // ISO-8859.4
    CS_ISO8859_5 = 35, // ISO-8859.5
    CS_ISO8859_6 = 36, // ISO-8859.6
    CS_ISO8859_7 = 37, // ISO-8859.7
    CS_ISO8859_8 = 38, // ISO-8859.8
    CS_ISO8859_9 = 39, // ISO-8859.9
    CS_ISO8859_13 = 40, // ISO-8859.13

    CS_KSC5601 = 44, // KOREAN STANDARD 5601

    CS_DOS_852 = 45, // DOS CP 852
    CS_DOS_857 = 46, // DOS CP 857
    CS_DOS_861 = 47, // DOS CP 861

    CS_DOS_866 = 48,
    CS_DOS_869 = 49,

    CS_CYRL = 50,
    CS_WIN1250 = 51, // Windows cp 1250
    CS_WIN1251 = 52, // Windows cp 1251
    CS_WIN1252 = 53, // Windows cp 1252
    CS_WIN1253 = 54, // Windows cp 1253
    CS_WIN1254 = 55, // Windows cp 1254

    CS_BIG5 = 56, // Big Five unicode cs
    CS_GB2312 = 57, // GB 2312-80 cs

    CS_WIN1255 = 58, // Windows cp 1255
    CS_WIN1256 = 59, // Windows cp 1256
    CS_WIN1257 = 60, // Windows cp 1257

    CS_UTF16 = 61, // UTF-16
    CS_UTF32 = 62, // UTF-32

    CS_KOI8R = 63, // Russian KOI8R
    CS_KOI8U = 64, // Ukrainian KOI8U

    CS_WIN1258 = 65, // Windows cp 1258

    CS_TIS620 = 66, // TIS620
    CS_GBK = 67, // GBK
    CS_CP943C = 68, // CP943C

    CS_GB18030 = 69 // GB18030
  );

  TCharsetMap = record
    CharsetID: Integer;
    CharSetName: AnsiString;
    CharSetWidth: Word;
    CodePage: Integer;
  end;


  function fb_type2string(Tp:Integer):AnsiString;
  function fb_get_charset_name(cs:Integer):AnsiString;

implementation

const
  CharSetMap: array [0 .. 69] of TCharsetMap = (
    (CharsetID: 0; CharSetName: 'NONE'; CharSetWidth: 1; CodePage: CP_ACP),
    (CharsetID: 1; CharSetName: 'OCTETS'; CharSetWidth: 1; CodePage: CP_NONE),
    (CharsetID: 2; CharSetName: 'ASCII'; CharSetWidth: 1; CodePage:  {CP_ASCII}  CP_ACP),
    (CharsetID: 3; CharSetName: 'UNICODE_FSS'; CharSetWidth: 3; CodePage: CP_UTF8),
    (CharsetID: 4; CharSetName: 'UTF8'; CharSetWidth: 4; CodePage: CP_UTF8),
    (CharsetID: 5; CharSetName: 'SJIS_0208'; CharSetWidth: 2; CodePage: 20932),
    (CharsetID: 6; CharSetName: 'EUCJ_0208'; CharSetWidth: 2; CodePage: 20932),
    (CharsetID: 7; CharSetName: 'Unknown'; CharSetWidth: 0; CodePage: CP_NONE),
    (CharsetID: 8; CharSetName: 'Unknown'; CharSetWidth: 0; CodePage: CP_NONE),
    (CharsetID: 9; CharSetName: 'DOS737'; CharSetWidth: 1; CodePage: 737),
    (CharsetID: 10; CharSetName: 'DOS437'; CharSetWidth: 1; CodePage: 437),
    (CharsetID: 11; CharSetName: 'DOS850'; CharSetWidth: 1; CodePage: 850),
    (CharsetID: 12; CharSetName: 'DOS865'; CharSetWidth: 1; CodePage: 865),
    (CharsetID: 13; CharSetName: 'DOS860'; CharSetWidth: 1; CodePage: 860),
    (CharsetID: 14; CharSetName: 'DOS863'; CharSetWidth: 1; CodePage: 863),
    (CharsetID: 15; CharSetName: 'DOS775'; CharSetWidth: 1; CodePage: 775),
    (CharsetID: 16; CharSetName: 'DOS858'; CharSetWidth: 1; CodePage: 858),
    (CharsetID: 17; CharSetName: 'DOS862'; CharSetWidth: 1; CodePage: 862),
    (CharsetID: 18; CharSetName: 'DOS864'; CharSetWidth: 1; CodePage: 864),
    (CharsetID: 19; CharSetName: 'NEXT'; CharSetWidth: 1; CodePage: CP_NONE),
    (CharsetID: 20; CharSetName: 'Unknown'; CharSetWidth: 0; CodePage: CP_NONE),
    (CharsetID: 21; CharSetName: 'ISO8859_1'; CharSetWidth: 1; CodePage: 28591),
    (CharsetID: 22; CharSetName: 'ISO8859_2'; CharSetWidth: 1; CodePage: 28592),
    (CharsetID: 23; CharSetName: 'ISO8859_3'; CharSetWidth: 1; CodePage: 28593),
    (CharsetID: 24; CharSetName: 'Unknown'; CharSetWidth: 0; CodePage: CP_NONE),
    (CharsetID: 25; CharSetName: 'Unknown'; CharSetWidth: 0; CodePage: CP_NONE),
    (CharsetID: 26; CharSetName: 'Unknown'; CharSetWidth: 0; CodePage: CP_NONE),
    (CharsetID: 27; CharSetName: 'Unknown'; CharSetWidth: 0; CodePage: CP_NONE),
    (CharsetID: 28; CharSetName: 'Unknown'; CharSetWidth: 0; CodePage: CP_NONE),
    (CharsetID: 29; CharSetName: 'Unknown'; CharSetWidth: 0; CodePage: CP_NONE),
    (CharsetID: 30; CharSetName: 'Unknown'; CharSetWidth: 0; CodePage: CP_NONE),
    (CharsetID: 31; CharSetName: 'Unknown'; CharSetWidth: 0; CodePage: CP_NONE),
    (CharsetID: 32; CharSetName: 'Unknown'; CharSetWidth: 0; CodePage: CP_NONE),
    (CharsetID: 33; CharSetName: 'Unknown'; CharSetWidth: 0; CodePage: CP_NONE),
    (CharsetID: 34; CharSetName: 'ISO8859_4'; CharSetWidth: 1; CodePage: 28594),
    (CharsetID: 35; CharSetName: 'ISO8859_5'; CharSetWidth: 1; CodePage: 28595),
    (CharsetID: 36; CharSetName: 'ISO8859_6'; CharSetWidth: 1; CodePage: 28596),
    (CharsetID: 37; CharSetName: 'ISO8859_7'; CharSetWidth: 1; CodePage: 28597),
    (CharsetID: 38; CharSetName: 'ISO8859_8'; CharSetWidth: 1; CodePage: 28598),
    (CharsetID: 39; CharSetName: 'ISO8859_9'; CharSetWidth: 1; CodePage: 28599),
    (CharsetID: 40; CharSetName: 'ISO8859_13'; CharSetWidth: 1; CodePage: 28603),
    (CharsetID: 41; CharSetName: 'Unknown'; CharSetWidth: 0; CodePage: CP_NONE),
    (CharsetID: 42; CharSetName: 'Unknown'; CharSetWidth: 0; CodePage: CP_NONE),
    (CharsetID: 43; CharSetName: 'Unknown'; CharSetWidth: 0; CodePage: CP_NONE),
    (CharsetID: 44; CharSetName: 'KSC_5601'; CharSetWidth: 2; CodePage: 949),
    (CharsetID: 45; CharSetName: 'DOS852'; CharSetWidth: 1; CodePage: 852),
    (CharsetID: 46; CharSetName: 'DOS857'; CharSetWidth: 1; CodePage: 857),
    (CharsetID: 47; CharSetName: 'DOS861'; CharSetWidth: 1; CodePage: 861),
    (CharsetID: 48; CharSetName: 'DOS866'; CharSetWidth: 1; CodePage: 866),
    (CharsetID: 49; CharSetName: 'DOS869'; CharSetWidth: 1; CodePage: 869),
    (CharsetID: 50; CharSetName: 'CYRL'; CharSetWidth: 1; CodePage: 1251),
    (CharsetID: 51; CharSetName: 'WIN1250'; CharSetWidth: 1; CodePage: 1250),
    (CharsetID: 52; CharSetName: 'WIN1251'; CharSetWidth: 1; CodePage: 1251),
    (CharsetID: 53; CharSetName: 'WIN1252'; CharSetWidth: 1; CodePage: 1252),
    (CharsetID: 54; CharSetName: 'WIN1253'; CharSetWidth: 1; CodePage: 1253),
    (CharsetID: 55; CharSetName: 'WIN1254'; CharSetWidth: 1; CodePage: 1254),
    (CharsetID: 56; CharSetName: 'BIG_5'; CharSetWidth: 2; CodePage: 950),
    (CharsetID: 57; CharSetName: 'GB_2312'; CharSetWidth: 2; CodePage: 936),
    (CharsetID: 58; CharSetName: 'WIN1255'; CharSetWidth: 1; CodePage: 1255),
    (CharsetID: 59; CharSetName: 'WIN1256'; CharSetWidth: 1; CodePage: 1256),
    (CharsetID: 60; CharSetName: 'WIN1257'; CharSetWidth: 1; CodePage: 1257),
    (CharsetID: 61; CharSetName: 'Unknown'; CharSetWidth: 0; CodePage: CP_NONE),
    (CharsetID: 62; CharSetName: 'Unknown'; CharSetWidth: 0; CodePage: CP_NONE),
    (CharsetID: 63; CharSetName: 'KOI8R'; CharSetWidth: 1; CodePage: 20866),
    (CharsetID: 64; CharSetName: 'KOI8U'; CharSetWidth: 1; CodePage: 21866),
    (CharsetID: 65; CharSetName: 'WIN1258'; CharSetWidth: 1; CodePage: 1258),
    (CharsetID: 66; CharSetName: 'TIS620'; CharSetWidth: 1; CodePage: 874),
    (CharsetID: 67; CharSetName: 'GBK'; CharSetWidth: 2; CodePage: 936),
    (CharsetID: 68; CharSetName: 'CP943C'; CharSetWidth: 2; CodePage: 943),
    (CharsetID: 69; CharSetName: 'GB18030'; CharSetWidth: 4; CodePage: 54936));



function fb_type2string(Tp: Integer): AnsiString;
begin
  case Tp of
    SQL_VARYING: Result:='VARCHAR'; // VARCHAR
    SQL_TEXT: Result:='CHAR'; // CHAR
    SQL_DOUBLE: Result:='DOUBLE PRECISION'; // DOUBLE PRECISION
    SQL_FLOAT: Result:='FLOAT'; // FLOAT
    SQL_LONG: Result:='INTEGER'; // INTEGER
    SQL_SHORT: Result:='SMALLINT'; // SMALLINT
    SQL_TIMESTAMP: Result:='TIMESTAMP'; // TIMESTAMP
    SQL_BLOB: Result:='BLOB'; // BLOB
    SQL_D_FLOAT: Result:='DOUBLE PRECISION (D_FLOAT)'; // DOUBLE PRECISION
    SQL_ARRAY: Result:='ARRAY'; // ARRAY
    SQL_QUAD: Result:='BLOB_ID (QUAD)'; // BLOB_ID (QUAD)
    SQL_TIME: Result:='TIME'; // TIME
    SQL_DATE: Result:='DATE'; // DATE
    SQL_INT64: Result:='BIGINT'; // BIGINT
    SQL_INT128: Result:='INT128'; // INT128
    SQL_TIMESTAMP_TZ: Result:='TIMESTAMP WITH TIME ZONE'; // TIMESTAMP WITH TIME ZONE
    SQL_TIME_TZ: Result:='TIME WITH TIME ZONE'; // TIME WITH TIME ZONE
    SQL_DEC16: Result:='DECFLOAT(16)'; // DECFLOAT(16)
    SQL_DEC34: Result:='DECFLOAT(34)'; // DECFLOAT(34)
    SQL_BOOLEAN: Result:='BOOLEAN'; // BOOLEAN
    SQL_NULL: Result:='NULL'; // NULL
    else Result:='UNDEFINED TYPE!'
  end;
end;

function fb_get_charset_name(cs: Integer): AnsiString;
begin
  if (cs>=0) and (cs<=69) then Result:=CharSetMap[cs].CharSetName else Result:='UNDEFINED CHARSET!';
end;

end.

