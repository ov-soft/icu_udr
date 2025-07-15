unit utils;

{$mode delphi}{$H+}

interface

type
  TTransformFlagsBool = record
    bQuotes,
    bWhiteSpaces,
    bRepeats,
    bBrackets,
    bDelimiters,
    bNonASCIIToHex,
    bDelSpaces,
    bSurogates,
    bASCIILocase,
    bY2I,
    bI2Y,
    bTranslit,
    bParentheses,
    bASCIICapitalize,
    bRomeToArabian: Boolean;
  end;

  TMultiReplaceString = String[50];

const
  LatinNumberAChars = [$0063, $0064, $0069, $006C, $006D, $0076, $0078]; //c,d,i,l,m,v,x
  LettersNumAChars = [$0030..$0039,$0041..$005A,$0061..$007A]; //
  LatinLowerAChars = [$0061..$007A]; //маленькие латинские
  ArabianNumbers = [$0030..$0039];  //цифры


function Transform(Input:PWideChar;Flags:TTransformFlagsBool;DestSize:SizeInt;InpLen:SizeInt=-1):SizeInt;
//function LongToHex(l: Longint; digits:Integer=1): ShortString;
procedure LowerCase(var s:ShortString);
function CheckNbsp(Input:PWideChar; var r:Word; Size:SizeInt):Integer;
function CheckStrIsDelimiters(Input:PWideChar; InpLen:SizeInt):Boolean;
function IsLineTerminator(c:Word):Boolean; inline;
function GetDefaultTransformFlags:TTransformFlagsBool;

function UTF8Length(p: PAnsiChar): SmallInt;
function Utf8IntToUnicode(Code: LongInt): LongInt;
function UnicodeIntToUtf8(Code: LongInt): LongInt;

implementation

uses SysUtils;

function CharLow(r:Word):Word; inline;
begin
  case r of
    $0041..$005A: Result:=r or $0020;
    $0410..$042F: Result:=r + $0020;
    else Result:=r;
  end;
end;

function CheckNbsp(Input:PWideChar; var r:Word; Size:SizeInt):Integer;
const
  NBSP_CONST_COUNT = 9;
  NBSP_MAX_WORD_LEN = 10;
  nbsp_consts: array[0..NBSP_CONST_COUNT-1] of String[NBSP_MAX_WORD_LEN] = (' nbsp;', '<lt;', '>gt;', '&amp;', '-ndash;',
                                                                            ' emsp;', ' ensp;', '-mdash;', '"quot;');
var
  i, p, found :SizeInt;
  w: Word;
  j: Byte;
  match: array [0..NBSP_CONST_COUNT-1] of Byte;
  match_count: Byte;
  EndAddr: PWideChar;
begin
  Result:=0;
  if not Assigned(Input) then exit;
  EndAddr:=Input+Size;
  match[0]:=0;
  p:=2; match_count:=0; found:=-1;
  //в буфере буквы с 1 (0 - &)
  Inc(Input);
  //цикл по буферу
  while (p < NBSP_MAX_WORD_LEN) and (Input <= EndAddr) do begin
    w:=CharLow(Word(Input^));
    if (p=2) then begin
      //в первой итерации заполняем match по совпадению первого символа
      for i:=0 to NBSP_CONST_COUNT-1 do begin
        if Word(nbsp_consts[i][p]) = w then begin
          match[match_count]:=i;
          Inc(match_count);
        end;
      end;
    end else begin
      //если ни одного совпадения - заканчиваем
      if match_count>0 then i:=match_count-1 else break;
      //отрабатываем только содержимое match с конца
      while (i >= 0) do begin
        j:=match[i];
        //если у нас 1 совпадающее и дошли до конца, то найдено!
        if (p>Length(nbsp_consts[j])) and (match_count=1)  then begin
          found:=j;
          break;
        end else begin
          //иначе проверяем пока не кончатся совпадения
          if (p>Length(nbsp_consts[j])) or (Word(nbsp_consts[j][p]) <> w) then begin
            if i < match_count-1 then match[i]:=match[match_count-1];
            Dec(match_count);
          end;
          Dec(i);
        end;
      end;
    end;
    if (found = -1) and (match_count > 0) then begin
      Inc(p);
      Inc(Input);
    end else begin
      //если нашли, то возвращаем
      if found<>-1 then begin
        r:=Word(nbsp_consts[found][1]);
        Result:=Length(nbsp_consts[found])-1;
      end;
      break;
    end;
  end;
end;

function CheckStrIsDelimiters(Input: PWideChar; InpLen: SizeInt): Boolean;
var
  i, n:SizeInt;
begin
  n:=0;
  Result:=False;
  if not Assigned(Input) then exit;
  for i:=0 to InpLen-1 do begin
    if (Word(Input[i]) > $007F) or (Byte(Input[i]) in LettersNumAChars) then Inc(n);
    if n>2 then exit;
  end;
  Result:=True;
end;


function Transform(Input:PWideChar;Flags:TTransformFlagsBool;DestSize:SizeInt;InpLen:SizeInt):SizeInt;
var
  i,j: SizeInt;
  w, r, prev: Word;

  //вставка нескольких символов
  procedure One2Many(const Repl: TMultiReplaceString);
  var
    n,k,s:Integer;
  begin
    //длина строки, на которую меняем символ
    n:=Length(Repl);
    //на сколько символов нужно увеличить буфер
    s:=j-i+n-1;  // индекс результата + длина на сколько увеличится - сколько уже есть
    //если >0, то требуется увеличение строки - а хватит ли памяти для этого
    if (s>0) then begin
      if InpLen+20 < DestSize then s:=20 else if InpLen+s >= DestSize then begin
        //для расширения не хватит памяти - надо сообщить
        Result:=-1;
        exit;   ///!!! Куда этот EXIT ????? - ПРОВЕРИТЬ!!!
      end;
      //расширение буфера если хватит сразу на 20, если нет - на необходимое количество
      for k:=InpLen downto i do Input[k+s]:=Input[k];
      Inc(InpLen,s);
      Inc(i,s);
      //так #0 должен переноситься, но на всякий случай
      Input[InpLen]:=#0;
    end;
    //есть место куда вставить новые данные
    for k:=1 to n do begin
      Input[j]:=WideChar(Repl[k]);
      Inc(j);
    end;
    r:=$0000;
    prev:=0;
  end;

begin
  i:=0; j:=0; prev:=$0000;

  if InpLen=-1 then InpLen:=Length(Input);
  if InpLen >= DestSize then begin
    Result:=-1;
    exit;
  end;
  //если в строке меньше 3 буквосимволов, то не удаляем разделители
  if Flags.bDelimiters  then Flags.bDelimiters:=not CheckStrIsDelimiters(Input,InpLen);

  while (i < InpLen) do begin
    w:=Word(Input[i]);
    r:=w;
    //формирование запасного буфера слов для возможности отката латинских цифр
    if Flags.bRepeats then begin
      if (CharLow(r) = prev) and not ((r < $007F) and (Byte(r) in ArabianNumbers)) then r:=$0000;
    end;
    //Повторения нелатиницы
    case r of
      $D800..$DBFF: if not Flags.bSurogates then r:=$0000 else begin
        //Surogates
        if (i+1) < InpLen then begin
          r:=(r-$D800) SHL 10;
          Inc(i);
          w:=Word(Input[i]);
          if (w>=$DC00) and (w<=$DFFF) then begin
            Inc(r,((w-$DC00) and $3FF));
            case r of
              $DF01: r:=$0067;
              $DF02: r:=$0047;
              $DF03: r:=$006B;
              $DF04: r:=$004C;
              $DF06: r:=$0079;
              $F110..$F129: r:=r-$F110+$0041;
              $F12A: r:=$0053;
              $F12B: r:=$0043;
              $F12C: r:=$0052;
              $F130..$F149: r:=r-$F130+$0041;
              $F150..$F169: r:=r-$F150+$0041;
              $F170..$F189: r:=r-$F170+$0041;
              $D400..$D6A3: begin
                r:=(r-$D400) mod 52;
                if r<26 then Inc(r,$0041) else Inc(r,$0047);
              end;
              $F18A: r:=$0050;
            end;
          end else r:=$0000;
        end else r:=$0000;
      end;
      $DC00..$DFFF: r:=$0000;
    end;
    if Flags.bWhiteSpaces then case r of
      //&nbsp; &amp; &quot; и другие
      $0026: Inc(i, CheckNbsp(Input+i, r, InpLen));
      //пробелы и управляющие коды - на пробелы
      $0001..$001F, $007F, $0085, $0088..$008F, $00A0, $0308,
      $1680, $180E, $2000..$200B,
      $2022, $202F, $205F,
      $3000, $30FB, $030A, $FEFF, $FFFD: r:=$0020;
      //коды для удаления совсем
      $0080..$0084, $0086..$0087, $0090..$009F, $00B8,
      $02CC, $0304, $0327, $0331,
      $20E3: r:=$0000;
    end;
    if Flags.bQuotes then case r of
      //кавычки
      $0022, $0060, $00AB, $00BB,
      $0374, $055B, $2018..$201F,
      $2039, $203A, $27EA,
      $27EB, $2E0C, $2E1C, $2E1D,
      $2E0D, $2E22, $2E23,
      $2E42, $2E28, $2E29,
      $300A..$300F, $301D..$301F,
      $FE72 : r:=$0022;
      //апострофы
      $00B4, $02BE, $02BF, $0301, $055D, $055E, $05F3, $05F4, $200E: r:=$0027;
    end;
    //диакритики и косяки ICU-транситерации
    if Flags.bTranslit then case r of
      //'
      $048C, $048D: r:=$0027;
      //С
      $00A9, $04AA, $04B4, $04B6, $04B8, $04CB: r:= $0043;
      //E
      $0462..$0464: r:= $0045;
      //G
      $0490, $492, $494: r:=$0047;
      //H
      $04B2, $04BA: r:=$0048;
      //I
      $0376: r:=$0049;
      //J
      $037F, $048A: r:=$004A;
      //K
      $049A, $049C, $049E, $04A0, $04C3: r:=$004B;
      //L
      $04C5: r:= $004C;
      //M
      $04CD: r:= $004D;
      //N
      $04A2, $04A4, $04C7, $04C9: r:=$004E;
      //O
      $04E8: r:=$004F;
      //P
      $04A6: r:=$0050;
      //R
      $00AE, $048E: r:=$0052;
      //T
      $04AC: r:=$0054;
      //U
      $04AE, $04B0..$04B1, $046A, $046C: r:= $0055;
      //W
      $0460, $047E: r:= $0057;
      //Z
      $0496, $0498, $04C1: r:= $005A;
      //Ж
      $0416: One2Many('ZH');
      //Х
      $0425: One2Many('KH');
      //Ц
      $0426: One2Many('TS');
      //Ч
      $0427: One2Many('CH');
      //Ш
      $0428: One2Many('SH');
      //Щ
      $0429: One2Many('SHCH');
      //ЪЬ
      $042A, $042C: r:=$0000;
      //Ю
      $042E: One2Many('YU');
      //Я
      $042F: One2Many('YA');
      //c
      $04AB, $04B5, $04B7, $04B9, $04CC: r:= $0063;
      //e
      $0465: r:= $0065;
      //g
      $0491, $493, $495: r:=$0067;
      //h
      $04B3, $04BB: r:= $0068;
      //i
      $0377: r:= $0069;
      //k
      $049B, $049D, $049F, $04A1, $04C4: r:= $006B;
      //l
      $04C6: r:= $006C;
      //m
      $04CE: r:= $006D;
      //n
      $04A3, $04A5, $04C8, $04CA: r:= $006E;
      //o
      $04E9: r:= $006F;
      //p
      $04A7: r:= $0070;
      //r
      $048F: r:=$0072;
      //t
      $04AD: r:=$0074;
      //u
      $04AF, $046B, $046D: r:= $0075;
      //w
      $0461, $047F: r:= $0077;
      //z
      $0497, $0499, $04C2: r:= $007A;
      //ж
      $0436: One2Many('zh');
      //х
      $0445: One2Many('kh');
      //ц
      $0446: One2Many('ts');
      //ч
      $0447: One2Many('ch');
      //ш
      $0448: One2Many('sh');
      //щ
      $0449: One2Many('shch');
      //ъь
      $044A, $044C: r:=$0000;
      //ю
      $044E: One2Many('yu');
      //я
      $044F: One2Many('ya');
      //he
      $10F1: One2Many('he');
      //hie
      $10F2: One2Many('hie');
      //hoe
      $10F5: One2Many('hoe');
    //Исправление косяков ICU
      //SPACE
      $00B7: r:=$0020;
      //#
      $266F: r:= $0023;
      // $
      $00A4: r:= $0024;
      //%
      $2030: r:= $0025;
      //-
      $00AC, $0336: r:= $002D;
      //A
      $A7BA..$A7BB, $2C6F, $0250: r:= $0041;
      //B
      $A796..$A797: r:= $0042;
      //C
      $A7C4, $A794, $2184, $A73E..$A73F: r:= $0043;
      //D
      $A7C7..$A7C8: r:= $0044;
      //E
      $018E..$018F,$AB32..$AB34, $A7AB, $2C7B, $1D94, $1D08, $01DD, $0258, $025C..$025E, $029A: r:= $0045;
      //F
      $20A3, $A7FB, $A798..$A799: r:= $0046;
      //G
      $AB36,$A7D0..$A7D1, $A7AC, $1D77, $A77D..$A77F, $1D79 : r:= $0047;
      //H
      $A7F5..$A7F6, $A795, $2C75..$2C76, $A78D, $02AE..$02AF, $0265: r:=$0048;
      //I
      $A7F7, $A7BC..$A7BD, $A7AE, $1D09, $026E: r:=$0049;
      //J
      $A7B2, $0284: r:=$004A;
      //K
      $A7B0, $029E: r:=$004B;
      //L
      $AB38..$AB39, $A7AD, $A78E, $A780..$A781: r:=$004C;
      //M
      $AB3A, $A7FF, $A7FD, $A7FA, $1D1F, $019C, $026F..$0270: r:=$004D;
      //N
      $AB3B, $1D0E, $0220: r:=$004E;
      //O
      $AB3D..$AB3F, $A7C0..$A7C1, $1D97, $1D16..$1D17, $1D10..$1D13, $0275, $019F, $0186: r:=$004F;
      //P
      $A7FC: r:=$0050;
      //Q
      $A7AF: r:=$0051;
      //R
      $20BD, $AB68, $AB45..$AB4C, $2C79, $1D19..$1D1A, $A782..$A783, $A75A..$A75B, $0279..$027B, $027F, $0281 : r:= $0052;
      //S
      $A7D6..$A7D9, $A7C9..$A7CA, $A7C5, $A784..$A785: r:=$0053;
      //T
      $0372..$0373, $A7B1, $0287: r:=$0054;
      //U
      $AB4E, $AB52, $A7BE..$A7BF, $A7B8..$A7B9, $1D1D..$1D1E: r:=$0055;
      //V
      $0245: r:=$0056;
      //W
      $A7C2..$A7C3, $028D: r:=$0057;
      //X
      $AB56..$AB59: r:=$0058;
      //Y
      $00A5, $AB5A, $028E: r:= $0059;
      //Z
      $A7C6, $A762..$A763 : r:= $005A;
      //a
      $0251..$0252 : r:= $0061;
      //c
      $00A2, $0297: r:= $0063;
      //d
      $02A4: r:= $0064;
      //e
      $0259, $20AC, $025A, $0292..$0293: r:= $0065;
      //f
      $00A3, $0283, $0285..$0286, $02A9: r:= $0066;
      //i
      $0269: r:=$0069;
      //l
      $019B: r:= $006C;
      //n
      $207F: r:= $006E;
      //o
      $00B0, $0254, $0298: r:= $006F;
      //p
      $0278: r:=$0070;
      //q
      $024A..$024B: r:=$0071;
      //t
      $02A7..$02A8: r:= $0074;
      //u
       $028A: r:= $0075;
      //v
      $028C: r:= $0076;
      //w
      $051C, $02AC: r:= $0077;
      // |
      $00A6: r:= $007C;
      ///// MultiSym
      //CHI
      $AB53..$AB55: One2Many('CHI');
      //EZH
      $01B7: One2Many('EZH');
      //AE
      $A79A..$A79B, $1D02: One2Many('AE');
      //ET
      $A76A..$A76B: One2Many('ET');
      //OE
      $AB40..$AB42, $AB62, $A79C..$A79D, $1D14: One2Many('OE');
      //OU
      $1D15, $0222..$0223: One2Many('OU');
      //UI
      $AB50..$AB51: One2Many('IU');
      //UE
      $A79E..$A79F : One2Many('UE');
      //UO
      $AB63: One2Many('UO');
      //DZ
      $AB66: One2Many('DZ');
      //TS
      $AB67: One2Many('TS');
      //GH
      $0194: One2Many('GH');
      //LS
      $AB37: One2Many('LS');
      //IA
      $A7FE: One2Many('IA');
      //IS
      $A76C..$A76D: One2Many('IS');
      //YR
      $01A6: One2Many('YR');
      //gh
      $0263: One2Many('gh');
      //tz
      $A728..$A729: One2Many('tz');
    end;
    //Скобки
    if Flags.bBrackets then begin
      case r of
        $003C, $005B, $007B: r:=$0028;      //<[{
        $003E, $005D, $007D: r:=$0029;      //>]}
      end;
    end;
    //Удаляем символы
          // $0021..$002F                // !"#$%&'()*+,-./
          // $003A..$0040                // :;<=>?@
          // $005B..$005F                // [\]^_
          // $007B..$007F                // {|}~`
          // not (0-9,a-z,A-Z)
    if (r<>$0000) and (r < $007F) and Flags.bDelimiters and (not (Byte(r) in LettersNumAChars)) then r:=$0020;
    // Y,y to I,i
    if Flags.bY2I then case r of
      $0059, $0079: Dec(r,$0010);
    end else if Flags.bI2Y then case r of
      $0049, $0069: Inc(r,$0010);
    end;
    //lowcase
    if Flags.bASCIILocase then r:=CharLow(r);
    //Повторения после транслитерации
    if (Flags.bRepeats and (CharLow(r)=prev)) and not ((r<$007F) and (Byte(r) in ArabianNumbers)) then r:=$0000;
    //Если вообще ничего не помогло
    if Flags.bNonASCIIToHex and (r>$007F) then begin
      One2Many('\u'+IntToHex(r,4));
    end;
    //Несколько пробелов всегда в один
    if (r=$0020) then begin
      //пробелы удаляем всегда
      if (prev in [$0000, $0020]) or Flags.bDelSpaces then r:=$0000;
      prev:=$0020;
    end;
    //первая буква большая, если она маленькая и перед ней разделитель
    if Flags.bASCIICapitalize and (r<$007F) and (prev < $007F)
                              and (Byte(r) in LatinLowerAChars)
                              and ((not (Byte(prev) in LettersNumAChars)) or (prev=$0000)) then r:=r and $FFDF;
    //применяем исправления
    if r<>$0000 then begin
      if (w<>r) or (j<i) then begin
        Input[j]:=WideChar(r);
      end;
      Inc(j);
      prev:=CharLow(r);
    end else if prev = $0000 then prev:=CharLow(w);
    Inc(i);
  end;
  //удаление пробелов в конце строки
  while (Word(Input[j-1]) = $0020) and (j>0) do Dec(j);
  //Окончание строки
  Input[j]:=#0;
  Result:=j;
end;

procedure LowerCase(var s:ShortString);
var
  i: Integer;
begin
  for i:=1 to Length(s) do begin
    if (s[i] in ['A'..'Z']) then s[i]:=Char(Byte(s[i]) or Byte($20));
  end;
end;

{
function LongToHex(l: Longint; digits:Integer=1): ShortString;
const
  HexDigits: array[0..15] of char = '0123456789ABCDEF';
var
  i: integer;
begin
  If digits=0 then digits:=1;
  SetLength(Result, digits);
  for i:=0 to digits-1 do
   begin
    Result[digits-i] := HexDigits[l and 15];
    l:=l shr 4;
   end ;
  while l<>0 do begin
    Result := HexDigits[l and 15] + result;
    l:=l shr 4;
  end;
end;
}

function Utf8IntToUnicode(Code: LongInt): LongInt;
var
  byte1, byte2, byte3, byte4: Byte;
begin
  //пропуск ведущих нулей
  while (Code<>0) and (Code shr 24 = 0) do Code:=Code shl 8;
  // Извлекаем байты из LongInt начиная с самого старшего байта
  byte1 := (Code shr 24) and $FF;  // Старший байт
  byte2 := (Code shr 16) and $FF;  // Второй байт
  byte3 := (Code shr 8) and $FF;   // Третий байт
  byte4 := Code and $FF;           // Младший байт

  // Проверяем, сколько байтов в коде и декодируем символ
  if (byte1 and $80) = 0 then begin
    // 1 байт: 0xxxxxxx (ASCII)
    Result := byte1;
  end else if (byte1 and $E0) = $C0 then begin
    // 2 байта: 110xxxxx 10yyyyyy
    Result := ((byte1 and $1F) shl 6) or (byte2 and $3F);
  end else if (byte1 and $F0) = $E0 then begin
    // 3 байта: 1110xxxx 10yyyyyy 10zzzzzz
    Result := ((byte1 and $0F) shl 12) or ((byte2 and $3F) shl 6) or (byte3 and $3F);
  end else if (byte1 and $F8) = $F0 then begin
    // 4 байта: 11110xxx 10yyyyyy 10zzzzzz 10wwwwww
    Result := ((byte1 and $07) shl 18) or ((byte2 and $3F) shl 12) or ((byte3 and $3F) shl 6) or (byte4 and $3F);
  end else Result:=-1;
end;

function UnicodeIntToUtf8(Code: LongInt): LongInt;
begin
  if Code <= $7F then
  begin
    // 1 байт: 0xxxxxxx
    Result := Code;
  end else if Code <= $7FF then begin
    // 2 байта: 110xxxxx 10yyyyyy
    Result := ((Code shr 6) and $1F) or $C000 or ((Code and $3F) shl 8);
  end else if Code <= $FFFF then begin
    // 3 байта: 1110xxxx 10yyyyyy 10zzzzzz
    Result := ((Code shr 12) and $0F) or $E00000 or ((Code shr 6) and $3F00) or ((Code and $3F) shl 12);
  end else if Code <= $10FFFF then begin
    // 4 байта: 11110xxx 10yyyyyy 10zzzzzz 10wwwwww
    Result := ((Code shr 18) and $07) or $F000000 or ((Code shr 12) and $3F00) or ((Code shr 6) and $3F0000) or ((Code and $3F) shl 18);
  end else Result:=0;
end;


function UTF8Length(p: PAnsiChar): SmallInt;
begin
  Result:= 0;
  while (p^ <>#0) do begin
    if (Byte(p^) and $C0)<>$80 then Inc(Result);
    Inc(p);
  end;
end;


function IsLineTerminator(c:Word):Boolean;
begin
  // Test for any of the Unicode line terminating characters.
  {  if (c & ~(0x0a | 0x0b | 0x0c | 0x0d | 0x85 | 0x2028 | 0x2029))
        return false;

    return (c<=0x0d && c>=0x0a) || c==0x85 || c==0x2028 || c==0x2029;
}
  Result:=c<>c;
end;

function GetDefaultTransformFlags: TTransformFlagsBool;
begin
  with Result do begin
    bQuotes := True;
    bWhiteSpaces := True;
    bRepeats := False;
    bBrackets := False;
    bDelimiters := False;
    bNonASCIIToHex := False;
    bDelSpaces := False;
    bASCIILocase := False;
    bY2I := False;
    bI2Y := False;
    bTranslit := False;
    bSurogates := False;
    bParentheses:= False;
    bASCIICapitalize:= False;
  end;
end;

end.

