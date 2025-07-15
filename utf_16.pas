unit UTF_16;

{$mode delphi}{$H+}

interface

const
  UTF16SortDataList_SIZE = 1024;

type

  TUTF16SortRec = record
    case Byte of
      1: (Q: QWord);
      2: (Start, Len, IsNumber: SmallInt);
  end;

  { TUTF16 }

  PUTF16SortDataList = ^TUTF16SortDataList;
  TUTF16SortDataList = array[0..UTF16SortDataList_SIZE-1] of TUTF16SortRec;

  TUTF16 = class
  private
    FBuf: PWideChar;
    FSize: SizeUInt;
    FLength: SizeUInt;

    procedure SetDataLength(Len:SizeUInt);
    function GetIsWhiteSpace:Boolean;
    function Pos(c:WideChar; Start: SizeInt=0):SizeInt;
    function StrLenComp(Start1, Len1, Start2, Len2: SizeInt): Sizeint;

  public
    constructor Create(Size: SizeUInt);virtual;overload;
    constructor Create(const UTF8Str: PUTF8Char; Count:SizeInt=-1);virtual;overload;
    constructor Create(const UTF16Str: PWideChar; Count:SizeInt=-1);virtual;overload;
    destructor Destroy; override;

    function FromUTF8(const UTF8Str:PUTF8Char; Count:SizeInt=-1):SizeInt;
    function ToUTF8(const Dest:PUTF8Char; DestSize: SizeInt):SizeInt;
    function ToUTF8Block(const Dest:PUTF8Char; DestSize: SizeInt;BlockStart,BlockEnd:SizeInt):SizeInt;
    function FromUTF16(const UTF16Str:PWideChar; Count: SizeInt=-1):SizeInt;
    procedure UpdateLength;
    function Sort(Delimiter: WideChar):Boolean;

    property PData: PWideChar read FBuf;
    property DataLength:SizeUInt read FLength write SetDataLength;
    property BufferSize:SizeUInt read FSize;
    property IsWitespace:Boolean read GetIsWhiteSpace;
  end;


implementation

uses
  SysUtils;

{ TUTF16 }

procedure TUTF16.SetDataLength(Len: SizeUInt);
begin
  if Assigned(FBuf) then begin
    if Len>FSize then Len:=FSize;
    FBuf[Len]:=#0;
    FLength:=Len;
  end;
end;

function TUTF16.GetIsWhiteSpace: Boolean;
var
  i:SizeInt;
begin
  Result:=True;
  if Assigned(FBuf) then for i:=0 to FLength-1 do begin
    if FBuf[i] <> ' ' then begin
      Result:=False;
      break;
    end;
  end;
end;

function TUTF16.Pos(c: WideChar; Start: SizeInt): SizeInt;
var
  i:SizeInt;
begin
  Result:=-1;
  if (not Assigned(FBuf)) or (Start>=FLength) then exit;
  for i:=Start to FLength-1 do if FBuf[i] = c then begin
    Result:=i;
    break;
  end;
end;

function TUTF16.StrLenComp(Start1, Len1, Start2, Len2: SizeInt): Sizeint;
var
  counter:SizeInt;
  c1, c2: widechar;
begin
  Result:=0;
  if Assigned(FBuf) then begin
    counter:=0;
    repeat
      if (Start1+counter >= FLength) or (Start2+counter >= FLength) then exit;
      c1:=FBuf[Start1+counter];
      c2:=FBuf[Start2+counter];
      inc(counter);
    until (c1<>c2) or (counter>=Len1) or (counter>=Len2);
    Result:=ord(c1)-ord(c2);
  end;
  if Result=0 then Result:=Len1-Len2;
end;

constructor TUTF16.Create(Size: SizeUInt);
begin
  FLength:=0;
  FSize:=0;
  if Size > 0 then begin
    FBuf:=Getmem((Size+1) * SizeOf(WideChar));
    if Assigned(FBuf) then begin
      FBuf[0]:=#0;
      FSize:=Size;
    end;
  end else FBuf:=nil;
end;

constructor TUTF16.Create(const UTF8Str: PUTF8Char; Count:SizeInt);
begin
  if Count=-1 then Count:=Length(UTF8Str);
  Create(Count);
  FromUTF8(UTF8Str, Count);
end;

constructor TUTF16.Create(const UTF16Str: PWideChar; Count: SizeInt);
begin
  if Count=-1 then Count:=Length(UTF16Str);
  Create(Count);
  FromUTF16(UTF16Str);
end;

destructor TUTF16.Destroy;
begin
  if Assigned(FBuf) then Freemem(FBuf);
  FBuf:=nil;
  FSize:=0;
  FLength:=0;
  inherited Destroy;
end;

function TUTF16.FromUTF8(const UTF8Str: PUTF8Char; Count:SizeInt=-1): SizeInt;
begin
  if Assigned(FBuf) and Assigned(UTF8Str) then begin
    if Count=-1 then Count:=Length(UTF8Str);
    FLength:=UTF8ToUnicode(FBuf, FSize, UTF8Str, Count)-1;
    FBuf[FLength]:=#0;
    Result:=FLength;
  end else Result:=0;
end;

function TUTF16.ToUTF8(const Dest: PUTF8Char; DestSize: SizeInt): SizeInt;
begin
  if Assigned(FBuf) and Assigned(Dest) then begin
    Result:=UnicodeToUtf8(Dest, DestSize, FBuf, FLength)-1;
  end else Result:=0;
  if Assigned(Dest) and (Result<DestSize) then Dest[Result]:=#0;
end;

function TUTF16.ToUTF8Block(const Dest: PUTF8Char; DestSize: SizeInt; BlockStart, BlockEnd: SizeInt): SizeInt;
begin
  if Assigned(FBuf) and Assigned(Dest) and (BlockEnd>=BlockStart) and (BlockEnd<=FLength) then begin
    Result:=UnicodeToUtf8(Dest, DestSize, FBuf+BlockStart, BlockEnd-BlockStart)-1;
  end else Result:=0;
  if Assigned(Dest) and (Result<DestSize) then Dest[Result]:=#0;
end;

function TUTF16.FromUTF16(const UTF16Str: PWideChar; Count: SizeInt): SizeInt;
begin
  if Assigned(FBuf) and Assigned(UTF16Str) then begin
    if Count=-1 then Count:=Length(UTF16Str);
    if Count>FSize then Count:=FSize;
    System.Move(PChar(UTF16Str)^, FBuf^, Count * SizeOf(WideChar));
    FBuf[Count]:=#0;
    FLength:=Count;
    Result:=Count;
  end else Result:=0;
end;

procedure TUTF16.UpdateLength;
begin
  FLength:=Length(FBuf);
end;

function TUTF16.Sort(Delimiter: WideChar):Boolean;
var
  i, n:SizeInt;
  SortList: array[0..UTF16SortDataList_SIZE-1] of TUTF16SortRec;
  SortListSize: SizeInt;
  word_start: Boolean;
  TmpBuf: PWideChar;
  w: WideChar;
  mn: TUTF16SortRec;

begin
  //Функция неправильно работает при наличии нескольких разделителей подряд!!!!!
  Result:=False;
  //если нечего сортировать - выходим
  if (not Assigned(FBuf)) or (DataLength<1) then exit;
  SortListSize:=0;
  word_start:=False;
  for i:=0 to DataLength-1 do begin
    w:= FBuf[i];
    if (w = Delimiter) and word_start then begin
      //если конец слова - проверяем, не число ли это
      word_start:=False;
      if (SortListSize>0) and (SortList[SortListSize].IsNumber <> 0) then begin
        //приклеиваем число к предыдущему слову
        Inc(SortList[SortListSize-1].Len,i-SortList[SortListSize].Start+1);
      end else begin
        SortList[SortListSize].Len:=i-SortList[SortListSize].Start;
        Inc(SortListSize);
      end;
    end else begin
      //Повтор разделителя - отключено для правильной склейки слов - переделать!
      //if (w = Delimiter) then continue;
      //слово не начато или не разделитель
      if word_start then begin
        if not (Word(w) in [$0030..$0039]) then SortList[SortListSize].IsNumber:=0;
        continue;
      end;
      //начинаем слово
      with SortList[SortListSize] do begin
        Start:=i;
        if Word(w) in [$0030..$0039] then IsNumber:=1 else IsNumber:=0;
      end;
      word_start:=True;
    end;
  end;
  if word_start then begin
    if (SortListSize>0) and (SortList[SortListSize].IsNumber <> 0) then begin
      //приклеиваем число к предыдущему слову
      Inc(SortList[SortListSize-1].Len,DataLength-SortList[SortListSize].Start+1);
    end else begin
      SortList[SortListSize].Len:=DataLength-SortList[SortListSize].Start;
      Inc(SortListSize);
    end;
  end;
  if SortListSize>1 then begin
    //сортировка пузырьком - позже оптимизировать!
    for n:=0 to SortListSize-2 do begin
      mn:=SortList[n];
      for i:=n+1 to SortListSize-1 do begin
        if StrLenComp(SortList[i].Start, SortList[i].Len,mn.Start, mn.Len) < 0 then begin
          //если нашли меньше текущего минимального
          SortList[n]:=SortList[i];
          SortList[i]:=mn;
          mn:=SortList[n];
        end;
      end;
    end;
    TmpBuf:=Getmem(DataLength*2+2);
    if Assigned(TmpBuf) then try
      //собираем строку заново
      n:=0;
      System.Move(PChar(FBuf)^, TmpBuf^, DataLength * SizeOf(WideChar));
      for i:=0 to SortListSize-1 do begin
        System.Move(PChar(TmpBuf+SortList[i].Start)^, PChar(FBuf+n)^,SortList[i].Len * SizeOf(WideChar));
        Inc(n, SortList[i].Len);
        if n+1>=FSize then break;
        if i=SortListSize-1 then FBuf[n]:=#0 else begin
          FBuf[n]:=Delimiter;
          Inc(n);
        end;
      end;
      //свойство контролирует размер буфера
      DataLength:=n;
    finally
      Freemem(TmpBuf);
    end else exit;
  end else begin
    //меньше двух слов - обойдемся без допбуфера
    if SortListSize=0 then DataLength:=0 else begin
      //Одно слово, просто обрезаем разделители, если есть
      System.Move(PChar(FBuf+SortList[0].Start)^, PChar(FBuf)^,SortList[0].Len * SizeOf(WideChar));
      DataLength:=SortList[0].Len;
    end;
  end;
end;

end.

