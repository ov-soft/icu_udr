unit log;

{$mode delphi}{$H+}

interface

uses
  Classes, Firebird;

const
  {$IFDEF WINDOWS}
    LogPath = 'c:\Temp\udr\';
  {$ENDIF}
  {$IFDEF UNIX}
    LogPath = '/var/log/hyper_udr/';
  {$ENDIF}

  function InitLogFile(const FName:AnsiString):Boolean;
  procedure FreeLogFile;
  function InitObjLogFile(const FName:AnsiString):Pointer;
  procedure LogStr(S:AnsiString);
  procedure LogObjStr(Obj:Pointer;S:AnsiString);
  procedure LogInt(const S: AnsiString; N:Integer);
  procedure LogObjInt(Obj:Pointer; const S: AnsiString; N:Integer);
  procedure LogObjValue(Obj:Pointer; Msg:Pointer; Meta:IMessageMetadata; AStatus:IStatus; Index:Integer;Tab:Integer=0);
  procedure FreeObj(Obj:Pointer);
  procedure PrintDump(Obj:Pointer; Data:PByte;Len:Integer;Tab:Integer=0);
  function GetHexPointer(P:Pointer):AnsiString;
  function GetHexQWord(Q:QWord):AnsiString;
  procedure LogObjFlush(Obj:Pointer);

implementation

uses fbtypes, SysUtils;

var
  LogFS: TFileStream;

procedure PrintDump(Obj:Pointer; Data:PByte;Len:Integer;Tab:Integer=0);
const
  DUMP_LEN = 32;
var
  S,T:AnsiString;
  i, n:Integer;
begin
  T:=''; S:='';
  for i:=0 to Tab-1 do T:=T+' ';
  n:=DUMP_LEN;
  while (Len>0) do begin
    S:=S+IntToHex(Byte(Data^),2)+' ';
    Inc(Data);
    Dec(Len);
    Dec(n);
    if n=0 then begin
      LogObjStr(Obj,T+S);
      S:='';
      n:=DUMP_LEN;
    end;
  end;
  if n<>DUMP_LEN then LogObjStr(Obj,T+S);
end;

function InitLogFile(const FName: AnsiString): Boolean;
begin
  LogFS:=TFileStream.Create(LogPath+FName,fmCreate+fmOpenWrite+$0040);
  Result:=True;
end;

procedure FreeLogFile;
begin
  LogFS.Free;
  LogFS:=nil;
end;


function InitObjLogFile(const FName: AnsiString): Pointer;
begin
  Result:=Pointer(TFileStream.Create(LogPath+FName,fmCreate+fmOpenWrite+fmShareDenyNone));  // $0040 =
end;

procedure LogStr(S: AnsiString);
begin
  S:=DateTimeToStr(Now)+ ' '+S{$IFDEF WINDOWS}+#13{$ENDIF}+#10;
  LogFS.Write(S[1],Length(S));
end;

procedure LogObjStr(Obj: Pointer; S: AnsiString);
begin
  S:=DateTimeToStr(Now)+ ' '+S{$IFDEF WINDOWS}+#13{$ENDIF}+#10;
  TFileStream(Obj).Write(S[1],Length(S));
end;

procedure LogInt(const S: AnsiString; N: Integer);
begin
  LogStr(S+IntToStr(N));
end;

procedure LogObjInt(Obj: Pointer; const S: AnsiString; N: Integer);
begin
  LogObjStr(Obj, S+IntToStr(N));
end;



procedure LogObjValue(Obj: Pointer;  Msg:Pointer; Meta: IMessageMetadata; AStatus:IStatus; Index: Integer; Tab:Integer);
var
  Int:Integer;
  Null:Boolean;
  Len: SmallInt;
  S:AnsiString;
  i:Integer;
  pData: PByte;
begin
  S:='';
  for i:=0 to Tab-1 do S:=S+' ';
  S:=S+'Param '+IntToStr(Index)+': ';
  Null:= Boolean(PWordBool(PByte(Msg) +  Meta.getNullOffset(AStatus, Index))^);
  pData:= PByte(Msg)+Meta.getOffset(AStatus, Index);
  if Null then LogObjStr(Obj,S+'NULL') else case Meta.getType(AStatus,Index) of
    SQL_LONG: begin
      Int:=PInteger(pData)^;
      S:=S+'INTEGER: '+IntToStr(Int);
      LogObjStr(Obj,S);
    end;
    SQL_TEXT: begin
      Len:=Meta.getLength(AStatus, Index);
      S:=S+'CHAR('+IntToStr(Len)+'): ' +  PChar(pData);
      LogObjStr(Obj,S);
      //PrintDump(Obj,pData+2,Len);
    end;
    SQL_VARYING: begin
      Len:=PSmallint(pData)^;
      S:=S+'VARCHAR('+IntToStr(Len)+'): ' +  PChar(pData+2);
      LogObjStr(Obj,S);
      //PrintDump(Obj,pData+2,Len);
    end;
    SQL_BOOLEAN: begin
      Null:=PBoolean(PByte(Msg)+Meta.getOffset(AStatus, Index))^;
      S:=S+'BOOLEAN=';
      if Null then S:=S+'True' else S:=S+'False';
      LogObjStr(Obj,S);
    end;
    else LogObjStr(Obj,S+'Can not print Value - not supported type: '+IntToStr(Meta.getType(AStatus,Index)));
  end;
end;

procedure FreeObj(Obj: Pointer);
begin
  TFileStream(Obj).Free;
end;

function GetHexPointer(P: Pointer): AnsiString;
begin
  Result:=IntToHex(QWord(P),16);
end;

function GetHexQWord(Q:QWord):AnsiString;
begin
  Result:=IntToHex(Q,16);
end;

procedure LogObjFlush(Obj: Pointer);
begin
  TFileStream(Obj).Seek(0, soBeginning);
  TFileStream(Obj).Seek(0, soEnd);
end;

end.

