unit normalization;

{$mode delphi}{$H+}

interface

//uses ;

//Result - текст ошибки или nil, если нет ошибок
function UniNormalize(const TranslitRule:PUTF8Char;const Src:PUTF8Char;SrcLen:SizeInt;const pDest:PUTF8Char;
                      var DestSize: SizeInt; bParentheses, bBrackets, bSortWords, bDelimiters, bRepeats,
                      bY2I, bI2Y, bDelSpaces, bLocase, bCapitalize, bRome2Arabian, bHex:Boolean{$IFDEF DEBUG_LOG};Log:Pointer {$ENDIF}):AnsiString;


implementation

uses
  UTF_16, utils, ICU{$IFDEF DEBUG_LOG}, log {$ENDIF}, SysUtils, parentheses, Rome2Arabian;

function UniNormalize(const TranslitRule:PUTF8Char;const Src:PUTF8Char;SrcLen:SizeInt;const pDest:PUTF8Char;
                      var DestSize: SizeInt; bParentheses, bBrackets, bSortWords, bDelimiters, bRepeats,
                      bY2I, bI2Y, bDelSpaces, bLocase, bCapitalize, bRome2Arabian, bHex:Boolean{$IFDEF DEBUG_LOG};Log:Pointer {$ENDIF}):AnsiString;
var
  Translit: PUTransliterator;
  Buffer: TUTF16;
  Status, Limit: Integer;
  Flags, FlagsBefore: TTransformFlagsBool;
  TmpSize: SizeInt;
begin
  if Assigned(Src) and Assigned(pDest) then begin
    Result:='';
    if SrcLen > 0 then begin
      Translit:=nil;
      if Assigned(TranslitRule) then begin
        Buffer:=TUTF16.Create(TranslitRule);
        //открываем правило транслитерации
        try
          Status:=0;
          Translit:=ICUFunc.utrans_openU(Buffer.PData ,-1, UTRANS_FORWARD, nil, -1, nil, Status);
        finally
          Buffer.Free;
          Buffer:=nil;
        end;
        if not ICU_CheckStatus(Status) then begin
          Result:=' - Open tranliteration error: '+DecodeError(Status)+' for ID =['+TranslitRule+'].';
          exit;
        end;
      end;
      //Перекодировка входной строки в UTF16
      Limit:=SrcLen * 6;
      if Limit>DestSize then Limit:=DestSize;
      {$IFDEF DEBUG_LOG}
        LogObjInt(Log,'Размер буфера для трансформации и транслитерации: ',Limit);
      {$ENDIF}
      Buffer:=TUTF16.Create(Limit);
      try
        if Buffer.FromUTF8(Src)=0 then begin
          Result:=' - Convertation from UTF8 error.';
          exit;
        end;
        //флаги
        Flags:=GetDefaultTransformFlags;
        Flags.bBrackets := bParentheses or bBrackets;
        Flags.bDelimiters := bDelimiters;
        Flags.bRepeats := bRepeats;
        Flags.bY2I := bY2I;
        Flags.bI2Y := bI2Y;
        Flags.bDelSpaces := bDelSpaces;
        Flags.bASCIILocase := bLocase;
        Flags.bNonASCIIToHex := bHex;
        Flags.bASCIICapitalize := bCapitalize;
        //Замена римских цифр арабскими
        if bRome2Arabian then begin
          Buffer.DataLength:=ReplaceRomeNumbers(Buffer.PData, Buffer.DataLength, Buffer.BufferSize);
        end;
        {$IFDEF DEBUG_LOG}
          LogObjInt(Log,'Данные до трансформации и транслитерации: ', Buffer.DataLength);
          PrintDump(Log, PByte(Buffer.PData),Buffer.DataLength*2);
        {$ENDIF}
        //первый проход нормализации - из-за скобок
        FlagsBefore:=GetDefaultTransformFlags;
        FlagsBefore.bSurogates := True;
        FlagsBefore.bTranslit := Assigned(TranslitRule);
        FlagsBefore.bBrackets := bParentheses or bBrackets;
        FlagsBefore.bRepeats := bRepeats;
        TmpSize:=Transform(Buffer.PData, FlagsBefore, Buffer.BufferSize, Buffer.DataLength);
        if TmpSize=-1 then begin
          Result:=' - Transform error: not enough space to replace diacritics.';
          exit;
        end;
        Buffer.DataLength:=TmpSize;
        {$IFDEF DEBUG_LOG}
          LogObjInt(Log,'Трансформация 1 проход: ', Buffer.DataLength);
          PrintDump(Log, PByte(Buffer.PData),Buffer.DataLength*2);
        {$ENDIF}
        if Assigned(TranslitRule) then begin
          Flags.bTranslit:= True;
          Flags.bSurogates:= True;
          //сама транслитерация
          //Для utrans_transUChars нужно, чтобы limit был установлен
          Limit:=Buffer.DataLength;
          //транслитерация
          ICUFunc.utrans_transUChars(Translit, Buffer.PData, nil , Buffer.BufferSize, 0, Limit, Status);
          if not ICU_CheckStatus(Status) then begin
            Result:=' - Transliteration error: '+DecodeError(Status);
            exit;
          end;
          Buffer.UpdateLength;
          {$IFDEF DEBUG_LOG}
            LogObjInt(Log,'Данные после транслитерации: ', Buffer.DataLength);
            PrintDump(Log, PByte(Buffer.PData),Buffer.DataLength*2);
          {$ENDIF}
        end;
        Limit:=DestSize;
        //
        if Buffer.DataLength > 0 then begin
          if bParentheses then begin
            //скобки удаляются перед трансформацией
            Buffer.DataLength:=ClearParetheses(Buffer.PData, Buffer.DataLength);
            {$IFDEF DEBUG_LOG}
              LogObjInt(Log,'Данные после удаления в скобках: ', Buffer.DataLength);
              PrintDump(Log, PByte(Buffer.PData),Buffer.DataLength*2);
            {$ENDIF}
          end;
          if (Flags.bTranslit or Flags.bParentheses or Flags.bASCIICapitalize or Flags.bASCIILocase or Flags.bDelimiters or
                                 Flags.bDelSpaces or Flags.bI2Y or Flags.bY2I or Flags.bNonASCIIToHex or Flags.bRepeats) then begin
            TmpSize:=Transform(Buffer.PData, Flags, Buffer.BufferSize, Buffer.DataLength);
            if TmpSize=-1 then begin
              Result:=' - Transform error: not enough space to replace diacritics.';
              exit;
            end;
            Buffer.DataLength:=TmpSize;
            {$IFDEF DEBUG_LOG}
              LogObjInt(Log,'Данные после финишной трансформации: ', Buffer.DataLength);
              PrintDump(Log, PByte(Buffer.PData),Buffer.DataLength*2);
            {$ENDIF}
          end;
          //сортировка по словам
          if bSortWords then begin
            Buffer.Sort(' ');
            {$IFDEF DEBUG_LOG}
              LogObjInt(Log,'Данные после сортировки: ', Buffer.DataLength);
              PrintDump(Log, PByte(Buffer.PData),Buffer.DataLength*2);
            {$ENDIF}
          end;
          //Перекодировка в UTF8
          DestSize:=Buffer.ToUTF8(pDest, DestSize);
          {$IFDEF DEBUG_LOG}
            LogObjInt(Log,'Данные в выходном буфере: ', DestSize);
            PrintDump(Log, PByte(pDest), DestSize);
          {$ENDIF}
          //проверка что данные не обрезаны по длине выходного параметра
          Status:=UTF8Length(pDest);
          if Status > (Limit div 4) then begin
            Result:=' - String right truncation. Expected length '+ IntToStr(Limit div 4)+ ', actual '+IntToStr(Buffer.DataLength);
            exit;
          end;
        end else begin
          //если после транслитерации пусто
          DestSize:=0;
          pDest[0]:=#0;
        end;
      finally
        Buffer.Free;
        if Assigned(Translit) then ICUFunc.utrans_close(Translit);
      end;
    end else begin
      //если на входе пусто
      DestSize:=0;
      pDest[0]:=#0;
    end;
  end else Result:=' - input or/and output NULL pointer.';
end;

end.

