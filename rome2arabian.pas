unit Rome2Arabian;

{$mode delphi}{$H+}

interface
//uses SysUtils,Dialogs;

function ReplaceRomeNumbers(Buffer:PWideChar; BufLen, MaxSize:SizeInt):SizeInt;

implementation

const
  MAX_ROME_NUMBERS_INFO = 256;

type
  TRomeNumbersInfo = record
    StartIndex: SmallInt;
    EndIndex: SmallInt;
    Value: SmallInt;
  end;

function ReplaceRomeNumbers(Buffer:PWideChar; BufLen, MaxSize:SizeInt):SizeInt;
var
  i: SizeInt = 0;
  V, lr, cr, dcnt: SizeInt;
  j, RomeCnt, OtherCnt: SizeInt;
  FoundList: array[0..MAX_ROME_NUMBERS_INFO-1] of TRomeNumbersInfo;
  FoundIndex: SizeInt = 0;
  w: WideChar;
  RomeFlag: Boolean;
begin
  Result:=BufLen;
  while (i < BufLen) do begin
    w:=Buffer[i];
    if w in ['i','v','x','I','V','X'] then begin
      //Если слово начинается с римской цифры, пробуем преобразовать
      j:=i; RomeCnt:=0; OtherCnt:=0; RomeFlag:=True;
      while (j < BufLen) and not(Buffer[j] in [#0..' ']) do begin
        if RomeFlag and (Buffer[j] in ['i','v','x','I','V','X']) then Inc(RomeCnt) else RomeFlag:=False;
        if not RomeFlag and not (Buffer[j] in ['.',',',':']) then Inc(OtherCnt);
        Inc(j);
      end;
      //Если посторонних символов меньше двух и есть римские - пробуем преобразовать
      if (RomeCnt>0) and (OtherCnt<2) then begin
        V:=0; lr:=0; dcnt:=0;
        j:=i+RomeCnt-1;
        while j>=i do begin
          case Buffer[j] of
            'i','I': cr:=1;
            'v','V': cr:=5;
            'x','X': cr:=10;
            else cr:=0;
          end;
          if (lr-cr >= cr) and (cr > 1) then begin
            V:=0;
            break;
          end;
          if (cr < lr) or ((cr = lr) and (dcnt > 0)) then begin
            Dec(V,cr);
            Inc(dcnt);
          end else begin
            Inc(V,cr);
            dcnt:=0;
          end;
          lr:=cr;
          Dec(j);
        end;
        //Если получилось число, то сохраняем в массив изменений
        if (V > 1) and (V < 50) and (dcnt < 2) then begin
          with FoundList[FoundIndex] do begin
            Value:=V;
            StartIndex:=i;
            EndIndex:=i+RomeCnt;
          end;
          Inc(FoundIndex);
        end;
      end;
    end;
    //проматываем до разделителя
    while (i < BufLen) and not(Buffer[i] in [#0..' ']) do Inc(i);
    //проматываем до нового слова
    while (i < BufLen) and (Buffer[i] in [#0..' ']) do Inc(i);
  end;
  //Меняем, если нужно
  if FoundIndex > 0 then begin
    //Перемещаем исходную строку в конец буфера
    System.Move(PChar(Buffer)^,PChar(Buffer+(MaxSize-BufLen))^,BufLen * SizeOf(WideChar));
    Result:=FoundList[0].StartIndex;
    for i:=1 to FoundIndex-1 do begin
      //рисуем арабскую цифру
      if FoundList[i-1].Value > 9 then begin
        Buffer[Result]:=WideChar($0030 + FoundList[i-1].Value div 10);
        Inc(Result);
        FoundList[i-1].Value:=FoundList[i-1].Value mod 10;
      end;
      Buffer[Result]:=WideChar($0030 + FoundList[i-1].Value);
      Inc(Result);
      //дописываем хвост
      System.Move(PChar(Buffer + (FoundList[i-1].EndIndex + MaxSize-BufLen))^, PChar(Buffer + Result)^, (FoundList[i].StartIndex - FoundList[i-1].EndIndex) * SizeOf(WideChar));
      Inc(Result, FoundList[i].StartIndex - FoundList[i-1].EndIndex);
    end;
    i:=FoundIndex-1;
    if FoundList[i].EndIndex <= BufLen then begin
      //рисуем арабскую цифру
      if FoundList[i].Value > 9 then begin
        Buffer[Result]:=WideChar($0030 + FoundList[i].Value div 10);
        Inc(Result);
        FoundList[i].Value:=FoundList[i].Value mod 10;
      end;
      Buffer[Result]:=WideChar($0030 + FoundList[i].Value);
      Inc(Result);
      //дописываем хвост
      System.Move(PChar(Buffer + (FoundList[i].EndIndex + MaxSize-BufLen))^, PChar(Buffer + Result)^, (BufLen - (FoundList[i].EndIndex -1)) * SizeOf(WideChar));
      Inc(Result, BufLen - FoundList[i].EndIndex);
    end;
    Buffer[Result]:=#0;
  end;
end;

end.

