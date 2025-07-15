unit parentheses;

{$mode delphi}{$H+}

interface


function ClearParetheses(Buffer:PWideChar; BufLen:SizeInt):SizeInt;

implementation

const
  FOUND_LIST_ARRAY_CAPACITY = 256;

type

  TFoundPairs = record
    StartIndex: SmallInt;
    EndIndex: SmallInt;
  end;

function ClearParetheses(Buffer:PWideChar; BufLen:SizeInt):SizeInt;
var
  FoundList: array[0..FOUND_LIST_ARRAY_CAPACITY-1] of TFoundPairs;
  NestedCount, FoundIndex, LastClose: SmallInt;
  i, j: SizeInt;
  Empty: Boolean;

  function CheckEmpty(StartPos,EndPos:Smallint):Boolean;
  var
    n:SizeInt;
  begin
    Result:=True;
    for n:=StartPos to EndPos do if not (Buffer[n] in [#0..' ']) then begin
      Result:=False;
      break;
    end;
  end;

begin
  Result:=BufLen;
  FoundIndex:=0; NestedCount:=0;
  i:=0;
  while(i < BufLen) do begin
    if Buffer[i] = '(' then begin
      //Нашли начальную скобку, запомнили, счетчик вложенных = 1
      FoundList[FoundIndex].StartIndex:=i;
      Inc(NestedCount);
      //Ищем последнюю подходящую закрывающую
      LastClose:= -1;
      j:=i+1;
      while(j < BufLen) do begin
        if Buffer[j] = ')' then begin
          if NestedCount > 0 then begin
            //Если скобка открыта, то дополняем закрывающей
            Dec(NestedCount);
            LastClose:=j;
            if NestedCount = 0 then FoundList[FoundIndex].EndIndex:=j;
          end else begin
            //Если пара уже закрыта, то переносим закрывающую скобку
            FoundList[FoundIndex].EndIndex:=j;
          end;
        end else if Buffer[j] = '(' then begin
          if NestedCount > 0 then begin
            //Если пара открыта, то увеличиваем счетчик вложенности
            Inc(NestedCount);
          end else begin
            //Если пара закрыта, то начинаем новый FoundList
            break;
          end;
        end;
        Inc(j);
      end;
      if NestedCount > 0 then begin
        //если так и не нашли закрывашку, то используем последнюю найденную, а если нет ничего, то выход
        if LastClose = -1 then break else FoundList[FoundIndex].EndIndex:=LastClose;
      end;
      i:=FoundList[FoundIndex].EndIndex;
      Inc(FoundIndex);
      //На всякий случай проверка на переполнение
      if FoundIndex>= FOUND_LIST_ARRAY_CAPACITY then exit;
      NestedCount:=0;
    end;
    Inc(i);
  end;

  //Корректировка буфера
  if FoundIndex > 0 then begin
    //Предварительная проверка, а не останется ли пустота после удаления
    j:=0; Empty:=True;
    for i:=0 to FoundIndex-1 do begin
      if not CheckEmpty(j,FoundList[i].StartIndex-1) then begin
        Empty:=False;
        break;
      end;
      j:=FoundList[i].EndIndex+1;
    end;
    if Empty and CheckEmpty(FoundList[FoundIndex-1].EndIndex+1, BufLen-1) then exit;

    //Вносим изменения в буфер
    Result:=FoundList[0].StartIndex;
    for i:=1 to FoundIndex-1 do begin
      System.Move(PChar(Buffer + FoundList[i-1].EndIndex + 1)^, PChar(Buffer + Result)^, (FoundList[i].StartIndex - FoundList[i-1].EndIndex - 1) * SizeOf(WideChar));
      Inc(Result, FoundList[i].StartIndex - FoundList[i-1].EndIndex - 1);
    end;
    //дописываем хвост, если есть
    if FoundList[FoundIndex-1].EndIndex < BufLen - 1 then begin
      System.Move(PChar(Buffer + FoundList[FoundIndex-1].EndIndex + 1)^, PChar(Buffer + Result)^, (BufLen - FoundList[FoundIndex-1].EndIndex - 1) * SizeOf(WideChar));
      Inc(Result, BufLen - FoundList[FoundIndex-1].EndIndex - 1);
    end;
    Buffer[Result]:=#0;
  end;
end;

end.

