unit normalize_func;

{$mode Delphi}{$H+}

interface

uses
  ovs_fb_udr,
  Firebird;


type
  TNormalizeModes = record
    Translit: Boolean;
    Parentheses: Boolean;
    SortWords: Boolean;
    Delimiters: Boolean;
    Repeats: Boolean;
    DelSpaces: Boolean;
    Brackets: Boolean;
    Locase: Boolean;
    Capitalize: Boolean;
    Rome2Arabian: Boolean;
    Y2I: Boolean;
    I2Y: Boolean;
    Hex: Boolean;
  end;

  { TNornalizeFunction }

  TNornalizeFunction = class(TOvsUdrFunction)
  const
    INPUT_PARAMS_COUNT = 14;
    INPUT_VARCHAR_TEXT = 0;
    INPUT_BOOLEAN_TRANSLIT = 1;
    INPUT_BOOLEAN_PARENTHESES = 2;
    INPUT_BOOLEAN_SORTWORDS = 3;
    INPUT_BOOLEAN_DELIMITERS = 4;
    INPUT_BOOLEAN_REPEATS = 5;
    INPUT_BOOLEAN_BRACKETS = 6;
    INPUT_BOOLEAN_LOWCASE = 7;
    INPUT_BOOLEAN_CAPITALIZE = 8;
    INPUT_BOOLEAN_ROME = 9;
    INPUT_BOOLEAN_DELSPACES = 10;
    INPUT_BOOLEAN_Y2I = 11;
    INPUT_BOOLEAN_I2Y = 12;
    INPUT_BOOLEAN_HEX = 13;

    OUTPUT_PARAMS_COUNT = 1;
    OUTPUT_VARCHAR_TEXT = 0;
  protected
    function CheckParameters:Boolean; override;
  public
    procedure Exec; override;
  end;

implementation

uses ICU, normalization, SysUtils;

{ TNornalizeFunction }

function TNornalizeFunction.CheckParameters:Boolean;
begin
  {$IFDEF DEBUG_LOG}
    IntLog('NornalizeFunction.CheckParameters()');
  {$ENDIF}
  //проверка параметров
  Result:=(Context.InParamsCount = INPUT_PARAMS_COUNT) and
                    (Context.OutParamsCount = OUTPUT_PARAMS_COUNT);
  if not Result then begin
    Context.Exception('Illegal parameters count.');
    exit;
  end;
end;

procedure TNornalizeFunction.Exec;
var
  Modes: TNormalizeModes;
  OutSize: SizeInt;
  Err: AnsiString;
begin
  if not Context.InputFieldCheckNull(INPUT_VARCHAR_TEXT) then begin
    with Modes, Context do begin
      Translit:=InputFieldCheckTrue(INPUT_BOOLEAN_TRANSLIT);
      Parentheses:=InputFieldCheckTrue(INPUT_BOOLEAN_PARENTHESES);
      SortWords:=InputFieldCheckTrue(INPUT_BOOLEAN_SORTWORDS);
      Delimiters:=InputFieldCheckTrue(INPUT_BOOLEAN_DELIMITERS);
      Repeats:=InputFieldCheckTrue(INPUT_BOOLEAN_REPEATS);
      DelSpaces:=InputFieldCheckTrue(INPUT_BOOLEAN_DELSPACES);
      Brackets:=InputFieldCheckTrue(INPUT_BOOLEAN_BRACKETS);
      Locase:=InputFieldCheckTrue(INPUT_BOOLEAN_LOWCASE);
      Capitalize:=InputFieldCheckTrue(INPUT_BOOLEAN_CAPITALIZE);
      Rome2Arabian:=InputFieldCheckTrue(INPUT_BOOLEAN_ROME);
      Y2I:=InputFieldCheckTrue(INPUT_BOOLEAN_Y2I);
      I2Y:=InputFieldCheckTrue(INPUT_BOOLEAN_I2Y);
      Hex:=InputFieldCheckTrue(INPUT_BOOLEAN_HEX);
    end;
    if (Modes.Translit or Modes.Parentheses) and not ICUFunc.icuLoaded then begin
      //нужна ICU, а ее нет
      Context.Exception('ICU library not found. Check files: '+ICU1_DLL+', '+ICU2_DLL{$IFDEF WINDOWS}+', '+ICU3_DLL{$ENDIF});
      exit;
    end;
    OutSize:=Context.OutputMetadata.getLength(Context.Status,OUTPUT_VARCHAR_TEXT);
    if Modes.Translit then with Modes do begin
      Err:=UniNormalize(Context.Body,
                        @PVarcharParam(Context.InMsg).Value,
                        PVarcharParam(Context.InMsg).Length,
                        @PVarcharParam(Context.OutMsg).Value,
                        OutSize,
                        Parentheses,
                        Brackets,
                        SortWords,
                        Delimiters,
                        Repeats,
                        Y2I, I2Y,
                        DelSpaces,
                        Locase,
                        Capitalize,
                        Rome2Arabian,
                        Hex {$IFDEF DEBUG_LOG}, Log {$ENDIF});
    end else with Modes do begin
      Err:=UniNormalize(nil,
                        @PVarcharParam(Context.InMsg).Value,
                        PVarcharParam(Context.InMsg).Length,
                        @PVarcharParam(Context.OutMsg).Value,
                        OutSize,
                        Parentheses,
                        Brackets,
                        SortWords,
                        Delimiters,
                        Repeats,
                        Y2I, I2Y,
                        DelSpaces,
                        Locase,
                        Capitalize,
                        Rome2Arabian,
                        Hex {$IFDEF DEBUG_LOG}, Log {$ENDIF});
    end;
    if Err<>'' then begin
      Context.Exception(Err);
      exit;
    end;
    PVarcharParam(Context.OutMsg).Length:=OutSize;
    Context.OutputFieldSetNull(OUTPUT_VARCHAR_TEXT,False);
  end else Context.OutputFieldSetNull(OUTPUT_VARCHAR_TEXT,True);
end;

end.



