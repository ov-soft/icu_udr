unit fb_udr_init;

{$mode delphi}{$H+}

interface

uses Firebird;

function firebird_udr_plugin(status: iStatus; theirUnloadFlagLocal: BooleanPtr; udrPlugin: iUdrPlugin): BooleanPtr; cdecl;

implementation

uses
  {$IFDEF DEBUG_LOG}
    log,
  {$ENDIF}
  ovs_fb_udr, icu, normalize_func;


var
  myUnloadFlag   : Boolean;
  theirUnloadFlag: BooleanPtr;


function firebird_udr_plugin(status: iStatus; theirUnloadFlagLocal: BooleanPtr; udrPlugin: iUdrPlugin): BooleanPtr; cdecl;
begin
  udrPlugin.registerFunction(status, 'UTF8_normalize', TOvsUdrFunctionFactory.Create(TNornalizeFunction));
  //
  theirUnloadFlag := theirUnloadFlagLocal;
  Result := @myUnloadFlag;
  InitICU;
  {$IFDEF DEBUG_LOG}
    if ICUFunc.icuLoaded then begin
      LogStr('ICU Initialized');
    end else begin
      LogStr('ICU Initialization error');
    end;
  {$ENDIF}
end;

initialization
  myUnloadFlag := false;
finalization
  {$IFDEF DEBUG_LOG}
    LogStr('Free ICU library...');
  {$ENDIF}
  CloseICU;
  {$IFDEF DEBUG_LOG}
    LogStr('Exiting DLL...');
    FreeLogFile;
  {$ENDIF}
  if ((theirUnloadFlag <> nil) and not myUnloadFlag) then theirUnloadFlag^ := True;
end.

