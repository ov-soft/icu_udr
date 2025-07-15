library icu_udr;

  //{$mode objfpc}
  {$mode delphi}{$H+}

uses
  {$IFDEF unix}
   // the c memory manager is on some systems much faster for multi-threading
  cmem,
  cthreads,
 {$ENDIF}
  fb_udr_init
  { you can add units after this }
  {$IFDEF DEBUG_LOG}
    , log
  {$ENDIF};

{$DEFINE NO_FBCLIENT}

exports firebird_udr_plugin;


begin
  IsMultiThread := True;
  {$IFDEF DEBUG_LOG}
    InitLogFile('main.log');
    LogStr('Compiled with DEBUG...');
    LogStr('Starting DLL');
  {$ENDIF}
end.

