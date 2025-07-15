unit ovs_fb_udr;

{$mode Delphi}{$H+}
interface

uses
  Firebird;

procedure OVSoft_UDR_Exception(AStatus:IStatus; msg: AnsiString);

type
  TInputOutputParam = (ptInput, ptOutput);

  PCharParam = ^TCharParam;
  TCharParam = array[0..32764] of AnsiChar;

  PVarcharParam = ^TVarcharParam;
  TVarcharParam = record
    Length: SmallInt;
    Value: array[0..32764] of AnsiChar;
  end;

  { TOvsUdrContext }

  TOvsUdrContext = class
  const
    EmptyPAnsiChar:PAnsiChar = '';
  private
    FInputMsg: Pointer;
    FOutputMsg: Pointer;
    FStatus: IStatus;
    FBody: PAnsiChar;
    FName: PAnsiChar;
    FRoutineName: PAnsiChar;

    FInParamsCount:Integer;
    FOutParamsCount:Integer;
    FMetadata: IRoutineMetadata;
    FContext: IExternalContext;
    FInputMetadata: IMessageMetadata;
    FOutputMetadata: IMessageMetadata;

    function GetInputMetadata:IMessageMetadata;
    function GetOutputMetadata:IMessageMetadata;
    function GetInParamCount:Integer;
    function GetOutParamCount:Integer;
    procedure FieldIndexOutOfBounds(const Name:AnsiString);

  public
    constructor Create(AMetadata: IRoutineMetadata;AStatus: IStatus;AContext: IExternalContext); virtual;
    destructor Destroy; override;
    procedure SetContext(AStatus: IStatus;AInMsg, AOutMsg: Pointer);
    procedure Exception(msg: AnsiString);

    function GetFieldLength(ParamType:TInputOutputParam; FieldIndex:Integer):SmallInt;
    function InputFieldCheckTrue(FieldIndex:Integer):Boolean;
    function GetFieldOffset(ParamType:TInputOutputParam; FieldIndex:Integer): PAnsiChar;

    function InputFieldCheckNull(FieldIndex:Integer):Boolean;
    procedure OutputFieldSetNull(FieldIndex:Integer; Value:Boolean);

    function InputFieldGetBoolean(FieldIndex:Integer):Boolean;
    procedure OutputFieldSetBoolean(FieldIndex:Integer; Value:Boolean);

    function InputFieldGetShort(FieldIndex:Integer): SmallInt;
    procedure OutputFieldSetShort(FieldIndex:Integer; Value:SmallInt);

    function InputFieldVarcharGetLength(FieldIndex:Integer):SmallInt;
    function InputFieldVarcharGetPointer(FieldIndex:Integer):PAnsiChar;

    property InMsg:Pointer read FInputMsg;
    property OutMsg:Pointer read FOutputMsg;
    property Status: IStatus read FStatus;
    property Body: PAnsiChar read FBody;
    property CallerName: PAnsiChar read FName;
    property RoutineName: PAnsiChar read FRoutineName;
    property InParamsCount:Integer read GetInParamCount;
    property OutParamsCount:Integer read GetOutParamCount;

    property InputMetadata:IMessageMetadata read GetInputMetadata;
    property OutputMetadata:IMessageMetadata read GetOutputMetadata;
  end;



  TOvsUdrFunction = class;
  TOvsUdrProcedure = class;
  TOvsUdrProcResultSet = class;

  TOvsUdrFunctionClass = class of TOvsUdrFunction;
  TOvsUdrProcedureClass = class of TOvsUdrProcedure;
  TOvsUdrProcResultSetClass = class of TOvsUdrProcResultSet;

  { TOvsUdrFunctionFactory }

  TOvsUdrFunctionFactory = class (IUdrFunctionFactoryImpl)
  private
    FFunctionClass: TOvsUdrFunctionClass;
  public
    constructor Create(FunctionClass: TOvsUdrFunctionClass);

    procedure dispose(); override;
    procedure setup(AStatus: IStatus; AContext: IExternalContext; AMetadata: IRoutineMetadata; AInBuilder: IMetadataBuilder; AOutBuilder: IMetadataBuilder); override;
    function newItem(AStatus: IStatus; AContext: IExternalContext; AMetadata: IRoutineMetadata): IExternalFunction; override;
  end;

  { TOvsUdrFunction }

  TOvsUdrFunction = class(IExternalFunctionImpl)
  private
    {$IFDEF DEBUG_LOG}
      FFunctionName: AnsiString;
      FLog:Pointer;
    {$ENDIF}
    FUdrContext: TOvsUdrContext;
    FParametersChecked: Boolean;
  protected
    function CheckParameters:Boolean; virtual;
  public
    constructor Create(Context:TOvsUdrContext); virtual;
    destructor Destroy; override;
    procedure Exec; virtual;

    procedure execute(AStatus: IStatus; AContext: IExternalContext; AInMsg: Pointer; AOutMsg: Pointer); override;
    procedure dispose(); override;
    procedure getCharSet(AStatus: IStatus; AContext: IExternalContext; AName: PAnsiChar; ANameSize: Cardinal); override;

    {$IFDEF DEBUG_LOG}
      procedure IntLog(const S:AnsiString);
      property Log:Pointer read FLog;
      property FunctionName: AnsiString read FFunctionName write FFunctionName;
    {$ENDIF}
    property Context: TOvsUdrContext read FUdrContext;
  end;

  { TOvsUdrProcedureFactory }

  TOvsUdrProcedureFactory = class (IUdrProcedureFactoryImpl)
  private
    FProcClass: TOvsUdrProcedureClass;
  public
    constructor Create(ProcClass: TOvsUdrProcedureClass);

    procedure dispose(); override;
    procedure setup(AStatus: IStatus; AContext: IExternalContext; AMetadata: IRoutineMetadata; AInBuilder: IMetadataBuilder; AOutBuilder: IMetadataBuilder); override;
    function newItem(AStatus: IStatus; AContext: IExternalContext; AMetadata: IRoutineMetadata): IExternalProcedure; override;
  end;

  { TOvsUdrProcedure }

  TOvsUdrProcedure = class(IExternalProcedureImpl)
  private
    {$IFDEF DEBUG_LOG}
      FLog:Pointer;
    {$ENDIF}
    FUdrContext: TOvsUdrContext;
    FParametersChecked: Boolean;
  protected
    function CheckParameters:Boolean; virtual;
  public
    constructor Create(Context:TOvsUdrContext); virtual;
    destructor Destroy; override;
    function Exec: IExternalResultSet; virtual;


    procedure dispose(); override;
    procedure getCharSet(AStatus: IStatus; AContext: IExternalContext; AName: PAnsiChar; ANameSize: Cardinal); override;
    function open(AStatus: IStatus; AContext: IExternalContext; AInMsg: Pointer; AOutMsg: Pointer): IExternalResultSet; override;

    property Context: TOvsUdrContext read FUdrContext;
    {$IFDEF DEBUG_LOG}
      procedure IntLog(const S:AnsiString);
      property Log:Pointer read FLog;
    {$ENDIF}
  end;

  { TOvsUdrSelectiveProcedure

  TOvsUdrSelectiveProcedure = class(TOvsUdrProcedure)
  protected
    class function GetResultSetClass:TOvsUdrProcResultSetClass; virtual; abstract;
  public
    function open(AStatus: IStatus; AContext: IExternalContext; AInMsg: Pointer; AOutMsg: Pointer): IExternalResultSet; override;
  end;
}
  { TOvsUdrProcResultSet }

  TOvsUdrProcResultSet = class(IExternalResultSetImpl)
  protected
    {$IFDEF DEBUG_LOG}
      FLog:Pointer;
    {$ENDIF}
  public
    procedure dispose(); override;
    function fetch(AStatus: IStatus): Boolean; override;

    {$IFDEF DEBUG_LOG}
      procedure IntLog(const S:AnsiString);
      property Log:Pointer read FLog;
    {$ENDIF}
  end;



implementation
  uses fbtypes
{$IFDEF DEBUG_LOG}
  , log
{$ENDIF};

{ TOvsUdrContext }

function TOvsUdrContext.GetInputMetadata: IMessageMetadata;
begin
  if not Assigned(FInputMetadata) then FInputMetadata:=FMetadata.getInputMetadata(FStatus);
  Result:=FInputMetadata;
end;

function TOvsUdrContext.GetOutputMetadata: IMessageMetadata;
begin
  if not Assigned(FOutputMetadata) then FOutputMetadata:=FMetadata.getOutputMetadata(FStatus);
  Result:=FOutputMetadata;
end;

function TOvsUdrContext.GetInParamCount: Integer;
begin
  if FInParamsCount = -1 then FInParamsCount:=InputMetadata.getCount(FStatus);
  Result:=FInParamsCount;
end;

function TOvsUdrContext.GetOutParamCount: Integer;
begin
  if FOutParamsCount = -1 then FOutParamsCount:=OutputMetadata.getCount(FStatus);
  Result:=FOutParamsCount;
end;

procedure TOvsUdrContext.FieldIndexOutOfBounds(const Name: AnsiString);
begin
  Exception(Name+' FieldIndex out of bounds.');
end;

constructor TOvsUdrContext.Create(AMetadata: IRoutineMetadata; AStatus: IStatus; AContext: IExternalContext);
begin
  FStatus:=AStatus;
  FBody:=AMetadata.getBody(AStatus);
  FName:=AMetadata.getName(AStatus);
  FRoutineName:=AMetadata.getEntryPoint(AStatus);
  FContext:=AContext;
  FMetadata:=AMetadata;
  FInParamsCount:=-1;
  FOutParamsCount:=-1;

  FInputMetadata:=nil;
  FOutputMetadata:=nil;
end;

destructor TOvsUdrContext.Destroy;
begin
  if Assigned(FInputMetadata) then FInputMetadata.release;
  if Assigned(FOutputMetadata) then FOutputMetadata.release;
  inherited Destroy;
end;

procedure TOvsUdrContext.SetContext(AStatus: IStatus;AInMsg, AOutMsg: Pointer);
begin
  FInputMsg := AInMsg;
  FOutputMsg := AOutMsg;
  FStatus:=AStatus;
end;

procedure TOvsUdrContext.Exception(msg: AnsiString);
var
  statusVector: array[0..4] of NativeIntPtr;
begin
  statusVector[0] := NativeIntPtr(isc_arg_gds);
  statusVector[1] := NativeIntPtr(isc_random);
  statusVector[2] := NativeIntPtr(isc_arg_string);
  statusVector[3] := NativeIntPtr(PAnsiChar(FRoutineName+': '+msg));
  statusVector[4] := NativeIntPtr(isc_arg_end);

  FStatus.setErrors(@statusVector);
end;

function TOvsUdrContext.InputFieldCheckNull(FieldIndex: Integer): Boolean;
begin
  if FieldIndex<InParamsCount then begin
    Result:=Boolean(PWordBool(FInputMsg+InputMetadata.getNullOffset(FStatus,FieldIndex))^);
  end else begin
    Result:=False;
    FieldIndexOutOfBounds('UdrContext.InputFieldCheckNull');
  end;
end;

procedure TOvsUdrContext.OutputFieldSetNull(FieldIndex: Integer; Value: Boolean);
begin
  if FieldIndex<OutParamsCount then begin
    PWordBool(FOutputMsg+OutputMetadata.getNullOffset(FStatus,FieldIndex))^:=Value;
  end else FieldIndexOutOfBounds('UdrContext.OutputFieldSetNull');
end;

function TOvsUdrContext.InputFieldGetBoolean(FieldIndex: Integer): Boolean;
begin
  if FieldIndex<InParamsCount then begin
    Result:=Boolean((FInputMsg+InputMetadata.getOffset(FStatus, FieldIndex))^);
  end else begin
    Result:=False;
    FieldIndexOutOfBounds('UdrContext.InputFieldGetBoolean');
  end;
end;

function TOvsUdrContext.InputFieldCheckTrue(FieldIndex: Integer): Boolean;
begin
  if FieldIndex<InParamsCount then begin
    Result:=(not Boolean(PWordBool(FInputMsg+InputMetadata.getNullOffset(FStatus,FieldIndex))^)) and
            Boolean((FInputMsg+InputMetadata.getOffset(FStatus, FieldIndex))^);
  end else begin
    Result:=False;
    FieldIndexOutOfBounds('UdrContext.InputFieldCheckTrue');
  end;
end;

function TOvsUdrContext.GetFieldOffset(ParamType: TInputOutputParam; FieldIndex: Integer): PAnsiChar;
begin
  case ParamType of
    ptInput: begin
      if FieldIndex<InParamsCount then begin
        Result:=FInputMsg+InputMetadata.getOffset(FStatus, FieldIndex);
      end else begin
        Result:=EmptyPAnsiChar;
        FieldIndexOutOfBounds('UdrContext.InputFieldGetOffset');
      end;
    end;
    ptOutput: begin
      if FieldIndex<OutParamsCount then begin
        Result:=FOutputMsg+OutputMetadata.getOffset(FStatus, FieldIndex);
      end else begin
        Result:=EmptyPAnsiChar;
        FieldIndexOutOfBounds('UdrContext.OutputFieldGetOffset');
      end;
    end;
    else Result:=EmptyPAnsiChar;
  end;
end;

function TOvsUdrContext.InputFieldVarcharGetLength(FieldIndex: Integer): SmallInt;
begin
  Result:=InputFieldGetShort(FieldIndex);
end;

function TOvsUdrContext.InputFieldVarcharGetPointer(FieldIndex: Integer): PAnsiChar;
begin
  Result:=GetFieldOffset(ptInput,FieldIndex)+2;
end;

function TOvsUdrContext.InputFieldGetShort(FieldIndex: Integer): SmallInt;
begin
  if FieldIndex<InParamsCount then begin
    Result:=PSmallInt(FInputMsg+InputMetadata.getOffset(FStatus, FieldIndex))^;
  end else begin
    Result:=0;
    FieldIndexOutOfBounds('UdrContext.InputFieldGetShort');
  end;
end;

procedure TOvsUdrContext.OutputFieldSetShort(FieldIndex: Integer; Value: SmallInt);
begin
  if FieldIndex<OutParamsCount then begin
    PSmallInt(FOutputMsg+OutputMetadata.getOffset(FStatus, FieldIndex))^:=Value;
  end else FieldIndexOutOfBounds('UdrContext.OutputFieldSetShort');
end;

procedure TOvsUdrContext.OutputFieldSetBoolean(FieldIndex: Integer; Value: Boolean);
begin
  if FieldIndex<OutParamsCount then begin
    Boolean((FOutputMsg+OutputMetadata.getOffset(FStatus, FieldIndex))^):=Value;
  end else FieldIndexOutOfBounds('UdrContext.OutputFieldSetBoolean');
end;

function TOvsUdrContext.GetFieldLength(ParamType: TInputOutputParam; FieldIndex: Integer): SmallInt;
begin
  case ParamType of
    ptInput: begin
      if FieldIndex<InParamsCount then begin
        Result:=InputMetadata.getLength(FStatus,FieldIndex);
      end else begin
        Result:=0;
        FieldIndexOutOfBounds('UdrContext.GetFieldLength');
      end;
    end;
    ptOutput: begin
      if FieldIndex<OutParamsCount then begin
        Result:=OutputMetadata.getLength(FStatus,FieldIndex);
      end else begin
        Result:=0;
        FieldIndexOutOfBounds('UdrContext.GetFieldLength');
      end;
    end;
    else Result:=0;
  end;
end;


{ TOvsUdrFunctionFactory }

constructor TOvsUdrFunctionFactory.Create(FunctionClass: TOvsUdrFunctionClass);
begin
  inherited create;
  FFunctionClass:=FunctionClass;
  {$IFDEF DEBUG_LOG}
    LogStr('UdrFunctionFactory.Create('+FunctionClass.ClassName+') - '+GetHexPointer(Self));
  {$ENDIF}
end;

procedure TOvsUdrFunctionFactory.dispose();
begin
  {$IFDEF DEBUG_LOG}
    LogStr('UdrFunctionFactory.dispose('+FFunctionClass.ClassName+') - '+GetHexPointer(Self));
  {$ENDIF}
  Free;
end;

procedure TOvsUdrFunctionFactory.setup(AStatus: IStatus; AContext: IExternalContext; AMetadata: IRoutineMetadata;AInBuilder: IMetadataBuilder; AOutBuilder: IMetadataBuilder);
begin
  {$IFDEF DEBUG_LOG}
    LogStr('UdrFunctionFactory.setup() - '+GetHexPointer(Self));
  {$ENDIF}
end;

function TOvsUdrFunctionFactory.newItem(AStatus: IStatus; AContext: IExternalContext; AMetadata: IRoutineMetadata): IExternalFunction;
begin
  {$IFDEF DEBUG_LOG}
    LogStr('UdrFunctionFactory.newItem() - '+GetHexPointer(Self));
  {$ENDIF}
  Result:=FFunctionClass.Create(TOvsUdrContext.Create(AMetadata, AStatus, AContext));
end;

{ TOvsUdrFunction }

function TOvsUdrFunction.CheckParameters:Boolean;
begin
  {$IFDEF DEBUG_LOG}
    IntLog('======= OvsUdrFunction.CheckParameters =======');
  {$ENDIF}
  Result:=True;
end;

constructor TOvsUdrFunction.Create(Context:TOvsUdrContext);
{$IFDEF DEBUG_LOG}
{$IFNDEF DEBUG_LOG_NO_DETAILS}
  var
    i:SizeInt;
{$ENDIF}
{$ENDIF}
begin
  inherited create;
  {$IFDEF DEBUG_LOG}
    FLog:=InitObjLogFile(GetHexPointer(Self)+'.log');
    IntLog('======= UdrFunction creation start =======');
  {$ENDIF}
  FUdrContext:=Context;
  FParametersChecked:=False;
  {$IFDEF DEBUG_LOG}
    {$IFNDEF DEBUG_LOG_NO_DETAILS}
      //
      IntLog('OvsUdrProcedure.Create() - Context:');
      IntLog('  DatabaseName: '+Context.FContext.getDatabaseName());
      IntLog('  UserName: '+Context.FContext.getUserName());
      IntLog('  ClientCharSet: '+Context.FContext.getClientCharSet());

      IntLog('OvsUdrProcedure.Create() - Metadata:');
      IntLog('  Name: '+Context.CallerName);
      IntLog('  Package: '+Context.FMetadata.getPackage(Context.Status));
      IntLog('  EntryPoint: '+Context.RoutineName);
      IntLog('  Body: '+Context.Body);

      LogObjInt(Log, '  InputParamCount: ', Context.InParamsCount);
      for i:=0 to Context.InParamsCount-1 do begin
        IntLog('    Param: '+Context.InputMetadata.getField(Context.Status,i));
        IntLog('      Type: '+fb_type2string(Context.InputMetadata.getType(Context.Status,i)));
        LogObjInt(Log, '      isNullable: ', Byte(Context.InputMetadata.isNullable(Context.Status,i)));
        LogObjInt(Log, '      Length: ', Context.InputMetadata.getLength(Context.Status,i));
        IntLog('      CharSet: '+fb_get_charset_name(Context.InputMetadata.getCharSet(Context.Status,i)));
        LogObjInt(Log, '      Offset: ', Context.InputMetadata.getOffset(Context.Status,i));
        LogObjInt(Log, '      NullOffset: ', Context.InputMetadata.getNullOffset(Context.Status,i));
      end;
      LogObjInt(Log, '  OutputParamCount: ', Context.OutParamsCount);
      for i:=0 to Context.OutParamsCount-1 do begin
        IntLog('    Param: '+Context.OutputMetadata.getField(Context.Status,i));
        IntLog('      Type: '+fb_type2string(Context.OutputMetadata.getType(Context.Status,i)));
        LogObjInt(Log, '      isNullable: ', Byte(Context.OutputMetadata.isNullable(Context.Status,i)));
        LogObjInt(Log, '      Length: ', Context.OutputMetadata.getLength(Context.Status,i));
        IntLog('      CharSet: '+fb_get_charset_name(Context.OutputMetadata.getCharSet(Context.Status,i)));
        LogObjInt(Log, '      Offset: ', Context.OutputMetadata.getOffset(Context.Status,i));
        LogObjInt(Log, '      NullOffset: ', Context.OutputMetadata.getNullOffset(Context.Status,i));
      end;
    {$ENDIF}
  {$ENDIF}
end;

destructor TOvsUdrFunction.Destroy;
begin
  FUdrContext.Free;
  {$IFDEF DEBUG_LOG}
    IntLog('======= Free and close this Log file =======');
    FreeObj(FLog);
  {$ENDIF}
  inherited Destroy;
end;

procedure TOvsUdrFunction.Exec;
begin
  {$IFDEF DEBUG_LOG}
    {$IFNDEF DEBUG_LOG_NO_DETAILS}
      IntLog('UdrFunction.Exec()');
    {$ENDIF}
  {$ENDIF}
end;

procedure TOvsUdrFunction.dispose();
begin
  {$IFDEF DEBUG_LOG}
    IntLog('UdrFunction.dispose()');
  {$ENDIF}
  Free;
end;

procedure TOvsUdrFunction.getCharSet(AStatus: IStatus; AContext: IExternalContext; AName: PAnsiChar; ANameSize: Cardinal);
begin
  {$IFDEF DEBUG_LOG}
    {$IFNDEF DEBUG_LOG_NO_DETAILS}
      LogObjInt(FLog, 'UdrFunction.getCharSet: Name: '+AName+', NameSize: ', ANameSize);
    {$ENDIF}
  {$ENDIF}
end;

procedure TOvsUdrFunction.execute(AStatus: IStatus; AContext: IExternalContext; AInMsg: Pointer; AOutMsg: Pointer);
begin
  {$IFDEF DEBUG_LOG}
    {$IFNDEF DEBUG_LOG_NO_DETAILS}
      IntLog('UdrFunction.execute()');
      IntLog('AInMsg = '+GetHexPointer(AInMsg));
      IntLog('AOutMsg = '+GetHexPointer(AOutMsg));
    {$ENDIF}
  {$ENDIF}
  FUdrContext.SetContext(AStatus, AInMsg, AOutMsg);
  FParametersChecked:= FParametersChecked or CheckParameters;
  if FParametersChecked then Exec;
end;

{$IFDEF DEBUG_LOG}
procedure TOvsUdrFunction.IntLog(const S: AnsiString);
begin
  LogObjStr(FLog,S);
end;
{$ENDIF}

{ TOvsUdrProcedureFactory }

constructor TOvsUdrProcedureFactory.Create(ProcClass: TOvsUdrProcedureClass);
begin
  inherited create;
  FProcClass:=ProcClass;
  {$IFDEF DEBUG_LOG}
    LogStr('UdrProcedureFactory.Create('+ProcClass.ClassName+') - '+GetHexPointer(Self));
  {$ENDIF}
end;

procedure TOvsUdrProcedureFactory.dispose();
begin
  {$IFDEF DEBUG_LOG}
    LogStr('UdrProcedureFactory.dispose('+FProcClass.ClassName+') - '+GetHexPointer(Self));
  {$ENDIF}
  Free;
end;

procedure TOvsUdrProcedureFactory.setup(AStatus: IStatus; AContext: IExternalContext; AMetadata: IRoutineMetadata;AInBuilder: IMetadataBuilder; AOutBuilder: IMetadataBuilder);
begin
  {$IFDEF DEBUG_LOG}
    LogStr('UdrProcedureFactory.setup() - '+GetHexPointer(Self));
  {$ENDIF}
end;

function TOvsUdrProcedureFactory.newItem(AStatus: IStatus; AContext: IExternalContext; AMetadata: IRoutineMetadata): IExternalProcedure;
begin
  {$IFDEF DEBUG_LOG}
    LogStr('UdrProcedureFactory.newItem() - '+GetHexPointer(Self));
  {$ENDIF}
  Result := FProcClass.Create(TOvsUdrContext.Create(AMetadata, AStatus, AContext));
end;

{ TOvsUdrProcedure }

function TOvsUdrProcedure.CheckParameters: Boolean;
begin
  Result:=True;
end;

constructor TOvsUdrProcedure.Create(Context:TOvsUdrContext);
{$IFDEF DEBUG_LOG}
{$IFNDEF DEBUG_LOG_NO_DETAILS}
var
  i:SizeInt;
{$ENDIF}
{$ENDIF}
begin
  inherited create;
  {$IFDEF DEBUG_LOG}
    FLog:=InitObjLogFile(GetHexPointer(Self)+'.log');
    IntLog('======= UdrProcedure creation start =======');
  {$ENDIF}
  FUdrContext:=Context;
  FParametersChecked:=False;
  {$IFDEF DEBUG_LOG}
    {$IFNDEF DEBUG_LOG_NO_DETAILS}
      //
      IntLog('OvsUdrProcedure.Create() - Context:');
      IntLog('  DatabaseName: '+Context.FContext.getDatabaseName());
      IntLog('  UserName: '+Context.FContext.getUserName());
      IntLog('  ClientCharSet: '+Context.FContext.getClientCharSet());

      IntLog('OvsUdrProcedure.Create() - Metadata:');
      IntLog('  Name: '+Context.CallerName);
      IntLog('  Package: '+Context.FMetadata.getPackage(Context.Status));
      IntLog('  EntryPoint: '+Context.RoutineName);
      IntLog('  Body: '+Context.Body);

      LogObjInt(Log, '  InputParamCount: ', Context.InParamsCount);
      for i:=0 to Context.InParamsCount-1 do begin
        IntLog('    Param: '+Context.InputMetadata.getField(Context.Status,i));
        IntLog('      Type: '+fb_type2string(Context.InputMetadata.getType(Context.Status,i)));
        LogObjInt(Log, '      isNullable: ', Byte(Context.InputMetadata.isNullable(Context.Status,i)));
        LogObjInt(Log, '      Length: ', Context.InputMetadata.getLength(Context.Status,i));
        IntLog('      CharSet: '+fb_get_charset_name(Context.InputMetadata.getCharSet(Context.Status,i)));
        LogObjInt(Log, '      Offset: ', Context.InputMetadata.getOffset(Context.Status,i));
        LogObjInt(Log, '      NullOffset: ', Context.InputMetadata.getNullOffset(Context.Status,i));
      end;
      LogObjInt(Log, '  OutputParamCount: ', Context.OutParamsCount);
      for i:=0 to Context.OutParamsCount-1 do begin
        IntLog('    Param: '+Context.OutputMetadata.getField(Context.Status,i));
        IntLog('      Type: '+fb_type2string(Context.OutputMetadata.getType(Context.Status,i)));
        LogObjInt(Log, '      isNullable: ', Byte(Context.OutputMetadata.isNullable(Context.Status,i)));
        LogObjInt(Log, '      Length: ', Context.OutputMetadata.getLength(Context.Status,i));
        IntLog('      CharSet: '+fb_get_charset_name(Context.OutputMetadata.getCharSet(Context.Status,i)));
        LogObjInt(Log, '      Offset: ', Context.OutputMetadata.getOffset(Context.Status,i));
        LogObjInt(Log, '      NullOffset: ', Context.OutputMetadata.getNullOffset(Context.Status,i));
      end;
    {$ENDIF}
  {$ENDIF}
end;

destructor TOvsUdrProcedure.Destroy;
begin
  FUdrContext.Free;
  {$IFDEF DEBUG_LOG}
    IntLog('======= Free and close this Log file =======');
    FreeObj(FLog);
  {$ENDIF}
  inherited Destroy;
end;

function TOvsUdrProcedure.Exec: IExternalResultSet;
begin
  Result:=nil;
  {$IFDEF DEBUG_LOG}
    {$IFNDEF DEBUG_LOG_NO_DETAILS}
      IntLog('UdrProcedure.Exec()');
    {$ENDIF}
  {$ENDIF}
end;

procedure TOvsUdrProcedure.dispose();
begin
  {$IFDEF DEBUG_LOG}
    IntLog('UdrProcedure.dispose()');
  {$ENDIF}
  Free;
end;

procedure TOvsUdrProcedure.getCharSet(AStatus: IStatus; AContext: IExternalContext; AName: PAnsiChar; ANameSize: Cardinal);
begin
  {$IFDEF DEBUG_LOG}
    {$IFNDEF DEBUG_LOG_NO_DETAILS}
      LogObjInt(FLog, 'UdrProcedure.getCharSet: Name: '+AName+', NameSize: ', ANameSize);
    {$ENDIF}
  {$ENDIF}
end;

function TOvsUdrProcedure.open(AStatus: IStatus; AContext: IExternalContext; AInMsg: Pointer; AOutMsg: Pointer): IExternalResultSet;
begin
  FUdrContext.SetContext(AStatus, AInMsg, AOutMsg);
  FParametersChecked:= FParametersChecked or CheckParameters;
  if FParametersChecked then Result:=Exec else Result:=nil;
end;

{$IFDEF DEBUG_LOG}
procedure TOvsUdrProcedure.IntLog(const S: AnsiString);
begin
  LogObjStr(FLog,S);
end;
{$ENDIF}

{ TOvsUdrProcResultSet }

procedure TOvsUdrProcResultSet.dispose();
begin
  {$IFDEF DEBUG_LOG}
    IntLog('UdrProcResultSet.dispose()');
  {$ENDIF}
  Free;
end;

function TOvsUdrProcResultSet.fetch(AStatus: IStatus): Boolean;
begin
  Result:=False;
end;

{$IFDEF DEBUG_LOG}
procedure TOvsUdrProcResultSet.IntLog(const S: AnsiString);
begin
  LogObjStr(FLog,S);
end;
{$ENDIF}

{ TOvsUdrSelectiveProcedure

function TOvsUdrSelectiveProcedure.open(AStatus: IStatus; AContext: IExternalContext; AInMsg: Pointer; AOutMsg: Pointer): IExternalResultSet;
begin
  FUdrContext.SetContext(AStatus, AInMsg, AOutMsg);
  FParametersChecked:= FParametersChecked or CheckParameters;
  //if FParametersChecked then Exec;
  Result:=GetResultSetClass.Create;
  {$IFDEF DEBUG_LOG}
{    IntLog('UdrSelectiveProcedure.open: Handle to ResultSet = '+GetHexPointer(Result));
  {$ENDIF}{
end;
}
{Это хак, нарытый в недрах Firebird ))) - вызов Firebird-Exception без системного Exception}
procedure OVSoft_UDR_Exception(AStatus: IStatus; msg: AnsiString);
var
  statusVector: array[0..4] of NativeIntPtr;
begin
  statusVector[0] := NativeIntPtr(isc_arg_gds);
  statusVector[1] := NativeIntPtr(isc_random);
  statusVector[2] := NativeIntPtr(isc_arg_string);
  statusVector[3] := NativeIntPtr(PAnsiChar(msg));
  statusVector[4] := NativeIntPtr(isc_arg_end);

  Astatus.setErrors(@statusVector);
end;

end.

