unit GBClient.Synapse;

interface

{$IFDEF WEAKPACKAGEUNIT}
  {$WEAKPACKAGEUNIT ON}
{$ENDIF}

uses
  System.Classes,
  System.SysUtils,
  System.JSON,
  System.Generics.Collections,
  System.TypInfo,
  Data.DB,
  blcksock,
  httpsend,
  ssl_openssl,
  synautil,
  syncobjs,
  GBClient.Interfaces,
  GBClient.RestClient.Response,
  GBClient.Core.Request,
  GBClient.Core.Helpers,
  GBClient.Core.Types,
  GBClient.Core.Exceptions,
  GBClient.RestClient.Auth;

type
  TGBClientSynapse = class(TGBClientCoreRequest, IGBClientRequest, IGBClientRequestParams)
  private
    FHTTPSend: THTTPSend;
    FIsUTF8: Boolean;
    FResponse: IGBClientResponse;
    FAccept: string;
    FContentType: string;

    procedure OnAWSAuthorization(const AAuth, AAmzDate: string);

    procedure CreateComponents;

    procedure PrepareRequest;
    procedure PrepareRequestProxy;
    procedure PrepareRequestHeaders;
    procedure PrepareRequestBody;
    procedure PrepareRequestAuth;
  protected
    function Component: TObject; override;
    function Authorization: IGBClientAuth; override;
    function Accept(const AValue: string): IGBClientRequest; override;
    function ContentType(const AValue: TGBContentType): IGBClientRequest; override;
    function GetFullUrl: string; override;

    function Send: IGBClientResponse; override;
    function Response: IGBClientResponse; override;
  public
    constructor Create; override;
    class function New: IGBClientRequest;
    destructor Destroy; override;
  end;

  TGBClientSynapseResponse = class(TInterfacedObject, IGBClientResponse)
  private
    [Weak]
    FParent: IGBClientRequest;
    FStatusCode: Integer;
    FStatusText: string;
    FBodyText: string;
    FJSONObject: TJSONObject;
    FJSONArray: TJSONArray;
    FByteStream: TBytesStream;
    FHeaders: TStrings;
  protected
    function StatusCode: Integer;
    function StatusText: string;

    function GetText: string;
    function GetJSONObject: TJSONObject;
    function GetJSONArray: TJSONArray;
    function DataSet(const AValue: TDataSet): IGBClientResponse;
    function GetObject(const AValue: TObject): IGBClientResponse;
    function GetList(const AValue: TList<TObject>; const AType: TClass): IGBClientResponse;
    function GetBytes: TBytes;
    function GetStream: TBytesStream;

    function HeaderAsString(const AName: string): string;
    function HeaderAsInteger(const AName: string): Integer;
    function HeaderAsFloat(const AName: string): Double;
    function HeaderAsDateTime(const AName: string): TDateTime;

    function &End: IGBClientRequest;
  public
    constructor Create(const AHTTPSend: THTTPSend; const AParent: IGBClientRequest);
    class function New(const AHTTPSend: THTTPSend; const AParent: IGBClientRequest): IGBClientResponse;
    destructor Destroy; override;
  end;

implementation

uses
  GBClient.Synapse.Auth;

{ TGBClientSynapse }

function TGBClientSynapse.Accept(const AValue: string): IGBClientRequest;
begin
  Result := Self;
  inherited;
  FAccept := AValue;
end;

function TGBClientSynapse.Authorization: IGBClientAuth;
begin
  if not Assigned(FAuthorization) then
    FAuthorization := TGBClientSynapseAuth.New(Self);
  Result := FAuthorization;
end;

function TGBClientSynapse.Component: TObject;
begin
  Result := FHTTPSend;
end;

function TGBClientSynapse.ContentType(const AValue: TGBContentType): IGBClientRequest;
begin
  Result := Self;
  inherited;
  FContentType := AValue.Value;
end;

constructor TGBClientSynapse.Create;
begin
  inherited;
  FTimeOut := 120000;
  FIsUTF8 := False;
  Self.ContentType(ctApplicationJson);
end;

procedure TGBClientSynapse.CreateComponents;
begin
  FreeAndNil(FHTTPSend);
  FHTTPSend := THTTPSend.Create;
end;

destructor TGBClientSynapse.Destroy;
begin
  FHTTPSend.Free;
  inherited;
end;

function TGBClientSynapse.GetFullUrl: string;
var
  LCount: Integer;
begin
  Result := inherited;
  if (Result.EndsWith('/')) or (Result.EndsWith('?')) then
    Result := Copy(Result, 1, Result.Length - 1);

  for LCount := 0 to Pred(FQueries.Count) do
  begin
    if Result.Contains('?') then
      Result := Result + '&'
    else
      Result := Result + '?';

    Result := Result + Format('%s=%s',
      [FQueries[LCount].Key, FQueries[LCount].Value]);
  end;
end;

class function TGBClientSynapse.New: IGBClientRequest;
begin
  Result := Self.Create;
end;

procedure TGBClientSynapse.OnAWSAuthorization(const AAuth, AAmzDate: string);
begin
  FHTTPSend.Headers.Values['x-amz-date'] := AAmzDate;
  FHTTPSend.Headers.Values['Authorization'] := AAuth;
end;

procedure TGBClientSynapse.PrepareRequest;
begin
  FResponse := nil;
  CreateComponents;

  FHTTPSend.Clear;
  FHTTPSend.Timeout := FTimeout;
  FHTTPSend.Sock.ConnectionTimeout := FTimeout;
  FHTTPSend.Sock.InterPacketTimeout := False;
  FHTTPSend.Sock.NonblockSendTimeout := FTimeout;
  FHTTPSend.Sock.SocksTimeout := FTimeout;
  FHTTPSend.Sock.HTTPTunnelTimeout := FTimeout;
  FHTTPSend.Sock.SSL.SSLType := LT_all;
  FHTTPSend.AddPortNumberToHost := False;

  PrepareRequestHeaders;
  PrepareRequestProxy;
  PrepareRequestBody;
  PrepareRequestAuth;
end;

procedure TGBClientSynapse.PrepareRequestAuth;
begin
  if Assigned(FAuthorization) then
  begin
    if FAuthorization.AuthType = atAWSv4 then
      FAuthorization.AWSv4.Host(GetFullUrl).HTTPVerb(FMethod.value).Payload(FBody);

    TGBClientSynapseAuth(FAuthorization).ApplyAuth;

    if FAuthorization.AuthType = atAWSv4 then
      OnAWSAuthorization(FAuthorization.AWSv4.Authorization, FAuthorization.AWSv4.XAmzDate);
  end;
end;

procedure TGBClientSynapse.PrepareRequestBody;
var
  LCount: Integer;
  LUrlData: string;
begin
  if FUrlEncodedParams.Count > 0 then
  begin
    for LCount := 0 to Pred(FUrlEncodedParams.Count) do
    begin
      if LCount > 0 then
        LUrlData := LUrlData + '&';
      LUrlData := LUrlData + Format('%s=%s', [FUrlEncodedParams[LCount].Key, FUrlEncodedParams[LCount].Value]);
    end;

    WriteStrToStream(FHTTPSend.Document, AnsiString(LUrlData));
  end
  else
  if (Assigned(FBody)) and (FBody.Size > 0) then
    FHTTPSend.Document.LoadFromStream(FBody)
  else
    FHTTPSend.Headers.Add('Content-Length:0');
end;

procedure TGBClientSynapse.PrepareRequestHeaders;
var
  LCount: Integer;
begin
  if not Assigned(FHeaders) then
    Exit;

  for LCount := 0 to Pred(FHeaders.Count) do
    FHTTPSend.Headers.Add(Format('%s:%s', [FHeaders[LCount].Key, FHeaders[LCount].Value]));

  if FContentType <> EmptyStr then
    FHTTPSend.MimeType := FContentType;

  if FAccept <> EmptyStr then
    FHTTPSend.Headers.Add(Format('Accept:%s', [FAccept]));
end;

procedure TGBClientSynapse.PrepareRequestProxy;
begin
  FHTTPSend.ProxyHost := FProxyServer;
  FHTTPSend.ProxyUser := FProxyUsername;
  FHTTPSend.ProxyPass := FProxyPassword;
  if FProxyPort > 0 then
    FHTTPSend.ProxyPort := FProxyPort.ToString;
end;

function TGBClientSynapse.Response: IGBClientResponse;
begin
  Result := FResponse;
end;

function TGBClientSynapse.Send: IGBClientResponse;
var
  LException: EGBRestException;
  LUrl: string;
begin
  LUrl := GetFullUrl;
  PrepareRequest;
  try
    FHTTPSend.Protocol := '1.1';
    FHTTPSend.HTTPMethod(FMethod.Value, LUrl);
    FResponse := TGBClientSynapseResponse.New(FHTTPSend, Self);
    Result := FResponse;
    if FResponse.StatusCode >= 400 then
    begin
      LException := EGBRestException.Create(FResponse.StatusCode, FResponse.StatusText, FResponse.GetText,
        FResponse.GetJSONObject);
      if Assigned(FOnException) then
        FOnException(LException);
      raise LException;
    end;
  finally
    Clear;
  end;
end;

{ TGBClientSynapseResponse }

function TGBClientSynapseResponse.&End: IGBClientRequest;
begin
  Result := FParent;
end;

constructor TGBClientSynapseResponse.Create(const AHTTPSend: THTTPSend; const AParent: IGBClientRequest);
begin
  FParent := AParent;
  FStatusText := AHTTPSend.ResultString;
  FStatusCode := AHTTPSend.ResultCode;
  FHeaders := TStringList.Create;
  FHeaders.Text := AHTTPSend.Headers.Text;

  FByteStream := TBytesStream.Create;
  try
    FByteStream.LoadFromStream(AHTTPSend.Document);
  except
    on E: Exception do
    begin
      FreeAndNil(FByteStream);
      E.Message := 'Error on read HTTPSend.Document: ' + E.Message;
      raise;
    end;
  end;
end;

function TGBClientSynapseResponse.DataSet(const AValue: TDataSet): IGBClientResponse;
begin
  Result := Self;
  AValue.FromJSON(GetText);
end;

destructor TGBClientSynapseResponse.Destroy;
begin
  FHeaders.Free;
  FByteStream.Free;
  FJSONObject.Free;
  FJSONArray.Free;
  inherited;
end;

function TGBClientSynapseResponse.GetBytes: TBytes;
var
  LStream: TBytesStream;
begin
  LStream := TBytesStream.Create;
  try
    LStream.LoadFromStream(Self.GetStream);
    Result := LStream.Bytes;
  finally
    LStream.Free;
  end;
end;

function TGBClientSynapseResponse.GetJSONArray: TJSONArray;
begin
  if Assigned(FJSONArray) then
    Exit(FJSONArray);
  FJSONArray := TJSONObject.ParseJSONValue(GetText) as TJSONArray;
  Result := FJSONArray;
end;

function TGBClientSynapseResponse.GetJSONObject: TJSONObject;
begin
  if Assigned(FJSONObject) then
    Exit(FJSONObject);
  FJSONObject := TJSONObject.ParseJSONValue(GetText) as TJSONObject;
  Result := FJSONObject;
end;

function TGBClientSynapseResponse.GetList(const AValue: TList<TObject>; const AType: TClass): IGBClientResponse;
var
  LParse: TGBOnParseJSONToObject;
  LJsonArray: TJSONArray;
  LObject: TObject;
  LCount: Integer;
begin
  Result := Self;
  LJsonArray := GetJSONArray;
  for LCount := 0 to Pred(LJsonArray.Count) do
  begin
    LParse := FParent.Settings.OnParseJSONToObject;
    if Assigned(LParse) then
    begin
      LObject := AType.Create;
      try
        LParse(TJSONObject(LJsonArray.Items[LCount]), LObject);
        AValue.Add(LObject);
      except
        LObject.Free;
        raise;
      end;
    end;
  end;
end;

function TGBClientSynapseResponse.GetObject(const AValue: TObject): IGBClientResponse;
var
  LParse: TGBOnParseJSONToObject;
begin
  Result := Self;
  LParse := FParent.Settings.OnParseJSONToObject;
  if Assigned(LParse) then
    LParse(GetJSONObject, AValue);
end;

function TGBClientSynapseResponse.GetStream: TBytesStream;
begin
  Result := FByteStream;
end;

function TGBClientSynapseResponse.GetText: string;
var
  LStringStream: TStringStream;
begin
  if FBodyText <> EmptyStr then
    Exit(FBodyText);

  LStringStream := TStringStream.Create(EmptyStr, TEncoding.UTF8);
  try
    LStringStream.LoadFromStream(FByteStream);
    LStringStream.Position := 0;
    FBodyText := LStringStream.DataString;
    Result := FBodyText;
  finally
    LStringStream.Free;
  end;
end;

function TGBClientSynapseResponse.HeaderAsDateTime(const AName: string): TDateTime;
begin
  Result.FromIso8601ToDateTime(HeaderAsString(AName));
end;

function TGBClientSynapseResponse.HeaderAsFloat(const AName: string): Double;
begin
  Result := StrToFloatDef(HeaderAsString(AName), 0);
end;

function TGBClientSynapseResponse.HeaderAsInteger(const AName: string): Integer;
begin
  Result := StrToIntDef(HeaderAsString(AName), 0);
end;

function TGBClientSynapseResponse.HeaderAsString(const AName: string): string;
begin
  Result := FHeaders.Values[AName];
end;

class function TGBClientSynapseResponse.New(const AHTTPSend: THTTPSend; const AParent: IGBClientRequest): IGBClientResponse;
begin
  Result := Self.Create(AHTTPSend, AParent);
end;

function TGBClientSynapseResponse.StatusCode: Integer;
begin
  Result := FStatusCode;
end;

function TGBClientSynapseResponse.StatusText: string;
begin
  Result := FStatusText;
end;

end.
