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
  GBClient.Synapse.Util,
  GBClient.Synapse.Compress,
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
    FContentBody: string;

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

{ TGBClientSynapse }

function TGBClientSynapse.Accept(const AValue: string): IGBClientRequest;
begin
  Result := Self;
  inherited;
  FAccept := AValue;
end;

function TGBClientSynapse.Authorization: IGBClientAuth;
begin

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

class function TGBClientSynapse.New: IGBClientRequest;
begin
  Result := Self.Create;
end;

procedure TGBClientSynapse.OnAWSAuthorization(const AAuth, AAmzDate: string);
begin

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
  PrepareRequestBody;
  PrepareRequestAuth;
end;

procedure TGBClientSynapse.PrepareRequestAuth;
begin

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

    WriteStrToStream(FHTTPSend.Document, LUrlData);
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
  if LUrl.EndsWith('/') then
    LUrl := Copy(LUrl, 1, LUrl.Length - 1);
  PrepareRequest;
  try
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

  function UnzipDoc: string;
  var
    LCT: string;
    LResp: AnsiString;
    LRespIsUTF8: Boolean;
    LZT: TCompressType;
  begin
    LZT := DetectCompressType(AHTTPSend.Document);
    if LZT = ctUnknown then
    begin
      AHTTPSend.Document.Position := 0;
      LResp := ReadStrFromStream(AHTTPSend.Document, AHTTPSend.Document.Size);
    end
    else
      LResp := DeCompress(AHTTPSend.Document);

    {$IF CompilerVersion <= 34.0}
      Exit(UTF8ToNativeString(LResp));
    {$ENDIF}
    LCT := LowerCase(HeaderAsString('Content-Type'));
    LRespIsUTF8 := (Pos('utf-8', LCT) > 0);
    if LRespIsUTF8 then
      Result := UTF8ToNativeString(LResp)
    else
      Result := string(LResp);
  end;
begin
  FParent := AParent;
  FStatusText := AHTTPSend.ResultString;
  FStatusCode := AHTTPSend.ResultCode;
  FHeaders := TStringList.Create;
  FHeaders.Text := AHTTPSend.Headers.Text;
  AHTTPSend.Document.SaveToFile('teste.txt');
  try
    FBodyText := UnzipDoc;
  except
    AHTTPSend.Document.Position := 0;
    FBodyText := ReadStrFromStream(AHTTPSend.Document, AHTTPSend.Document.Size);
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
  FreeAndNil(FJSONArray);
  FJSONArray := TJSONObject.ParseJSONValue(GetText) as TJSONArray;
  Result := FJSONArray;
end;

function TGBClientSynapseResponse.GetJSONObject: TJSONObject;
begin
  FreeAndNil(FJSONObject);
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
  FreeAndNil(FByteStream);
  FByteStream := TBytesStream.Create(GetBytes);
  Result := FByteStream;
end;

function TGBClientSynapseResponse.GetText: string;
begin
  Result := FBodyText;
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
