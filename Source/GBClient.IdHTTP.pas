unit GBClient.IdHTTP;

interface

{$IFDEF WEAKPACKAGEUNIT}
  {$WEAKPACKAGEUNIT ON}
{$ENDIF}

uses
  System.Classes,
  System.NetEncoding,
  System.SysUtils,
  System.JSON,
  System.Generics.Collections,
  System.TypInfo,  Data.DB,
  IdBaseComponent,
  IdMultipartFormData,
  IdComponent,
  IdTCPConnection,
  IdTCPClient,
  IdHTTP,
  IdIOHandler,
  IdIOHandlerSocket,
  IdIOHandlerStack,
  IdSSL,
  IdSSLOpenSSL,
  GBClient.Interfaces,
  GBClient.Core.Request,
  GBClient.Core.Helpers,
  GBClient.Core.Types,
  GBClient.Core.Exceptions,
  GBClient.IdHTTP.Auth;

type
  TGBClientIdHTTP = class(TGBClientCoreRequest, IGBClientRequest, IGBClientRequestParams, IGBClientResponse)
  private
    FIdHTTP: TIdHTTP;
    FHandler: TIdSSLIOHandlerSocketOpenSSL;
    FResponseStream: TStringStream;
    FBodyForm: TIdMultiPartFormDataStream;
    FContentType: string;
    FJSONArray: TJSONArray;
    FJSONObject: TJSONObject;
    FBytes: TBytesStream;

    procedure OnAWSAuthorization(const AAuth, AAmzDate: string);

    procedure CreateComponents;

    procedure PrepareRequest;
    procedure PrepareRequestProxy;
    procedure PrepareRequestHeaders;
    procedure PrepareRequestQueries;
    procedure PrepareRequestPathParams;
    procedure PrepareRequestBody;
    procedure PrepareRequestAuth;

    function GetFullUrl: string;
  protected
    function Component: TObject; override;
    function Authorization: IGBClientAuth; override;

    function ContentType(const AValue: TGBContentType): IGBClientRequest; override;

    function Send: IGBClientResponse; override;
    function Response: IGBClientResponse; override;

    // Response
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
  public
    constructor Create; override;
    class function New: IGBClientRequest;
    destructor Destroy; override;
  end;

implementation

{ TGBClientIdHTTP }

function TGBClientIdHTTP.Authorization: IGBClientAuth;
begin
  if not Assigned(FAuthorization) then
    FAuthorization := TGBClientIdHTTPAuth.New(Self);
  Result := FAuthorization;
end;

function TGBClientIdHTTP.Component: TObject;
begin
  Result := FIdHTTP;
end;

function TGBClientIdHTTP.ContentType(const AValue: TGBContentType): IGBClientRequest;
begin
  Result := Self;
  inherited ContentType(AValue);
  FContentType := AValue.Value;
end;

constructor TGBClientIdHTTP.Create;
begin
  inherited;
  FResponseStream := TStringStream.Create;
  FBodyForm:= TIdMultiPartFormDataStream.Create;
end;

procedure TGBClientIdHTTP.CreateComponents;
begin
  FreeAndNil(FIdHTTP);
  FreeAndNil(FHandler);

  FIdHTTP := TIdHTTP.Create(nil);
  FIdHTTP.Request.ContentType := FContentType;
  FIdHTTP.ConnectTimeout := FTimeOut;

  FHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  FHandler.SSLOptions.Method := sslvTLSv1_2;
  FHandler.SSLOptions.Mode := sslmClient;
  FHandler.SSLOptions.SSLVersions := [sslvTLSv1, sslvTLSv1_1, sslvTLSv1_2];

  FIdHTTP.IOHandler := FHandler;
end;

function TGBClientIdHTTP.DataSet(const AValue: TDataSet): IGBClientResponse;
begin
  Result := Self;
  AValue.FromJSON(GetText);
end;

destructor TGBClientIdHTTP.Destroy;
begin
  FreeAndNil(FResponseStream);
  FreeAndNil(FBodyForm);
  FreeAndNil(FHandler);
  FreeAndNil(FIdHTTP);
  FreeAndNil(FJSONArray);
  FreeAndNil(FJSONObject);
  FreeAndNil(FBytes);
  inherited;
end;

function TGBClientIdHTTP.GetBytes: TBytes;
var
  LStream: TBytesStream;
begin
  LStream := TBytesStream.Create;
  try
    LStream.LoadFromStream(FIdHTTP.Response.ContentStream);
    Result := LStream.Bytes;
  finally
    LStream.Free;
  end;
end;

function TGBClientIdHTTP.GetFullUrl: string;
var
  LResource: string;
begin
  Result := FBaseUrl;
  if not FBaseUrl.EndsWith('/') then
    Result := Result + '/';

  LResource := FResource;
  if LResource.StartsWith('/') then
    LResource := Copy(LResource, 2, LResource.Length - 1);

  Result := Result + LResource;
end;

function TGBClientIdHTTP.GetJSONArray: TJSONArray;
begin
  FreeAndNil(FJSONArray);
  FJSONArray := TJSONObject.ParseJSONValue(GetText) as TJSONArray;
  Result := FJSONArray;
end;

function TGBClientIdHTTP.GetJSONObject: TJSONObject;
begin
  FreeAndNil(FJSONObject);
  FJSONObject := TJSONObject.ParseJSONValue(GetText) as TJSONObject;
  Result := FJSONObject;
end;

function TGBClientIdHTTP.GetList(const AValue: TList<TObject>; const AType: TClass): IGBClientResponse;
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
    LParse := Settings.OnParseJSONToObject;
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

function TGBClientIdHTTP.GetObject(const AValue: TObject): IGBClientResponse;
var
  LParse: TGBOnParseJSONToObject;
begin
  Result := Self;
  LParse := Settings.OnParseJSONToObject;
  if Assigned(LParse) then
    LParse(GetJSONObject, AValue);
end;

function TGBClientIdHTTP.GetStream: TBytesStream;
begin
  FreeAndNil(FBytes);
  FBytes := TBytesStream.Create(GetBytes);
  Result := FBytes;
end;

function TGBClientIdHTTP.GetText: string;
var
  LStream: TStringStream;
begin
  LStream := TStringStream.Create;
  try
    LStream.LoadFromStream(FIdHTTP.Response.ContentStream);
    Result := LStream.DataString;
    Result := UTF8ToString(RawByteString(Result));
  finally
    LStream.Free;
  end;
end;

function TGBClientIdHTTP.HeaderAsDateTime(const AName: string): TDateTime;
begin
  Result.FromIso8601ToDateTime(HeaderAsString(AName));
end;

function TGBClientIdHTTP.HeaderAsFloat(const AName: string): Double;
begin
  Result := StrToFloatDef(HeaderAsString(AName), 0);
end;

function TGBClientIdHTTP.HeaderAsInteger(const AName: string): Integer;
begin
  Result := StrToIntDef(HeaderAsString(AName), 0);
end;

function TGBClientIdHTTP.HeaderAsString(const AName: string): string;
begin
  Result := FIdHTTP.Response.CustomHeaders.Values[AName];
end;

class function TGBClientIdHTTP.New: IGBClientRequest;
begin
  Result := Self.Create;
end;

procedure TGBClientIdHTTP.OnAWSAuthorization(const AAuth, AAmzDate: string);
begin
  FIdHTTP.Request.CustomHeaders.Values['x-amz-date'] := AAmzDate;
  FIdHTTP.Request.CustomHeaders.Values['Authorization'] := AAuth;
end;

procedure TGBClientIdHTTP.PrepareRequest;
begin
  CreateComponents;
  FIdHTTP.Request.Method := Self.FMethod.Value;
  PrepareRequestProxy;
  PrepareRequestPathParams;
  PrepareRequestHeaders;
  PrepareRequestQueries;
  PrepareRequestBody;
  PrepareRequestAuth;
end;

procedure TGBClientIdHTTP.PrepareRequestAuth;
begin
  if Assigned(FAuthorization) then
  begin
    if FAuthorization.AuthType = atAWSv4 then
      FAuthorization.AWSv4
        .Host(GetFullUrl)
        .HTTPVerb(FMethod.Value)
        .Payload(FBody);

    TGBClientIdHTTPAuth(FAuthorization).ApplyAuth;

    if FAuthorization.AuthType = atAWSv4 then
      OnAWSAuthorization(FAuthorization.AWSv4.Authorization, FAuthorization.AWSv4.XAmzDate);
  end;
end;

procedure TGBClientIdHTTP.PrepareRequestBody;
var
  LCount: Integer;
  LName: string;
  LValue: string;
begin
  FBodyForm.Clear;
  for LCount := 0 to Pred(FUrlEncodedParams.Count) do
  begin
    LName := FUrlEncodedParams[LCount].Key;
    LValue := FUrlEncodedParams[LCount].Value;
    FBodyForm.AddFormField(LName, LValue);
  end;
end;

procedure TGBClientIdHTTP.PrepareRequestHeaders;
var
  LCount: Integer;
  LName: string;
  LValue: string;
begin
  for LCount := 0 to Pred(FHeaders.Count) do
  begin
    LName := FHeaders[LCount].Key;
    LValue := FHeaders[LCount].Value;
    FIdHTTP.Request.CustomHeaders.Values[LName] := LValue;
  end;
end;

procedure TGBClientIdHTTP.PrepareRequestPathParams;
var
  LCount: Integer;
  LUrl: string;
begin
  LUrl := GetFullUrl;
  if Assigned(FPaths) then
  begin
    for LCount := 0 to Pred(FPaths.Count) do
      LUrl := LUrl.Replace(Format('{%s}', [FPaths[LCount].Key]), FPaths[LCount].Value);
  end;

  FIdHTTP.Request.URL := LUrl;
end;

procedure TGBClientIdHTTP.PrepareRequestProxy;
begin
  FIdHTTP.ProxyParams.ProxyServer := FProxyServer;
  FIdHTTP.ProxyParams.ProxyPort := FProxyPort;
  FIdHTTP.ProxyParams.ProxyUsername := FProxyUsername;
  FIdHTTP.ProxyParams.ProxyPassword := FProxyPassword;;
end;

procedure TGBClientIdHTTP.PrepareRequestQueries;
var
  LCount: Integer;
  LQueryParam: string;
begin
  LQueryParam := EmptyStr;

  for LCount := 0 to Pred(FQueries.Count) do
  begin
    if LCount > 0 then
      LQueryParam := LQueryParam + '&';
    LQueryParam := LQueryParam + FQueries[LCount].Key + '=' + TNetEncoding.URL.EncodeQuery(FQueries[LCount].Value);
  end;

  if not LQueryParam.IsEmpty then
    FIdHTTP.Request.URL := FIdHTTP.Request.URL + '?' + LQueryParam;
end;

function TGBClientIdHTTP.Response: IGBClientResponse;
begin
  Result := Self;
end;

function TGBClientIdHTTP.Send: IGBClientResponse;
var
  LUrl: string;
  LException: EGBRestException;
begin
  FResponseStream.Clear;
  PrepareRequest;
  LUrl := FIdHTTP.Request.URL;
  try
    try
      case FMethod of
        gmtGET:
          FIdHTTP.Get(LUrl, FResponseStream);
        gmtDELETE:
          FIdHTTP.Delete(LUrl, FResponseStream);
        gmtPATCH:
          FIdHTTP.Patch(LUrl, FResponseStream);

        gmtPOST: begin
          if FBodyForm.Size > 0 then
            FIdHTTP.Post(LUrl, FBodyForm, FResponseStream)
          else
            FIdHTTP.Post(LUrl, FBody, FResponseStream);
        end;

        gmtPUT: begin
          if FBodyForm.Size > 0 then
            FIdHTTP.Put(LUrl, FBodyForm, FResponseStream)
          else
            FIdHTTP.Put(LUrl, FBody, FResponseStream);
        end;
      end;

      Result := Self;

      if StatusCode >= 400 then
      begin
        LException := EGBRestException.create(StatusCode, StatusText, GetText, GetJSONObject);
        if Assigned(FOnException) then
          FOnException(LException);
        raise LException;
      end;
    except
      on e: EGBRestException do
        raise;
    end;
  finally
    Clear;;
  end;
end;

function TGBClientIdHTTP.StatusCode: Integer;
begin
  Result := FIdHTTP.ResponseCode;
end;

function TGBClientIdHTTP.StatusText: string;
begin
  Result := FIdHTTP.ResponseText;
end;

end.
