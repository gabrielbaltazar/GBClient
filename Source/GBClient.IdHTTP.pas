unit GBClient.IdHTTP;

interface

{$IFDEF WEAKPACKAGEUNIT}
  {$WEAKPACKAGEUNIT ON}
{$ENDIF}

uses
  Data.DB,
  GBClient.Interfaces,
  GBClient.Core.Request,
  GBClient.Core.Helpers,
  GBClient.Core.Types,
  GBClient.Core.Exceptions,
  GBClient.IdHTTP.Auth,
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
  System.Classes,
  System.NetEncoding,
  System.SysUtils,
  System.JSON,
  System.Generics.Collections,
  System.TypInfo;

type
  TGBClientIdHTTP = class(TGBClientCoreRequest, IGBClientRequest,
    IGBClientRequestParams, IGBClientResponse)
  private
    FIdHTTP: TIdHTTP;
    FHandler: TIdSSLIOHandlerSocketOpenSSL;
    FResponseStream: TStringStream;
    FBodyForm: TIdMultiPartFormDataStream;
    FContentType: string;
    FJSONArray: TJSONArray;
    FJSONObject: TJSONObject;
    FBytes: TBytesStream;

    procedure OnAWSAuthorization(AAuth, AAmzDate: string);
    procedure CreateComponents;
    procedure PrepareRequest;
    procedure PrepareRequestProxy;
    procedure PrepareRequestHeaders;
    procedure PrepareRequestQueries;
    procedure PrepareRequestPathParams;
    procedure PrepareRequestBody;
    procedure PrepareRequestFormData;
    procedure PrepareRequestAuth;

    function GetFullUrl: string;
  protected
    function Component: TComponent; override;
    function Authorization: IGBClientAuth; override;

    function ContentType(AValue: TGBContentType): IGBClientRequest; override;

    function Send: IGBClientResponse; override;
    function Response: IGBClientResponse; override;

    // Response
    function StatusCode: Integer;
    function StatusText: string;
    function GetText: string;
    function GetJSONObject: TJSONObject;
    function GetJSONArray: TJSONArray;
    function DataSet(AValue: TDataSet): IGBClientResponse;
    function GetObject(AValue: TObject): IGBClientResponse;
    function GetList(AValue: TList<TObject>; AType: TClass): IGBClientResponse;
    function GetBytes: TBytes;
    function GetStream: TBytesStream;
    function HeaderAsString(AName: string): string;
    function HeaderAsInteger(AName: string): Integer;
    function HeaderAsFloat(AName: string): Double;
    function HeaderAsDateTime(AName: string): TDateTime;
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

function TGBClientIdHTTP.Component: TComponent;
begin
  Result := FIdHTTP;
end;

function TGBClientIdHTTP.ContentType(AValue: TGBContentType): IGBClientRequest;
begin
  Result := Self;
  inherited ContentType(AValue);
  FContentType := AValue.value;
end;

constructor TGBClientIdHTTP.Create;
begin
  inherited;
  FResponseStream := TStringStream.Create;
  FBodyForm := TIdMultiPartFormDataStream.Create;
end;

procedure TGBClientIdHTTP.CreateComponents;
begin
  FreeAndNil(FIdHTTP);
  FreeAndNil(FHandler);

  FIdHTTP := TIdHTTP.Create(nil);
  FIdHTTP.Request.ContentType := FContentType;
  FIdHTTP.ConnectTimeout := FTimeOut;
  FIdHTTP.HTTPOptions := [hoNoProtocolErrorException, hoWantProtocolErrorContent];

  FHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  FHandler.SSLOptions.Method := sslvTLSv1_2;
  FHandler.SSLOptions.Mode := sslmClient;
  FHandler.SSLOptions.SSLVersions := [sslvTLSv1, sslvTLSv1_1, sslvTLSv1_2];

  FIdHTTP.IOHandler := FHandler;
end;

function TGBClientIdHTTP.DataSet(AValue: TDataSet): IGBClientResponse;
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

function TGBClientIdHTTP.GetList(AValue: TList<TObject>; AType: TClass): IGBClientResponse;
var
  LParse: TGBOnParseJSONToObject;
  LJsonArray: TJSONArray;
  LObject: TObject;
  I: Integer;
begin
  Result := Self;
  LJsonArray := GetJSONArray;

  for I := 0 to Pred(LJsonArray.Count) do
  begin
    LParse := Settings.OnParseJSONToObject;
    if Assigned(LParse) then
    begin
      LObject := AType.Create;
      try
        LParse(TJSONObject( LJsonArray.Items[I] ), LObject);
        AValue.Add(LObject);
      except
        LObject.Free;
        raise;
      end;
    end;
  end;
end;

function TGBClientIdHTTP.GetObject(AValue: TObject): IGBClientResponse;
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
  LStringStream: TStringStream;
begin
  LStringStream := TStringStream.Create;
  try
    LStringStream.LoadFromStream(FIdHTTP.Response.ContentStream);
    Result := LStringStream.DataString;
    Result := UTF8ToString( RawByteString( Result ));
  finally
    LStringStream.Free;
  end;
end;

function TGBClientIdHTTP.HeaderAsDateTime(AName: string): TDateTime;
begin
  Result.fromIso8601ToDateTime(HeaderAsString(AName));
end;

function TGBClientIdHTTP.HeaderAsFloat(AName: string): Double;
begin
  Result := StrToFloatDef(HeaderAsString(AName), 0);
end;

function TGBClientIdHTTP.HeaderAsInteger(AName: string): Integer;
begin
  Result := StrToIntDef(HeaderAsString(AName), 0);
end;

function TGBClientIdHTTP.HeaderAsString(AName: string): string;
begin
  Result := FIdHTTP.Response.CustomHeaders.Values[AName];
end;

class function TGBClientIdHTTP.New: IGBClientRequest;
begin
  Result := Self.Create;
end;

procedure TGBClientIdHTTP.OnAWSAuthorization(AAuth, AAmzDate: string);
begin
  FIdHTTP.Request.CustomHeaders.Values['x-amz-date'] := AAmzDate;
  FIdHTTP.Request.CustomHeaders.Values['Authorization'] := AAuth;
end;

procedure TGBClientIdHTTP.PrepareRequest;
begin
  CreateComponents;
  FIdHTTP.Request.Method := Self.FMethod.value;

  PrepareRequestProxy;
  PrepareRequestPathParams;
  PrepareRequestHeaders;
  PrepareRequestQueries;
  PrepareRequestBody;
  PrepareRequestFormData;
  PrepareRequestAuth;
end;

procedure TGBClientIdHTTP.PrepareRequestAuth;
begin
  if Assigned(FAuthorization) then
  begin
    if FAuthorization.AuthType = atAWSv4 then
    begin
      FAuthorization.AWSv4
        .Host(GetFullUrl)
        .HTTPVerb(FMethod.value)
        .Payload(FBody);
    end;

    TGBClientIdHTTPAuth(FAuthorization).ApplyAuth;

    if FAuthorization.AuthType = atAWSv4 then
    begin
      OnAWSAuthorization(FAuthorization.AWSv4.Authorization,
                         FAuthorization.AWSv4.XAmzDate);
    end;
  end;
end;

procedure TGBClientIdHTTP.PrepareRequestBody;
var
  I: Integer;
  LName: string;
  LValue: string;
begin
  FBodyForm.Clear;
  for I := 0 to Pred(FUrlEncodedParams.Count) do
  begin
    LName := FUrlEncodedParams[I].Key;
    LValue:= FUrlEncodedParams[I].Value;
    FBodyForm.AddFormField(LName, LValue);
  end;
end;

procedure TGBClientIdHTTP.PrepareRequestFormData;
var
  I: Integer;
  LName: string;
  LValue: string;
begin
  FBodyForm.Clear;
  for I := 0 to Pred(FFormData.Count) do
  begin
    LName := FFormData[I].Key;
    LValue := FFormData[I].Value;
    FBodyForm.AddFormField(LName, LValue);
  end;

  for LName in FFormDataStream.Keys do
    FBodyForm.AddFormField(LName, '', '', FFormDataStream.Items[LName]);
end;

procedure TGBClientIdHTTP.PrepareRequestHeaders;
var
  I: Integer;
  LName: string;
  LValue: string;
begin
  for I := 0 to Pred(FHeaders.Count) do
  begin
    LName := FHeaders[I].Key;
    LValue:= FHeaders[I].Value;
    FIdHTTP.Request.CustomHeaders.Values[LName] := LValue;
  end;
end;

procedure TGBClientIdHTTP.PrepareRequestPathParams;
var
  I: Integer;
  LUrl : string;
begin
  LUrl := GetFullUrl;
  if Assigned(FPaths) then
  begin
    for I := 0 to Pred(FPaths.Count) do
      LUrl := LUrl.Replace(Format('{%s}', [FPaths[I].Key]), FPaths[I].Value);
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
  I: Integer;
  LQueryParam: string;
begin
  LQueryParam := EmptyStr;
  for I := 0 to Pred(FQueries.Count) do
  begin
    if I > 0 then
      LQueryParam := LQueryParam + '&';
    LQueryParam := LQueryParam + FQueries[I].Key + '=' + TNetEncoding.URL.EncodeQuery(FQueries[I].Value);
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
        gmtGET: FIdHTTP.Get(LUrl, FResponseStream);
        gmtDELETE: FIdHTTP.Delete(LUrl, FResponseStream);
        gmtPATCH: FIdHTTP.Patch(LUrl, FResponseStream);
        gmtPOST:
          begin
            if FBodyForm.Size > 0 then
              FIdHTTP.Post(LUrl, FBodyForm, FResponseStream)
            else
              FIdHTTP.Post(LUrl, FBody, FResponseStream);
          end;

        gmtPUT:
          begin
            if FBodyForm.Size > 0 then
              FIdHTTP.Put(LUrl, FBodyForm, FResponseStream)
            else
              FIdHTTP.Put(LUrl, FBody, FResponseStream);
          end;
      end;

      Result := Self;

      if StatusCode >= 400 then
        raise EIdHTTPProtocolException.CreateFmt('%s: %s', [StatusCode.ToString, StatusText]);
    except
      on E: EIdHTTPProtocolException do
      begin
        LException := EGBRestException.Create(StatusCode, StatusText, GetText, GetJSONObject);
        if Assigned(FOnException) then
          FOnException(LException);
        raise LException;
      end;

      on E: EGBRestException do
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
