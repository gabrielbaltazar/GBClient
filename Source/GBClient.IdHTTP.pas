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

type TGBClientIdHTTP = class(TGBClientCoreRequest, IGBClientRequest,
                                                   IGBClientRequestParams,
                                                   IGBClientResponse)
  private
    FIdHTTP: TIdHTTP;
    FHandler: TIdSSLIOHandlerSocketOpenSSL;
    FResponseStream: TStringStream;
    FBodyForm: TIdMultiPartFormDataStream;
    FContentType: string;

    FJSONArray: TJSONArray;
    FJSONObject: TJSONObject;
    FBytes: TBytesStream;

    procedure OnAWSAuthorization(Auth, AmzDate: string);

    procedure createComponents;

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

    function ContentType(Value: TGBContentType): IGBClientRequest; override;

    function Send: IGBClientResponse; override;
    function Response : IGBClientResponse; override;

    // Response
    function StatusCode: Integer;
    function StatusText: string;

    function GetText: string;
    function GetJSONObject: TJSONObject;
    function GetJSONArray: TJSONArray;
    function DataSet(Value: TDataSet): IGBClientResponse;
    function GetObject(Value: TObject): IGBClientResponse;
    function GetList(Value: TList<TObject>; AType: TClass): IGBClientResponse;
    function GetBytes: TBytes;
    function GetStream: TBytesStream;

    function HeaderAsString(Name: String): string;
    function HeaderAsInteger(Name: String): Integer;
    function HeaderAsFloat(Name: String): Double;
    function HeaderAsDateTime(Name: String): TDateTime;

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
  result := FIdHTTP;
end;

function TGBClientIdHTTP.ContentType(Value: TGBContentType): IGBClientRequest;
begin
  result := Self;
  inherited ContentType(Value);

  FContentType := Value.value;
end;

constructor TGBClientIdHTTP.Create;
begin
  inherited;
  FResponseStream := TStringStream.Create;
  FBodyForm:= TIdMultiPartFormDataStream.Create;
end;

procedure TGBClientIdHTTP.createComponents;
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

function TGBClientIdHTTP.DataSet(Value: TDataSet): IGBClientResponse;
begin
  result := Self;
  Value.FromJSON(GetText);
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
  stream: TBytesStream;
begin
  stream := TBytesStream.Create;
  try
    stream.LoadFromStream(FIdHTTP.Response.ContentStream);
    result := stream.Bytes;
  finally
    stream.Free;
  end;
end;

function TGBClientIdHTTP.GetFullUrl: string;
var
  resource: string;
begin
  result := FBaseUrl;
  if not FBaseUrl.EndsWith('/') then
    Result := result + '/';

  resource := FResource;
  if resource.StartsWith('/') then
    resource := Copy(resource, 2, resource.Length - 1);

  result := result + resource;
end;

function TGBClientIdHTTP.GetJSONArray: TJSONArray;
begin
  FreeAndNil(FJSONArray);
  FJSONArray := TJSONObject.ParseJSONValue(GetText) as TJSONArray;
  result := FJSONArray;
end;

function TGBClientIdHTTP.GetJSONObject: TJSONObject;
begin
  FreeAndNil(FJSONObject);
  FJSONObject := TJSONObject.ParseJSONValue(GetText) as TJSONObject;
  result := FJSONObject;
end;

function TGBClientIdHTTP.GetList(Value: TList<TObject>; AType: TClass): IGBClientResponse;
var
  parse     : TGBOnParseJSONToObject;
  jsonArray : TJSONArray;
  LObject   : TObject;
  i         : Integer;
begin
  result := Self;
  jsonArray := GetJSONArray;

  for i := 0 to Pred(jsonArray.Count) do
  begin
    parse := Settings.OnParseJSONToObject;
    if Assigned(parse) then
    begin
      LObject := AType.Create;
      try
        parse(TJSONObject( jsonArray.Items[i] ), LObject);
        Value.Add(LObject);
      except
        LObject.Free;
        raise;
      end;
    end;
  end;
end;

function TGBClientIdHTTP.GetObject(Value: TObject): IGBClientResponse;
var
  parse: TGBOnParseJSONToObject;
begin
  result := Self;
  parse := Settings.OnParseJSONToObject;
  if Assigned(parse) then
    parse(GetJSONObject, Value);
end;

function TGBClientIdHTTP.GetStream: TBytesStream;
begin
  FreeAndNil(FBytes);
  FBytes := TBytesStream.Create(GetBytes);
  result := FBytes;
end;

function TGBClientIdHTTP.GetText: string;
var
  stringStream: TStringStream;
begin
  stringStream := TStringStream.Create;
  try
    stringStream.LoadFromStream(FIdHTTP.Response.ContentStream);
    result := stringStream.DataString;
    Result := UTF8ToString( RawByteString( result ));
  finally
    stringStream.Free;
  end;
end;

function TGBClientIdHTTP.HeaderAsDateTime(Name: String): TDateTime;
begin
  result.fromIso8601ToDateTime(HeaderAsString(Name));
end;

function TGBClientIdHTTP.HeaderAsFloat(Name: String): Double;
begin
  result := StrToFloatDef(HeaderAsString(Name), 0);
end;

function TGBClientIdHTTP.HeaderAsInteger(Name: String): Integer;
begin
  result := StrToIntDef(HeaderAsString(Name), 0);
end;

function TGBClientIdHTTP.HeaderAsString(Name: String): string;
begin
  result := FIdHTTP.Response.CustomHeaders.Values[Name];
end;

class function TGBClientIdHTTP.New: IGBClientRequest;
begin
  result := Self.Create;
end;

procedure TGBClientIdHTTP.OnAWSAuthorization(Auth, AmzDate: string);
begin
  FIdHTTP.Request.CustomHeaders.Values['x-amz-date'] := AmzDate;
  FIdHTTP.Request.CustomHeaders.Values['Authorization'] := Auth;
end;

procedure TGBClientIdHTTP.PrepareRequest;
begin
  createComponents;
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
  i: Integer;
  name: String;
  value: String;
begin
  FBodyForm.Clear;
  for i := 0 to Pred(FUrlEncodedParams.Count) do
  begin
    name := FUrlEncodedParams[i].Key;
    value:= FUrlEncodedParams[i].Value;

    FBodyForm.AddFormField(name, value);
  end;
end;

procedure TGBClientIdHTTP.PrepareRequestFormData;
var
  i: Integer;
  LName: String;
  LValue: String;
begin
  FBodyForm.Clear;
  for i := 0 to Pred(FFormData.Count) do
  begin
    LName := FFormData[i].Key;
    LValue := FFormData[i].Value;

    FBodyForm.AddFormField(LName, LValue);
  end;

  for LName in FFormDataStream.Keys do
    FBodyForm.AddFormField(LName, '', '', FFormDataStream.Items[LName]);
end;

procedure TGBClientIdHTTP.PrepareRequestHeaders;
var
  i: Integer;
  name: string;
  value: string;
begin
  for i := 0 to Pred(FHeaders.Count) do
  begin
    name := FHeaders[i].Key;
    value:= FHeaders[i].Value;

    FIdHTTP.Request.CustomHeaders.Values[name] := value;
  end;
end;

procedure TGBClientIdHTTP.PrepareRequestPathParams;
var
  i : Integer;
  url : string;
begin
  url := GetFullUrl;

  if Assigned(FPaths) then
  begin
    for i := 0 to Pred(FPaths.Count) do
      url := url.Replace(Format('{%s}', [FPaths[i].Key]), FPaths[i].Value);
  end;

  FIdHTTP.Request.URL := url;
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
  i : Integer;
  queryParam: string;
begin
  queryParam := EmptyStr;

  for i := 0 to Pred(FQueries.Count) do
  begin
    if i > 0 then
      queryParam := queryParam + '&';
    queryParam := queryParam + FQueries[i].Key + '=' + TNetEncoding.URL.EncodeQuery(FQueries[i].Value);
  end;

  if not queryParam.IsEmpty then
    FIdHTTP.Request.URL := FIdHTTP.Request.URL + '?' + queryParam;
end;

function TGBClientIdHTTP.Response: IGBClientResponse;
begin
  result := Self;
end;

function TGBClientIdHTTP.Send: IGBClientResponse;
var
  url: string;
  LException: EGBRestException;
begin
  FResponseStream.Clear;
  PrepareRequest;
  url := FIdHTTP.Request.URL;
  try
    try
      case FMethod of
        gmtGET    : FIdHTTP.Get(url, FResponseStream);
        gmtDELETE : FIdHTTP.Delete(url, FResponseStream);
        gmtPATCH  : FIdHTTP.Patch(url, FResponseStream);

        gmtPOST: begin
          if FBodyForm.Size > 0 then
            FIdHTTP.Post(url, FBodyForm, FResponseStream)
          else
            FIdHTTP.Post(url, FBody, FResponseStream);
        end;

        gmtPUT: begin
          if FBodyForm.Size > 0 then
            FIdHTTP.Put(url, FBodyForm, FResponseStream)
          else
            FIdHTTP.Put(url, FBody, FResponseStream);
        end;
      end;

      result := Self;

      if StatusCode >= 400 then
        raise EIdHTTPProtocolException.CreateFmt('%s: %s', [StatusCode.ToString, StatusText]);
    except
      on e: EIdHTTPProtocolException do
      begin
        LException := EGBRestException.Create(StatusCode, StatusText, GetText, GetJSONObject);
        if Assigned(FOnException) then
          FOnException(LException);
        raise LException;
      end;

      on e: EGBRestException do
        raise;
    end;
  finally
    Clear;;
  end;
end;

function TGBClientIdHTTP.StatusCode: Integer;
begin
  result := FIdHTTP.ResponseCode;
end;

function TGBClientIdHTTP.StatusText: string;
begin
  result := FIdHTTP.ResponseText;
end;

end.
