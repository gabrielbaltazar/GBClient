unit GBClient.IdHTTP.Request;

interface

uses
  Data.DB,
  GBClient.Interfaces,
  GBClient.Helpers,
  GBClient.Types,
  GBClient.IdHTTP.Request.Auth,
  GBClient.Base.Request.ParamPath,
  GBClient.Base.Request.ParamHeader,
  GBClient.Base.Request.ParamQuery,
  GBClient.Settings.Default,
  GBClient.IdHTTP.Exceptions,
  GBClient.IdHTTP.Response,
  IdBaseComponent,
  IdComponent,
  IdTCPConnection,
  IdTCPClient,
  IdHTTP,
  IdIOHandler,
  IdIOHandlerSocket,
  IdIOHandlerStack,
  IdSSL,
  IdSSLOpenSSL,
  REST.Json,
  System.JSON,
  System.Generics.Collections,
  System.NetEncoding,
  System.SysUtils,
  System.Classes;

type TGBClientIdHTTPRequest = class(TInterfacedObject, IGBClientRequest,
                                                       IGBClientBodyRequest)

  private
    FIdHTTP         : TIdHTTP;
    FHandler        : TIdSSLIOHandlerSocketOpenSSL;
    FBodyStream     : TStringStream;
    FResponseStream : TStringStream;
    FOnException    : TGBOnException;
    FResponse       : IGBClientResponse;
    FAuth           : IGBClientAuth;
    FMethodType     : TGBMethodType;
    FBaseUrl        : string;
    FResource       : string;

    FParamPath: IGBClientParamPath;
    FHeader   : IGBClientParamHeader;
    FQuery    : IGBClientParamQuery;
    FSettings : IGBClientSettings;

    function GetFullUrl: string;

    procedure ClearRequest;
    procedure PrepareRequest;
    procedure PrepareRequestPaths;
    procedure PrepareRequestHeaders;
    procedure PrepareRequestQuery;
    procedure PrepareRequestBody;
  protected
    function POST  : IGBClientRequest;
    function PUT   : IGBClientRequest;
    function GET   : IGBClientRequest;
    function DELETE: IGBClientRequest;
    function PATCH : IGBClientRequest;

    function Authorization: IGBClientAuth;

    function Header    : IGBClientParamHeader;
    function ParamPath : IGBClientParamPath;
    function Query     : IGBClientParamQuery;
    function Body      : IGBClientBodyRequest;

    function Accept         (Value: string): IGBClientRequest;
    function AcceptCharset  (Value: string): IGBClientRequest;
    function AcceptEncoding (Value: string): IGBClientRequest;
    function ContentType    (Value: TGBContentType): IGBClientRequest; overload;
    function ContentType    (Value: String)        : IGBClientRequest; overload;
    function BaseURL        (Value : String)       : IGBClientRequest;
    function Resource       (Value : String)       : IGBClientRequest;
    function TimeOut        (Value : Integer)      : IGBClientRequest;

    function Execute  : IGBClientResponse;
    function Send     : IGBClientResponse;
    function Response : IGBClientResponse;

    function Settings: IGBClientSettings;

    function OnException (Value: TGBOnException): IGBClientRequest;
    function OnPreExecute(Value: TOnPreExecute): IGBClientRequest;

    // Body
    function AddOrSet(Value : String)                              : IGBClientBodyRequest; overload;
    function AddOrSet(Value : TJSONObject; AOwner: Boolean = False): IGBClientBodyRequest; overload;
    function AddOrSet(Value : TJSONArray;  AOwner: Boolean = False): IGBClientBodyRequest; overload;
    function AddOrSet(Value : TObject;  AOwner: Boolean = False): IGBClientBodyRequest; overload;
    function AddOrSet(Value : TList<TObject>; AOwner: Boolean = False): IGBClientBodyRequest; overload;
    function AddOrSet(Value : TDataSet; ACurrent: Boolean = True): IGBClientBodyRequest; overload;

    function &End: IGBClientRequest;

  public
    class function New: IGBClientRequest;
    constructor create;
    destructor Destroy; override;

end;

implementation

{ TGBClientIdHTTPRequest }

function TGBClientIdHTTPRequest.AddOrSet(Value: TJSONArray; AOwner: Boolean): IGBClientBodyRequest;
begin
  result := Self;
  ContentType(TGBContentType.ctApplicationJson);

  AddOrSet(Value.ToString);
  if AOwner then
    Value.Free;
end;

function TGBClientIdHTTPRequest.AddOrSet(Value: TJSONObject; AOwner: Boolean): IGBClientBodyRequest;
begin
  result := Self;
  ContentType(TGBContentType.ctApplicationJson);

  AddOrSet(Value.ToString);
  if AOwner then
    Value.Free;
end;

function TGBClientIdHTTPRequest.AddOrSet(Value: String): IGBClientBodyRequest;
begin
  FreeAndNil(FBodyStream);
  Result := Self;
  FBodyStream := TStringStream.Create(Value);
end;

function TGBClientIdHTTPRequest.Accept(Value: string): IGBClientRequest;
begin
  result := Self;
  FIdHTTP.Request.Accept := Value;
end;

function TGBClientIdHTTPRequest.AcceptCharset(Value: string): IGBClientRequest;
begin
  result := Self;
  FIdHTTP.Request.AcceptCharSet := Value;
end;

function TGBClientIdHTTPRequest.AcceptEncoding(Value: string): IGBClientRequest;
begin
  result := Self;
  FIdHTTP.Request.AcceptEncoding := Value;
end;

function TGBClientIdHTTPRequest.AddOrSet(Value: TDataSet; ACurrent: Boolean): IGBClientBodyRequest;
begin
  result := Self;
  ContentType(TGBContentType.ctApplicationJson);
  if ACurrent then
    AddOrSet(Value.ToJSONObject, True)
  else
    AddOrSet(Value.ToJSONArray, True);
end;

function TGBClientIdHTTPRequest.AddOrSet(Value: TList<TObject>; AOwner: Boolean): IGBClientBodyRequest;
var
  parse: TGBOnParseObjectToJSON;
  jsonArray: TJSONArray;
  i: Integer;
begin
  parse  := Settings.OnParseObjectToJSON;
  result := Self;

  jsonArray := TJSONArray.Create;
  try
    for i := 0 to Pred(Value.Count) do
      jsonArray.AddElement(parse(Value[i]));

    AddOrSet(jsonArray);
  finally
    jsonArray.Free;
    if AOwner then
      FreeAndNil(Value);
  end;
end;

function TGBClientIdHTTPRequest.AddOrSet(Value: TObject; AOwner: Boolean): IGBClientBodyRequest;
begin
  result := Self;
  AddOrSet(TJson.ObjectToJsonString(Value));
end;

function TGBClientIdHTTPRequest.Authorization: IGBClientAuth;
begin
  if not Assigned(FAuth) then
    FAuth := TGBClientIdHTTPAuth.New(Self, FIdHTTP);
  result := FAuth;
end;

function TGBClientIdHTTPRequest.BaseURL(Value: String): IGBClientRequest;
begin
  result   := Self;
  FBaseUrl := Value;
end;

function TGBClientIdHTTPRequest.Body: IGBClientBodyRequest;
begin
  result := Self;
end;

procedure TGBClientIdHTTPRequest.ClearRequest;
begin
  if Assigned(FHeader) then
    TGBClientBaseRequestParamHeader(FHeader).Clear;

  if Assigned(FQuery) then
    TGBClientBaseRequestParamQuery(FQuery).Clear;

  if Assigned(FParamPath) then
    TGBClientBaseRequestParamPath(FParamPath).Clear;
end;

function TGBClientIdHTTPRequest.ContentType(Value: TGBContentType): IGBClientRequest;
begin
  result := Self;
  ContentType(Value.value);
end;

function TGBClientIdHTTPRequest.ContentType(Value: String): IGBClientRequest;
begin
  result := Self;
  FIdHTTP.Request.ContentType := Value;
end;

constructor TGBClientIdHTTPRequest.create;
begin
  FBodyStream := TStringStream.Create;
  FResponseStream := TStringStream.Create;
  FIdHTTP := TIdHTTP.Create(nil);
  FIdHTTP.Request.ContentType := 'application/json';

  FHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  FHandler.SSLOptions.Method := sslvTLSv1_2;
  FHandler.SSLOptions.Mode := sslmClient;
  FHandler.SSLOptions.SSLVersions := [sslvTLSv1, sslvTLSv1_1, sslvTLSv1_2];

  FIdHTTP.IOHandler := FHandler;
end;

function TGBClientIdHTTPRequest.DELETE: IGBClientRequest;
begin
  result := Self;
  FMethodType := gmtDELETE;
end;

destructor TGBClientIdHTTPRequest.Destroy;
begin
  FResponseStream.Free;
  FBodyStream.Free;
  FHandler.Free;
  FIdHTTP.Free;
  inherited;
end;

function TGBClientIdHTTPRequest.&End: IGBClientRequest;
begin
  result := Self;
end;

function TGBClientIdHTTPRequest.Execute: IGBClientResponse;
begin
  result := Send;
end;

function TGBClientIdHTTPRequest.GET: IGBClientRequest;
begin
  result := Self;
  FMethodType := gmtGET;
end;

function TGBClientIdHTTPRequest.GetFullUrl: string;
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

function TGBClientIdHTTPRequest.Header: IGBClientParamHeader;
begin
  if not Assigned(FHeader) then
    FHeader := TGBClientBaseRequestParamHeader.New(Self);
  result := FHeader;
end;

class function TGBClientIdHTTPRequest.New: IGBClientRequest;
begin
  Result := Self.create;
end;

function TGBClientIdHTTPRequest.OnException(Value: TGBOnException): IGBClientRequest;
begin
  result := Self;
  FOnException := Value;
end;

function TGBClientIdHTTPRequest.OnPreExecute(Value: TOnPreExecute): IGBClientRequest;
begin
  result := Self;
end;

function TGBClientIdHTTPRequest.ParamPath: IGBClientParamPath;
begin
  if not Assigned(FParamPath) then
    FParamPath := TGBClientBaseRequestParamPath.New(Self);
  Result := FParamPath;
end;

function TGBClientIdHTTPRequest.PATCH: IGBClientRequest;
begin
  result := Self;
  FMethodType := gmtPATCH;
end;

function TGBClientIdHTTPRequest.POST: IGBClientRequest;
begin
  result := Self;
  FMethodType := gmtPOST;
end;

procedure TGBClientIdHTTPRequest.PrepareRequest;
begin
  FIdHTTP.Request.Method := FMethodType.value;
  PrepareRequestPaths;
  PrepareRequestHeaders;
  PrepareRequestQuery;
  PrepareRequestBody;
end;

procedure TGBClientIdHTTPRequest.PrepareRequestBody;
begin
end;

procedure TGBClientIdHTTPRequest.PrepareRequestHeaders;
var
  headers: TStrings;
  i: Integer;
begin
  if not Assigned(FHeader) then
    exit;

  headers := TGBClientBaseRequestParamHeader(FHeader).Params;
  for i := 0 to Pred(headers.Count) do
    FIdHTTP.Request.CustomHeaders.Values[headers.Names[i]] := headers.ValueFromIndex[i];
end;

procedure TGBClientIdHTTPRequest.PrepareRequestPaths;
var
  paths: TStrings;
  i : Integer;
  url : string;
begin
  url := GetFullUrl;

  if Assigned(FParamPath) then
  begin
    paths := TGBClientBaseRequestParamPath(FParamPath).Params;

    for i := 0 to Pred(paths.Count) do
      url := url.Replace(Format('{%s}', [paths.Names[i]]), paths.ValueFromIndex[i]);
  end;

  FIdHTTP.Request.URL := url;
end;

procedure TGBClientIdHTTPRequest.PrepareRequestQuery;
var
  queries: TStrings;
  i : Integer;
  queryParam: string;
begin
  queryParam := EmptyStr;
  if not Assigned(FQuery) then
    Exit;

  queries := TGBClientBaseRequestParamQuery(FQuery).Params;

  for i := 0 to Pred(queries.Count) do
  begin
    if i > 0 then
      queryParam := queryParam + '&';
    queryParam := queryParam + queries.Names[i] + '=' + TNetEncoding.URL.EncodeQuery(queries.ValueFromIndex[i]);
  end;

  if not queryParam.IsEmpty then
    FIdHTTP.Request.URL := FIdHTTP.Request.URL + '?' + queryParam;
end;

function TGBClientIdHTTPRequest.PUT: IGBClientRequest;
begin
  result := Self;
  FMethodType := gmtPUT;
end;

function TGBClientIdHTTPRequest.Query: IGBClientParamQuery;
begin
  if not Assigned(FQuery) then
    FQuery := TGBClientBaseRequestParamQuery.New(Self);
  result := FQuery;
end;

function TGBClientIdHTTPRequest.Resource(Value: String): IGBClientRequest;
begin
  result := Self;
  FResource := Value;
end;

function TGBClientIdHTTPRequest.Response: IGBClientResponse;
begin
  result := FResponse;
end;

function TGBClientIdHTTPRequest.Send: IGBClientResponse;
var
  url: string;
  LException: EGBIdHTTPException;
begin
  FResponseStream.Clear;
  PrepareRequest;
  url := FIdHTTP.Request.URL;
  try
    try
      case FMethodType of
        gmtGET    : FIdHTTP.Get(url, FResponseStream);
        gmtPOST   : FIdHTTP.Post(url, FBodyStream, FResponseStream);
        gmtPUT    : FIdHTTP.Put(url, FBodyStream, FResponseStream);
        gmtDELETE : FIdHTTP.Delete(url, FResponseStream);
        gmtPATCH  : FIdHTTP.Patch(url, FResponseStream);
      end;

      FResponse := TGBClientIdHTTPResponse.New(Self, FIdHTTP);
      result := FResponse;

      if FResponse.StatusCode >= 400 then
      begin
        LException := EGBIdHTTPException.create(FIdHTTP);
        if Assigned(FOnException) then
          FOnException(LException);
        raise LException;
      end;
    except
      on e: EGBIdHTTPException do
        raise;
    end;
  finally
    ClearRequest;
  end;
end;

function TGBClientIdHTTPRequest.Settings: IGBClientSettings;
begin
  if not Assigned(FSettings) then
    FSettings := TGBClientSettingsDefault.New(Self);
  Result := FSettings;
end;

function TGBClientIdHTTPRequest.TimeOut(Value: Integer): IGBClientRequest;
begin
  Result := Self;
  FIdHTTP.ConnectTimeout := Value;
end;

end.
