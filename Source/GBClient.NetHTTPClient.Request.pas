unit GBClient.NetHTTPClient.Request;

interface

uses
  Data.DB,
  GBClient.Interfaces,
  GBClient.Types,
  GBClient.Helpers,
  GBClient.NetHTTPClient.Response,
  GBClient.NetHTTPClient.Exceptions,
  GBClient.Base.Request.ParamPath,
  GBClient.Base.Request.ParamHeader,
  GBClient.Base.Request.ParamQuery,
  GBClient.NetHTTPClient.Request.Auth,
  GBClient.Settings.Default,
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.NetEncoding,
  System.Generics.Collections,
  System.Net.URLClient,
  System.Net.HttpClient,
  System.Net.HttpClientComponent;

type TGBClientNetHttpClientRequest = class(TInterfacedObject, IGBClientRequest,
                                                              IGBClientBodyRequest)

  private
    FClient     : TNetHTTPClient;
    FOnException: TGBOnException;
    FRequest    : TNetHTTPRequest;
    FResponse   : IGBClientResponse;
    FAuth       : IGBClientAuth;
    FBodyStream : TStream;
    FMethodType : TGBMethodType;
    FBaseUrl    : string;
    FResource   : string;

    FParamPath: IGBClientParamPath;
    FHeader   : IGBClientParamHeader;
    FQuery    : IGBClientParamQuery;
    FSettings : IGBClientSettings;

    function GetFullUrl: string;

    procedure PrepareRequest;
    procedure PrepareRequestPaths;
    procedure PrepareRequestHeaders;
    procedure PrepareRequestQuery;
    procedure PrepareRequestBody;

    procedure ClearRequest;
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

    function AcceptCharset  (Value: string): IGBClientRequest;
    function AcceptEncoding (Value: string): IGBClientRequest;
    function Accept         (Value: string): IGBClientRequest;
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

uses
  REST.Json;

{ TGBClientNetHttpClientRequest }

function TGBClientNetHttpClientRequest.&End: IGBClientRequest;
begin
  result := Self;
end;

function TGBClientNetHttpClientRequest.AddOrSet(Value: String): IGBClientBodyRequest;
begin
  FreeAndNil(FBodyStream);
  Result := Self;
  FBodyStream := TStringStream.Create(Value);
  FRequest.SourceStream := FBodyStream;
end;

function TGBClientNetHttpClientRequest.AddOrSet(Value: TJSONObject; AOwner: Boolean): IGBClientBodyRequest;
begin
  result := Self;
  ContentType(TGBContentType.ctApplicationJson);

  AddOrSet(Value.ToString);
  if AOwner then
    Value.Free;
end;

function TGBClientNetHttpClientRequest.AddOrSet(Value: TJSONArray; AOwner: Boolean): IGBClientBodyRequest;
begin
  result := Self;
  ContentType(TGBContentType.ctApplicationJson);

  AddOrSet(Value.ToString);
  if AOwner then
    Value.Free;
end;

function TGBClientNetHttpClientRequest.AddOrSet(Value: TObject; AOwner: Boolean): IGBClientBodyRequest;
begin
  result := Self;
  AddOrSet(TJson.ObjectToJsonString(Value));
end;

function TGBClientNetHttpClientRequest.Authorization: IGBClientAuth;
begin
  if not Assigned(FAuth) then
    FAuth := TGBClientNetHTTPClientAuth.New(Self, FRequest);
  result := FAuth;
end;

function TGBClientNetHttpClientRequest.BaseURL(Value: String): IGBClientRequest;
begin
  result   := Self;
  FBaseUrl := Value;
end;

function TGBClientNetHttpClientRequest.Body: IGBClientBodyRequest;
begin
  result := Self;
end;

procedure TGBClientNetHttpClientRequest.ClearRequest;
begin
  if Assigned(FHeader) then
    TGBClientBaseRequestParamHeader(FHeader).Clear;

  if Assigned(FQuery) then
    TGBClientBaseRequestParamQuery(FQuery).Clear;

  if Assigned(FParamPath) then
    TGBClientBaseRequestParamPath(FParamPath).Clear;
end;

function TGBClientNetHttpClientRequest.ContentType(Value: String): IGBClientRequest;
begin
  result := Self;
  FClient.ContentType := Value;
end;

function TGBClientNetHttpClientRequest.ContentType(Value: TGBContentType): IGBClientRequest;
begin
  result := ContentType( Value.value );
end;

constructor TGBClientNetHttpClientRequest.create;
begin
  FClient := TNetHTTPClient.Create(nil);
  FRequest := TNetHTTPRequest.Create(nil);
  FRequest.Client := FClient;
  FMethodType := gmtGET;
end;

function TGBClientNetHttpClientRequest.DELETE: IGBClientRequest;
begin
  result := Self;
  FMethodType := gmtDELETE;
end;

destructor TGBClientNetHttpClientRequest.Destroy;
begin
  FRequest.Free;
  FClient.Free;
  FBodyStream.Free;
  inherited;
end;

function TGBClientNetHttpClientRequest.Execute: IGBClientResponse;
begin
  result := Send;
end;

function TGBClientNetHttpClientRequest.GET: IGBClientRequest;
begin
  result := Self;
  FRequest.MethodString := 'GET';
end;

function TGBClientNetHttpClientRequest.GetFullUrl: string;
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

function TGBClientNetHttpClientRequest.Header: IGBClientParamHeader;
begin
  if not Assigned(FHeader) then
    FHeader := TGBClientBaseRequestParamHeader.New(Self);
  result := FHeader;
end;

class function TGBClientNetHttpClientRequest.New: IGBClientRequest;
begin
  result := Self.create;
end;

function TGBClientNetHttpClientRequest.OnException(Value: TGBOnException): IGBClientRequest;
begin
  result := Self;
  FOnException := Value;
end;

function TGBClientNetHttpClientRequest.OnPreExecute(Value: TOnPreExecute): IGBClientRequest;
begin
  result := Self;
end;

function TGBClientNetHttpClientRequest.ParamPath: IGBClientParamPath;
begin
  if not Assigned(FParamPath) then
    FParamPath := TGBClientBaseRequestParamPath.New(Self);
  Result := FParamPath;
end;

function TGBClientNetHttpClientRequest.PATCH: IGBClientRequest;
begin
  result := Self;
  FMethodType := gmtPATCH;
end;

function TGBClientNetHttpClientRequest.POST: IGBClientRequest;
begin
  result := Self;
  FMethodType := gmtPOST;
end;

procedure TGBClientNetHttpClientRequest.PrepareRequest;
begin
  FRequest.MethodString := FMethodType.value;
  PrepareRequestPaths;
  PrepareRequestHeaders;
  PrepareRequestQuery;
  PrepareRequestBody;
end;

procedure TGBClientNetHttpClientRequest.PrepareRequestBody;
begin
end;

procedure TGBClientNetHttpClientRequest.PrepareRequestHeaders;
var
  headers: TStrings;
  i: Integer;
begin
  if not Assigned(FHeader) then
    exit;

  headers := TGBClientBaseRequestParamHeader(FHeader).Params;
  for i := 0 to Pred(headers.Count) do
    FRequest.CustomHeaders[headers.Names[i]] := headers.ValueFromIndex[i];
end;

procedure TGBClientNetHttpClientRequest.PrepareRequestPaths;
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

  FRequest.URL := url;
end;

procedure TGBClientNetHttpClientRequest.PrepareRequestQuery;
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
    FRequest.URL := FRequest.URL + '?' + queryParam;
end;

function TGBClientNetHttpClientRequest.PUT: IGBClientRequest;
begin
  result := Self;
  FMethodType := gmtPUT;
end;

function TGBClientNetHttpClientRequest.Query: IGBClientParamQuery;
begin
  if not Assigned(FQuery) then
    FQuery := TGBClientBaseRequestParamQuery.New(Self);
  result := FQuery;
end;

function TGBClientNetHttpClientRequest.Resource(Value: String): IGBClientRequest;
begin
  result := Self;
  FResource := Value;
end;

function TGBClientNetHttpClientRequest.Response: IGBClientResponse;
begin
  result := FResponse;
end;

function TGBClientNetHttpClientRequest.Send: IGBClientResponse;
var
  response: IHTTPResponse;
  LException: EGBNetHTTPClientException;
begin
  FResponse := nil;
  try
    PrepareRequest;
    response := FRequest.Execute;

    FResponse := TGBClientNetHTTPClientResponse.New(Self, response);
    if FResponse.StatusCode >= 400 then
    begin
      LException := EGBNetHTTPClientException.create(response);
      if Assigned(FOnException) then
        FOnException(LException);
      raise LException;
    end;
    result := FResponse
  finally
    ClearRequest;
  end;
end;

function TGBClientNetHttpClientRequest.Settings: IGBClientSettings;
begin
  if not Assigned(FSettings) then
    FSettings := TGBClientSettingsDefault.New(Self);
  Result := FSettings;
end;

function TGBClientNetHttpClientRequest.TimeOut(Value: Integer): IGBClientRequest;
begin
  result := Self;
  FClient.ConnectionTimeout := Value;
end;

function TGBClientNetHttpClientRequest.Accept(Value: string): IGBClientRequest;
begin
  Result := Self;
  FClient.Accept := Value;
  FRequest.Accept := Value;
end;

function TGBClientNetHttpClientRequest.AcceptCharset(Value: string): IGBClientRequest;
begin
  result := Self;
  FClient.AcceptCharSet := Value;
  FRequest.AcceptCharSet := Value;
end;

function TGBClientNetHttpClientRequest.AcceptEncoding(Value: string): IGBClientRequest;
begin
  result := Self;
  FClient.AcceptEncoding := Value;
  FRequest.AcceptEncoding := Value;
end;

function TGBClientNetHttpClientRequest.AddOrSet(Value: TDataSet; ACurrent: Boolean): IGBClientBodyRequest;
begin
  result := Self;
  ContentType(TGBContentType.ctApplicationJson);
  if ACurrent then
    AddOrSet(Value.ToJSONObject, True)
  else
    AddOrSet(Value.ToJSONArray, True);
end;

function TGBClientNetHttpClientRequest.AddOrSet(Value: TList<TObject>; AOwner: Boolean): IGBClientBodyRequest;
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

end.
