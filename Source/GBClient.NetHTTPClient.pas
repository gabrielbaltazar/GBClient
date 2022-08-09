unit GBClient.NetHTTPClient;

interface

{$IFDEF WEAKPACKAGEUNIT}
	{$WEAKPACKAGEUNIT ON}
{$ENDIF}

uses
  Data.DB,
  GBClient.Interfaces,
  GBClient.Core.Request,
  GBClient.Core.Types,
  GBClient.Core.Helpers,
  GBClient.Core.Settings,
  GBClient.Core.Exceptions,
  GBClient.NetHTTPClient.Auth,
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.NetEncoding,
  System.Net.Mime,
  System.Generics.Collections,
  System.Net.URLClient,
  System.Net.HttpClient,
  System.Net.HttpClientComponent;

type TGBClientNetHTTPClient = class(TGBClientCoreRequest, IGBClientRequest,
                                                          IGBClientRequestParams,
                                                          IGBClientResponse)
  private
    FClient : TNetHTTPClient;
    FRequest : TNetHTTPRequest;
    FResponse : IHTTPResponse;
    FFormData : TMultipartFormData;
    FByteStream : TBytesStream;
    FJSONArray: TJSONArray;
    FJSONObject: TJSONObject;
    FContentType: string;

    procedure OnAWSAuthorization(Auth, AmzDate: string);

    procedure createComponents;

    procedure PrepareRequest;
    procedure PrepareRequestProxy;
    procedure PrepareRequestHeaders;
    procedure PrepareRequestQueries;
    procedure PrepareRequestPathParams;
    procedure PrepareRequestBody;
    procedure PrepareRequestAuth;

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

{ TGBClientNetHTTPClient }

function TGBClientNetHTTPClient.Authorization: IGBClientAuth;
begin
  if not Assigned(FAuthorization) then
    FAuthorization := TGBClientNetHTTPClientAuth.New(Self);
  result := FAuthorization;
end;

function TGBClientNetHTTPClient.Component: TComponent;
begin
  result := FRequest;
end;

function TGBClientNetHTTPClient.ContentType(Value: TGBContentType): IGBClientRequest;
begin
  result := Self;
  inherited ContentType(Value);
  FContentType := Value.value;
end;

constructor TGBClientNetHTTPClient.Create;
begin
  inherited;
  FContentType := 'application/json';
end;

procedure TGBClientNetHTTPClient.createComponents;
begin
  FreeAndNil(FRequest);
  FreeAndNil(FClient);

  FClient := TNetHTTPClient.Create(nil);
  FRequest := TNetHTTPRequest.Create(nil);
  FRequest.Client := FClient;
end;

function TGBClientNetHTTPClient.DataSet(Value: TDataSet): IGBClientResponse;
begin
  result := Self;
  Value.FromJSON(GetText);
end;

destructor TGBClientNetHTTPClient.Destroy;
begin
  FreeAndNil(FRequest);
  FreeAndNil(FClient);
  FreeAndNil(FByteStream);
  FreeAndNil(FJSONArray);
  FreeAndNil(FJSONObject);
  FreeAndNil(FFormData);
  inherited;
end;

function TGBClientNetHTTPClient.GetBytes: TBytes;
var
  stream: TBytesStream;
begin
  stream := TBytesStream.Create;
  try
    stream.LoadFromStream(FResponse.ContentStream);
    result := stream.Bytes;
  finally
    stream.Free;
  end;
end;

function TGBClientNetHTTPClient.GetJSONArray: TJSONArray;
begin
  FreeAndNil(FJSONArray);
  FJSONArray := TJSONObject.ParseJSONValue(GetText) as TJSONArray;
  result := FJSONArray;
end;

function TGBClientNetHTTPClient.GetJSONObject: TJSONObject;
begin
  FreeAndNil(FJSONArray);
  FJSONObject := TJSONObject.ParseJSONValue(GetText) as TJSONObject;
  result := FJSONObject;
end;

function TGBClientNetHTTPClient.GetList(Value: TList<TObject>; AType: TClass): IGBClientResponse;
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

function TGBClientNetHTTPClient.GetObject(Value: TObject): IGBClientResponse;
var
  parse: TGBOnParseJSONToObject;
begin
  result := Self;
  parse := Settings.OnParseJSONToObject;
  if Assigned(parse) then
    parse(GetJSONObject, Value);
end;

function TGBClientNetHTTPClient.GetStream: TBytesStream;
begin
  FreeAndNil(FByteStream);
  FByteStream := TBytesStream.Create(GetBytes);
  Result := FByteStream;
end;

function TGBClientNetHTTPClient.GetText: string;
begin
  result := FResponse.ContentAsString;
end;

function TGBClientNetHTTPClient.HeaderAsDateTime(Name: String): TDateTime;
begin
  result.fromIso8601ToDateTime(HeaderAsString(Name));
end;

function TGBClientNetHTTPClient.HeaderAsFloat(Name: String): Double;
begin
  result := StrToFloatDef(HeaderAsString(Name), 0);
end;

function TGBClientNetHTTPClient.HeaderAsInteger(Name: String): Integer;
begin
  result := StrToIntDef(HeaderAsString(Name), 0);
end;

function TGBClientNetHTTPClient.HeaderAsString(Name: String): string;
begin
  if FResponse.ContainsHeader(Name) then
    result := FResponse.HeaderValue[Name];
end;

class function TGBClientNetHTTPClient.New: IGBClientRequest;
begin
  result := Self.Create;
end;

procedure TGBClientNetHTTPClient.OnAWSAuthorization(Auth, AmzDate: string);
begin
  FRequest.CustomHeaders['x-amz-date'] := AmzDate;
  FRequest.CustomHeaders['Authorization'] := Auth;
end;

procedure TGBClientNetHTTPClient.PrepareRequest;
begin
  FResponse := nil;
  createComponents;

  FRequest.MethodString := FMethod.value;

  PrepareRequestProxy;
  PrepareRequestPathParams;
  PrepareRequestHeaders;
  PrepareRequestQueries;
  PrepareRequestBody;
  PrepareRequestAuth;
end;

procedure TGBClientNetHTTPClient.PrepareRequestAuth;
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

    TGBClientNetHTTPClientAuth(FAuthorization).ApplyAuth;

    if FAuthorization.AuthType = atAWSv4 then
    begin
      OnAWSAuthorization(FAuthorization.AWSv4.Authorization,
                         FAuthorization.AWSv4.XAmzDate);
    end;  end;
end;

procedure TGBClientNetHTTPClient.PrepareRequestBody;
var
  i: Integer;
begin
  if FUrlEncodedParams.Count > 0 then
  begin
    FreeAndNil(FFormData);
    FFormData := TMultipartFormData.Create(True);
    for i := 0 to Pred(FUrlEncodedParams.Count) do
      FFormData.AddField(FUrlEncodedParams[i].Key, FUrlEncodedParams[i].Value);

    FRequest.ContentStream := FFormData.Stream;
  end
  else
    FRequest.SourceStream := FBody;
end;

procedure TGBClientNetHTTPClient.PrepareRequestHeaders;
var
  i: Integer;
begin
  FClient.ContentType := FContentType;
  for i := 0 to Pred(FHeaders.Count) do
    FRequest.CustomHeaders[FHeaders[i].Key] := FHeaders[i].Value;
end;

procedure TGBClientNetHTTPClient.PrepareRequestPathParams;
var
  i : Integer;
  url : string;
  name: String;
  value: string;
begin
  url := GetFullUrl;

  for i := 0 to Pred(FPaths.Count) do
  begin
    name := FPaths[i].Key;
    value:= FPaths[i].Value;

    url := url.Replace(Format('{%s}', [name]), value);
  end;

  FRequest.URL := url;
end;

procedure TGBClientNetHTTPClient.PrepareRequestProxy;
begin
end;

procedure TGBClientNetHTTPClient.PrepareRequestQueries;
var
  i : Integer;
  queryParam: string;
  name: string;
  value: string;
begin
  queryParam := EmptyStr;

  for i := 0 to Pred(FQueries.Count) do
  begin
    name := FQueries[i].Key;
    value:= FQueries[i].Value;

    if i > 0 then
      queryParam := queryParam + '&';
    queryParam := queryParam + name + '=' + TNetEncoding.URL.EncodeQuery(value);
  end;

  if not queryParam.IsEmpty then
    FRequest.URL := FRequest.URL + '?' + queryParam;
end;

function TGBClientNetHTTPClient.Response: IGBClientResponse;
begin
  result := Self;
end;

function TGBClientNetHTTPClient.Send: IGBClientResponse;
var
  LException: EGBRestException;
begin
  FResponse := nil;
  Result := Self;
  try
    PrepareRequest;
    FResponse := FRequest.Execute;

    if FResponse.StatusCode >= 400 then
    begin
      LException := EGBRestException.Create(StatusCode, StatusText, GetText, GetJSONObject);
      if Assigned(FOnException) then
        FOnException(LException);
      raise LException;
    end;
  finally
    Clear;
  end;
end;

function TGBClientNetHTTPClient.StatusCode: Integer;
begin
  result := FResponse.StatusCode;
end;

function TGBClientNetHTTPClient.StatusText: string;
begin
  result := FResponse.StatusText;
end;

end.
