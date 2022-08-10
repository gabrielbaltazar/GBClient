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

type
  TGBClientNetHTTPClient = class(TGBClientCoreRequest, IGBClientRequest,
    IGBClientRequestParams, IGBClientResponse)
  private
    FClient: TNetHTTPClient;
    FRequest: TNetHTTPRequest;
    FResponse: IHTTPResponse;
    FFormData: TMultipartFormData;
    FByteStream: TBytesStream;
    FJSONArray: TJSONArray;
    FJSONObject: TJSONObject;
    FContentType: string;

    procedure OnAWSAuthorization(AAuth, AAmzDate: string);
    procedure CreateComponents;
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

{ TGBClientNetHTTPClient }

function TGBClientNetHTTPClient.Authorization: IGBClientAuth;
begin
  if not Assigned(FAuthorization) then
    FAuthorization := TGBClientNetHTTPClientAuth.New(Self);
  Result := FAuthorization;
end;

function TGBClientNetHTTPClient.Component: TComponent;
begin
  Result := FRequest;
end;

function TGBClientNetHTTPClient.ContentType(AValue: TGBContentType): IGBClientRequest;
begin
  Result := Self;
  inherited ContentType(AValue);
  FContentType := AValue.value;
end;

constructor TGBClientNetHTTPClient.Create;
begin
  inherited;
  FContentType := 'application/json';
end;

procedure TGBClientNetHTTPClient.CreateComponents;
begin
  FreeAndNil(FRequest);
  FreeAndNil(FClient);

  FClient := TNetHTTPClient.Create(nil);
  FRequest := TNetHTTPRequest.Create(nil);
  FRequest.Client := FClient;
end;

function TGBClientNetHTTPClient.DataSet(AValue: TDataSet): IGBClientResponse;
begin
  Result := Self;
  AValue.FromJSON(GetText);
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
    Result := stream.Bytes;
  finally
    stream.Free;
  end;
end;

function TGBClientNetHTTPClient.GetJSONArray: TJSONArray;
begin
  FreeAndNil(FJSONArray);
  FJSONArray := TJSONObject.ParseJSONValue(GetText) as TJSONArray;
  Result := FJSONArray;
end;

function TGBClientNetHTTPClient.GetJSONObject: TJSONObject;
begin
  FreeAndNil(FJSONArray);
  FJSONObject := TJSONObject.ParseJSONValue(GetText) as TJSONObject;
  Result := FJSONObject;
end;

function TGBClientNetHTTPClient.GetList(AValue: TList<TObject>; AType: TClass): IGBClientResponse;
var
  parse     : TGBOnParseJSONToObject;
  jsonArray : TJSONArray;
  LObject   : TObject;
  i         : Integer;
begin
  Result := Self;
  jsonArray := GetJSONArray;

  for i := 0 to Pred(jsonArray.Count) do
  begin
    parse := Settings.OnParseJSONToObject;
    if Assigned(parse) then
    begin
      LObject := AType.Create;
      try
        parse(TJSONObject( jsonArray.Items[i] ), LObject);
        AValue.Add(LObject);
      except
        LObject.Free;
        raise;
      end;
    end;
  end;
end;

function TGBClientNetHTTPClient.GetObject(AValue: TObject): IGBClientResponse;
var
  parse: TGBOnParseJSONToObject;
begin
  Result := Self;
  parse := Settings.OnParseJSONToObject;
  if Assigned(parse) then
    parse(GetJSONObject, AValue);
end;

function TGBClientNetHTTPClient.GetStream: TBytesStream;
begin
  FreeAndNil(FByteStream);
  FByteStream := TBytesStream.Create(GetBytes);
  Result := FByteStream;
end;

function TGBClientNetHTTPClient.GetText: string;
begin
  Result := FResponse.ContentAsString;
end;

function TGBClientNetHTTPClient.HeaderAsDateTime(AName: string): TDateTime;
begin
  Result.fromIso8601ToDateTime(HeaderAsString(AName));
end;

function TGBClientNetHTTPClient.HeaderAsFloat(AName: string): Double;
begin
  Result := StrToFloatDef(HeaderAsString(AName), 0);
end;

function TGBClientNetHTTPClient.HeaderAsInteger(AName: string): Integer;
begin
  Result := StrToIntDef(HeaderAsString(AName), 0);
end;

function TGBClientNetHTTPClient.HeaderAsString(AName: string): string;
begin
  if FResponse.ContainsHeader(AName) then
    Result := FResponse.HeaderValue[AName];
end;

class function TGBClientNetHTTPClient.New: IGBClientRequest;
begin
  Result := Self.Create;
end;

procedure TGBClientNetHTTPClient.OnAWSAuthorization(AAuth, AAmzDate: string);
begin
  FRequest.CustomHeaders['x-amz-date'] := AAmzDate;
  FRequest.CustomHeaders['Authorization'] := AAuth;
end;

procedure TGBClientNetHTTPClient.PrepareRequest;
begin
  FResponse := nil;
  CreateComponents;

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
  name: string;
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
  Result := Self;
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
  Result := FResponse.StatusCode;
end;

function TGBClientNetHTTPClient.StatusText: string;
begin
  Result := FResponse.StatusText;
end;

end.
