unit GBClient.NetHTTPClient;

interface

{$IFDEF WEAKPACKAGEUNIT}
  {$WEAKPACKAGEUNIT ON}
{$ENDIF}

uses
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.NetEncoding,
  System.Net.Mime,
  System.Generics.Collections,
  System.Net.URLClient,
  System.Net.HttpClient,
  System.Net.HttpClientComponent,
  Data.DB,
  GBClient.Interfaces,
  GBClient.Core.Request,
  GBClient.Core.Types,
  GBClient.Core.Helpers,
  GBClient.Core.Settings,
  GBClient.Core.Exceptions,
  GBClient.NetHTTPClient.Auth;

type
  TGBClientNetHTTPClient = class(TGBClientCoreRequest, IGBClientRequest, IGBClientRequestParams, IGBClientResponse)
  private
    FClient: TNetHTTPClient;
    FRequest: TNetHTTPRequest;
    FResponse: IHTTPResponse;
    FFormData: TMultipartFormData;
    FByteStream: TBytesStream;
    FJSONArray: TJSONArray;
    FJSONObject: TJSONObject;
    FContentType: string;

    procedure OnAWSAuthorization(const AAuth, AAmzDate: string);

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

function TGBClientNetHTTPClient.ContentType(const AValue: TGBContentType): IGBClientRequest;
begin
  Result := Self;
  inherited ContentType(AValue);
  FContentType := AValue.Value;
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

function TGBClientNetHTTPClient.DataSet(const AValue: TDataSet): IGBClientResponse;
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
  LStream: TBytesStream;
begin
  LStream := TBytesStream.Create;
  try
    LStream.LoadFromStream(FResponse.ContentStream);
    Result := LStream.Bytes;
  finally
    LStream.Free;
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

function TGBClientNetHTTPClient.GetList(const AValue: TList<TObject>; const AType: TClass): IGBClientResponse;
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

function TGBClientNetHTTPClient.GetObject(const AValue: TObject): IGBClientResponse;
var
  LParse: TGBOnParseJSONToObject;
begin
  Result := Self;
  LParse := Settings.OnParseJSONToObject;
  if Assigned(LParse) then
    LParse(GetJSONObject, AValue);
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

function TGBClientNetHTTPClient.HeaderAsDateTime(const AName: string): TDateTime;
begin
  Result.fromIso8601ToDateTime(HeaderAsString(AName));
end;

function TGBClientNetHTTPClient.HeaderAsFloat(const AName: string): Double;
begin
  Result := StrToFloatDef(HeaderAsString(AName), 0);
end;

function TGBClientNetHTTPClient.HeaderAsInteger(const AName: string): Integer;
begin
  Result := StrToIntDef(HeaderAsString(AName), 0);
end;

function TGBClientNetHTTPClient.HeaderAsString(const AName: string): string;
begin
  if FResponse.ContainsHeader(AName) then
    Result := FResponse.HeaderValue[AName];
end;

class function TGBClientNetHTTPClient.New: IGBClientRequest;
begin
  Result := Self.create;
end;

procedure TGBClientNetHTTPClient.OnAWSAuthorization(const AAuth, AAmzDate: string);
begin
  FRequest.CustomHeaders['x-amz-date'] := AAmzDate;
  FRequest.CustomHeaders['Authorization'] := AAuth;
end;

procedure TGBClientNetHTTPClient.PrepareRequest;
begin
  FResponse := nil;
  CreateComponents;

  FRequest.MethodString := FMethod.Value;

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
      FAuthorization.AWSv4.Host(GetFullUrl).HTTPVerb(FMethod.value).Payload(FBody);

    TGBClientNetHTTPClientAuth(FAuthorization).ApplyAuth;

    if FAuthorization.AuthType = atAWSv4 then
      OnAWSAuthorization(FAuthorization.AWSv4.Authorization, FAuthorization.AWSv4.XAmzDate);
  end;
end;

procedure TGBClientNetHTTPClient.PrepareRequestBody;
var
  LCount: Integer;
begin
  if FUrlEncodedParams.Count > 0 then
  begin
    FreeAndNil(FFormData);
    FFormData := TMultipartFormData.Create(True);
    for LCount := 0 to Pred(FUrlEncodedParams.Count) do
      FFormData.AddField(FUrlEncodedParams[LCount].Key, FUrlEncodedParams[LCount].Value);

    FRequest.ContentStream := FFormData.Stream;
  end
  else
    FRequest.SourceStream := FBody;
end;

procedure TGBClientNetHTTPClient.PrepareRequestHeaders;
var
  LCount: Integer;
begin
  FClient.ContentType := FContentType;
  for LCount := 0 to Pred(FHeaders.Count) do
    FRequest.CustomHeaders[FHeaders[LCount].Key] := FHeaders[LCount].Value;
end;

procedure TGBClientNetHTTPClient.PrepareRequestPathParams;
var
  LCount: Integer;
  LUrl: string;
  LName: string;
  LValue: string;
begin
  LUrl := GetFullUrl;
  for LCount := 0 to Pred(FPaths.Count) do
  begin
    LName := FPaths[LCount].Key;
    LValue:= FPaths[LCount].Value;
    LUrl := LUrl.Replace(Format('{%s}', [LName]), LValue);
  end;
  FRequest.URL := LUrl;
end;

procedure TGBClientNetHTTPClient.PrepareRequestProxy;
begin
end;

procedure TGBClientNetHTTPClient.PrepareRequestQueries;
var
  LCount: Integer;
  LQueryParam: string;
  LName: string;
  LValue: string;
begin
  LQueryParam := EmptyStr;
  for LCount := 0 to Pred(FQueries.Count) do
  begin
    LName := FQueries[LCount].Key;
    LValue:= FQueries[LCount].Value;
    if LCount > 0 then
      LQueryParam := LQueryParam + '&';
    LQueryParam := LQueryParam + LName + '=' + TNetEncoding.URL.EncodeQuery(LValue);
  end;

  if not LQueryParam.IsEmpty then
    FRequest.URL := FRequest.URL + '?' + LQueryParam;
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
      LException := EGBRestException.create(StatusCode, StatusText, GetText, GetJSONObject);
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
