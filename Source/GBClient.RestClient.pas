unit GBClient.RestClient;

interface

uses
  GBClient.Interfaces,
  GBClient.Core.Request,
  GBClient.Core.Helpers,
  GBClient.Core.Types,
  GBClient.RestClient.Auth,
  GBClient.RestClient.Exceptions,
  Data.DB,
  REST.Client,
  REST.Types,
  IPPeerCommon,
  IPPeerAPI,
  IPPeerClient,
  System.Classes,
  System.SysUtils,
  System.JSON,
  System.Generics.Collections,
  System.TypInfo;

type TGBClientRestClient = class(TGBClientCoreRequest, IGBClientRequest,
                                                       IGBClientRequestParams,
                                                       IGBClientResponse)
  private
    FRestClient: TRESTClient;
    FRestRequest: TRESTRequest;
    FRestResponse: TRESTResponse;
    FByteStream: TBytesStream;
    FAuthorization: IGBClientAuth;
    FContentType: TRESTContentType;

    procedure OnAWSAuthorization(Auth, AmzDate: string);

    procedure createComponents;

    procedure PrepareRequest;
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
    constructor create; override;
    class function New: IGBClientRequest;
    destructor Destroy; override;
end;

implementation

{ TGBClientRestClientRequest }

function TGBClientRestClient.Authorization: IGBClientAuth;
begin
  if not Assigned(FAuthorization) then
    FAuthorization := TGBClientRestClientAuth.New(Self);
  result := FAuthorization;
end;

function TGBClientRestClient.Component: TComponent;
begin
  result := FRestRequest;
end;

function TGBClientRestClient.ContentType(Value: TGBContentType): IGBClientRequest;
begin
  result := Self;
  inherited ContentType(Value);
  case Value of
    ctApplicationJson: FContentType := ctAPPLICATION_JSON;
    ctApplicationXml: FContentType := ctAPPLICATION_XML;
    TGBContentType.ctApplication_x_www_form_urlencoded: FContentType := TRESTContentType.ctAPPLICATION_X_WWW_FORM_URLENCODED;
  end;

end;

constructor TGBClientRestClient.create;
begin
  inherited;
  FContentType := ctAPPLICATION_JSON;
end;

procedure TGBClientRestClient.createComponents;
begin
  FreeAndNil(FRestResponse);
  FreeAndNil(FRestRequest);
  FreeAndNil(FRestClient);

  FRestResponse := TRESTResponse.Create(nil);
  FRestClient := TRESTClient.Create(nil);
  FRestClient.SynchronizedEvents := False;
  FRestClient.RaiseExceptionOn500 := False;

  FRestRequest := TRESTRequest.Create(nil);
  FRestRequest.SynchronizedEvents := False;

  FRestRequest.Client := FRestClient;
  FRestRequest.Response := FRestResponse;
end;

function TGBClientRestClient.DataSet(Value: TDataSet): IGBClientResponse;
var
  parse: TGBOnParseJSONToDataSet;
begin
  result := Self;
  parse := Settings.OnParseJSONToDataSet;
  parse(GetJSONObject, Value);
end;

destructor TGBClientRestClient.Destroy;
begin
  FreeAndNil(FRestResponse);
  FreeAndNil(FRestRequest);
  FreeAndNil(FRestClient);
  FreeAndNil(FByteStream);
  inherited;
end;

function TGBClientRestClient.GetBytes: TBytes;
begin
  result := FRestResponse.RawBytes;
end;

function TGBClientRestClient.GetJSONArray: TJSONArray;
begin
  result := TJSONArray(FRestResponse.JSONValue);
end;

function TGBClientRestClient.GetJSONObject: TJSONObject;
begin
  result := TJSONObject(FRestResponse.JSONValue);
end;

function TGBClientRestClient.GetList(Value: TList<TObject>; AType: TClass): IGBClientResponse;
var
  parse : TGBOnParseJSONToObject;
  jsonArray : TJSONArray;
  LObject : TObject;
  i : Integer;
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

function TGBClientRestClient.GetObject(Value: TObject): IGBClientResponse;
var
  parse: TGBOnParseJSONToObject;
begin
  Result := Self;
  parse := Settings.OnParseJSONToObject;
  if Assigned( Settings.OnParseJSONToObject ) then
    parse(GetJSONObject, Value);
end;

function TGBClientRestClient.GetStream: TBytesStream;
begin
  FreeAndNil(FByteStream);
  FByteStream := TBytesStream.Create(GetBytes);
  result := FByteStream;
end;

function TGBClientRestClient.GetText: string;
begin
  result := FRestResponse.Content;
end;

function TGBClientRestClient.HeaderAsDateTime(Name: String): TDateTime;
begin
  result.fromIso8601ToDateTime( HeaderAsString(Name));
end;

function TGBClientRestClient.HeaderAsFloat(Name: String): Double;
begin
  result := HeaderAsString(Name).ToDouble;
end;

function TGBClientRestClient.HeaderAsInteger(Name: String): Integer;
begin
  result := HeaderAsString(Name).ToInteger;
end;

function TGBClientRestClient.HeaderAsString(Name: String): string;
begin
  result := FRestResponse.Headers.Values[Name];
end;

class function TGBClientRestClient.New: IGBClientRequest;
begin
  result := Self.create;
end;

procedure TGBClientRestClient.OnAWSAuthorization(Auth, AmzDate: string);
begin
  FRestRequest.Params.AddItem('x-amz-date', AmzDate, pkHTTPHEADER, [poDoNotEncode]);
  FRestRequest.Params.AddItem('Authorization', Auth, pkHTTPHEADER, [poDoNotEncode]);
end;

procedure TGBClientRestClient.PrepareRequest;
var
  i: Integer;
begin
  FRestClient.BaseURL := FBaseUrl;
  FRestRequest.Resource := FResource;
  FRestRequest.Timeout := FTimeOut;

  case FMethod of
    gmtGET: FRestRequest.Method := rmGET;
    gmtPOST: FRestRequest.Method := rmPOST;
    gmtPUT: FRestRequest.Method := rmPUT;
    gmtDELETE: FRestRequest.Method := rmDELETE;
    gmtPATCH: FRestRequest.Method := rmPATCH;
  end;

  PrepareRequestHeaders;
  PrepareRequestQueries;
  PrepareRequestPathParams;
  PrepareRequestBody;
  PrepareRequestAuth;

  if Assigned(FOnPreExecute) then
  begin
    for i := 0 to Pred(FRestRequest.Params.Count) do
    begin
      FOnPreExecute(
        'ParamName = ' + FRestRequest.Params[i].name + sLineBreak +
        'ParamType = ' + GetEnumName(TypeInfo(TRESTRequestParameterKind), Integer( FRestRequest.Params[i].Kind)) + sLineBreak +
        'ParamValue = ' + FRestRequest.Params[i].Value
      );
    end;
  end;
end;

procedure TGBClientRestClient.PrepareRequestAuth;
begin
  if Assigned(FAuthorization) then
  begin
    if FAuthorization.AuthType = atAWSv4 then
    begin
      FAuthorization.AWSv4.OnAWSSignature(Self.OnAWSAuthorization);
      FAuthorization.AWSv4
        .Host(GetFullUrl)
        .HTTPVerb(FMethod.value)
        .Payload(FBody);
    end;

    TGBClientRestClientAuth(FAuthorization).ApplyAuth;
  end;
end;

procedure TGBClientRestClient.PrepareRequestBody;
var
  i: Integer;
  name: String;
  value: string;
begin
  if (FUrlEncodedParams.Count = 0) and (Assigned(FBody)) then
    FRestRequest.AddBody(FBody, FContentType)
  else
  begin
    for i := 0 to Pred(FUrlEncodedParams.Count) do
    begin
      name := FUrlEncodedParams[i].Key;
      value := FUrlEncodedParams[i].Value;
      FRestRequest.AddParameter(name, value, pkGETorPOST);
    end;
  end;
end;

procedure TGBClientRestClient.PrepareRequestHeaders;
var
  i: Integer;
  parameter: TRESTRequestParameter;
begin
  for i := 0 to Pred(FHeaders.Count) do
  begin
    parameter := FRestRequest.Params.AddItem;
    parameter.Kind := pkHTTPHEADER;
    parameter.name := FHeaders[i].Key;
    parameter.Value := FHeaders[i].Value;

    if not FHeaders[i].Encoding then
      parameter.Options := [poDoNotEncode];
  end;
end;

procedure TGBClientRestClient.PrepareRequestPathParams;
var
  i: Integer;
begin
  for i := 0 to Pred(FPaths.Count) do
    FRestRequest.Params.AddUrlSegment(FPaths[i].Key, FPaths[i].Value);
end;

procedure TGBClientRestClient.PrepareRequestQueries;
var
  i: Integer;
  options: TRESTRequestParameterOptions;
begin
  for i := 0 to Pred(FQueries.Count) do
  begin
    options := [];
    if not FQueries[i].Encoding then
      options := [poDoNotEncode];

    {$IF COMPILERVERSION < 33}
    FRestRequest.AddParameter(FQueries[i].Key, FQueries[i].Value, pkGETorPOST, options);
    {$ELSE}
    FRestRequest.AddParameter(FQueries[i].Key, FQueries[i].Value, pkQuery, options);
    {$ENDIF}
  end;
end;

function TGBClientRestClient.Response: IGBClientResponse;
begin
  result := Self;
end;

function TGBClientRestClient.Send: IGBClientResponse;
var
  LException: EGBRestException;
begin
  result := Self;
  createComponents;
  PrepareRequest;
  try
    try
      FRestRequest.Execute;
    except
      on e: Exception do
      begin
        if e.Message.Contains('(12002)') then
          raise EGBRestExceptionTimeout.CreateFmt(e.Message, []);
        raise;
      end;
    end;

    if FRestResponse.StatusCode >= 400 then
    begin
      LException := EGBRestRequestException.create(FRestRequest);
      if Assigned(FOnException) then
        FOnException(LException);
      raise LException;
    end;
  finally
    FRestRequest.Body.ClearBody;
    FRestRequest.Params.Clear;
    Clear;
  end;
end;

function TGBClientRestClient.StatusCode: Integer;
begin
  result := FRestResponse.StatusCode;
end;

function TGBClientRestClient.StatusText: string;
begin
  result := FRestResponse.StatusText;
end;

end.
