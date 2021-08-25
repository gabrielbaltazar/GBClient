unit GBClient.RestClient.Request;

interface

uses
  GBClient.Interfaces,
  GBClient.Request.Base,
  GBClient.Helpers,
  GBClient.Types,
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

type TGBClientRestClientRequest = class(TGBClientRequestBase, IGBClientRequest,
                                                              IGBClientRequestParams,
                                                              IGBClientResponse)
  private
    FRestClient: TRESTClient;
    FRestRequest: TRESTRequest;
    FRestResponse: TRESTResponse;
    FByteStream: TBytesStream;
    FAuthorization: IGBClientAuth;
    FContentType: TRESTContentType;

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

function TGBClientRestClientRequest.Authorization: IGBClientAuth;
begin
  if not Assigned(FAuthorization) then
    FAuthorization := TGBClientRestClientAuth.New(Self);
  result := FAuthorization;
end;

function TGBClientRestClientRequest.Component: TComponent;
begin
  result := FRestRequest;
end;

function TGBClientRestClientRequest.ContentType(Value: TGBContentType): IGBClientRequest;
begin
  result := Self;
  inherited ContentType(Value);
  case Value of
    ctApplicationJson: FContentType := ctAPPLICATION_JSON;
    ctApplicationXml: FContentType := ctAPPLICATION_XML;
    TGBContentType.ctApplication_x_www_form_urlencoded: FContentType := TRESTContentType.ctAPPLICATION_X_WWW_FORM_URLENCODED;
  end;

end;

constructor TGBClientRestClientRequest.create;
begin
  inherited;
  FContentType := ctAPPLICATION_JSON;
end;

procedure TGBClientRestClientRequest.createComponents;
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

function TGBClientRestClientRequest.DataSet(Value: TDataSet): IGBClientResponse;
var
  parse: TGBOnParseJSONToDataSet;
begin
  result := Self;
  parse := Settings.OnParseJSONToDataSet;
  parse(GetJSONObject, Value);
end;

destructor TGBClientRestClientRequest.Destroy;
begin
  FreeAndNil(FRestResponse);
  FreeAndNil(FRestRequest);
  FreeAndNil(FRestClient);
  FreeAndNil(FByteStream);
  inherited;
end;

function TGBClientRestClientRequest.GetBytes: TBytes;
begin
  result := FRestResponse.RawBytes;
end;

function TGBClientRestClientRequest.GetJSONArray: TJSONArray;
begin
  result := TJSONArray(FRestResponse.JSONValue);
end;

function TGBClientRestClientRequest.GetJSONObject: TJSONObject;
begin
  result := TJSONObject(FRestResponse.JSONValue);
end;

function TGBClientRestClientRequest.GetList(Value: TList<TObject>; AType: TClass): IGBClientResponse;
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

function TGBClientRestClientRequest.GetObject(Value: TObject): IGBClientResponse;
var
  parse: TGBOnParseJSONToObject;
begin
  Result := Self;
  parse := Settings.OnParseJSONToObject;
  if Assigned( Settings.OnParseJSONToObject ) then
    parse(GetJSONObject, Value);
end;

function TGBClientRestClientRequest.GetStream: TBytesStream;
begin
  FreeAndNil(FByteStream);
  FByteStream := TBytesStream.Create(GetBytes);
  result := FByteStream;
end;

function TGBClientRestClientRequest.GetText: string;
begin
  result := FRestResponse.Content;
end;

function TGBClientRestClientRequest.HeaderAsDateTime(Name: String): TDateTime;
begin
  result.fromIso8601ToDateTime( HeaderAsString(Name));
end;

function TGBClientRestClientRequest.HeaderAsFloat(Name: String): Double;
begin
  result := HeaderAsString(Name).ToDouble;
end;

function TGBClientRestClientRequest.HeaderAsInteger(Name: String): Integer;
begin
  result := HeaderAsString(Name).ToInteger;
end;

function TGBClientRestClientRequest.HeaderAsString(Name: String): string;
begin
  result := FRestResponse.Headers.Values[Name];
end;

class function TGBClientRestClientRequest.New: IGBClientRequest;
begin
  result := Self.create;
end;

procedure TGBClientRestClientRequest.PrepareRequest;
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

procedure TGBClientRestClientRequest.PrepareRequestAuth;
begin
  if Assigned(FAuthorization) then
    TGBClientRestClientAuth(FAuthorization).ApplyAuth;
end;

procedure TGBClientRestClientRequest.PrepareRequestBody;
begin
  FRestRequest.AddBody(FBody, FContentType);
end;

procedure TGBClientRestClientRequest.PrepareRequestHeaders;
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

procedure TGBClientRestClientRequest.PrepareRequestPathParams;
var
  i: Integer;
begin
  for i := 0 to Pred(FPaths.Count) do
    FRestRequest.Params.AddUrlSegment(FPaths[i].Key, FPaths[i].Value);
end;

procedure TGBClientRestClientRequest.PrepareRequestQueries;
var
  i: Integer;
  options: TRESTRequestParameterOptions;
begin
  for i := 0 to Pred(FQueries.Count) do
  begin
    options := [];
    if not FQueries[i].Encoding then
      options := [poDoNotEncode];

    FRestRequest.AddParameter(FQueries[i].Key, FQueries[i].Value, pkGETorPOST, options);
  end;
end;

function TGBClientRestClientRequest.Response: IGBClientResponse;
begin
  result := Self;
end;

function TGBClientRestClientRequest.Send: IGBClientResponse;
var
  LException: EGBRestException;
begin
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

function TGBClientRestClientRequest.StatusCode: Integer;
begin
  result := FRestResponse.StatusCode;
end;

function TGBClientRestClientRequest.StatusText: string;
begin
  result := FRestResponse.StatusText;
end;

end.
