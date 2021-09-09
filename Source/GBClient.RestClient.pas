unit GBClient.RestClient;

interface

uses
  GBClient.Interfaces,
  GBClient.RestClient.Response,
  GBClient.Core.Request,
  GBClient.Core.Helpers,
  GBClient.Core.Types,
  GBClient.Core.Exceptions,
  GBClient.RestClient.Auth,
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
                                                       IGBClientRequestParams)
  private
    FRestClient: TRESTClient;
    FRestRequest: TRESTRequest;
    FRestResponse: TRESTResponse;
    FResponse: IGBClientResponse;
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

destructor TGBClientRestClient.Destroy;
begin
  FreeAndNil(FRestResponse);
  FreeAndNil(FRestRequest);
  FreeAndNil(FRestClient);
  inherited;
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
      //FAuthorization.AWSv4.OnAWSSignature(Self.OnAWSAuthorization);
      FAuthorization.AWSv4
        .Host(GetFullUrl)
        .HTTPVerb(FMethod.value)
        .Payload(FBody);
    end;

    TGBClientRestClientAuth(FAuthorization).ApplyAuth;

    if FAuthorization.AuthType = atAWSv4 then
    begin
      OnAWSAuthorization(FAuthorization.AWSv4.Authorization,
                         FAuthorization.AWSv4.XAmzDate);
    end;
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
  result := FResponse;
end;

function TGBClientRestClient.Send: IGBClientResponse;
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

    FResponse := TGBClientRestClientResponse.New(Self);

    if FResponse.StatusCode >= 400 then
    begin
      LException := EGBRestException.create(FResponse.StatusCode,
                                            FResponse.StatusText,
                                            FResponse.GetText,
                                            FResponse.GetJSONObject);
      if Assigned(FOnException) then
        FOnException(LException);
      raise LException;
    end;

    result := FResponse;
  finally
    FRestRequest.Body.ClearBody;
    FRestRequest.Params.Clear;
    Clear;
  end;
end;

end.
