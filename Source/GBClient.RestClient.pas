unit GBClient.RestClient;

interface

{$IFDEF WEAKPACKAGEUNIT}
  {$WEAKPACKAGEUNIT ON}
{$ENDIF}

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

type
  TGBClientRestClient = class(TGBClientCoreRequest, IGBClientRequest,
    IGBClientRequestParams)
  private
    FRestClient: TRESTClient;
    FRestRequest: TRESTRequest;
    FRestResponse: TRESTResponse;
    FResponse: IGBClientResponse;
    FContentType: TRESTContentType;

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
  protected
    function Component: TComponent; override;
    function Authorization: IGBClientAuth; override;

    function ContentType(AValue: TGBContentType): IGBClientRequest; override;

    function Send: IGBClientResponse; override;
    function Response: IGBClientResponse; override;
  public
    constructor Create; override;
    class function New: IGBClientRequest;
    destructor Destroy; override;
  end;

implementation

{ TGBClientRestClientRequest }

function TGBClientRestClient.Authorization: IGBClientAuth;
begin
  if not Assigned(FAuthorization) then
    FAuthorization := TGBClientRestClientAuth.New(Self);
  Result := FAuthorization;
end;

function TGBClientRestClient.Component: TComponent;
begin
  Result := FRestRequest;
end;

function TGBClientRestClient.ContentType(AValue: TGBContentType): IGBClientRequest;
begin
  Result := Self;
  inherited ContentType(AValue);
  case AValue of
    ctApplicationJson: FContentType := ctAPPLICATION_JSON;
    ctApplicationXml: FContentType := ctAPPLICATION_XML;
    TGBContentType.ctApplication_x_www_form_urlencoded: FContentType := TRESTContentType.ctAPPLICATION_X_WWW_FORM_URLENCODED;
  end;
end;

constructor TGBClientRestClient.Create;
begin
  inherited;
  FContentType := ctAPPLICATION_JSON;
end;

procedure TGBClientRestClient.CreateComponents;
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
  Result := Self.Create;
end;

procedure TGBClientRestClient.OnAWSAuthorization(AAuth, AAmzDate: string);
begin
  FRestRequest.Params.AddItem('x-amz-date', AAmzDate, pkHTTPHEADER, [poDoNotEncode]);
  FRestRequest.Params.AddItem('Authorization', AAuth, pkHTTPHEADER, [poDoNotEncode]);
end;

procedure TGBClientRestClient.PrepareRequest;
var
  I: Integer;
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

  PrepareRequestProxy;
  PrepareRequestHeaders;
  PrepareRequestQueries;
  PrepareRequestPathParams;
  PrepareRequestBody;
  PrepareRequestFormData;
  PrepareRequestAuth;

  if Assigned(FOnPreExecute) then
  begin
    for I := 0 to Pred(FRestRequest.Params.Count) do
    begin
      FOnPreExecute(
        'ParamName = ' + FRestRequest.Params[I].name + sLineBreak +
        'ParamType = ' + GetEnumName(TypeInfo(TRESTRequestParameterKind), Integer( FRestRequest.Params[I].Kind)) + sLineBreak +
        'ParamValue = ' + FRestRequest.Params[I].Value
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
  I: Integer;
  LName: string;
  LValue: string;
begin
  if (FUrlEncodedParams.Count = 0) and (Assigned(FBody)) then
    FRestRequest.AddBody(FBody, FContentType)
  else
  begin
    for I := 0 to Pred(FUrlEncodedParams.Count) do
    begin
      LName := FUrlEncodedParams[I].Key;
      LValue := FUrlEncodedParams[I].Value;
      FRestRequest.AddParameter(LName, LValue, pkGETorPOST);
    end;
  end;
end;

procedure TGBClientRestClient.PrepareRequestFormData;
var
  I: Integer;
begin
  for I := 0 to Pred(FFormData.Count) do
  begin
//    FRestRequest.Client.ContentType := 'multipart/form-data';
    FRestRequest.Params.AddItem(FFormData[I].Key,
                                FFormData[I].Value,
                                TRESTRequestParameterKind.pkREQUESTBODY,
                                [],
                                ctMULTIPART_FORM_DATA);
  end;

end;

procedure TGBClientRestClient.PrepareRequestHeaders;
var
  I: Integer;
  LParameter: TRESTRequestParameter;
begin
  for I := 0 to Pred(FHeaders.Count) do
  begin
    LParameter := FRestRequest.Params.AddItem;
    LParameter.Kind := pkHTTPHEADER;
    LParameter.name := FHeaders[I].Key;
    LParameter.Value := FHeaders[I].Value;

    if not FHeaders[I].Encoding then
      LParameter.Options := [poDoNotEncode];
  end;
end;

procedure TGBClientRestClient.PrepareRequestPathParams;
var
  I: Integer;
begin
  for I := 0 to Pred(FPaths.Count) do
    FRestRequest.Params.AddUrlSegment(FPaths[I].Key, FPaths[I].Value);
end;

procedure TGBClientRestClient.PrepareRequestProxy;
begin
  FRestClient.ProxyServer := FProxyServer;
  FRestClient.ProxyPort := FProxyPort;
  FRestClient.ProxyPassword := FProxyPassword;
  FRestClient.ProxyUsername := FProxyUsername;
end;

procedure TGBClientRestClient.PrepareRequestQueries;
var
  I: Integer;
  LOptions: TRESTRequestParameterOptions;
begin
  for I := 0 to Pred(FQueries.Count) do
  begin
    LOptions := [];
    if not FQueries[I].Encoding then
      LOptions := [poDoNotEncode];

    {$IF COMPILERVERSION < 33}
    FRestRequest.AddParameter(FQueries[i].Key, FQueries[i].Value, pkGETorPOST, options);
    {$ELSE}
    FRestRequest.AddParameter(FQueries[I].Key, FQueries[I].Value, pkQuery, LOptions);
    {$ENDIF}
  end;
end;

function TGBClientRestClient.Response: IGBClientResponse;
begin
  Result := FResponse;
end;

function TGBClientRestClient.Send: IGBClientResponse;
var
  LException: EGBRestException;
begin
  CreateComponents;
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
      LException := EGBRestException.Create(FResponse.StatusCode,
                                            FResponse.StatusText,
                                            FResponse.GetText,
                                            FResponse.GetJSONObject);
      if Assigned(FOnException) then
        FOnException(LException);
      raise LException;
    end;

    Result := FResponse;
  finally
    FRestRequest.Body.ClearBody;
    FRestRequest.Params.Clear;
    Clear;
  end;
end;

end.
