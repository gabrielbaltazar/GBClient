unit GBClient.RestClient.Request;

interface

uses
  GBClient.Interfaces,
  GBClient.Types,
  GBClient.RestClient.Exceptions,
  GBClient.RestClient.Request.Header,
  GBClient.RestClient.Request.Body,
  GBClient.RestClient.Request.ParamPath,
  GBClient.RestClient.Request.Query,
  GBClient.RestClient.Request.Auth,
  GBClient.RestClient.Response,
  GBClient.Exceptions,
  GBClient.Settings.Default,
  IPPeerCommon,
  IPPeerAPI,
  IPPeerClient,
  REST.Client,
  Rest.Types,
  System.Classes,
  System.TypInfo,
  System.SysUtils;

type TGBClientRequest = class(TInterfacedObject, IGBClientRequest)

  private
    FRestClient   : TRESTClient;
    FRestRequest  : TRESTRequest;
    FRestResponse : TRESTResponse;

    FHeaders  : IGBClientParamHeader;
    FParamPath: IGBClientParamPath;
    FQueries  : IGBClientParamQuery;
    FResponse : IGBClientResponse;
    FBody     : IGBClientBodyRequest;
    FAuth     : IGBClientAuth;
    FSettings : IGBClientSettings;

    FOnException : TGBOnException;
    FOnPreExecute: TOnPreExecute;

    procedure PrepareRequest;
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
    function ContentType    (Value : String)       : IGBClientRequest; overload;
    function BaseURL        (Value : String)       : IGBClientRequest;
    function Resource       (Value : String)       : IGBClientRequest;
    function TimeOut        (Value : Integer)      : IGBClientRequest;

    function Execute  : IGBClientResponse;
    function Send     : IGBClientResponse;
    function Response : IGBClientResponse;

    function Settings: IGBClientSettings;

    function OnException (Value: TGBOnException): IGBClientRequest;
    function OnPreExecute(Value: TOnPreExecute): IGBClientRequest;
  public
    constructor create;
    destructor  Destroy; override;
    class function New: IGBClientRequest;
end;

implementation

{ TGBClientRequest }

function TGBClientRequest.Accept(Value: string): IGBClientRequest;
begin
  result := Self;
  FRestClient.Accept := Value;
  FRestRequest.Accept := Value;
end;

function TGBClientRequest.AcceptCharset(Value: string): IGBClientRequest;
begin
  result := Self;
  FRestClient.AcceptCharset := Value;
  FRestRequest.AcceptCharset := Value;
end;

function TGBClientRequest.AcceptEncoding(Value: string): IGBClientRequest;
begin
  result := Self;
  FRestClient.AcceptEncoding := Value;
  FRestRequest.AcceptEncoding := Value;
end;

function TGBClientRequest.Authorization: IGBClientAuth;
begin
  if not Assigned(FAuth) then
    FAuth := TGBClientRestClientAuth.New(Self, FRestRequest);
  result := FAuth;
end;

function TGBClientRequest.BaseURL(Value: String): IGBClientRequest;
begin
  result := Self;
  FRestClient.BaseURL := Value;
end;

function TGBClientRequest.Body: IGBClientBodyRequest;
begin
  if not Assigned(FBody) then
    FBody := TGBClientRestClientRequestBody.New(Self, FRestRequest);
  result := FBody;
end;

function TGBClientRequest.ContentType(Value: String): IGBClientRequest;
begin
  result := Self;
  FRestClient.ContentType := Value;
end;

function TGBClientRequest.ContentType(Value: TGBContentType): IGBClientRequest;
begin
  result := ContentType( Value.value );
end;

constructor TGBClientRequest.create;
begin
  FRestClient   := TRESTClient.Create('http://localhost:80');
  FRestRequest  := TRESTRequest.Create(nil);
  FRestResponse := TRESTResponse.Create(nil);

  FRestClient.ContentType := 'application/json';
  FRestClient.RaiseExceptionOn500 := False;

  FRestRequest.Client := FRestClient;
  FRestRequest.Response := FRestResponse;
end;

function TGBClientRequest.DELETE: IGBClientRequest;
begin
  result := Self;
  FRestRequest.Method := rmDELETE;
end;

destructor TGBClientRequest.Destroy;
begin
  FRestClient.Free;
  FRestRequest.Free;
  FRestResponse.Free;
  inherited;
end;

function TGBClientRequest.Resource(Value: String): IGBClientRequest;
begin
  result := Self;
  FRestRequest.Resource := Value;
end;

function TGBClientRequest.Execute: IGBClientResponse;
begin
  result := Send;
end;

function TGBClientRequest.GET: IGBClientRequest;
begin
  result          := Self;
  FRestRequest.Method := rmGET;
end;

function TGBClientRequest.Header: IGBClientParamHeader;
begin
  if not Assigned(FHeaders) then
    FHeaders := TGBClientRestClientRequestHeader.New(Self, FRestRequest);
  result := FHeaders;
end;

class function TGBClientRequest.New: IGBClientRequest;
begin
  Result := Self.create;
end;

function TGBClientRequest.OnException(Value: TGBOnException): IGBClientRequest;
begin
  Result := Self;
  FOnException := Value;
end;

function TGBClientRequest.OnPreExecute(Value: TOnPreExecute): IGBClientRequest;
begin
  result := Self;
  FOnPreExecute := Value;
end;

function TGBClientRequest.ParamPath: IGBClientParamPath;
begin
  if not Assigned(FParamPath) then
    FParamPath := TGBClientRestClientRequestParamPath.New(Self, FRestRequest);
  result := FParamPath;
end;

function TGBClientRequest.PATCH: IGBClientRequest;
begin
  result          := Self;
  FRestRequest.Method := rmPATCH;
end;

function TGBClientRequest.POST: IGBClientRequest;
begin
  result          := Self;
  FRestRequest.Method := rmPOST;
end;

procedure TGBClientRequest.PrepareRequest;
var
  i : Integer;
begin
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

function TGBClientRequest.PUT: IGBClientRequest;
begin
  result          := Self;
  FRestRequest.Method := rmPUT;
end;

function TGBClientRequest.Query: IGBClientParamQuery;
begin
  if not Assigned(FQueries) then
    FQueries := TGBClientRequestQuery.New(Self, FRestRequest);
  result := FQueries;
end;

function TGBClientRequest.Response: IGBClientResponse;
begin
  if not Assigned(FResponse) then
    FResponse := TGBClientRestClientResponse.New(Self, FRestResponse);

  result := FResponse;
end;

function TGBClientRequest.Send: IGBClientResponse;
var
  LException : EGBRestException;
begin
  PrepareRequest;

  FRestRequest.Execute;
  if FRestResponse.StatusCode >= 400 then
  begin
    LException := EGBRestRequestException.create(FRestRequest);
    if Assigned(FOnException) then
      FOnException(LException);
    raise LException;
  end;

  if not Assigned(FResponse) then
    FResponse := TGBClientRestClientResponse.New(Self, FRestResponse);

  result := FResponse;
end;

function TGBClientRequest.Settings: IGBClientSettings;
begin
  if not Assigned(FSettings) then
    FSettings := TGBClientSettingsDefault.New(Self);
  result := FSettings;
end;

function TGBClientRequest.TimeOut(Value: Integer): IGBClientRequest;
begin
  result := Self;
  FRestRequest.Timeout := Value;
end;

end.
