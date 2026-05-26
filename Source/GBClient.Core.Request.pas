unit GBClient.Core.Request;

interface

{$IFDEF WEAKPACKAGEUNIT}
  {$WEAKPACKAGEUNIT ON}
{$ENDIF}

uses
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.Generics.Collections,
  Data.DB,
  GBClient.Interfaces,
  GBClient.Core.Types,
  GBClient.Core.Helpers,
  GBClient.Core.Settings,
  GBClient.Core.Request.Param;

type
  TGBClientCoreRequest = class abstract(TInterfacedObject, IGBClientRequest, IGBClientProxy, IGBClientRequestParams)
  protected
    FMethod: TGBMethodType;
    FHeaders: TObjectList<TGBClientCoreRequestParam>;
    FPaths: TObjectList<TGBClientCoreRequestParam>;
    FQueries: TObjectList<TGBClientCoreRequestParam>;
    FUrlEncodedParams: TObjectList<TGBClientCoreRequestParam>;
    FAuthorization: IGBClientAuth;
    FBody: TStream;
    FBaseUrl: string;
    FResource: string;
    FTimeOut: Integer;
    FProxyServer: string;
    FProxyPort: Integer;
    FProxyUsername: string;
    FProxyPassword: string;

    FSettings: IGBClientSettings;

    FOnException: TGBOnException;
    FOnPreExecute: TOnPreExecute;

    function GetFullUrl: string;

    procedure Clear;
    function Component: TComponent; virtual; abstract;
    function Authorization: IGBClientAuth; virtual; abstract;

    function POST: IGBClientRequest;
    function PUT: IGBClientRequest;
    function GET: IGBClientRequest;
    function DELETE: IGBClientRequest;
    function PATCH: IGBClientRequest;

    function Accept(const AValue: string): IGBClientRequest;
    function AcceptCharset(const AValue: string): IGBClientRequest;
    function AcceptEncoding(const AValue: string): IGBClientRequest;
    function ContentType(const AValue: TGBContentType): IGBClientRequest; overload; virtual;
    function ContentType(const AValue: string): IGBClientRequest; overload;
    function BaseURL(const AValue: string): IGBClientRequest;
    function Resource(const AValue: string): IGBClientRequest;
    function TimeOut(const AValue: Integer): IGBClientRequest;

{$REGION 'PARAMS'}
    function Params: IGBClientRequestParams;

    // Header Params
    function HeaderAddOrSet(const AKey, AValue: string; const AEncode: Boolean = True):
      IGBClientRequestParams; overload;
    function HeaderAddOrSet(const AKey: string; const AValue: Integer; const AEncode: Boolean = True):
      IGBClientRequestParams; overload;
    function HeaderAddOrSet(const AKey: string; const AValue: Extended; const AEncode: Boolean = True):
      IGBClientRequestParams; overload;
    function HeaderAddOrSet(const AKey: string; const AValue: TDateTime; const AEncode: Boolean = True):
      IGBClientRequestParams; overload;

    // Path Params
    function PathAddOrSet(const AKey, AValue: string): IGBClientRequestParams; overload;
    function PathAddOrSet(const AKey: string; const AValue: Integer): IGBClientRequestParams; overload;
    function PathAddOrSet(const AKey: string; const AValue: Extended): IGBClientRequestParams; overload;
    function PathAddOrSet(const AKey: string; const AValue: TDateTime): IGBClientRequestParams; overload;

    // Query Params
    function QueryAddOrSet(const AKey, AValue: string; const AEncode: Boolean = True):
      IGBClientRequestParams; overload;
    function QueryAddOrSet(const AKey: string; const AValue: Integer; const AEncode: Boolean = True):
      IGBClientRequestParams; overload;
    function QueryAddOrSet(const AKey: string; const AValue: Extended; const AEncode: Boolean = True):
      IGBClientRequestParams; overload;
    function QueryAddOrSet(const AKey: string; const AValue: TDateTime; const AEncode: Boolean = True):
      IGBClientRequestParams; overload;

    // Body Params
    function BodyAddOrSet(const AValue: string) : IGBClientRequestParams; overload;
    function BodyAddOrSet(const AValue: TJSONObject; const AOwner: Boolean = False): IGBClientRequestParams; overload;
    function BodyAddOrSet(const AValue: TJSONArray; const AOwner: Boolean = False): IGBClientRequestParams; overload;
    function BodyAddOrSet(const AValue: TObject; const AOwner: Boolean = False): IGBClientRequestParams; overload;
    function BodyAddOrSet(const AValue: TList<TObject>; const AOwner: Boolean = False): IGBClientRequestParams; overload;
    function BodyAddOrSet(const AValue: TDataSet; const ACurrent: Boolean = True): IGBClientRequestParams; overload;
    function BodyAddOrSet(const AKey, AValue: string): IGBClientRequestParams; overload;

    function BodyBinary(const AFileName: string): IGBClientRequestParams; overload;
    function BodyBinary(const AStream: TStream; const AOwner: Boolean = False): IGBClientRequestParams; overload;

    function GetBody: string;
{$ENDREGION}

{$REGION 'PROXY'}
    function Proxy: IGBClientProxy;
    function Server(const AValue: string): IGBClientProxy;
    function Port(const AValue: Integer): IGBClientProxy;
    function Username(const AValue: string): IGBClientProxy;
    function Password(const AValue: string): IGBClientProxy;
{$ENDREGION}

    function &End: IGBClientRequest;
    function Send: IGBClientResponse; virtual; abstract;
    function Response: IGBClientResponse; virtual; abstract;

    function Settings: IGBClientSettings;

    function OnException(const AValue: TGBOnException): IGBClientRequest;
    function OnPreExecute(const AValue: TOnPreExecute): IGBClientRequest;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

implementation

{ TGBClientCoreRequest }

function TGBClientCoreRequest.&End: IGBClientRequest;
begin
  Result := Self;
end;

function TGBClientCoreRequest.Accept(const AValue: string): IGBClientRequest;
begin
  Result := Self;
  HeaderAddOrSet('Accept', AValue, False);
end;

function TGBClientCoreRequest.AcceptCharset(const AValue: string):
    IGBClientRequest;
begin
  Result := Self;
  HeaderAddOrSet('Accept-Charset', AValue);
end;

function TGBClientCoreRequest.AcceptEncoding(const AValue: string):
    IGBClientRequest;
begin
  Result := Self;
  HeaderAddOrSet('Accept-Encoding', AValue);
end;

function TGBClientCoreRequest.BaseURL(const AValue: string): IGBClientRequest;
begin
  Result := Self;
  FBaseUrl := AValue;
end;

function TGBClientCoreRequest.BodyAddOrSet(const AValue: TObject;
  const AOwner: Boolean = False): IGBClientRequestParams;
var
  LParse : TGBOnParseObjectToJSON;
  LJson: TJSONObject;
begin
  Result := Self;
  LParse := Settings.OnParseObjectToJSON;
  LJson := LParse(AValue);
  try
    BodyAddOrSet(LJson, False);
    if AOwner then
      FreeAndNil(AValue);
  finally
    LJson.Free;
  end;
end;

function TGBClientCoreRequest.BodyAddOrSet(const AValue: TList<TObject>;
  const AOwner: Boolean = False): IGBClientRequestParams;
var
  LParse: TGBOnParseObjectToJSON;
  LJsonArray: TJSONArray;
  LCount: Integer;
begin
  Result := Self;
  LParse := Settings.OnParseObjectToJSON;
  LJsonArray := TJSONArray.Create;
  try
    for LCount := 0 to Pred(AValue.Count) do
      LJsonArray.AddElement(LParse(AValue[LCount]));
    BodyAddOrSet(LJsonArray);
  finally
    LJsonArray.Free;
    if AOwner then
      FreeAndNil(AValue);
  end;
end;

function TGBClientCoreRequest.BodyAddOrSet(const AKey, AValue: string): IGBClientRequestParams;
begin
  Result := Self;
  TGBClientCoreRequestParam.AddOrSet(FUrlEncodedParams, AKey, AValue);
  ContentType(TGBContentType.ctApplication_x_www_form_urlencoded);
end;

function TGBClientCoreRequest.BodyAddOrSet(const AValue: TDataSet;
  const ACurrent: Boolean = True): IGBClientRequestParams;
begin
  Result := Self;
  if ACurrent then
    BodyAddOrSet(AValue.ToJSONObject, True)
  else
    BodyAddOrSet(AValue.ToJSONArray, True);
end;

function TGBClientCoreRequest.BodyAddOrSet(const AValue: string): IGBClientRequestParams;
begin
  Result := Self;
  FreeAndNil(FBody);
  FBody := TStringStream.Create(AValue);
  FBody.Position := 0;
end;

function TGBClientCoreRequest.BodyAddOrSet(const AValue: TJSONObject;
  const AOwner: Boolean = False): IGBClientRequestParams;
begin
  Result := Self;
  BodyAddOrSet(AValue.ToJson);
  ContentType(TGBContentType.ctApplicationJson);
  if AOwner then
    FreeAndNil(AValue);
end;

function TGBClientCoreRequest.BodyAddOrSet(const AValue: TJSONArray;
  const AOwner: Boolean = False): IGBClientRequestParams;
begin
  Result := Self;
  BodyAddOrSet(AValue.ToString);
  ContentType(TGBContentType.ctApplicationJson);
  if AOwner then
    FreeAndNil(AValue);
end;

function TGBClientCoreRequest.BodyBinary(const AFileName: string): IGBClientRequestParams;
begin
  Result := Self;
  FreeAndNil(FBody);
  FBody := TMemoryStream.Create;
  try
    TMemoryStream(FBody).LoadFromFile(AFileName);
    FBody.Position := 0;
  except
    FreeAndNil(FBody);
    raise;
  end;
end;

function TGBClientCoreRequest.BodyBinary(const AStream: TStream; const AOwner: Boolean = False): IGBClientRequestParams;
begin
  Result := Self;
  FreeAndNil(FBody);
  FBody := TMemoryStream.Create;
  try
    TMemoryStream(FBody).LoadFromStream(AStream);
    FBody.Position := 0;
  except
    FreeAndNil(FBody);
    raise;
  end;
end;

function TGBClientCoreRequest.ContentType(const AValue: TGBContentType): IGBClientRequest;
begin
  Result := Self;
  ContentType(AValue.value);
end;

procedure TGBClientCoreRequest.Clear;
begin
  FHeaders.Clear;
  FPaths.Clear;
  FQueries.Clear;
  FUrlEncodedParams.Clear;
  FreeAndNil(FBody);
end;

function TGBClientCoreRequest.ContentType(const AValue: string): IGBClientRequest;
begin
  Result := Self;
  HeaderAddOrSet('content-type', AValue, False);
end;

constructor TGBClientCoreRequest.Create;
begin
  FHeaders := TObjectList<TGBClientCoreRequestParam>.Create;
  FPaths := TObjectList<TGBClientCoreRequestParam>.Create;
  FQueries := TObjectList<TGBClientCoreRequestParam>.Create;
  FUrlEncodedParams := TObjectList<TGBClientCoreRequestParam>.Create;

  FMethod := gmtGET;
  AcceptCharset('utf-8, *;q=0.8');
  ContentType('application/json');

{$IF CompilerVersion > 31}
  HeaderAddOrSet('Accept', 'application/json', False);
{$ELSE}
  Accept('application/json');
{$ENDIF}
end;

function TGBClientCoreRequest.DELETE: IGBClientRequest;
begin
  Result := Self;
  FMethod := gmtDELETE;
end;

destructor TGBClientCoreRequest.Destroy;
begin
  FHeaders.Free;
  FPaths.Free;
  FQueries.Free;
  FUrlEncodedParams.Free;
  inherited;
end;

function TGBClientCoreRequest.GET: IGBClientRequest;
begin
  Result := Self;
  FMethod := gmtGET;
end;

function TGBClientCoreRequest.GetBody: string;
var
  LBodyStream: TStringStream;
begin
  if Assigned(FBody) then
  begin
    LBodyStream := TStringStream.Create;
    try
      LBodyStream.LoadFromStream(FBody);
      Result := LBodyStream.DataString;
    finally
      LBodyStream.Free;
    end;
  end;
end;

function TGBClientCoreRequest.GetFullUrl: string;
var
  LResource: string;
begin
  Result := FBaseUrl;
  if not FBaseUrl.EndsWith('/') then
    Result := Result + '/';

  LResource := FResource;
  if LResource.StartsWith('/') then
    LResource := Copy(LResource, 2, LResource.Length - 1);

  Result := Result + LResource;
end;

function TGBClientCoreRequest.HeaderAddOrSet(const AKey: string; const AValue: TDateTime;
  const AEncode: Boolean = True): IGBClientRequestParams;
begin
  Result := Self;
  TGBClientCoreRequestParam.AddOrSet(FHeaders, AKey, AValue, AEncode);
end;

function TGBClientCoreRequest.OnException(const AValue: TGBOnException): IGBClientRequest;
begin
  Result := Self;
  FOnException := AValue;
end;

function TGBClientCoreRequest.OnPreExecute(const AValue: TOnPreExecute): IGBClientRequest;
begin
  Result := Self;
  FOnPreExecute := AValue;
end;

function TGBClientCoreRequest.HeaderAddOrSet(const AKey: string; const AValue: Extended;
  const AEncode: Boolean): IGBClientRequestParams;
begin
  Result := Self;
  TGBClientCoreRequestParam.AddOrSet(FHeaders, AKey, AValue, AEncode);
end;

function TGBClientCoreRequest.HeaderAddOrSet(const AKey: string; const AValue: Integer;
  const AEncode: Boolean = True): IGBClientRequestParams;
begin
  Result := Self;
  TGBClientCoreRequestParam.AddOrSet(FHeaders, AKey, AValue, AEncode);
end;

function TGBClientCoreRequest.HeaderAddOrSet(const AKey, AValue: string;
  const AEncode: Boolean = True): IGBClientRequestParams;
begin
  Result := Self;
  TGBClientCoreRequestParam.AddOrSet(FHeaders, AKey, AValue, AEncode);
end;

function TGBClientCoreRequest.Params: IGBClientRequestParams;
begin
  Result := Self;
end;

function TGBClientCoreRequest.Password(const AValue: string): IGBClientProxy;
begin
  Result := Self;
  FProxyPassword := AValue;
end;

function TGBClientCoreRequest.PATCH: IGBClientRequest;
begin
  Result := Self;
  FMethod := gmtPATCH;
end;

function TGBClientCoreRequest.PathAddOrSet(const AKey, AValue: string): IGBClientRequestParams;
begin
  Result := Self;
  TGBClientCoreRequestParam.AddOrSet(FPaths, AKey, AValue, False);
end;

function TGBClientCoreRequest.PathAddOrSet(const AKey: string; const AValue: Integer): IGBClientRequestParams;
begin
  Result := Self;
  TGBClientCoreRequestParam.AddOrSet(FPaths, AKey, AValue, False);
end;

function TGBClientCoreRequest.PathAddOrSet(const AKey: string; const AValue: TDateTime): IGBClientRequestParams;
begin
  Result := Self;
  TGBClientCoreRequestParam.AddOrSet(FPaths, AKey, AValue, False);
end;

function TGBClientCoreRequest.Port(const AValue: Integer): IGBClientProxy;
begin
  Result := Self;
  FProxyPort := AValue;
end;

function TGBClientCoreRequest.PathAddOrSet(const AKey: string; const AValue: Extended): IGBClientRequestParams;
begin
  Result := Self;
  TGBClientCoreRequestParam.AddOrSet(FPaths, AKey, AValue, False);
end;

function TGBClientCoreRequest.POST: IGBClientRequest;
begin
  Result := Self;
  FMethod := gmtPOST;
end;

function TGBClientCoreRequest.Proxy: IGBClientProxy;
begin
  Result := Self;
end;

function TGBClientCoreRequest.PUT: IGBClientRequest;
begin
  Result := Self;
  FMethod := gmtPUT;
end;

function TGBClientCoreRequest.QueryAddOrSet(const AKey: string; const AValue: Integer; const AEncode: Boolean = True):
  IGBClientRequestParams;
begin
  Result := Self;
  TGBClientCoreRequestParam.AddOrSet(FQueries, AKey, AValue, AEncode);
  if (Assigned(FAuthorization)) and (FAuthorization.AuthType = atAWSv4) then
    FAuthorization.AWSv4.QueryAddOrSet(AKey, AValue.ToString);
end;

function TGBClientCoreRequest.QueryAddOrSet(const AKey, AValue: string; const AEncode: Boolean = True):
  IGBClientRequestParams;
begin
  Result := Self;
  TGBClientCoreRequestParam.AddOrSet(FQueries, AKey, AValue, AEncode);
  if (Assigned(FAuthorization)) and (FAuthorization.AuthType = atAWSv4) then
    FAuthorization.AWSv4.QueryAddOrSet(AKey, AValue);
end;

function TGBClientCoreRequest.QueryAddOrSet(const AKey: string; const AValue: TDateTime;
  const AEncode: Boolean = True): IGBClientRequestParams;
begin
  Result := Self;
  TGBClientCoreRequestParam.AddOrSet(FQueries, AKey, AValue, AEncode);
  if (Assigned(FAuthorization)) and (FAuthorization.AuthType = atAWSv4) then
    FAuthorization.AWSv4.QueryAddOrSet(AKey, AValue.DateTimeToIso8601);
end;

function TGBClientCoreRequest.QueryAddOrSet(const AKey: string; const AValue: Extended;
  const AEncode: Boolean = True): IGBClientRequestParams;
begin
  Result := Self;
  TGBClientCoreRequestParam.AddOrSet(FQueries, AKey, AValue, AEncode);
  if (Assigned(FAuthorization)) and (FAuthorization.AuthType = atAWSv4) then
    FAuthorization.AWSv4.QueryAddOrSet(AKey, AValue.ToString);
end;

function TGBClientCoreRequest.Resource(const AValue: string): IGBClientRequest;
begin
  Result := Self;
  FResource := AValue;
end;

function TGBClientCoreRequest.Server(const AValue: string): IGBClientProxy;
begin
  Result := Self;
  FProxyServer := AValue;
end;

function TGBClientCoreRequest.Settings: IGBClientSettings;
begin
  if not Assigned(FSettings) then
    FSettings := TGBClientCoreSettings.New(Self);
  Result := FSettings;
end;

function TGBClientCoreRequest.TimeOut(const AValue: Integer): IGBClientRequest;
begin
  Result := Self;
  FTimeOut := AValue;
end;

function TGBClientCoreRequest.Username(const AValue: string): IGBClientProxy;
begin
  Result := Self;
  FProxyUsername := AValue;
end;

end.
