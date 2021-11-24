unit GBClient.Core.Request;

interface

uses
  GBClient.Interfaces,
  GBClient.Core.Types,
  GBClient.Core.Helpers,
  GBClient.Core.Settings,
  GBClient.Core.Request.Param,
  Data.DB,
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.Generics.Collections;

type TGBClientCoreRequest = class abstract (TInterfacedObject, IGBClientRequest,
                                                               IGBClientProxy,
                                                               IGBClientRequestParams)

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
    FProxyUsername: String;
    FProxyPassword: String;

    FSettings: IGBClientSettings;

    FOnException: TGBOnException;
    FOnPreExecute: TOnPreExecute;

    function GetFullUrl: String;

    procedure Clear;
    function Component: TComponent; virtual; abstract;
    function Authorization: IGBClientAuth; virtual; abstract;

    function POST: IGBClientRequest;
    function PUT: IGBClientRequest;
    function GET: IGBClientRequest;
    function DELETE: IGBClientRequest;
    function PATCH: IGBClientRequest;

    function Accept(Value: string): IGBClientRequest;
    function AcceptCharset(Value: string): IGBClientRequest;
    function AcceptEncoding(Value: string): IGBClientRequest;
    function ContentType(Value: TGBContentType): IGBClientRequest; overload; virtual;
    function ContentType(Value: String): IGBClientRequest; overload;
    function BaseURL(Value: String): IGBClientRequest;
    function Resource(Value: String): IGBClientRequest;
    function TimeOut(Value: Integer): IGBClientRequest;

    {$REGION 'PARAMS'}
    function Params: IGBClientRequestParams;

    // Header Params
    function HeaderAddOrSet(Key: string; Value: String; bEncode: Boolean = True): IGBClientRequestParams; overload;
    function HeaderAddOrSet(Key: string; Value: Integer; bEncode: Boolean = True): IGBClientRequestParams; overload;
    function HeaderAddOrSet(Key: string; Value: Extended; bEncode: Boolean = True): IGBClientRequestParams; overload;
    function HeaderAddOrSet(Key: string; Value: TDateTime; bEncode: Boolean = True): IGBClientRequestParams; overload;

    // Path Params
    function PathAddOrSet(Key: string; Value: String): IGBClientRequestParams; overload;
    function PathAddOrSet(Key: string; Value: Integer): IGBClientRequestParams; overload;
    function PathAddOrSet(Key: string; Value: Extended): IGBClientRequestParams; overload;
    function PathAddOrSet(Key: string; Value: TDateTime): IGBClientRequestParams; overload;

    // Query Params
    function QueryAddOrSet(Key: string; Value: String; bEncode: Boolean = True): IGBClientRequestParams; overload;
    function QueryAddOrSet(Key: string; Value: Integer; bEncode: Boolean = True): IGBClientRequestParams; overload;
    function QueryAddOrSet(Key: string; Value: Extended; bEncode: Boolean = True): IGBClientRequestParams; overload;
    function QueryAddOrSet(Key: string; Value: TDateTime; bEncode: Boolean = True): IGBClientRequestParams; overload;

    // Body Params
    function BodyAddOrSet(Value: String) : IGBClientRequestParams; overload;
    function BodyAddOrSet(Value: TJSONObject; AOwner: Boolean = False): IGBClientRequestParams; overload;
    function BodyAddOrSet(Value: TJSONArray; AOwner: Boolean = False): IGBClientRequestParams; overload;
    function BodyAddOrSet(Value: TObject; AOwner: Boolean = False): IGBClientRequestParams; overload;
    function BodyAddOrSet(Value: TList<TObject>; AOwner: Boolean = False): IGBClientRequestParams; overload;
    function BodyAddOrSet(Value: TDataSet; ACurrent: Boolean = True): IGBClientRequestParams; overload;
    function BodyAddOrSet(Key, Value: String): IGBClientRequestParams; overload;

    function BodyBinary(AFileName: String): IGBClientRequestParams; overload;
    function BodyBinary(AStream : TStream; AOwner: Boolean = False): IGBClientRequestParams; overload;

    function GetBody: String;
    {$ENDREGION}

    {$REGION 'PROXY'}
    function Proxy: IGBClientProxy;
    function Server(Value: String): IGBClientProxy;
    function Port(Value: Integer): IGBClientProxy;
    function Username(Value: String): IGBClientProxy;
    function Password(Value: String): IGBClientProxy;
    {$ENDREGION}

    function &End: IGBClientRequest;
    function Send: IGBClientResponse; virtual; abstract;
    function Response : IGBClientResponse; virtual; abstract;

    function Settings: IGBClientSettings;

    function OnException (Value: TGBOnException): IGBClientRequest;
    function OnPreExecute(Value: TOnPreExecute): IGBClientRequest;
  public
    constructor create; virtual;
    destructor Destroy; override;
end;

implementation

{ TGBClientCoreRequest }

function TGBClientCoreRequest.&End: IGBClientRequest;
begin
  result := Self;
end;

function TGBClientCoreRequest.Accept(Value: string): IGBClientRequest;
begin
  result := Self;
  HeaderAddOrSet('Accept', Value, False);
end;

function TGBClientCoreRequest.AcceptCharset(Value: string): IGBClientRequest;
begin
  result := Self;
  HeaderAddOrSet('Accept-Charset', Value);
end;

function TGBClientCoreRequest.AcceptEncoding(Value: string): IGBClientRequest;
begin
  result := Self;
  HeaderAddOrSet('Accept-Encoding', Value);
end;

function TGBClientCoreRequest.BaseURL(Value: String): IGBClientRequest;
begin
  result := Self;
  FBaseUrl := Value;
end;

function TGBClientCoreRequest.BodyAddOrSet(Value: TObject; AOwner: Boolean): IGBClientRequestParams;
var
  parse : TGBOnParseObjectToJSON;
  json: TJSONObject;
begin
  result := Self;
  parse := Settings.OnParseObjectToJSON;
  json := parse(Value);
  try
    BodyAddOrSet(json, False);

    if AOwner then
      FreeAndNil(Value);
  finally
    json.Free;
  end;
end;

function TGBClientCoreRequest.BodyAddOrSet(Value: TList<TObject>; AOwner: Boolean): IGBClientRequestParams;
var
  parse: TGBOnParseObjectToJSON;
  jsonArray: TJSONArray;
  i: Integer;
begin
  result := Self;
  parse := Settings.OnParseObjectToJSON;

  jsonArray := TJSONArray.Create;
  try
    for i := 0 to Pred(Value.Count) do
      jsonArray.AddElement(parse(Value[i]));

    BodyAddOrSet(jsonArray);
  finally
    jsonArray.Free;
    if AOwner then
      FreeAndNil(Value);
  end;
end;

function TGBClientCoreRequest.BodyAddOrSet(Key, Value: String): IGBClientRequestParams;
begin
  result := Self;
  TGBClientCoreRequestParam.AddOrSet(FUrlEncodedParams, Key, Value);
  ContentType(TGBContentType.ctApplication_x_www_form_urlencoded);
end;

function TGBClientCoreRequest.BodyAddOrSet(Value: TDataSet; ACurrent: Boolean): IGBClientRequestParams;
begin
  result := Self;
  if ACurrent then
    BodyAddOrSet(Value.ToJSONObject, True)
  else
    BodyAddOrSet(Value.ToJSONArray, True);
end;

function TGBClientCoreRequest.BodyAddOrSet(Value: String): IGBClientRequestParams;
begin
  result := Self;
  FreeAndNil(FBody);
  FBody := TStringStream.Create(Value);
  FBody.Position := 0;
end;

function TGBClientCoreRequest.BodyAddOrSet(Value: TJSONObject; AOwner: Boolean): IGBClientRequestParams;
begin
  result := Self;
  BodyAddOrSet(Value.ToString);
  ContentType(TGBContentType.ctApplicationJson);

  if AOwner then
    FreeAndNil(Value);
end;

function TGBClientCoreRequest.BodyAddOrSet(Value: TJSONArray; AOwner: Boolean): IGBClientRequestParams;
begin
  result := Self;
  BodyAddOrSet(Value.ToString);
  ContentType(TGBContentType.ctApplicationJson);

  if AOwner then
    FreeAndNil(Value);
end;

function TGBClientCoreRequest.BodyBinary(AFileName: String): IGBClientRequestParams;
begin
  result := Self;
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

function TGBClientCoreRequest.BodyBinary(AStream: TStream; AOwner: Boolean): IGBClientRequestParams;
begin
  result := Self;
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

function TGBClientCoreRequest.ContentType(Value: TGBContentType): IGBClientRequest;
begin
  result := Self;
  ContentType(Value.value);
end;

procedure TGBClientCoreRequest.Clear;
begin
  FHeaders.Clear;
  FPaths.Clear;
  FQueries.Clear;
  FUrlEncodedParams.Clear;
  FreeAndNil(FBody);
end;

function TGBClientCoreRequest.ContentType(Value: String): IGBClientRequest;
begin
  result := Self;
  HeaderAddOrSet('content-type', Value, False);
end;

constructor TGBClientCoreRequest.create;
begin
  FHeaders := TObjectList<TGBClientCoreRequestParam>.create;
  FPaths := TObjectList<TGBClientCoreRequestParam>.create;
  FQueries := TObjectList<TGBClientCoreRequestParam>.create;
  FUrlEncodedParams := TObjectList<TGBClientCoreRequestParam>.create;

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
  result := Self;
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
  result := Self;
  FMethod := gmtGET;
end;

function TGBClientCoreRequest.GetBody: String;
var
  bodyStream: TStringStream;
begin
  if Assigned(FBody) then
  begin
    bodyStream := TStringStream.Create;
    try
      bodyStream.LoadFromStream(FBody);
      result := bodyStream.DataString;
    finally
      bodyStream.Free;
    end;
  end;
end;

function TGBClientCoreRequest.GetFullUrl: String;
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

function TGBClientCoreRequest.HeaderAddOrSet(Key: string; Value: TDateTime; bEncode: Boolean): IGBClientRequestParams;
begin
  Result := Self;
  TGBClientCoreRequestParam.AddOrSet(FHeaders, Key, Value, bEncode);
end;

function TGBClientCoreRequest.OnException(Value: TGBOnException): IGBClientRequest;
begin
  result := Self;
  FOnException := Value;
end;

function TGBClientCoreRequest.OnPreExecute(Value: TOnPreExecute): IGBClientRequest;
begin
  result := Self;
  FOnPreExecute := Value;
end;

function TGBClientCoreRequest.HeaderAddOrSet(Key: string; Value: Extended; bEncode: Boolean): IGBClientRequestParams;
begin
  Result := Self;
  TGBClientCoreRequestParam.AddOrSet(FHeaders, Key, Value, bEncode);
end;

function TGBClientCoreRequest.HeaderAddOrSet(Key: string; Value: Integer; bEncode: Boolean): IGBClientRequestParams;
begin
  Result := Self;
  TGBClientCoreRequestParam.AddOrSet(FHeaders, Key, Value, bEncode);
end;

function TGBClientCoreRequest.HeaderAddOrSet(Key, Value: String; bEncode: Boolean): IGBClientRequestParams;
begin
  Result := Self;
  TGBClientCoreRequestParam.AddOrSet(FHeaders, Key, Value, bEncode);
end;

function TGBClientCoreRequest.Params: IGBClientRequestParams;
begin
  result := Self;
end;

function TGBClientCoreRequest.Password(Value: String): IGBClientProxy;
begin
  result := Self;
  FProxyPassword := Value;
end;

function TGBClientCoreRequest.PATCH: IGBClientRequest;
begin
  result := Self;
  FMethod := gmtPATCH;
end;

function TGBClientCoreRequest.PathAddOrSet(Key, Value: String): IGBClientRequestParams;
begin
  Result := Self;
  TGBClientCoreRequestParam.AddOrSet(FPaths, Key, Value, False);
end;

function TGBClientCoreRequest.PathAddOrSet(Key: string; Value: Integer): IGBClientRequestParams;
begin
  Result := Self;
  TGBClientCoreRequestParam.AddOrSet(FPaths, Key, Value, False);
end;

function TGBClientCoreRequest.PathAddOrSet(Key: string; Value: TDateTime): IGBClientRequestParams;
begin
  Result := Self;
  TGBClientCoreRequestParam.AddOrSet(FPaths, Key, Value, False);
end;

function TGBClientCoreRequest.Port(Value: Integer): IGBClientProxy;
begin
  result := Self;
  FProxyPort := Value;
end;

function TGBClientCoreRequest.PathAddOrSet(Key: string; Value: Extended): IGBClientRequestParams;
begin
  Result := Self;
  TGBClientCoreRequestParam.AddOrSet(FPaths, Key, Value, False);
end;

function TGBClientCoreRequest.POST: IGBClientRequest;
begin
  result := Self;
  FMethod := gmtPOST;
end;

function TGBClientCoreRequest.Proxy: IGBClientProxy;
begin
  result := Self;
end;

function TGBClientCoreRequest.PUT: IGBClientRequest;
begin
  result := Self;
  FMethod := gmtPUT;
end;

function TGBClientCoreRequest.QueryAddOrSet(Key: string; Value: Integer; bEncode: Boolean = True): IGBClientRequestParams;
begin
  Result := Self;
  TGBClientCoreRequestParam.AddOrSet(FQueries, Key, Value, bEncode);

  if (Assigned(FAuthorization)) and (FAuthorization.AuthType = atAWSv4) then
    FAuthorization.AWSv4.QueryAddOrSet(Key, Value.ToString);
end;

function TGBClientCoreRequest.QueryAddOrSet(Key, Value: String; bEncode: Boolean = True): IGBClientRequestParams;
begin
  Result := Self;
  TGBClientCoreRequestParam.AddOrSet(FQueries, Key, Value, bEncode);

  if (Assigned(FAuthorization)) and (FAuthorization.AuthType = atAWSv4) then
    FAuthorization.AWSv4.QueryAddOrSet(Key, Value);
end;

function TGBClientCoreRequest.QueryAddOrSet(Key: string; Value: TDateTime; bEncode: Boolean = True): IGBClientRequestParams;
begin
  Result := Self;
  TGBClientCoreRequestParam.AddOrSet(FQueries, Key, Value, bEncode);

  if (Assigned(FAuthorization)) and (FAuthorization.AuthType = atAWSv4) then
    FAuthorization.AWSv4.QueryAddOrSet(Key, Value.DateTimeToIso8601);
end;

function TGBClientCoreRequest.QueryAddOrSet(Key: string; Value: Extended; bEncode: Boolean = True): IGBClientRequestParams;
begin
  Result := Self;
  TGBClientCoreRequestParam.AddOrSet(FQueries, Key, Value, bEncode);

  if (Assigned(FAuthorization)) and (FAuthorization.AuthType = atAWSv4) then
    FAuthorization.AWSv4.QueryAddOrSet(Key, Value.ToString);
end;

function TGBClientCoreRequest.Resource(Value: String): IGBClientRequest;
begin
  result := Self;
  FResource := Value;
end;

function TGBClientCoreRequest.Server(Value: String): IGBClientProxy;
begin
  result := Self;
  FProxyServer := Value;
end;

function TGBClientCoreRequest.Settings: IGBClientSettings;
begin
  if not Assigned(FSettings) then
    FSettings := TGBClientCoreSettings.New(Self);
  result := FSettings;
end;

function TGBClientCoreRequest.TimeOut(Value: Integer): IGBClientRequest;
begin
  result := Self;
  FTimeOut := Value;
end;

function TGBClientCoreRequest.Username(Value: String): IGBClientProxy;
begin
  result := Self;
  FProxyUsername := Value;
end;

end.
