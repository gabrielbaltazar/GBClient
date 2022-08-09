unit GBClient.Core.Request;

interface

{$IFDEF WEAKPACKAGEUNIT}
  {$WEAKPACKAGEUNIT ON}
{$ENDIF}

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

type
  TGBClientCoreRequest = class abstract (TInterfacedObject, IGBClientRequest,
    IGBClientProxy, IGBClientRequestParams)
  protected
    FMethod: TGBMethodType;
    FHeaders: TObjectList<TGBClientCoreRequestParam>;
    FPaths: TObjectList<TGBClientCoreRequestParam>;
    FQueries: TObjectList<TGBClientCoreRequestParam>;
    FUrlEncodedParams: TObjectList<TGBClientCoreRequestParam>;
    FFormData: TObjectList<TGBClientCoreRequestParam>;
    FFormDataStream: TDictionary<string, TStream>;
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

    function Accept(AValue: string): IGBClientRequest;
    function AcceptCharset(AValue: string): IGBClientRequest;
    function AcceptEncoding(AValue: string): IGBClientRequest;
    function ContentType(AValue: TGBContentType): IGBClientRequest; overload; virtual;
    function ContentType(AValue: string): IGBClientRequest; overload;
    function BaseURL(AValue: string): IGBClientRequest;
    function Resource(AValue: string): IGBClientRequest;
    function TimeOut(AValue: Integer): IGBClientRequest;

    {$REGION 'PARAMS'}
    function Params: IGBClientRequestParams;
    // Header Params
    function HeaderAddOrSet(AKey: string; AValue: string; AEncode: Boolean = True): IGBClientRequestParams; overload;
    function HeaderAddOrSet(AKey: string; AValue: Integer; AEncode: Boolean = True): IGBClientRequestParams; overload;
    function HeaderAddOrSet(AKey: string; AValue: Extended; AEncode: Boolean = True): IGBClientRequestParams; overload;
    function HeaderAddOrSet(AKey: string; AValue: TDateTime; AEncode: Boolean = True): IGBClientRequestParams; overload;
    // Path Params
    function PathAddOrSet(AKey: string; AValue: string): IGBClientRequestParams; overload;
    function PathAddOrSet(AKey: string; AValue: Integer): IGBClientRequestParams; overload;
    function PathAddOrSet(AKey: string; AValue: Extended): IGBClientRequestParams; overload;
    function PathAddOrSet(AKey: string; AValue: TDateTime): IGBClientRequestParams; overload;
    // Query Params
    function QueryAddOrSet(AKey: string; AValue: string; AEncode: Boolean = True): IGBClientRequestParams; overload;
    function QueryAddOrSet(AKey: string; AValue: Integer; AEncode: Boolean = True): IGBClientRequestParams; overload;
    function QueryAddOrSet(AKey: string; AValue: Extended; AEncode: Boolean = True): IGBClientRequestParams; overload;
    function QueryAddOrSet(aKey: string; AValue: TDateTime; AEncode: Boolean = True): IGBClientRequestParams; overload;
    // Body Params
    function BodyAddOrSet(AValue: string) : IGBClientRequestParams; overload;
    function BodyAddOrSet(AValue: TJSONObject; AOwner: Boolean = False): IGBClientRequestParams; overload;
    function BodyAddOrSet(AValue: TJSONArray; AOwner: Boolean = False): IGBClientRequestParams; overload;
    function BodyAddOrSet(AValue: TObject; AOwner: Boolean = False): IGBClientRequestParams; overload;
    function BodyAddOrSet(AValue: TList<TObject>; AOwner: Boolean = False): IGBClientRequestParams; overload;
    function BodyAddOrSet(AValue: TDataSet; ACurrent: Boolean = True): IGBClientRequestParams; overload;
    function BodyAddOrSet(AKey, AValue: string): IGBClientRequestParams; overload;
    function BodyBinary(AFileName: string): IGBClientRequestParams; overload;
    function BodyBinary(AStream : TStream; AOwner: Boolean = False): IGBClientRequestParams; overload;
    function FormDataAddOrSet(AKey: string; AValue: string; AEncode: Boolean = True): IGBClientRequestParams; overload;
    function FormDataAddOrSet(AKey: string; AValue: Integer; AEncode: Boolean = True): IGBClientRequestParams; overload;
    function FormDataAddOrSet(AKey: string; AValue: Extended; AEncode: Boolean = True): IGBClientRequestParams; overload;
    function FormDataAddOrSet(AKey: string; AValue: TDateTime; AEncode: Boolean = True): IGBClientRequestParams; overload;
    function FormDataAddFile(AKey: string; AValue: TStream): IGBClientRequestParams; overload;
    function FormDataAddFile(AKey: string; AFileName: string): IGBClientRequestParams; overload;
    function GetBody: string;
    {$ENDREGION}

    {$REGION 'PROXY'}
    function Proxy: IGBClientProxy;
    function Server(AValue: string): IGBClientProxy;
    function Port(AValue: Integer): IGBClientProxy;
    function Username(AValue: string): IGBClientProxy;
    function Password(AValue: string): IGBClientProxy;
    {$ENDREGION}

    function &End: IGBClientRequest;
    function Send: IGBClientResponse; virtual; abstract;
    function Response : IGBClientResponse; virtual; abstract;
    function Settings: IGBClientSettings;
    function OnException (AValue: TGBOnException): IGBClientRequest;
    function OnPreExecute(AValue: TOnPreExecute): IGBClientRequest;
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

function TGBClientCoreRequest.FormDataAddFile(AKey, AFileName: string): IGBClientRequestParams;
var
  LMemoryStream: TMemoryStream;
begin
  Result := Self;
  ContentType(ctMultipart_form_data);
  LMemoryStream := TMemoryStream.Create;
  try
    LMemoryStream.LoadFromFile(AFileName);
    FormDataAddFile(AKey, LMemoryStream);
  finally
    LMemoryStream.Free;
  end;
end;

function TGBClientCoreRequest.FormDataAddFile(AKey: string; AValue: TStream): IGBClientRequestParams;
var
  LMemoryStream: TMemoryStream;
begin
  Result := Self;
  if FFormDataStream.ContainsKey(AKey) then
    Exit;

  ContentType(ctMultipart_form_data);
  LMemoryStream := TMemoryStream.Create;
  try
    AValue.Position := 0;
    LMemoryStream.LoadFromStream(AValue);
    FFormDataStream.Add(AKey, LMemoryStream);
  except
    LMemoryStream.Free;
    raise;
  end;
end;

function TGBClientCoreRequest.FormDataAddOrSet(AKey, AValue: string; AEncode: Boolean): IGBClientRequestParams;
begin
  Result := Self;
  ContentType(ctMultipart_form_data);
  TGBClientCoreRequestParam.AddOrSet(FFormData, AKey, AValue, AEncode);
end;

function TGBClientCoreRequest.FormDataAddOrSet(AKey: string; AValue: TDateTime; AEncode: Boolean): IGBClientRequestParams;
begin
  Result := Self;
  ContentType(ctMultipart_form_data);
  TGBClientCoreRequestParam.AddOrSet(FFormData, AKey, AValue, AEncode);
end;

function TGBClientCoreRequest.FormDataAddOrSet(AKey: string; AValue: Extended; AEncode: Boolean): IGBClientRequestParams;
begin
  Result := Self;
  ContentType(ctMultipart_form_data);
  TGBClientCoreRequestParam.AddOrSet(FFormData, AKey, AValue, AEncode);
end;

function TGBClientCoreRequest.FormDataAddOrSet(AKey: string; AValue: Integer; AEncode: Boolean): IGBClientRequestParams;
begin
  Result := Self;
  ContentType(ctMultipart_form_data);
  TGBClientCoreRequestParam.AddOrSet(FFormData, AKey, AValue, AEncode);
end;

function TGBClientCoreRequest.Accept(AValue: string): IGBClientRequest;
begin
  Result := Self;
  HeaderAddOrSet('Accept', AValue, False);
end;

function TGBClientCoreRequest.AcceptCharset(AValue: string): IGBClientRequest;
begin
  Result := Self;
  HeaderAddOrSet('Accept-Charset', AValue);
end;

function TGBClientCoreRequest.AcceptEncoding(AValue: string): IGBClientRequest;
begin
  Result := Self;
  HeaderAddOrSet('Accept-Encoding', AValue);
end;

function TGBClientCoreRequest.BaseURL(AValue: string): IGBClientRequest;
begin
  Result := Self;
  FBaseUrl := AValue;
end;

function TGBClientCoreRequest.BodyAddOrSet(AValue: TObject; AOwner: Boolean): IGBClientRequestParams;
var
  LParse: TGBOnParseObjectToJSON;
  LJSON: TJSONObject;
begin
  Result := Self;
  LParse := Settings.OnParseObjectToJSON;
  LJSON := LParse(AValue);
  try
    BodyAddOrSet(LJSON, False);
    if AOwner then
      FreeAndNil(AValue);
  finally
    LJSON.Free;
  end;
end;

function TGBClientCoreRequest.BodyAddOrSet(AValue: TList<TObject>; AOwner: Boolean): IGBClientRequestParams;
var
  LParse: TGBOnParseObjectToJSON;
  LJsonArray: TJSONArray;
  I: Integer;
begin
  Result := Self;
  LParse := Settings.OnParseObjectToJSON;
  LJsonArray := TJSONArray.Create;
  try
    for I := 0 to Pred(AValue.Count) do
      LJsonArray.AddElement(LParse(AValue[I]));
    BodyAddOrSet(LJsonArray);
  finally
    LJsonArray.Free;
    if AOwner then
      FreeAndNil(AValue);
  end;
end;

function TGBClientCoreRequest.BodyAddOrSet(AKey, AValue: string): IGBClientRequestParams;
begin
  Result := Self;
  TGBClientCoreRequestParam.AddOrSet(FUrlEncodedParams, AKey, AValue);
  ContentType(TGBContentType.ctApplication_x_www_form_urlencoded);
end;

function TGBClientCoreRequest.BodyAddOrSet(AValue: TDataSet; ACurrent: Boolean): IGBClientRequestParams;
begin
  Result := Self;
  if ACurrent then
    BodyAddOrSet(AValue.ToJSONObject, True)
  else
    BodyAddOrSet(AValue.ToJSONArray, True);
end;

function TGBClientCoreRequest.BodyAddOrSet(AValue: string): IGBClientRequestParams;
begin
  Result := Self;
  FreeAndNil(FBody);
  FBody := TStringStream.Create(AValue, TEncoding.UTF8);
  FBody.Position := 0;
end;

function TGBClientCoreRequest.BodyAddOrSet(AValue: TJSONObject; AOwner: Boolean): IGBClientRequestParams;
begin
  Result := Self;
  BodyAddOrSet(AValue.ToString);
  ContentType(TGBContentType.ctApplicationJson);
  if AOwner then
    FreeAndNil(AValue);
end;

function TGBClientCoreRequest.BodyAddOrSet(AValue: TJSONArray; AOwner: Boolean): IGBClientRequestParams;
begin
  Result := Self;
  BodyAddOrSet(AValue.ToString);
  ContentType(TGBContentType.ctApplicationJson);
  if AOwner then
    FreeAndNil(AValue);
end;

function TGBClientCoreRequest.BodyBinary(AFileName: string): IGBClientRequestParams;
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

function TGBClientCoreRequest.BodyBinary(AStream: TStream; AOwner: Boolean): IGBClientRequestParams;
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

function TGBClientCoreRequest.ContentType(AValue: TGBContentType): IGBClientRequest;
begin
  Result := Self;
  ContentType(AValue.value);
end;

procedure TGBClientCoreRequest.Clear;
var
  LKey: string;
begin
  FHeaders.Clear;
  FPaths.Clear;
  FQueries.Clear;
  FUrlEncodedParams.Clear;
  FreeAndNil(FBody);
  for LKey in FFormDataStream.Keys do
    FFormDataStream.ExtractPair(LKey).Value.Free;
end;

function TGBClientCoreRequest.ContentType(AValue: string): IGBClientRequest;
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
  FFormData := TObjectList<TGBClientCoreRequestParam>.Create;
  FFormDataStream := TDictionary<string, TStream>.Create;
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
  FFormData.Free;
  FFormDataStream.Free;
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

function TGBClientCoreRequest.HeaderAddOrSet(AKey: string; AValue: TDateTime; AEncode: Boolean): IGBClientRequestParams;
begin
  Result := Self;
  TGBClientCoreRequestParam.AddOrSet(FHeaders, AKey, AValue, AEncode);
end;

function TGBClientCoreRequest.OnException(AValue: TGBOnException): IGBClientRequest;
begin
  Result := Self;
  FOnException := AValue;
end;

function TGBClientCoreRequest.OnPreExecute(AValue: TOnPreExecute): IGBClientRequest;
begin
  Result := Self;
  FOnPreExecute := AValue;
end;

function TGBClientCoreRequest.HeaderAddOrSet(AKey: string; AValue: Extended; AEncode: Boolean): IGBClientRequestParams;
begin
  Result := Self;
  TGBClientCoreRequestParam.AddOrSet(FHeaders, AKey, AValue, AEncode);
end;

function TGBClientCoreRequest.HeaderAddOrSet(AKey: string; AValue: Integer; AEncode: Boolean): IGBClientRequestParams;
begin
  Result := Self;
  TGBClientCoreRequestParam.AddOrSet(FHeaders, AKey, AValue, AEncode);
end;

function TGBClientCoreRequest.HeaderAddOrSet(AKey, AValue: string; AEncode: Boolean): IGBClientRequestParams;
begin
  Result := Self;
  TGBClientCoreRequestParam.AddOrSet(FHeaders, AKey, AValue, AEncode);
end;

function TGBClientCoreRequest.Params: IGBClientRequestParams;
begin
  Result := Self;
end;

function TGBClientCoreRequest.Password(AValue: string): IGBClientProxy;
begin
  Result := Self;
  FProxyPassword := AValue;
end;

function TGBClientCoreRequest.PATCH: IGBClientRequest;
begin
  Result := Self;
  FMethod := gmtPATCH;
end;

function TGBClientCoreRequest.PathAddOrSet(AKey, AValue: string): IGBClientRequestParams;
begin
  Result := Self;
  TGBClientCoreRequestParam.AddOrSet(FPaths, AKey, AValue, False);
end;

function TGBClientCoreRequest.PathAddOrSet(AKey: string; AValue: Integer): IGBClientRequestParams;
begin
  Result := Self;
  TGBClientCoreRequestParam.AddOrSet(FPaths, AKey, AValue, False);
end;

function TGBClientCoreRequest.PathAddOrSet(AKey: string; AValue: TDateTime): IGBClientRequestParams;
begin
  Result := Self;
  TGBClientCoreRequestParam.AddOrSet(FPaths, AKey, AValue, False);
end;

function TGBClientCoreRequest.Port(AValue: Integer): IGBClientProxy;
begin
  Result := Self;
  FProxyPort := AValue;
end;

function TGBClientCoreRequest.PathAddOrSet(AKey: string; AValue: Extended): IGBClientRequestParams;
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

function TGBClientCoreRequest.QueryAddOrSet(AKey: string; AValue: Integer; AEncode: Boolean = True): IGBClientRequestParams;
begin
  Result := Self;
  TGBClientCoreRequestParam.AddOrSet(FQueries, AKey, AValue, AEncode);
  if (Assigned(FAuthorization)) and (FAuthorization.AuthType = atAWSv4) then
    FAuthorization.AWSv4.QueryAddOrSet(AKey, AValue.ToString);
end;

function TGBClientCoreRequest.QueryAddOrSet(AKey, AValue: string; AEncode: Boolean = True): IGBClientRequestParams;
begin
  Result := Self;
  TGBClientCoreRequestParam.AddOrSet(FQueries, AKey, AValue, AEncode);
  if (Assigned(FAuthorization)) and (FAuthorization.AuthType = atAWSv4) then
    FAuthorization.AWSv4.QueryAddOrSet(AKey, AValue);
end;

function TGBClientCoreRequest.QueryAddOrSet(aKey: string; AValue: TDateTime; AEncode: Boolean = True): IGBClientRequestParams;
begin
  Result := Self;
  TGBClientCoreRequestParam.AddOrSet(FQueries, aKey, AValue, AEncode);
  if (Assigned(FAuthorization)) and (FAuthorization.AuthType = atAWSv4) then
    FAuthorization.AWSv4.QueryAddOrSet(aKey, AValue.DateTimeToIso8601);
end;

function TGBClientCoreRequest.QueryAddOrSet(AKey: string; AValue: Extended; AEncode: Boolean = True): IGBClientRequestParams;
begin
  Result := Self;
  TGBClientCoreRequestParam.AddOrSet(FQueries, AKey, AValue, AEncode);
  if (Assigned(FAuthorization)) and (FAuthorization.AuthType = atAWSv4) then
    FAuthorization.AWSv4.QueryAddOrSet(AKey, AValue.ToString);
end;

function TGBClientCoreRequest.Resource(AValue: string): IGBClientRequest;
begin
  Result := Self;
  FResource := AValue;
end;

function TGBClientCoreRequest.Server(AValue: string): IGBClientProxy;
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

function TGBClientCoreRequest.TimeOut(AValue: Integer): IGBClientRequest;
begin
  Result := Self;
  FTimeOut := AValue;
end;

function TGBClientCoreRequest.Username(AValue: string): IGBClientProxy;
begin
  Result := Self;
  FProxyUsername := AValue;
end;

end.
