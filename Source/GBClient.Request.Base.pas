unit GBClient.Request.Base;

interface

uses
  GBClient.Interfaces,
  GBClient.Types,
  GBClient.Helpers,
  GBClient.Settings.Default,
  GBClient.Request.Base.Param,
  Data.DB,
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.Generics.Collections;

type TGBClientRequestBase = class abstract (TInterfacedObject, IGBClientRequest,
                                                               IGBClientRequestParams)

  protected
    FMethod: TGBMethodType;
    FHeaders: TObjectList<TGBClientRequestBaseParam>;
    FPaths: TObjectList<TGBClientRequestBaseParam>;
    FQueries: TObjectList<TGBClientRequestBaseParam>;
    FUrlEncodedParams: TObjectList<TGBClientRequestBaseParam>;
    FBody: TStream;
    FBaseUrl: string;
    FResource: string;
    FTimeOut: Integer;

    FSettings: IGBClientSettings;

    FOnException: TGBOnException;
    FOnPreExecute: TOnPreExecute;

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
    function ContentType(Value: TGBContentType): IGBClientRequest; overload;
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
    function QueryAddOrSet(Key: string; Value: String): IGBClientRequestParams; overload;
    function QueryAddOrSet(Key: string; Value: Integer): IGBClientRequestParams; overload;
    function QueryAddOrSet(Key: string; Value: Extended): IGBClientRequestParams; overload;
    function QueryAddOrSet(Key: string; Value: TDateTime): IGBClientRequestParams; overload;

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
    {$ENDREGION}

    function &End: IGBClientRequest;
    function Send: IGBClientResponse; virtual; abstract;
    function Response : IGBClientResponse; virtual; abstract;

    function Settings: IGBClientSettings;

    function OnException (Value: TGBOnException): IGBClientRequest;
    function OnPreExecute(Value: TOnPreExecute): IGBClientRequest;
  public
    constructor create;
    destructor Destroy; override;
end;

implementation

{ TGBClientRequestBase }

function TGBClientRequestBase.&End: IGBClientRequest;
begin
  result := Self;
end;

function TGBClientRequestBase.Accept(Value: string): IGBClientRequest;
begin
  result := Self;
  HeaderAddOrSet('Accept', Value);
end;

function TGBClientRequestBase.AcceptCharset(Value: string): IGBClientRequest;
begin
  result := Self;
  HeaderAddOrSet('Accept-Charset', Value);
end;

function TGBClientRequestBase.AcceptEncoding(Value: string): IGBClientRequest;
begin
  result := Self;
  HeaderAddOrSet('Accept-Encoding', Value);
end;

function TGBClientRequestBase.BaseURL(Value: String): IGBClientRequest;
begin
  result := Self;
  FBaseUrl := Value;
end;

function TGBClientRequestBase.BodyAddOrSet(Value: TObject; AOwner: Boolean): IGBClientRequestParams;
var
  parse : TGBOnParseObjectToJSON;
  json: TJSONObject;
begin
  result := Self;
  parse := Settings.OnParseObjectToJSON;
  json := parse(Value);
  try
    BodyAddOrSet(parse(Value));

    if AOwner then
      FreeAndNil(Value);
  finally
    json.Free;
  end;
end;

function TGBClientRequestBase.BodyAddOrSet(Value: TList<TObject>; AOwner: Boolean): IGBClientRequestParams;
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

function TGBClientRequestBase.BodyAddOrSet(Key, Value: String): IGBClientRequestParams;
begin
  result := Self;
  TGBClientRequestBaseParam.AddOrSet(FUrlEncodedParams, Key, Value);
end;

function TGBClientRequestBase.BodyAddOrSet(Value: TDataSet; ACurrent: Boolean): IGBClientRequestParams;
begin
  result := Self;
  if ACurrent then
    BodyAddOrSet(Value.ToJSONObject, True)
  else
    BodyAddOrSet(Value.ToJSONArray, True);
end;

function TGBClientRequestBase.BodyAddOrSet(Value: String): IGBClientRequestParams;
begin
  result := Self;
  FreeAndNil(FBody);
  FBody := TStringStream.Create(Value);
  FBody.Position := 0;
end;

function TGBClientRequestBase.BodyAddOrSet(Value: TJSONObject; AOwner: Boolean): IGBClientRequestParams;
begin
  result := Self;
  BodyAddOrSet(Value.ToString);
  ContentType(TGBContentType.ctApplicationJson);

  if AOwner then
    FreeAndNil(Value);
end;

function TGBClientRequestBase.BodyAddOrSet(Value: TJSONArray; AOwner: Boolean): IGBClientRequestParams;
begin
  result := Self;
  BodyAddOrSet(Value.ToString);
  ContentType(TGBContentType.ctApplicationJson);

  if AOwner then
    FreeAndNil(Value);
end;

function TGBClientRequestBase.BodyBinary(AFileName: String): IGBClientRequestParams;
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

function TGBClientRequestBase.BodyBinary(AStream: TStream; AOwner: Boolean): IGBClientRequestParams;
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

function TGBClientRequestBase.ContentType(Value: TGBContentType): IGBClientRequest;
begin
  result := Self;
  ContentType(Value.value);
end;

procedure TGBClientRequestBase.Clear;
begin
  FHeaders.Clear;
  FPaths.Clear;
  FQueries.Clear;
  FUrlEncodedParams.Clear;
  FreeAndNil(FBody);
end;

function TGBClientRequestBase.ContentType(Value: String): IGBClientRequest;
begin
  result := Self;
  HeaderAddOrSet('content-type', Value);
end;

constructor TGBClientRequestBase.create;
begin
  FHeaders := TObjectList<TGBClientRequestBaseParam>.create;
  FPaths := TObjectList<TGBClientRequestBaseParam>.create;
  FQueries := TObjectList<TGBClientRequestBaseParam>.create;
  FUrlEncodedParams := TObjectList<TGBClientRequestBaseParam>.create;

  FMethod := gmtGET;
  AcceptCharset('utf-8, *;q=0.8');
  ContentType('application/json');
end;

function TGBClientRequestBase.DELETE: IGBClientRequest;
begin
  result := Self;
  FMethod := gmtDELETE;
end;

destructor TGBClientRequestBase.Destroy;
begin
  FHeaders.Free;
  FPaths.Free;
  FQueries.Free;
  FUrlEncodedParams.Free;
  inherited;
end;

function TGBClientRequestBase.GET: IGBClientRequest;
begin
  result := Self;
  FMethod := gmtGET;
end;

function TGBClientRequestBase.HeaderAddOrSet(Key: string; Value: TDateTime; bEncode: Boolean): IGBClientRequestParams;
begin
  Result := Self;
  TGBClientRequestBaseParam.AddOrSet(FHeaders, Key, Value, bEncode);
end;

function TGBClientRequestBase.OnException(Value: TGBOnException): IGBClientRequest;
begin
  result := Self;
  FOnException := Value;
end;

function TGBClientRequestBase.OnPreExecute(Value: TOnPreExecute): IGBClientRequest;
begin
  result := Self;
  FOnPreExecute := Value;
end;

function TGBClientRequestBase.HeaderAddOrSet(Key: string; Value: Extended; bEncode: Boolean): IGBClientRequestParams;
begin
  Result := Self;
  TGBClientRequestBaseParam.AddOrSet(FHeaders, Key, Value, bEncode);
end;

function TGBClientRequestBase.HeaderAddOrSet(Key: string; Value: Integer; bEncode: Boolean): IGBClientRequestParams;
begin
  Result := Self;
  TGBClientRequestBaseParam.AddOrSet(FHeaders, Key, Value, bEncode);
end;

function TGBClientRequestBase.HeaderAddOrSet(Key, Value: String; bEncode: Boolean): IGBClientRequestParams;
begin
  Result := Self;
  TGBClientRequestBaseParam.AddOrSet(FHeaders, Key, Value, bEncode);
end;

function TGBClientRequestBase.Params: IGBClientRequestParams;
begin
  result := Self;
end;

function TGBClientRequestBase.PATCH: IGBClientRequest;
begin
  result := Self;
  FMethod := gmtPATCH;
end;

function TGBClientRequestBase.PathAddOrSet(Key, Value: String): IGBClientRequestParams;
begin
  Result := Self;
  TGBClientRequestBaseParam.AddOrSet(FPaths, Key, Value, False);
end;

function TGBClientRequestBase.PathAddOrSet(Key: string; Value: Integer): IGBClientRequestParams;
begin
  Result := Self;
  TGBClientRequestBaseParam.AddOrSet(FPaths, Key, Value, False);
end;

function TGBClientRequestBase.PathAddOrSet(Key: string; Value: TDateTime): IGBClientRequestParams;
begin
  Result := Self;
  TGBClientRequestBaseParam.AddOrSet(FPaths, Key, Value, False);
end;

function TGBClientRequestBase.PathAddOrSet(Key: string; Value: Extended): IGBClientRequestParams;
begin
  Result := Self;
  TGBClientRequestBaseParam.AddOrSet(FPaths, Key, Value, False);
end;

function TGBClientRequestBase.POST: IGBClientRequest;
begin
  result := Self;
  FMethod := gmtPOST;
end;

function TGBClientRequestBase.PUT: IGBClientRequest;
begin
  result := Self;
  FMethod := gmtPUT;
end;

function TGBClientRequestBase.QueryAddOrSet(Key: string; Value: Integer): IGBClientRequestParams;
begin
  Result := Self;
  TGBClientRequestBaseParam.AddOrSet(FQueries, Key, Value, False);
end;

function TGBClientRequestBase.QueryAddOrSet(Key, Value: String): IGBClientRequestParams;
begin
  Result := Self;
  TGBClientRequestBaseParam.AddOrSet(FQueries, Key, Value, False);
end;

function TGBClientRequestBase.QueryAddOrSet(Key: string; Value: TDateTime): IGBClientRequestParams;
begin
  Result := Self;
  TGBClientRequestBaseParam.AddOrSet(FQueries, Key, Value, False);
end;

function TGBClientRequestBase.QueryAddOrSet(Key: string; Value: Extended): IGBClientRequestParams;
begin
  Result := Self;
  TGBClientRequestBaseParam.AddOrSet(FQueries, Key, Value, False);
end;

function TGBClientRequestBase.Resource(Value: String): IGBClientRequest;
begin
  result := Self;
  FResource := Value;
end;

function TGBClientRequestBase.Settings: IGBClientSettings;
begin
  if not Assigned(FSettings) then
    FSettings := TGBClientSettingsDefault.New(Self);
  result := FSettings;
end;

function TGBClientRequestBase.TimeOut(Value: Integer): IGBClientRequest;
begin
  result := Self;
  FTimeOut := Value;
end;

end.
