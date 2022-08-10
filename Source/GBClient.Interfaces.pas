unit GBClient.Interfaces;

interface

{$IFDEF WEAKPACKAGEUNIT}
  {$WEAKPACKAGEUNIT ON}
{$ENDIF}

uses
  Data.DB,
  GBClient.Core.Types,
  GBClient.Core.Exceptions,
  System.Classes,
  System.SysUtils,
  System.JSON,
  System.Generics.Collections;

type
  TGBContentType = GBClient.Core.Types.TGBContentType;

  TGBOnParseJSONToObject = procedure(AJSON: TJSONObject; AObject: TObject);
  TGBOnParseObjectToJSON = function(AObject: TObject): TJSONObject;

  TGBOnParseJSONToDataSet = procedure(AJSON: TJSONObject; ADataSet: TDataSet);
  TGBOnParseDataSetToJSONObject = function(ADataSet: TDataSet): TJSONObject;
  TGBOnParseDataSetToJSONArray  = function(ADataSet: TDataSet): TJSONArray;

  TGBOnException = procedure(AException: EGBRestException);

  TGBOnAWSSignature = reference to procedure(AAuthorization, AAmzDate: string);

  TGBAuthType = (atNone, atBasic, atBearer, atAWSv4);

  IGBClientResponse = interface;
  IGBClientAuth  = interface;
  IGBClientAuthBasic = interface;
  IGBClientAuthBearer = interface;
  IGBClientAuthAWSv4 = interface;
  IGBClientSettings = interface;
  IGBClientRequestParams = interface;
  IGBClientProxy = interface;

  TOnPreExecute = reference to procedure(AValue: string);

  IGBClientRequest = interface
    ['{9287B63B-BF21-4C69-B2B1-7D27FCD1F7FE}']
    function Component: TComponent;
    function POST: IGBClientRequest;
    function PUT: IGBClientRequest;
    function GET: IGBClientRequest;
    function DELETE: IGBClientRequest;
    function PATCH: IGBClientRequest;

    function Authorization: IGBClientAuth;
    function Params: IGBClientRequestParams;
    function Proxy: IGBClientProxy;

    function Accept(AValue: string): IGBClientRequest;
    function AcceptCharset(AValue: string): IGBClientRequest;
    function AcceptEncoding(AValue: string): IGBClientRequest;
    function ContentType(AValue: TGBContentType): IGBClientRequest; overload;
    function ContentType(AValue: string): IGBClientRequest; overload;
    function BaseURL(AValue: string): IGBClientRequest;
    function Resource(AValue: string): IGBClientRequest;
    function TimeOut(AValue: Integer): IGBClientRequest;

    function Send: IGBClientResponse;
    function Response: IGBClientResponse;
    function Settings: IGBClientSettings;

    function OnException(AValue: TGBOnException): IGBClientRequest;
    function OnPreExecute(AValue: TOnPreExecute): IGBClientRequest;
  end;

  IGBClientProxy = interface
    ['{A2CE47E4-523B-4FC0-B9B7-1BD30CDCB326}']
    function Server(AValue: string): IGBClientProxy;
    function Port(AValue: Integer): IGBClientProxy;
    function Username(AValue: string): IGBClientProxy;
    function Password(AValue: string): IGBClientProxy;
    function &End: IGBClientRequest;
  end;

  IGBClientRequestParams = interface
    ['{C65778A0-8894-491E-9A86-D25FF5CF580A}']
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
    function QueryAddOrSet(AKey: string; AValue: TDateTime; AEncode: Boolean = True): IGBClientRequestParams; overload;
    // Body Params
    function BodyAddOrSet(AValue: string) : IGBClientRequestParams; overload;
    function BodyAddOrSet(AValue: TJSONObject; AOwner: Boolean = False): IGBClientRequestParams; overload;
    function BodyAddOrSet(AValue: TJSONArray; AOwner: Boolean = False): IGBClientRequestParams; overload;
    function BodyAddOrSet(AValue: TObject; AOwner: Boolean = False): IGBClientRequestParams; overload;
    function BodyAddOrSet(AValue: TList<TObject>; AOwner: Boolean = False): IGBClientRequestParams; overload;
    function BodyAddOrSet(AKey, AValue: string): IGBClientRequestParams; overload;
    function BodyBinary(AFileName: string): IGBClientRequestParams; overload;
    function BodyBinary(AStream : TStream; AOwner: Boolean = False): IGBClientRequestParams; overload;
    // FormData
    function FormDataAddOrSet(AKey: string; AValue: string; AEncode: Boolean = True): IGBClientRequestParams; overload;
    function FormDataAddOrSet(AKey: string; AValue: Integer; AEncode: Boolean = True): IGBClientRequestParams; overload;
    function FormDataAddOrSet(AKey: string; AValue: Extended; AEncode: Boolean = True): IGBClientRequestParams; overload;
    function FormDataAddOrSet(AKey: string; AValue: TDateTime; AEncode: Boolean = True): IGBClientRequestParams; overload;
    function FormDataAddFile(AKey: string; AValue: TStream): IGBClientRequestParams; overload;
    function FormDataAddFile(AKey: string; AFileName: string): IGBClientRequestParams; overload;
    function GetBody: string;
    function &End: IGBClientRequest;
  end;

  IGBClientSettings = interface
    ['{B2B782C4-3B08-44A7-823F-2005AA1CE08A}']
    function OnParseJSONToObject(AValue: TGBOnParseJSONToObject): IGBClientSettings; overload;
    function OnParseJSONToObject: TGBOnParseJSONToObject; overload;

    function OnParseObjectToJSON(AValue: TGBOnParseObjectToJSON): IGBClientSettings; overload;
    function OnParseObjectToJSON: TGBOnParseObjectToJSON; overload;

    function OnParseJSONToDataSet(AValue: TGBOnParseJSONToDataSet): IGBClientSettings; overload;
    function OnParseJSONToDataSet: TGBOnParseJSONToDataSet; overload;

    function OnParseDataSetToJSONObject(AValue: TGBOnParseDataSetToJSONObject): IGBClientSettings; overload;
    function OnParseDataSetToJSONObject: TGBOnParseDataSetToJSONObject; overload;

    function OnParseDataSetToJSONArray(AValue: TGBOnParseDataSetToJSONArray): IGBClientSettings; overload;
    function OnParseDataSetToJSONArray: TGBOnParseDataSetToJSONArray; overload;

    function &End: IGBClientRequest;
  end;

  IGBClientResponse = interface
    ['{3FE95DD9-41B1-4F79-868D-1E3A44BCAC45}']
    function StatusCode: Integer;
    function StatusText: string;
    function GetText: string;
    function GetJSONObject: TJSONObject;
    function GetJSONArray: TJSONArray;
    function DataSet(AValue: TDataSet): IGBClientResponse;
    function GetObject(AValue: TObject): IGBClientResponse;
    function GetList(AValue: TList<TObject>; AType: TClass): IGBClientResponse;
    function GetBytes: TBytes;
    function GetStream: TBytesStream;
    function HeaderAsString(AName: string): string;
    function HeaderAsInteger(AName: string): Integer;
    function HeaderAsFloat(AName: string): Double;
    function HeaderAsDateTime(AName: string): TDateTime;
    function &End: IGBClientRequest;
  end;

  IGBClientAuth = interface
    ['{AFAC2F54-25F7-4345-A2C7-4481B79DFE12}']
    function Basic: IGBClientAuthBasic;
    function Bearer: IGBClientAuthBearer;
    function AWSv4: IGBClientAuthAWSv4;
    function AuthType: TGBAuthType;
    function &End: IGBClientRequest;
  end;

  IGBClientAuthBasic = interface
    ['{0A5A8AE5-86ED-4B0E-9357-657E6ECFAE05}']
    function Username(AValue: string): IGBClientAuthBasic;
    function Password(AValue: string): IGBClientAuthBasic;
    function &End: IGBClientRequest;
  end;

  IGBClientAuthBearer = interface
    ['{8384F5C6-77D6-4285-B274-1A415356D311}']
    function Token(AValue: string): IGBClientAuthBearer;
    function &End: IGBClientRequest;
  end;

  IGBClientAuthAWSv4 = interface
    ['{F67B251F-C5D7-40E0-BA33-60DD02218C13}']
    function AccessKey(AValue: string): IGBClientAuthAWSv4;
    function SecretKey(AValue: string): IGBClientAuthAWSv4;
    function Region(AValue: string): IGBClientAuthAWSv4;
    function Service(AValue: string): IGBClientAuthAWSv4;
    function HTTPVerb(AValue: string): IGBClientAuthAWSv4;
    function Host(AValue: string): IGBClientAuthAWSv4;
    function HeaderAddOrSet(AKey, AValue: string): IGBClientAuthAWSv4;
    function QueryAddOrSet(AKey, AValue: string): IGBClientAuthAWSv4;
    function Payload(AValue: string): IGBClientAuthAWSv4; overload;
    function Payload(AValue: TStream): IGBClientAuthAWSv4; overload;

    function XAmzDate: string;
    function Authorization: string;
    function Apply: IGBClientAuthAWSv4;
    function &End: IGBClientRequest;
  end;

function NewClientRequest: IGBClientRequest; overload;
function NewClientRequest(ABaseUrl: string): IGBClientRequest; overload;

implementation

uses
  GBClient.RestClient,
{$IFDEF NetHTTP}
  GBClient.NetHTTPClient,
{$ENDIF}
{$IFDEF IdHTTP}
  GBClient.IdHTTP,
{$ENDIF}
  REST.Json;

function NewClientRequest: IGBClientRequest;
begin
{$IFDEF NetHTTP}
  Exit( TGBClientNetHttpClient.New );
{$ENDIF}

{$IFDEF IdHTTP}
  Exit( TGBClientIdHTTP.New );
{$ENDIF}

  Result := TGBClientRestClient.New;
end;

function NewClientRequest(ABaseUrl: string): IGBClientRequest;
begin
  Result := NewClientRequest.BaseURL(ABaseUrl);
end;

end.
