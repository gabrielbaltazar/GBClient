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

  TGBOnParseJSONToObject = procedure(const AJSON: TJSONObject; const AObject: TObject);

  TGBOnParseObjectToJSON = function(const AObject: TObject): TJSONObject;

  TGBOnParseJSONToDataSet = procedure(const AJSON: TJSONObject; const ADataSet: TDataSet);

  TGBOnParseDataSetToJSONObject = function(const ADataSet: TDataSet): TJSONObject;

  TGBOnParseDataSetToJSONArray = function(const ADataSet: TDataSet): TJSONArray;

  TGBOnException = procedure(const TheException: EGBRestException);

  TGBOnAWSSignature = reference to procedure(const AAuthorization, AAmzDate: string);

  TGBAuthType = (atNone, atBasic, atBearer, atAWSv4);

  IGBClientResponse = interface;

  IGBClientAuth = interface;

  IGBClientAuthBasic = interface;

  IGBClientAuthBearer = interface;

  IGBClientAuthAWSv4 = interface;

  IGBClientSettings = interface;

  IGBClientRequestParams = interface;

  IGBClientProxy = interface;

  TOnPreExecute = reference to procedure(Value: string);

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

    function Accept(const AValue: string): IGBClientRequest;
    function AcceptCharset(const AValue: string): IGBClientRequest;
    function AcceptEncoding(const AValue: string): IGBClientRequest;
    function ContentType(const AValue: TGBContentType): IGBClientRequest; overload;
    function ContentType(const AValue: string): IGBClientRequest; overload;
    function BaseURL(const AValue: string): IGBClientRequest;
    function Resource(const AValue: string): IGBClientRequest;
    function TimeOut(const AValue: Integer): IGBClientRequest;

    function Send: IGBClientResponse;
    function Response: IGBClientResponse;

    function Settings: IGBClientSettings;

    function OnException(const AValue: TGBOnException): IGBClientRequest;
    function OnPreExecute(const AValue: TOnPreExecute): IGBClientRequest;
  end;

  IGBClientProxy = interface
    ['{A2CE47E4-523B-4FC0-B9B7-1BD30CDCB326}']
    function Server(const AValue: string): IGBClientProxy;
    function Port(const AValue: Integer): IGBClientProxy;
    function Username(const AValue: string): IGBClientProxy;
    function Password(const AValue: string): IGBClientProxy;

    function &End: IGBClientRequest;
  end;

  IGBClientRequestParams = interface
    ['{C65778A0-8894-491E-9A86-D25FF5CF580A}']
    // Header Params
    function HeaderAddOrSet(const AKey, AValue: string;
      const AEncode: Boolean = True): IGBClientRequestParams; overload;
    function HeaderAddOrSet(const AKey: string; const AValue: Integer;
      const AEncode: Boolean = True): IGBClientRequestParams; overload;
    function HeaderAddOrSet(const AKey: string; const AValue: Extended;
      const AEncode: Boolean = True): IGBClientRequestParams; overload;
    function HeaderAddOrSet(const AKey: string; const AValue: TDateTime;
      const AEncode: Boolean = True): IGBClientRequestParams; overload;

    // Path Params
    function PathAddOrSet(const AKey: string; const AValue: string): IGBClientRequestParams; overload;
    function PathAddOrSet(const AKey: string; const AValue: Integer): IGBClientRequestParams; overload;
    function PathAddOrSet(const AKey: string; const AValue: Extended): IGBClientRequestParams; overload;
    function PathAddOrSet(const AKey: string; const AValue: TDateTime): IGBClientRequestParams; overload;

    // Query Params
    function QueryAddOrSet(const AKey, AValue: string; const AEncode: Boolean = True): IGBClientRequestParams; overload;
    function QueryAddOrSet(const AKey: string; const AValue: Integer;
      const AEncode: Boolean = True): IGBClientRequestParams; overload;
    function QueryAddOrSet(const AKey: string; const AValue: Extended;
      const AEncode: Boolean = True): IGBClientRequestParams; overload;
    function QueryAddOrSet(const AKey: string; const AValue: TDateTime;
      const AEncode: Boolean = True): IGBClientRequestParams; overload;

    // Body Params
    function BodyAddOrSet(const AValue: string) : IGBClientRequestParams; overload;
    function BodyAddOrSet(const AValue: TJSONObject; const AOwner: Boolean = False): IGBClientRequestParams; overload;
    function BodyAddOrSet(const AValue: TJSONArray; const AOwner: Boolean = False): IGBClientRequestParams; overload;
    function BodyAddOrSet(const AValue: TObject; const AOwner: Boolean = False): IGBClientRequestParams; overload;
    function BodyAddOrSet(const AValue: TList<TObject>;
      const AOwner: Boolean = False): IGBClientRequestParams; overload;
//    function BodyAddOrSet(AValue: TDataSet; ACurrent: Boolean = True): IGBClientRequestParams; overload;
    function BodyAddOrSet(const AKey, AValue: string): IGBClientRequestParams; overload;

    function BodyBinary(const AFileName: string): IGBClientRequestParams; overload;
    function BodyBinary(const AStream : TStream; const AOwner: Boolean = False): IGBClientRequestParams; overload;

    function GetBody: string;

    function &End: IGBClientRequest;
  end;

  IGBClientSettings = interface
    ['{B2B782C4-3B08-44A7-823F-2005AA1CE08A}']
    function OnParseJSONToObject(const AValue: TGBOnParseJSONToObject): IGBClientSettings; overload;
    function OnParseJSONToObject: TGBOnParseJSONToObject; overload;

    function OnParseObjectToJSON(const AValue: TGBOnParseObjectToJSON): IGBClientSettings; overload;
    function OnParseObjectToJSON: TGBOnParseObjectToJSON; overload;

    function OnParseJSONToDataSet(const AValue: TGBOnParseJSONToDataSet): IGBClientSettings; overload;
    function OnParseJSONToDataSet: TGBOnParseJSONToDataSet; overload;

    function OnParseDataSetToJSONObject(const AValue: TGBOnParseDataSetToJSONObject): IGBClientSettings; overload;
    function OnParseDataSetToJSONObject: TGBOnParseDataSetToJSONObject; overload;

    function OnParseDataSetToJSONArray(const AValue: TGBOnParseDataSetToJSONArray): IGBClientSettings; overload;
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
    function DataSet(const AValue: TDataSet): IGBClientResponse;
    function GetObject(const AValue: TObject): IGBClientResponse;
    function GetList(const AValue: TList<TObject>; const AType: TClass): IGBClientResponse;
    function GetBytes: TBytes;
    function GetStream: TBytesStream;
    function HeaderAsString(const AName: string): string;
    function HeaderAsInteger(const AName: string): Integer;
    function HeaderAsFloat(const AName: string): Double;
    function HeaderAsDateTime(const AName: string): TDateTime;

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
    function Username(const AValue: string): IGBClientAuthBasic;
    function Password(const AValue: string): IGBClientAuthBasic;

    function &End: IGBClientRequest;
  end;

  IGBClientAuthBearer = interface
    ['{8384F5C6-77D6-4285-B274-1A415356D311}']
    function Token(const AValue: string): IGBClientAuthBearer;

    function &End: IGBClientRequest;
  end;

  IGBClientAuthAWSv4 = interface
    ['{F67B251F-C5D7-40E0-BA33-60DD02218C13}']
    function AccessKey(const AValue: string): IGBClientAuthAWSv4;
    function SecretKey(const AValue: string): IGBClientAuthAWSv4;
    function Region(const AValue: string): IGBClientAuthAWSv4;
    function Service(const AValue: string): IGBClientAuthAWSv4;

    function HTTPVerb(const AValue: string): IGBClientAuthAWSv4;
    function Host(const AValue: string): IGBClientAuthAWSv4;

    function HeaderAddOrSet(const AKey, AValue: string): IGBClientAuthAWSv4;
    function QueryAddOrSet(const AKey, AValue: string): IGBClientAuthAWSv4;

    function Payload(const AValue: string): IGBClientAuthAWSv4; overload;
    function Payload(const AValue: TStream): IGBClientAuthAWSv4; overload;

    function XAmzDate: string;
    function Authorization: string;
    function Apply: IGBClientAuthAWSv4;

    function &End: IGBClientRequest;
  end;

function NewClientRequest: IGBClientRequest; overload;
function NewClientRequest(const ABaseUrl: string): IGBClientRequest; overload;

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
  Exit(TGBClientNetHttpClient.New);
{$ENDIF}

{$IFDEF IdHTTP}
  Exit(TGBClientIdHTTP.New);
{$ENDIF}

  Result := TGBClientRestClient.New;
end;

function NewClientRequest(const ABaseUrl: string): IGBClientRequest;
begin
  Result := NewClientRequest.BaseURL(ABaseUrl);
end;

end.
