unit GBClient.Interfaces;

interface

uses
  Data.DB,
  GBClient.Types,
  GBClient.Exceptions,
  System.Classes,
  System.SysUtils,
  System.JSON,
  System.Generics.Collections;

type
  EGBRestExceptionTimeout = GBClient.Exceptions.EGBRestExceptionTimeout;
  EGBRestException = GBClient.Exceptions.EGBRestException;
  TGBContentType = GBClient.Types.TGBContentType;

  TGBOnParseJSONToObject = procedure (AJSON: TJSONObject; AObject: TObject);
  TGBOnParseObjectToJSON = function  (AObject: TObject): TJSONObject;

  TGBOnParseJSONToDataSet = procedure (AJSON: TJSONObject; ADataSet: TDataSet);
  TGBOnParseDataSetToJSONObject = function (ADataSet: TDataSet): TJSONObject;
  TGBOnParseDataSetToJSONArray  = function (ADataSet: TDataSet): TJSONArray;

  TGBOnException = procedure (TheException: EGBRestException);

  IGBClientResponse      = interface;
  IGBClientAuth          = interface;
  IGBClientAuthBasic     = interface;
  IGBClientAuthBearer    = interface;
  IGBClientSettings      = interface;
  IGBClientRequestParams = interface;

  TOnPreExecute = reference to procedure(Value: String);

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

    function Accept(Value: string): IGBClientRequest;
    function AcceptCharset(Value: string): IGBClientRequest;
    function AcceptEncoding(Value: string): IGBClientRequest;
    function ContentType(Value: TGBContentType): IGBClientRequest; overload;
    function ContentType(Value: String): IGBClientRequest; overload;
    function BaseURL(Value: String): IGBClientRequest;
    function Resource(Value: String): IGBClientRequest;
    function TimeOut(Value: Integer): IGBClientRequest;

    function Send: IGBClientResponse;
    function Response : IGBClientResponse;

    function Settings: IGBClientSettings;

    function OnException (Value: TGBOnException): IGBClientRequest;
    function OnPreExecute(Value: TOnPreExecute): IGBClientRequest;
  end;

  IGBClientRequestParams = interface
    ['{C65778A0-8894-491E-9A86-D25FF5CF580A}']
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
//    function BodyAddOrSet(Value: TDataSet; ACurrent: Boolean = True): IGBClientRequestParams; overload;
    function BodyAddOrSet(Key, Value: String): IGBClientRequestParams; overload;

    function BodyBinary(AFileName: String): IGBClientRequestParams; overload;
    function BodyBinary(AStream : TStream; AOwner: Boolean = False): IGBClientRequestParams; overload;

    function &End: IGBClientRequest;
  end;

  IGBClientSettings = interface
    ['{B2B782C4-3B08-44A7-823F-2005AA1CE08A}']
    function OnParseJSONToObject(Value: TGBOnParseJSONToObject): IGBClientSettings; overload;
    function OnParseJSONToObject: TGBOnParseJSONToObject; overload;

    function OnParseObjectToJSON(Value: TGBOnParseObjectToJSON): IGBClientSettings; overload;
    function OnParseObjectToJSON: TGBOnParseObjectToJSON; overload;

    function OnParseJSONToDataSet(Value: TGBOnParseJSONToDataSet): IGBClientSettings; overload;
    function OnParseJSONToDataSet: TGBOnParseJSONToDataSet; overload;

    function OnParseDataSetToJSONObject(Value: TGBOnParseDataSetToJSONObject): IGBClientSettings; overload;
    function OnParseDataSetToJSONObject: TGBOnParseDataSetToJSONObject; overload;

    function OnParseDataSetToJSONArray(Value: TGBOnParseDataSetToJSONArray): IGBClientSettings; overload;
    function OnParseDataSetToJSONArray: TGBOnParseDataSetToJSONArray; overload;

    function &End: IGBClientRequest;
  end;

  IGBClientResponse = interface
    ['{3FE95DD9-41B1-4F79-868D-1E3A44BCAC45}']
    function StatusCode: Integer;
    function StatusText: string;

    function GetText       : string;
    function GetJSONObject : TJSONObject;
    function GetJSONArray  : TJSONArray;
    function DataSet(Value: TDataSet): IGBClientResponse;
    function GetObject(Value: TObject): IGBClientResponse;
    function GetList(Value: TList<TObject>; AType: TClass): IGBClientResponse;
    function GetBytes: TBytes;
    function GetStream: TBytesStream;

    function HeaderAsString   (Name: String): string;
    function HeaderAsInteger  (Name: String): Integer;
    function HeaderAsFloat    (Name: String): Double;
    function HeaderAsDateTime (Name: String): TDateTime;

    function &End: IGBClientRequest;
  end;

  IGBClientAuth = interface
    ['{AFAC2F54-25F7-4345-A2C7-4481B79DFE12}']
    function Basic: IGBClientAuthBasic;
    function Bearer: IGBClientAuthBearer;

    function &End: IGBClientRequest;
  end;

  IGBClientAuthBasic = interface
    ['{0A5A8AE5-86ED-4B0E-9357-657E6ECFAE05}']
    function Username(Value: String): IGBClientAuthBasic;
    function Password(Value: String): IGBClientAuthBasic;

    function &End: IGBClientRequest;
  end;

  IGBClientAuthBearer = interface
    ['{8384F5C6-77D6-4285-B274-1A415356D311}']
    function Token(Value: String): IGBClientAuthBearer;

    function &End: IGBClientRequest;
  end;

function NewClientRequest: IGBClientRequest; overload;
function NewClientRequest(BaseUrl: String): IGBClientRequest; overload;

implementation

//uses
//  GBClient.RestClient.Request,
//  {$IFDEF NetHTTP} GBClient.NetHTTPClient.Request, {$ENDIF}
//  {$IFDEF IdHTTP} GBClient.IdHTTP.Request, {$ENDIF}
//  REST.Json;

function NewClientRequest: IGBClientRequest;
begin
//  {$IFDEF NetHTTP}
//    Exit( TGBClientNetHttpClientRequest.New );
//  {$ENDIF}
//
//  {$IFDEF IdHTTP}
//    Exit( TGBClientIdHTTPRequest.New );
//  {$ENDIF}
//
//  result := TGBClientRequest.New;
end;

function NewClientRequest(BaseUrl: String): IGBClientRequest;
begin
  result := NewClientRequest.BaseURL(BaseUrl);
end;

end.
