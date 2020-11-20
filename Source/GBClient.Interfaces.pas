unit GBClient.Interfaces;

interface

uses
  Data.DB,
  GBClient.Types,
  GBClient.Exceptions,
  System.SysUtils,
  System.JSON,
  System.Generics.Collections;

type
  EGBRestException = GBClient.Exceptions.EGBRestException;

  TGBOnParseJSONToObject = procedure (AJSON: TJSONObject; AObject: TObject);
  TGBOnParseObjectToJSON = function  (AObject: TObject): TJSONObject;

  TGBOnParseJSONToDataSet = procedure (AJSON: TJSONObject; ADataSet: TDataSet);
  TGBOnParseDataSetToJSONObject = function (ADataSet: TDataSet): TJSONObject;
  TGBOnParseDataSetToJSONArray  = function (ADataSet: TDataSet): TJSONArray;

  TGBOnException = procedure (TheException: EGBRestException);

  IGBClientParamHeader = interface;
  IGBClientParamPath     = interface;
  IGBClientParamQuery    = interface;
  IGBClientBodyRequest   = interface;
  IGBClientResponse      = interface;
  IGBClientAuth          = interface;
  IGBClientAuthBasic     = interface;
  IGBClientAuthBearer    = interface;
  IGBClientSettings      = interface;

  TOnPreExecute = reference to procedure(Value: String);

  IGBClientRequest = interface
    ['{9287B63B-BF21-4C69-B2B1-7D27FCD1F7FE}']
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

    function Accept         (Value: string): IGBClientRequest;
    function AcceptCharset  (Value: string): IGBClientRequest;
    function AcceptEncoding (Value: string): IGBClientRequest;
    function ContentType    (Value : TGBContentType) : IGBClientRequest; overload;
    function ContentType    (Value : String) : IGBClientRequest; overload;
    function BaseURL        (Value : String) : IGBClientRequest;
    function Resource       (Value : String) : IGBClientRequest;
    function TimeOut        (Value : Integer): IGBClientRequest;

    function Execute  : IGBClientResponse;
    function Send     : IGBClientResponse;
    function Response : IGBClientResponse;

    function Settings: IGBClientSettings;

    function OnException (Value: TGBOnException): IGBClientRequest;
    function OnPreExecute(Value: TOnPreExecute): IGBClientRequest;
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

  IGBClientParamHeader = interface
    ['{E79EC294-59C1-45FF-951F-ABD1FE6F8268}']
    function AddOrSet(Key: string; Value: String)   : IGBClientParamHeader; overload;
    function AddOrSet(Key: string; Value: Integer)  : IGBClientParamHeader; overload;
    function AddOrSet(Key: string; Value: Extended) : IGBClientParamHeader; overload;
    function AddOrSet(Key: string; Value: TDateTime): IGBClientParamHeader; overload;

    function &End: IGBClientRequest;
  end;

  IGBClientParamPath = interface
    ['{6DE7B69F-C34B-4C0B-87B3-F81A14FA62A5}']
    function AddOrSet(Key: string; Value: String)   : IGBClientParamPath; overload;
    function AddOrSet(Key: string; Value: Integer)  : IGBClientParamPath; overload;
    function AddOrSet(Key: string; Value: Extended) : IGBClientParamPath; overload;
    function AddOrSet(Key: string; Value: TDateTime): IGBClientParamPath; overload;

    function &End: IGBClientRequest;
  end;

  IGBClientParamQuery = interface
    ['{22B13D96-8631-44C6-B2DF-D0F77D0C6C8E}']
    function AddOrSet(Key: string; Value: String)   : IGBClientParamQuery; overload;
    function AddOrSet(Key: string; Value: Integer)  : IGBClientParamQuery; overload;
    function AddOrSet(Key: string; Value: Extended) : IGBClientParamQuery; overload;
    function AddOrSet(Key: string; Value: TDateTime): IGBClientParamQuery; overload;

    function &End: IGBClientRequest;
  end;

  IGBClientBodyRequest = interface
    ['{EDD14993-3C98-4334-AB6C-AAAF54647F03}']
    function AddOrSet(Value : String)                              : IGBClientBodyRequest; overload;
    function AddOrSet(Value : TJSONObject; AOwner: Boolean = False): IGBClientBodyRequest; overload;
    function AddOrSet(Value : TJSONArray;  AOwner: Boolean = False): IGBClientBodyRequest; overload;
    function AddOrSet(Value : TObject;  AOwner: Boolean = False): IGBClientBodyRequest; overload;
    function AddOrSet(Value : TList<TObject>; AOwner: Boolean = False): IGBClientBodyRequest; overload;
    function AddOrSet(Value : TDataSet; ACurrent: Boolean = True): IGBClientBodyRequest; overload;

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

    function HeaderAsString   (Name: String): string;
    function HeaderAsInteger  (Name: String): Integer;
    function HeaderAsFloat    (Name: String): Double;
    function HeaderAsDateTime (Name: String): TDateTime;

    function &End: IGBClientRequest;
  end;

  IGBClientAuth = interface
    ['{AFAC2F54-25F7-4345-A2C7-4481B79DFE12}']
    function Basic  : IGBClientAuthBasic;
    function Bearer : IGBClientAuthBearer;

    function &End: IGBClientRequest;
  end;

  IGBClientAuthBasic = interface
    ['{0A5A8AE5-86ED-4B0E-9357-657E6ECFAE05}']
    function Username(Value: String): IGBClientAuthBasic;
    function Password(Value: String): IGBClientAuthBasic;

    function &End: IGBClientAuth;
  end;

  IGBClientAuthBearer = interface
    ['{8384F5C6-77D6-4285-B274-1A415356D311}']
    function Token(Value: String): IGBClientAuthBearer;

    function &End: IGBClientAuth;
  end;

function NewClientRequest: IGBClientRequest; overload;
function NewClientRequest(BaseUrl: String): IGBClientRequest; overload;

implementation

uses
  GBClient.RestClient.Request,
  {$IFDEF NetHTTP} GBClient.NetHTTPClient.Request, {$ENDIF}
  {$IFDEF IdHTTP} GBClient.IdHTTP.Request, {$ENDIF}
  REST.Json;

function NewClientRequest: IGBClientRequest;
begin
  {$IFDEF NetHTTP}
    Exit( TGBClientNetHttpClientRequest.New );
  {$ENDIF}

  {$IFDEF IdHTTP}
    Exit( TGBClientIdHTTPRequest.New );
  {$ENDIF}

  result := TGBClientRequest.New;
end;

function NewClientRequest(BaseUrl: String): IGBClientRequest;
begin
  result := NewClientRequest.BaseURL(BaseUrl);
end;

end.
