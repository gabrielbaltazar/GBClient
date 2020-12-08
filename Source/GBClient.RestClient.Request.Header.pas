unit GBClient.RestClient.Request.Header;

interface

uses
  GBClient.Interfaces,
  GBClient.Helpers,
  REST.Types,
  REST.Client,
  System.SysUtils;

type TGBClientRestClientRequestHeader = class(TInterfacedObject, IGBClientParamHeader)

  private
    [Weak]
    FParent  : IGBClientRequest;
    FRequest : TRESTRequest;

  protected
    function AddOrSet(Key: string; Value: String; bEncode: Boolean = True)   : IGBClientParamHeader; overload;
    function AddOrSet(Key: string; Value: Integer; bEncode: Boolean = True)  : IGBClientParamHeader; overload;
    function AddOrSet(Key: string; Value: Extended; bEncode: Boolean = True) : IGBClientParamHeader; overload;
    function AddOrSet(Key: string; Value: TDateTime; bEncode: Boolean = True): IGBClientParamHeader; overload;

    function &End: IGBClientRequest;
  public
    class function New(Parent: IGBClientRequest; Request: TRESTRequest): IGBClientParamHeader;
    constructor create(Parent: IGBClientRequest; Request: TRESTRequest);
    destructor Destroy; override;
end;

implementation

{ TGBClientRestClientRequestHeader }

function TGBClientRestClientRequestHeader.AddOrSet(Key, Value: String; bEncode: Boolean = True): IGBClientParamHeader;
var
  parameter : TRESTRequestParameter;
begin
  result := Self;
  parameter := FRequest.Params.AddHeader(Key, Value);

  if not bEncode then
    parameter.Options := [poDoNotEncode];
end;

function TGBClientRestClientRequestHeader.AddOrSet(Key: string; Value: Integer; bEncode: Boolean = True): IGBClientParamHeader;
begin
  result := AddOrSet(Key, Value.ToString, bEncode);
end;

function TGBClientRestClientRequestHeader.AddOrSet(Key: string; Value: Extended; bEncode: Boolean = True): IGBClientParamHeader;
begin
  result := AddOrSet(Key, Value.ToString, bEncode);
end;

function TGBClientRestClientRequestHeader.&End: IGBClientRequest;
begin
  result := FParent;
end;

function TGBClientRestClientRequestHeader.AddOrSet(Key: string; Value: TDateTime; bEncode: Boolean = True): IGBClientParamHeader;
begin
  result := AddOrSet(Key, Value.DateTimeToIso8601, bEncode);
end;

constructor TGBClientRestClientRequestHeader.create(Parent: IGBClientRequest; Request: TRESTRequest);
begin
  FParent  := Parent;
  FRequest := Request;
end;

destructor TGBClientRestClientRequestHeader.Destroy;
begin

  inherited;
end;

class function TGBClientRestClientRequestHeader.New(Parent: IGBClientRequest; Request: TRESTRequest): IGBClientParamHeader;
begin
  Result := Self.create(Parent, Request);
end;

end.
