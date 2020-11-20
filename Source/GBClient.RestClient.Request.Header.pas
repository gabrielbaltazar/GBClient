unit GBClient.RestClient.Request.Header;

interface

uses
  GBClient.Interfaces,
  GBClient.Helpers,
  REST.Client,
  System.SysUtils;

type TGBClientRestClientRequestHeader = class(TInterfacedObject, IGBClientParamHeader)

  private
    [Weak]
    FParent  : IGBClientRequest;
    FRequest : TRESTRequest;

  protected
    function AddOrSet(Key: string; Value: String)   : IGBClientParamHeader; overload;
    function AddOrSet(Key: string; Value: Integer)  : IGBClientParamHeader; overload;
    function AddOrSet(Key: string; Value: Extended) : IGBClientParamHeader; overload;
    function AddOrSet(Key: string; Value: TDateTime): IGBClientParamHeader; overload;

    function &End: IGBClientRequest;
  public
    class function New(Parent: IGBClientRequest; Request: TRESTRequest): IGBClientParamHeader;
    constructor create(Parent: IGBClientRequest; Request: TRESTRequest);
    destructor Destroy; override;
end;

implementation

{ TGBClientRestClientRequestHeader }

function TGBClientRestClientRequestHeader.AddOrSet(Key, Value: String): IGBClientParamHeader;
begin
  result := Self;
  FRequest.Params.AddHeader(Key, Value);
end;

function TGBClientRestClientRequestHeader.AddOrSet(Key: string; Value: Integer): IGBClientParamHeader;
begin
  result := AddOrSet(Key, Value.ToString);
end;

function TGBClientRestClientRequestHeader.AddOrSet(Key: string; Value: Extended): IGBClientParamHeader;
begin
  result := AddOrSet(Key, Value.ToString);
end;

function TGBClientRestClientRequestHeader.&End: IGBClientRequest;
begin
  result := FParent;
end;

function TGBClientRestClientRequestHeader.AddOrSet(Key: string; Value: TDateTime): IGBClientParamHeader;
begin
  result := AddOrSet(Key, Value.DateTimeToIso8601);
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
