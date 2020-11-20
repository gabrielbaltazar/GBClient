unit GBClient.RestClient.Request.Query;

interface

uses
  GBClient.Interfaces,
  GBClient.Helpers,
  REST.Client,
  REST.Types,
  System.SysUtils;

type TGBClientRequestQuery = class(TInterfacedObject, IGBClientParamQuery)

  private
    [Weak]
    FParent  : IGBClientRequest;
    FRequest : TRESTRequest;

  protected
    function AddOrSet(Key: string; Value: String)   : IGBClientParamQuery; overload;
    function AddOrSet(Key: string; Value: Integer)  : IGBClientParamQuery; overload;
    function AddOrSet(Key: string; Value: Extended) : IGBClientParamQuery; overload;
    function AddOrSet(Key: string; Value: TDateTime): IGBClientParamQuery; overload;

    function &End: IGBClientRequest;
  public
    class function New(Parent: IGBClientRequest; Request: TRESTRequest): IGBClientParamQuery;
    constructor create(Parent: IGBClientRequest; Request: TRESTRequest);
    destructor Destroy; override;
end;

implementation

{ TGBClientRequestQuery }

function TGBClientRequestQuery.AddOrSet(Key, Value: String): IGBClientParamQuery;
begin
  result := Self;
  FRequest.AddParameter(Key, Value);
end;

function TGBClientRequestQuery.AddOrSet(Key: string; Value: Integer): IGBClientParamQuery;
begin
  result := AddOrSet(Key, Value.ToString);
end;

function TGBClientRequestQuery.AddOrSet(Key: string; Value: Extended): IGBClientParamQuery;
begin
  result := AddOrSet(Key, Value.ToString);
end;

function TGBClientRequestQuery.&End: IGBClientRequest;
begin
  result := FParent;
end;

function TGBClientRequestQuery.AddOrSet(Key: string; Value: TDateTime): IGBClientParamQuery;
begin
  result := AddOrSet(Key, Value.DateTimeToIso8601);
end;

constructor TGBClientRequestQuery.create(Parent: IGBClientRequest; Request: TRESTRequest);
begin
  FParent  := Parent;
  FRequest := Request;
end;

destructor TGBClientRequestQuery.Destroy;
begin

  inherited;
end;

class function TGBClientRequestQuery.New(Parent: IGBClientRequest; Request: TRESTRequest): IGBClientParamQuery;
begin
  Result := Self.create(Parent, Request);
end;

end.
