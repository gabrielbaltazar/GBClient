unit GBClient.RestClient.Request.ParamPath;

interface

uses
  GBClient.Interfaces,
  GBClient.Helpers,
  REST.Client,
  REST.Types,
  System.SysUtils;

type TGBClientRestClientRequestParamPath = class(TInterfacedObject, IGBClientParamPath)

  private
    [Weak]
    FParent  : IGBClientRequest;
    FRequest : TRESTRequest;

  protected
    function AddOrSet(Key: string; Value: String)   : IGBClientParamPath; overload;
    function AddOrSet(Key: string; Value: Integer)  : IGBClientParamPath; overload;
    function AddOrSet(Key: string; Value: Extended) : IGBClientParamPath; overload;
    function AddOrSet(Key: string; Value: TDateTime): IGBClientParamPath; overload;

    function &End: IGBClientRequest;
  public
    class function New(Parent: IGBClientRequest; Request: TRESTRequest): IGBClientParamPath;
    constructor create(Parent: IGBClientRequest; Request: TRESTRequest);
    destructor Destroy; override;
end;

implementation

{ TGBClientRestClientRequestParamPath }

function TGBClientRestClientRequestParamPath.AddOrSet(Key, Value: String): IGBClientParamPath;
begin
  result := Self;
  FRequest.Params.AddUrlSegment(Key, Value);
end;

function TGBClientRestClientRequestParamPath.AddOrSet(Key: string; Value: Integer): IGBClientParamPath;
begin
  result := AddOrSet(Key, Value.ToString);
end;

function TGBClientRestClientRequestParamPath.AddOrSet(Key: string; Value: Extended): IGBClientParamPath;
begin
  result := AddOrSet(Key, Value.ToString);
end;

function TGBClientRestClientRequestParamPath.&End: IGBClientRequest;
begin
  result := FParent;
end;

function TGBClientRestClientRequestParamPath.AddOrSet(Key: string; Value: TDateTime): IGBClientParamPath;
begin
  result := AddOrSet(Key, Value.DateTimeToIso8601);
end;

constructor TGBClientRestClientRequestParamPath.create(Parent: IGBClientRequest; Request: TRESTRequest);
begin
  FParent  := Parent;
  FRequest := Request;
end;

destructor TGBClientRestClientRequestParamPath.Destroy;
begin

  inherited;
end;

class function TGBClientRestClientRequestParamPath.New(Parent: IGBClientRequest; Request: TRESTRequest): IGBClientParamPath;
begin
  Result := Self.create(Parent, Request);
end;

end.

