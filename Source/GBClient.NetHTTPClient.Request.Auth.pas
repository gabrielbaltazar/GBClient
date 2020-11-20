unit GBClient.NetHTTPClient.Request.Auth;

interface

uses
  GBClient.Interfaces,
  GBClient.NetHTTPClient.Request.Auth.Basic,
  GBClient.NetHTTPClient.Request.Auth.Bearer,
  System.Net.HttpClientComponent,
  System.SysUtils;

type TGBClientNetHTTPClientAuth = class(TInterfacedObject, IGBClientAuth)

  private
    [Weak]
    FParent: IGBClientRequest;

    FAuthBasic  : IGBClientAuthBasic;
    FAuthBearer : IGBClientAuthBearer;
    FRequest    : TNetHTTPRequest;

  protected
    function Basic  : IGBClientAuthBasic;
    function Bearer : IGBClientAuthBearer;

    function &End: IGBClientRequest;

  public
    constructor create(Parent: IGBClientRequest; Request: TNetHTTPRequest);
    class function New(Parent: IGBClientRequest; Request: TNetHTTPRequest): IGBClientAuth;
    destructor  Destroy; override;
end;


implementation

{ TGBClientNetHTTPClientAuth }

function TGBClientNetHTTPClientAuth.Basic: IGBClientAuthBasic;
begin
  if not Assigned(FAuthBasic) then
    FAuthBasic := TGBClientNetHTTPClientRequestAuthBasic.New(Self, FRequest);
  result := FAuthBasic;
end;

function TGBClientNetHTTPClientAuth.Bearer: IGBClientAuthBearer;
begin
  if not Assigned(FAuthBearer) then
    FAuthBearer := TGBClientNetHTTPClientAuthBearer.New(Self, FRequest);
  result := FAuthBearer;
end;

function TGBClientNetHTTPClientAuth.&End: IGBClientRequest;
begin
  result := FParent;
end;

constructor TGBClientNetHTTPClientAuth.create(Parent: IGBClientRequest; Request: TNetHTTPRequest);
begin
  FParent  := Parent;
  FRequest := Request;
end;

destructor TGBClientNetHTTPClientAuth.Destroy;
begin
  inherited;
end;

class function TGBClientNetHTTPClientAuth.New(Parent: IGBClientRequest; Request: TNetHTTPRequest): IGBClientAuth;
begin
  result := Self.create(Parent, Request);
end;

end.

