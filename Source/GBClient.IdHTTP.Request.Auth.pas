unit GBClient.IdHTTP.Request.Auth;

interface

uses
  GBClient.Interfaces,
  GBClient.IdHTTP.Request.Auth.Basic,
  GBClient.IdHTTP.Request.Auth.Bearer,
  IdHTTP,
  System.SysUtils;

type TGBClientIdHTTPAuth = class(TInterfacedObject, IGBClientAuth)

  private
    [Weak]
    FParent: IGBClientRequest;

    FAuthBasic  : IGBClientAuthBasic;
    FAuthBearer : IGBClientAuthBearer;
    FIdHTTP     : TIdHTTP;

  protected
    function Basic  : IGBClientAuthBasic;
    function Bearer : IGBClientAuthBearer;

    function &End: IGBClientRequest;

  public
    constructor create(Parent: IGBClientRequest; IdHTTP: TIdHTTP);
    class function New(Parent: IGBClientRequest; IdHTTP: TIdHTTP): IGBClientAuth;
    destructor  Destroy; override;
end;


implementation

{ TGBClientIdHTTPAuth }

function TGBClientIdHTTPAuth.Basic: IGBClientAuthBasic;
begin
  if not Assigned(FAuthBasic) then
    FAuthBasic := TGBClientIdHTTPRequestAuthBasic.New(Self, FIdHTTP);
  result := FAuthBasic;
end;

function TGBClientIdHTTPAuth.Bearer: IGBClientAuthBearer;
begin
  if not Assigned(FAuthBearer) then
    FAuthBearer := TGBClientIdHTTPAuthBearer.New(Self, FIdHTTP);
  result := FAuthBearer;
end;

function TGBClientIdHTTPAuth.&End: IGBClientRequest;
begin
  result := FParent;
end;

constructor TGBClientIdHTTPAuth.create(Parent: IGBClientRequest; IdHTTP: TIdHTTP);
begin
  FParent := Parent;
  FIdHTTP := IdHTTP;
end;

destructor TGBClientIdHTTPAuth.Destroy;
begin
  inherited;
end;

class function TGBClientIdHTTPAuth.New(Parent: IGBClientRequest; IdHTTP: TIdHTTP): IGBClientAuth;
begin
  result := Self.create(Parent, IdHTTP);
end;

end.

