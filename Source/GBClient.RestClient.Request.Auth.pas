unit GBClient.RestClient.Request.Auth;

interface

uses
  GBClient.Interfaces,
  GBClient.RestClient.Request.Auth.Basic,
  GBClient.RestClient.Request.Auth.Bearer,
  REST.Client,
  System.SysUtils;

type TGBClientRestClientAuth = class(TInterfacedObject, IGBClientAuth)

  private
    [Weak]
    FParent: IGBClientRequest;

    FAuthBasic  : IGBClientAuthBasic;
    FAuthBearer : IGBClientAuthBearer;
    FRequest    : TRESTRequest;

  protected
    function Basic  : IGBClientAuthBasic;
    function Bearer : IGBClientAuthBearer;

    function &End: IGBClientRequest;

  public
    constructor create(Parent: IGBClientRequest; Request: TRESTRequest);
    class function New(Parent: IGBClientRequest; Request: TRESTRequest): IGBClientAuth;
    destructor  Destroy; override;
end;


implementation

{ TGBClientRestClientAuth }

function TGBClientRestClientAuth.Basic: IGBClientAuthBasic;
begin
  if not Assigned(FAuthBasic) then
    FAuthBasic := TGBClientRestClientRequestAuthBasic.New(Self, FRequest);
  result := FAuthBasic;
end;

function TGBClientRestClientAuth.Bearer: IGBClientAuthBearer;
begin
  if not Assigned(FAuthBearer) then
    FAuthBearer := TGBClientRestClientRequestAuthBearer.New(Self, FRequest);
  result := FAuthBearer;
end;

function TGBClientRestClientAuth.&End: IGBClientRequest;
begin
  result := FParent;
end;

constructor TGBClientRestClientAuth.create(Parent: IGBClientRequest; Request: TRESTRequest);
begin
  FParent  := Parent;
  FRequest := Request;
end;

destructor TGBClientRestClientAuth.Destroy;
begin
  inherited;
end;

class function TGBClientRestClientAuth.New(Parent: IGBClientRequest; Request: TRESTRequest): IGBClientAuth;
begin
  result := Self.create(Parent, Request);
end;

end.

