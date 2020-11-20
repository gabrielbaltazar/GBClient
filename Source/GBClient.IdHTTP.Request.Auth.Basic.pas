unit GBClient.IdHTTP.Request.Auth.Basic;

interface

uses
  GBClient.Interfaces,
  IdHTTP;

type TGBClientIdHTTPRequestAuthBasic = class(TInterfacedObject, IGBClientAuthBasic)

  private
    [Weak]
    FParent : IGBClientAuth;
    FIdHTTP : TIdHTTP;

  protected
    function Password(Value: string): IGBClientAuthBasic;
    function Username(Value: string): IGBClientAuthBasic;

    function &End: IGBClientAuth;

  public
    constructor create(Parent: IGBClientAuth; IdHTTP: TIdHTTP);
    class function New(Parent: IGBClientAuth; IdHTTP: TIdHTTP): IGBClientAuthBasic;
    destructor  Destroy; override;

end;

implementation

{ TGBClientIdHTTPRequestAuthBasic }

constructor TGBClientIdHTTPRequestAuthBasic.create(Parent: IGBClientAuth; IdHTTP: TIdHTTP);
begin
  FParent := Parent;
  FIdHTTP := IdHTTP;

  FIdHTTP.Request.BasicAuthentication := True;
end;

destructor TGBClientIdHTTPRequestAuthBasic.Destroy;
begin
  inherited;
end;

function TGBClientIdHTTPRequestAuthBasic.&End: IGBClientAuth;
begin
  result := FParent;
end;

class function TGBClientIdHTTPRequestAuthBasic.New(Parent: IGBClientAuth; IdHTTP: TIdHTTP): IGBClientAuthBasic;
begin
  result := Self.create(Parent, IdHTTP);
end;

function TGBClientIdHTTPRequestAuthBasic.Password(Value: string): IGBClientAuthBasic;
begin
  result := Self;
  FIdHTTP.Request.Password := Value;
end;

function TGBClientIdHTTPRequestAuthBasic.Username(Value: string): IGBClientAuthBasic;
begin
  result := Self;
  FIdHTTP.Request.Username := Value;
end;

end.
