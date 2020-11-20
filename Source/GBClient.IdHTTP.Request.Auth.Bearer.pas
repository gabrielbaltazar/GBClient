unit GBClient.IdHTTP.Request.Auth.Bearer;

interface

uses
  GBClient.Interfaces,
  IdHTTP,
  System.SysUtils;

const
  HEADER_AUTH   = 'Authorization';
  HEADER_BEARER = 'Bearer ';

type TGBClientIdHTTPAuthBearer = class(TInterfacedObject, IGBClientAuthBearer)

  private
    [Weak]
    FParent : IGBClientAuth;
    FIdHTTP : TIdHTTP;

  protected
    function Token(Value: string): IGBClientAuthBearer;

    function &End: IGBClientAuth;

  public
    constructor create(Parent: IGBClientAuth; IdHTTP: TIdHTTP);
    class function New(Parent: IGBClientAuth; IdHTTP: TIdHTTP): IGBClientAuthBearer;
    destructor  Destroy; override;
end;

implementation

{ TGBClientIdHTTPAuthBearer }

constructor TGBClientIdHTTPAuthBearer.create(Parent: IGBClientAuth; IdHTTP: TIdHTTP);
begin
  FParent := Parent;
  FIdHTTP := IdHTTP;
end;

destructor TGBClientIdHTTPAuthBearer.Destroy;
begin

  inherited;
end;

function TGBClientIdHTTPAuthBearer.&End: IGBClientAuth;
begin
  result := FParent;
end;

class function TGBClientIdHTTPAuthBearer.New(Parent: IGBClientAuth; IdHTTP: TIdHTTP): IGBClientAuthBearer;
begin
  result := Self.create(Parent, IdHTTP);
end;

function TGBClientIdHTTPAuthBearer.Token(Value: string): IGBClientAuthBearer;
var
  token : string;
begin
  result := Self;
  token  := Value;
  if not token.ToLower.StartsWith(HEADER_BEARER.ToLower) then
    token := HEADER_BEARER + token;
  FParent.&End.Header.AddOrSet(HEADER_AUTH, token);
end;

end.
