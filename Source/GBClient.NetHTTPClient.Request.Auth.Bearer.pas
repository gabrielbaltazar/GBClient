unit GBClient.NetHTTPClient.Request.Auth.Bearer;

interface

uses
  GBClient.Interfaces,
  System.Net.HttpClientComponent,
  System.SysUtils;

const
  HEADER_AUTH   = 'Authorization';
  HEADER_BEARER = 'Bearer ';

type TGBClientNetHTTPClientAuthBearer = class(TInterfacedObject, IGBClientAuthBearer)

  private
    [Weak]
    FParent  : IGBClientAuth;
    FRequest : TNetHTTPRequest;

  protected
    function Token(Value: string): IGBClientAuthBearer;

    function &End: IGBClientAuth;

  public
    constructor create(Parent: IGBClientAuth; Request: TNetHTTPRequest);
    class function New(Parent: IGBClientAuth; Request: TNetHTTPRequest): IGBClientAuthBearer;
    destructor  Destroy; override;
end;

implementation

{ TGBClientNetHTTPClientAuthBearer }

constructor TGBClientNetHTTPClientAuthBearer.create(Parent: IGBClientAuth; Request: TNetHTTPRequest);
begin
  FParent  := Parent;
  FRequest := Request;
end;

destructor TGBClientNetHTTPClientAuthBearer.Destroy;
begin

  inherited;
end;

function TGBClientNetHTTPClientAuthBearer.&End: IGBClientAuth;
begin
  result := FParent;
end;

class function TGBClientNetHTTPClientAuthBearer.New(Parent: IGBClientAuth; Request: TNetHTTPRequest): IGBClientAuthBearer;
begin
  result := Self.create(Parent, Request);
end;

function TGBClientNetHTTPClientAuthBearer.Token(Value: string): IGBClientAuthBearer;
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
