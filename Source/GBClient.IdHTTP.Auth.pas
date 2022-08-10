unit GBClient.IdHTTP.Auth;

interface

{$IFDEF WEAKPACKAGEUNIT}
  {$WEAKPACKAGEUNIT ON}
{$ENDIF}

uses
  GBClient.Interfaces,
  GBClient.Core.Request.Auth,
  IdHTTP,
  System.SysUtils;

type
  TGBClientIdHTTPAuth = class(TGBClientCoreRequestAuth, IGBClientAuth,
    IGBClientAuthBasic, IGBClientAuthBearer)
  private
    procedure ApplyBasicAuth;
    procedure ApplyBearerAuth;
  public
    procedure ApplyAuth;
    destructor Destroy; override;
  end;

implementation

{ TGBClientIdHTTPAuth }

procedure TGBClientIdHTTPAuth.ApplyAuth;
begin
  case FAuthType of
    atNone:;
    atBasic: ApplyBasicAuth;
    atBearer: ApplyBearerAuth;
    atAWSv4: ApplyAWSv4;
  end;
end;

procedure TGBClientIdHTTPAuth.ApplyBasicAuth;
begin
  TIdHTTP(FParent.Component).Request.Username := FUsername;
  TIdHTTP(FParent.Component).Request.Password := FPassword;
end;

procedure TGBClientIdHTTPAuth.ApplyBearerAuth;
const
  HEADER_AUTH = 'Authorization';
  HEADER_BEARER = 'Bearer ';
var
  LToken : string;
begin
  LToken := FToken;
  if not LToken.ToLower.StartsWith(HEADER_BEARER.ToLower) then
    LToken := HEADER_BEARER + LToken;
  TIdHTTP(FParent.Component).Request.CustomHeaders.AddValue(HEADER_AUTH, LToken);
end;

destructor TGBClientIdHTTPAuth.Destroy;
begin
  inherited;
end;

end.
