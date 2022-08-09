unit GBClient.NetHTTPClient.Auth;

interface

{$IFDEF WEAKPACKAGEUNIT}
	{$WEAKPACKAGEUNIT ON}
{$ENDIF}

uses
  GBClient.Interfaces,
  GBClient.Core.Request.Auth,
  System.Net.HttpClientComponent,
  System.Net.URLClient,
  System.SysUtils;

type TGBClientNetHTTPClientAuth = class(TGBClientCoreRequestAuth, IGBClientAuth,
                                                                  IGBClientAuthBasic,
                                                                  IGBClientAuthBearer)
  private
    procedure ApplyBasicAuth;
    procedure ApplyBearerAuth;

    procedure AuthBasicEvent(const Sender: TObject; AnAuthTarget: TAuthTargetType;
                             const ARealm, AURL: string; var AUserName, APassword: string;
                             var AbortAuth: Boolean;
                             var Persistence: TAuthPersistenceType);
  public
    procedure ApplyAuth;

    destructor Destroy; override;
end;

implementation

{ TGBClientNetHTTPClientAuth }

procedure TGBClientNetHTTPClientAuth.ApplyAuth;
begin
  case FAuthType of
    atNone:;
    atBasic: ApplyBasicAuth;
    atBearer: ApplyBearerAuth;
    atAWSv4: ApplyAWSv4;
  end;
end;

procedure TGBClientNetHTTPClientAuth.ApplyBasicAuth;
begin
  TNetHTTPRequest(FParent.Component)
    .Client.OnAuthEvent := AuthBasicEvent;
end;

procedure TGBClientNetHTTPClientAuth.ApplyBearerAuth;
const
  HEADER_AUTH = 'Authorization';
  HEADER_BEARER = 'Bearer ';
var
  token : string;
begin
  token  := FToken;
  if not token.ToLower.StartsWith(HEADER_BEARER.ToLower) then
    token := HEADER_BEARER + token;

  TNetHTTPRequest(FParent.Component).CustomHeaders[HEADER_AUTH] := token;
end;

procedure TGBClientNetHTTPClientAuth.AuthBasicEvent(const Sender: TObject;
  AnAuthTarget: TAuthTargetType; const ARealm, AURL: string; var AUserName,
  APassword: string; var AbortAuth: Boolean;
  var Persistence: TAuthPersistenceType);
begin
  AUserName := FUsername;
  APassword := FPassword;
  AbortAuth := FUsername = '';
end;

destructor TGBClientNetHTTPClientAuth.Destroy;
begin

  inherited;
end;

end.
