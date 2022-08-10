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

type
  TGBClientNetHTTPClientAuth = class(TGBClientCoreRequestAuth, IGBClientAuth,
    IGBClientAuthBasic, IGBClientAuthBearer)
  private
    procedure ApplyBasicAuth;
    procedure ApplyBearerAuth;

    procedure AuthBasicEvent(const ASender: TObject;
      AAnAuthTarget: TAuthTargetType; const ARealm, AURL: string;
      var AUserName, APassword: string; var AbortAuth: Boolean;
      var APersistence: TAuthPersistenceType);
  public
    destructor Destroy; override;

    procedure ApplyAuth;
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
  LToken : string;
begin
  LToken  := FToken;
  if not LToken.ToLower.StartsWith(HEADER_BEARER.ToLower) then
    LToken := HEADER_BEARER + LToken;
  TNetHTTPRequest(FParent.Component).CustomHeaders[HEADER_AUTH] := LToken;
end;

procedure TGBClientNetHTTPClientAuth.AuthBasicEvent(const ASender: TObject;
  AAnAuthTarget: TAuthTargetType; const ARealm, AURL: string; var AUserName,
  APassword: string; var AbortAuth: Boolean;
  var APersistence: TAuthPersistenceType);
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
