unit GBClient.NetHTTPClient.Auth;

interface

{$IFDEF WEAKPACKAGEUNIT}
  {$WEAKPACKAGEUNIT ON}
{$ENDIF}

uses
  System.Net.HttpClientComponent,
  System.Net.URLClient,
  System.SysUtils,
  GBClient.Interfaces,
  GBClient.Core.Request.Auth;

type
  TGBClientNetHTTPClientAuth = class(TGBClientCoreRequestAuth, IGBClientAuth, IGBClientAuthBasic, IGBClientAuthBearer)
  private
    procedure ApplyBasicAuth;
    procedure ApplyBearerAuth;

    procedure AuthBasicEvent(const ASender: TObject; AAnAuthTarget: TAuthTargetType; const ARealm, AURL: string;
      var AUserName, APassword: string; var AAbortAuth: Boolean; var APersistence: TAuthPersistenceType);
  public
    procedure ApplyAuth;
  end;

implementation

{ TGBClientNetHTTPClientAuth }

procedure TGBClientNetHTTPClientAuth.ApplyAuth;
begin
  case FAuthType of
    atBasic:
      ApplyBasicAuth;
    atBearer:
      ApplyBearerAuth;
    atAWSv4:
      ApplyAWSv4;
  end;
end;

procedure TGBClientNetHTTPClientAuth.ApplyBasicAuth;
begin
  TNetHTTPRequest(FParent.Component)
    .Client.OnAuthEvent := AuthBasicEvent;
end;

procedure TGBClientNetHTTPClientAuth.ApplyBearerAuth;
const
  CHeaderAuth = 'Authorization';
  CHeaderBearer = 'Bearer ';
var
  LToken: string;
begin
  LToken  := FToken;
  if not LToken.ToLower.StartsWith(CHeaderBearer.ToLower) then
    LToken := CHeaderBearer + LToken;

  TNetHTTPRequest(FParent.Component).CustomHeaders[CHeaderAuth] := LToken;
end;

procedure TGBClientNetHTTPClientAuth.AuthBasicEvent(const ASender: TObject;
  AAnAuthTarget: TAuthTargetType; const ARealm, AURL: string; var AUserName,
  APassword: string; var AAbortAuth: Boolean;
  var APersistence: TAuthPersistenceType);
begin
  AUserName := FUsername;
  APassword := FPassword;
  AAbortAuth := FUsername = '';
end;

end.
