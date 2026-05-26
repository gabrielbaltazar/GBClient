unit GBClient.Synapse.Auth;

interface

{$IFDEF WEAKPACKAGEUNIT}
  {$WEAKPACKAGEUNIT ON}
{$ENDIF}

uses
  System.SysUtils,
  blcksock,
  httpsend,
  ssl_openssl,
  synautil,
  syncobjs,
  GBClient.Interfaces,
  GBClient.Core.Request.Auth;

type
  TGBClientSynapseAuth = class(TGBClientCoreRequestAuth, IGBClientAuth, IGBClientAuthBasic, IGBClientAuthBearer)
  private
    procedure ApplyBasicAuth;
    procedure ApplyBearerAuth;
  public
    procedure ApplyAuth;
  end;

implementation

{ TGBClientSynapseAuth }

procedure TGBClientSynapseAuth.ApplyAuth;
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

procedure TGBClientSynapseAuth.ApplyBasicAuth;
var
  LHTTPSend: THTTPSend;
begin
  if (not FUsername.IsEmpty) and (not FPassword.IsEmpty) then
  begin
    LHTTPSend := THTTPSend(FParent.Component);
    LHTTPSend.UserName := FUsername;
    LHTTPSend.Password := FPassword;
  end;
end;

procedure TGBClientSynapseAuth.ApplyBearerAuth;
const
  CHeaderBearer = 'Bearer ';
var
  LHTTPSend: THTTPSend;
  LToken: string;
begin
  LToken := FToken;
  if not LToken.ToLower.StartsWith(CHeaderBearer.ToLower) then
    LToken := CHeaderBearer + LToken;
  LHTTPSend := THTTPSend(FParent.Component);
  LHTTPSend.Headers.Add('Authorization: ' + LToken)
end;

end.
