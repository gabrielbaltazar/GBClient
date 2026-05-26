unit GBClient.IdHTTP.Auth;

interface

{$IFDEF WEAKPACKAGEUNIT}
  {$WEAKPACKAGEUNIT ON}
{$ENDIF}

uses
  GBClient.Interfaces,
  GBClient.Core.Request.Auth;

type
  TGBClientIdHTTPAuth = class(TGBClientCoreRequestAuth, IGBClientAuth, IGBClientAuthBasic, IGBClientAuthBearer)
  private
    procedure ApplyBasicAuth;
    procedure ApplyBearerAuth;
  public
    procedure ApplyAuth;
  end;

implementation

uses
  IdHTTP,
  System.SysUtils;

{ TGBClientIdHTTPAuth }

procedure TGBClientIdHTTPAuth.ApplyAuth;
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

procedure TGBClientIdHTTPAuth.ApplyBasicAuth;
begin
  TIdHTTP(FParent.Component).Request.Username := FUsername;
  TIdHTTP(FParent.Component).Request.Password := FPassword;
end;

procedure TGBClientIdHTTPAuth.ApplyBearerAuth;
const
  CHeaderAuth = 'Authorization';
  CHeaderBearer = 'Bearer ';
var
  LToken: string;
begin
  LToken := FToken;
  if not LToken.ToLower.StartsWith(CHeaderBearer.ToLower) then
    LToken := CHeaderBearer + LToken;
  TIdHTTP(FParent.Component).Request.CustomHeaders.AddValue(CHeaderAuth, LToken);
end;

end.
