unit GBClient.RestClient.Auth;

interface

{$IFDEF WEAKPACKAGEUNIT}
  {$WEAKPACKAGEUNIT ON}
{$ENDIF}

uses
  GBClient.Interfaces,
  GBClient.Core.Request.Auth,
  REST.Client,
  REST.Authenticator.Basic,
  REST.Authenticator.OAuth,
  System.Classes,
  System.SysUtils;

type
  TGBClientRestClientAuth = class(TGBClientCoreRequestAuth, IGBClientAuth,
    IGBClientAuthBasic, IGBClientAuthBearer)
  private
    FAuthenticator: TCustomAuthenticator;

    procedure ApplyBasicAuth;
    procedure ApplyBearerAuth;
  public
    destructor Destroy; override;
    procedure ApplyAuth;
  end;

implementation

{ TGBClientRestClientAuth }

procedure TGBClientRestClientAuth.ApplyAuth;
begin
  case FAuthType of
    atNone:;
    atBasic: ApplyBasicAuth;
    atBearer: ApplyBearerAuth;
    atAWSv4: ApplyAWSv4;
  end;
end;

procedure TGBClientRestClientAuth.ApplyBasicAuth;
begin
  FreeAndNil(FAuthenticator);
  FAuthenticator := THTTPBasicAuthenticator.Create(nil);
  THTTPBasicAuthenticator(FAuthenticator).Username := FUsername;
  THTTPBasicAuthenticator(FAuthenticator).Password := FPassword;
  TRESTRequest(FParent.Component).Client.Authenticator := FAuthenticator;
end;

procedure TGBClientRestClientAuth.ApplyBearerAuth;
begin
  FreeAndNil(FAuthenticator);
  FAuthenticator := TOAuth2Authenticator.Create(nil);
  TOAuth2Authenticator(FAuthenticator).AccessToken := FToken;
  TOAuth2Authenticator(FAuthenticator).TokenType := TOAuth2TokenType.ttBEARER;
  TRESTRequest(FParent.Component).Client.Authenticator := FAuthenticator;
end;

destructor TGBClientRestClientAuth.Destroy;
begin
  FAuthenticator.Free;
  inherited;
end;

end.
