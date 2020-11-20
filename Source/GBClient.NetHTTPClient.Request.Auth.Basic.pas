unit GBClient.NetHTTPClient.Request.Auth.Basic;

interface

uses
  GBClient.Interfaces,
  System.Net.HttpClientComponent,
  System.Net.URLClient;

type TGBClientNetHTTPClientRequestAuthBasic = class(TInterfacedObject, IGBClientAuthBasic)

  private
    [Weak]
    FParent : IGBClientAuth;
    FRequest: TNetHTTPRequest;
    FUsername: string;
    FPassword: String;

    procedure AuthBasicEvent(const Sender: TObject; AnAuthTarget: TAuthTargetType;
                             const ARealm, AURL: string; var AUserName, APassword: string;
                             var AbortAuth: Boolean;
                             var Persistence: TAuthPersistenceType);

  protected
    function Password(Value: string): IGBClientAuthBasic;
    function Username(Value: string): IGBClientAuthBasic;

    function &End: IGBClientAuth;

  public
    constructor create(Parent: IGBClientAuth; Request: TNetHTTPRequest);
    class function New(Parent: IGBClientAuth; Request: TNetHTTPRequest): IGBClientAuthBasic;
    destructor  Destroy; override;

end;

implementation

{ TGBClientNetHTTPClientRequestAuthBasic }

procedure TGBClientNetHTTPClientRequestAuthBasic.AuthBasicEvent(
  const Sender: TObject; AnAuthTarget: TAuthTargetType; const ARealm,
  AURL: string; var AUserName, APassword: string; var AbortAuth: Boolean;
  var Persistence: TAuthPersistenceType);
begin
  AUserName := FUsername;
  APassword := FPassword;
  AbortAuth := FUsername = '';
end;

constructor TGBClientNetHTTPClientRequestAuthBasic.create(Parent: IGBClientAuth; Request: TNetHTTPRequest);
begin
  FParent  := Parent;
  FRequest := Request;

  FRequest.Client.OnAuthEvent := AuthBasicEvent;
end;

destructor TGBClientNetHTTPClientRequestAuthBasic.Destroy;
begin
  inherited;
end;

function TGBClientNetHTTPClientRequestAuthBasic.&End: IGBClientAuth;
begin
  result := FParent;
end;

class function TGBClientNetHTTPClientRequestAuthBasic.New(Parent: IGBClientAuth; Request: TNetHTTPRequest): IGBClientAuthBasic;
begin
  result := Self.create(Parent, Request);
end;

function TGBClientNetHTTPClientRequestAuthBasic.Password(Value: string): IGBClientAuthBasic;
begin
  result    := Self;
  FPassword := Value;
end;

function TGBClientNetHTTPClientRequestAuthBasic.Username(Value: string): IGBClientAuthBasic;
begin
  result    := Self;
  FUsername := Value;
end;

end.
