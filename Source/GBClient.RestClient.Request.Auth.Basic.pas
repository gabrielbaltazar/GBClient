unit GBClient.RestClient.Request.Auth.Basic;

interface

uses
  GBClient.Interfaces,
  REST.Client,
  REST.Authenticator.Basic;

type TGBClientRestClientRequestAuthBasic = class(TInterfacedObject, IGBClientAuthBasic)

  private
    [Weak]
    FParent : IGBClientAuth;
    FRequest: TRESTRequest;
    FAuth   : THTTPBasicAuthenticator;

  protected
    function Password(Value: string): IGBClientAuthBasic;
    function Username(Value: string): IGBClientAuthBasic;

    function &End: IGBClientAuth;

  public
    constructor create(Parent: IGBClientAuth; Request: TRESTRequest);
    class function New(Parent: IGBClientAuth; Request: TRESTRequest): IGBClientAuthBasic;
    destructor  Destroy; override;

end;

implementation

{ TGBClientRestClientRequestAuthBasic }

constructor TGBClientRestClientRequestAuthBasic.create(Parent: IGBClientAuth; Request: TRESTRequest);
begin
  FParent  := Parent;
  FRequest := Request;
  FAuth    := THTTPBasicAuthenticator.Create(nil);

  FRequest.Client.Authenticator := FAuth;
end;

destructor TGBClientRestClientRequestAuthBasic.Destroy;
begin
  FAuth.Free;
  inherited;
end;

function TGBClientRestClientRequestAuthBasic.&End: IGBClientAuth;
begin
  result := FParent;
end;

class function TGBClientRestClientRequestAuthBasic.New(Parent: IGBClientAuth; Request: TRESTRequest): IGBClientAuthBasic;
begin
  result := Self.create(Parent, Request);
end;

function TGBClientRestClientRequestAuthBasic.Password(Value: string): IGBClientAuthBasic;
begin
  result := Self;
  FAuth.Password := Value;
end;

function TGBClientRestClientRequestAuthBasic.Username(Value: string): IGBClientAuthBasic;
begin
  result := Self;
  FAuth.Username := Value;
end;

end.
