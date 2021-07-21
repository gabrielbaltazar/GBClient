unit GBClient.RestClient.Request.Auth.Bearer;

interface

uses
  GBClient.Interfaces,
  REST.Client,
  System.SysUtils;

const
  HEADER_AUTH   = 'Authorization';
  HEADER_BEARER = 'Bearer ';

type TGBClientRestClientRequestAuthBearer = class(TInterfacedObject, IGBClientAuthBearer)

  private
    [Weak]
    FParent  : IGBClientAuth;
    FRequest : TRESTRequest;

  protected
    function Token(Value: string): IGBClientAuthBearer;

    function &End: IGBClientAuth;

  public
    constructor create(Parent: IGBClientAuth; Request: TRESTRequest);
    class function New(Parent: IGBClientAuth; Request: TRESTRequest): IGBClientAuthBearer;
    destructor  Destroy; override;
end;

implementation

{ TGBClientRestClientRequestAuthBearer }

constructor TGBClientRestClientRequestAuthBearer.create(Parent: IGBClientAuth; Request: TRESTRequest);
begin
  FParent  := Parent;
  FRequest := Request;
end;

destructor TGBClientRestClientRequestAuthBearer.Destroy;
begin

  inherited;
end;

function TGBClientRestClientRequestAuthBearer.&End: IGBClientAuth;
begin
  result := FParent;
end;

class function TGBClientRestClientRequestAuthBearer.New(Parent: IGBClientAuth; Request: TRESTRequest): IGBClientAuthBearer;
begin
  result := Self.create(Parent, Request);
end;

function TGBClientRestClientRequestAuthBearer.Token(Value: string): IGBClientAuthBearer;
begin
  result := Self;
  FParent.&End.Header.AddOrSet(HEADER_AUTH, HEADER_BEARER + Value, False);
end;

end.
