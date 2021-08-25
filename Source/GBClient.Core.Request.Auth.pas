unit GBClient.Core.Request.Auth;

interface

uses
  GBClient.Interfaces,
  System.SysUtils,
  System.Classes;

type TGBClientCoreRequestAuth = class(TInterfacedObject, IGBClientAuth,
                                                         IGBClientAuthBasic,
                                                         IGBClientAuthBearer)
  protected
    [Weak]
    FParent: IGBClientRequest;

    FUsername: String;
    FPassword: string;
    FToken: string;

    function Basic: IGBClientAuthBasic;
    function Bearer: IGBClientAuthBearer;

    function Username(Value: String): IGBClientAuthBasic;
    function Password(Value: String): IGBClientAuthBasic;
    function Token(Value: String): IGBClientAuthBearer;

    function &End: IGBClientRequest;

  public
    constructor create(Parent: IGBClientRequest);
    class function New(Parent: IGBClientRequest): IGBClientAuth;
end;

implementation

{ TGBClientCoreRequestAuth }

function TGBClientCoreRequestAuth.Basic: IGBClientAuthBasic;
begin
  Result := Self;
end;

function TGBClientCoreRequestAuth.Bearer: IGBClientAuthBearer;
begin
  Result := Self;
end;

constructor TGBClientCoreRequestAuth.create(Parent: IGBClientRequest);
begin
  FParent := Parent;
end;

function TGBClientCoreRequestAuth.&End: IGBClientRequest;
begin
  result := FParent;
end;

class function TGBClientCoreRequestAuth.New(Parent: IGBClientRequest): IGBClientAuth;
begin
  result := Self.create(Parent);
end;

function TGBClientCoreRequestAuth.Password(Value: String): IGBClientAuthBasic;
begin
  result := Self;
  FPassword := Value;
end;

function TGBClientCoreRequestAuth.Token(Value: String): IGBClientAuthBearer;
begin
  result := Self;
  FToken := Value;
end;

function TGBClientCoreRequestAuth.Username(Value: String): IGBClientAuthBasic;
begin
  result := Self;
  FUsername := Value;
end;

end.
