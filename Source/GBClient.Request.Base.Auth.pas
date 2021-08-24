unit GBClient.Request.Base.Auth;

interface

uses
  GBClient.Interfaces,
  System.SysUtils,
  System.Classes;

type TGBClientRequestBaseAuth = class(TInterfacedObject, IGBClientAuth,
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

{ TGBClientRequestBaseAuth }

function TGBClientRequestBaseAuth.Basic: IGBClientAuthBasic;
begin
  Result := Self;
end;

function TGBClientRequestBaseAuth.Bearer: IGBClientAuthBearer;
begin
  Result := Self;
end;

constructor TGBClientRequestBaseAuth.create(Parent: IGBClientRequest);
begin
  FParent := Parent;
end;

function TGBClientRequestBaseAuth.&End: IGBClientRequest;
begin
  result := FParent;
end;

class function TGBClientRequestBaseAuth.New(Parent: IGBClientRequest): IGBClientAuth;
begin
  result := Self.create(Parent);
end;

function TGBClientRequestBaseAuth.Password(Value: String): IGBClientAuthBasic;
begin
  result := Self;
  FPassword := Value;
end;

function TGBClientRequestBaseAuth.Token(Value: String): IGBClientAuthBearer;
begin
  result := Self;
  FToken := Value;
end;

function TGBClientRequestBaseAuth.Username(Value: String): IGBClientAuthBasic;
begin
  result := Self;
  FUsername := Value;
end;

end.
