unit GBClient.Core.Request.Auth;

interface

{$IFDEF WEAKPACKAGEUNIT}
	{$WEAKPACKAGEUNIT ON}
{$ENDIF}

uses
  GBClient.Interfaces,
  GBClient.Core.Request.Auth.AWS,
  System.SysUtils,
  System.Classes;

type TGBClientCoreRequestAuth = class(TInterfacedObject, IGBClientAuth,
                                                         IGBClientAuthBasic,
                                                         IGBClientAuthBearer)
  protected
    [Weak]
    FParent: IGBClientRequest;

    FAuthType: TGBAuthType;
    FUsername: String;
    FPassword: string;
    FToken: string;
    FAWSv4: IGBClientAuthAWSv4;

    function Basic: IGBClientAuthBasic;
    function Bearer: IGBClientAuthBearer;
    function AWSv4: IGBClientAuthAWSv4;

    function AuthType: TGBAuthType;

    function Username(Value: String): IGBClientAuthBasic;
    function Password(Value: String): IGBClientAuthBasic;
    function Token(Value: String): IGBClientAuthBearer;

    procedure ApplyAWSv4;

    function &End: IGBClientRequest;

  public
    constructor create(Parent: IGBClientRequest);
    class function New(Parent: IGBClientRequest): IGBClientAuth;
end;

implementation

{ TGBClientCoreRequestAuth }

procedure TGBClientCoreRequestAuth.ApplyAWSv4;
begin
  TGBClientCoreRequestAuthAWS(FAWSv4).Apply;
end;

function TGBClientCoreRequestAuth.AuthType: TGBAuthType;
begin
  result := FAuthType;
end;

function TGBClientCoreRequestAuth.AWSv4: IGBClientAuthAWSv4;
begin
  if not Assigned(FAWSv4) then
    FAWSv4 := TGBClientCoreRequestAuthAWS.New(FParent);
  result := FAWSv4;
  FAuthType := atAWSv4;
end;

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
  FAuthType := atNone;
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
  FAuthType := atBasic;
end;

function TGBClientCoreRequestAuth.Token(Value: String): IGBClientAuthBearer;
begin
  result := Self;
  FToken := Value;
  FAuthType := atBearer;
end;

function TGBClientCoreRequestAuth.Username(Value: String): IGBClientAuthBasic;
begin
  result := Self;
  FUsername := Value;
  FAuthType := atBasic;
end;

end.
