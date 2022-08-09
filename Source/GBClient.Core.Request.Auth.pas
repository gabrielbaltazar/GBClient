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

type
  TGBClientCoreRequestAuth = class(TInterfacedObject, IGBClientAuth,
    IGBClientAuthBasic, IGBClientAuthBearer)
  protected
    [Weak]
    FParent: IGBClientRequest;
    FAuthType: TGBAuthType;
    FUsername: string;
    FPassword: string;
    FToken: string;
    FAWSv4: IGBClientAuthAWSv4;

    function Basic: IGBClientAuthBasic;
    function Bearer: IGBClientAuthBearer;
    function AWSv4: IGBClientAuthAWSv4;
    function AuthType: TGBAuthType;

    function Username(AValue: string): IGBClientAuthBasic;
    function Password(AValue: string): IGBClientAuthBasic;
    function Token(AValue: string): IGBClientAuthBearer;
    procedure ApplyAWSv4;
    function &End: IGBClientRequest;
  public
    constructor Create(AParent: IGBClientRequest);
    class function New(AParent: IGBClientRequest): IGBClientAuth;
  end;

implementation

{ TGBClientCoreRequestAuth }

procedure TGBClientCoreRequestAuth.ApplyAWSv4;
begin
  TGBClientCoreRequestAuthAWS(FAWSv4).Apply;
end;

function TGBClientCoreRequestAuth.AuthType: TGBAuthType;
begin
  Result := FAuthType;
end;

function TGBClientCoreRequestAuth.AWSv4: IGBClientAuthAWSv4;
begin
  if not Assigned(FAWSv4) then
    FAWSv4 := TGBClientCoreRequestAuthAWS.New(FParent);
  Result := FAWSv4;
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

constructor TGBClientCoreRequestAuth.Create(AParent: IGBClientRequest);
begin
  FParent := AParent;
  FAuthType := atNone;
end;

function TGBClientCoreRequestAuth.&End: IGBClientRequest;
begin
  Result := FParent;
end;

class function TGBClientCoreRequestAuth.New(AParent: IGBClientRequest): IGBClientAuth;
begin
  Result := Self.Create(AParent);
end;

function TGBClientCoreRequestAuth.Password(AValue: string): IGBClientAuthBasic;
begin
  Result := Self;
  FPassword := AValue;
  FAuthType := atBasic;
end;

function TGBClientCoreRequestAuth.Token(AValue: string): IGBClientAuthBearer;
begin
  Result := Self;
  FToken := AValue;
  FAuthType := atBearer;
end;

function TGBClientCoreRequestAuth.Username(AValue: string): IGBClientAuthBasic;
begin
  Result := Self;
  FUsername := AValue;
  FAuthType := atBasic;
end;

end.
