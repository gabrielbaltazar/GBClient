unit GBClient.Base.Request.ParamPath;

interface

uses
  GBClient.Interfaces,
  GBClient.Helpers,
  System.SysUtils,
  System.Classes;

type TGBClientBaseRequestParamPath = class(TInterfacedObject, IGBClientParamPath)

  private
    [Weak]
    FParent: IGBClientRequest;
    FParams: TStrings;

  protected
    function AddOrSet(Key: string; Value: String)   : IGBClientParamPath; overload;
    function AddOrSet(Key: string; Value: Integer)  : IGBClientParamPath; overload;
    function AddOrSet(Key: string; Value: Extended) : IGBClientParamPath; overload;
    function AddOrSet(Key: string; Value: TDateTime): IGBClientParamPath; overload;

    function &End: IGBClientRequest;

  public
    function Params: TStrings;
    procedure Clear;

    class function New(Parent: IGBClientRequest): IGBClientParamPath;
    constructor create(Parent: IGBClientRequest);
    destructor  Destroy; override;
end;

implementation

{ TGBClientBaseRequestParamPath }

function TGBClientBaseRequestParamPath.AddOrSet(Key, Value: String): IGBClientParamPath;
begin
  result := Self;
  FParams.Values[Key] := Value;
end;

function TGBClientBaseRequestParamPath.AddOrSet(Key: string; Value: Integer): IGBClientParamPath;
begin
  result := AddOrSet(Key, Value.ToString);
end;

function TGBClientBaseRequestParamPath.AddOrSet(Key: string; Value: TDateTime): IGBClientParamPath;
begin
  result := AddOrSet(Key, Value.DateTimeToIso8601);
end;

procedure TGBClientBaseRequestParamPath.Clear;
begin
  FParams.Clear;
end;

function TGBClientBaseRequestParamPath.AddOrSet(Key: string; Value: Extended): IGBClientParamPath;
begin
  result := AddOrSet(Key, Value.ToString)
end;

function TGBClientBaseRequestParamPath.&End: IGBClientRequest;
begin
  Result := FParent;
end;

constructor TGBClientBaseRequestParamPath.create(Parent: IGBClientRequest);
begin
  FParent := Parent;
  FParams := TStringList.Create;
end;

destructor TGBClientBaseRequestParamPath.Destroy;
begin
  FParams.Free;
  inherited;
end;

class function TGBClientBaseRequestParamPath.New(Parent: IGBClientRequest): IGBClientParamPath;
begin
  result := Self.create(Parent);
end;

function TGBClientBaseRequestParamPath.Params: TStrings;
begin
  result := FParams;
end;

end.
