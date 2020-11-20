unit GBClient.Base.Request.ParamQuery;

interface

uses
  GBClient.Interfaces,
  GBClient.Helpers,
  System.SysUtils,
  System.Classes;

type TGBClientBaseRequestParamQuery = class(TInterfacedObject, IGBClientParamQuery)

  private
    [Weak]
    FParent: IGBClientRequest;
    FParams: TStrings;

  protected
    function AddOrSet(Key: string; Value: String)   : IGBClientParamQuery; overload;
    function AddOrSet(Key: string; Value: Integer)  : IGBClientParamQuery; overload;
    function AddOrSet(Key: string; Value: Extended) : IGBClientParamQuery; overload;
    function AddOrSet(Key: string; Value: TDateTime): IGBClientParamQuery; overload;

    function &End: IGBClientRequest;

  public
    function Params: TStrings;
    procedure Clear;

    class function New(Parent: IGBClientRequest): IGBClientParamQuery;
    constructor create(Parent: IGBClientRequest);
    destructor  Destroy; override;
end;

implementation

{ TGBClientBaseRequestParamQuery }

function TGBClientBaseRequestParamQuery.AddOrSet(Key, Value: String): IGBClientParamQuery;
begin
  result := Self;
  FParams.Values[Key] := Value;
end;

function TGBClientBaseRequestParamQuery.AddOrSet(Key: string; Value: Integer): IGBClientParamQuery;
begin
  result := AddOrSet(Key, Value.ToString);
end;

function TGBClientBaseRequestParamQuery.AddOrSet(Key: string; Value: TDateTime): IGBClientParamQuery;
begin
  result := AddOrSet(Key, Value.DateTimeToIso8601);
end;

procedure TGBClientBaseRequestParamQuery.Clear;
begin
  FParams.Clear;
end;

function TGBClientBaseRequestParamQuery.AddOrSet(Key: string; Value: Extended): IGBClientParamQuery;
begin
  result := AddOrSet(Key, Value.ToString)
end;

function TGBClientBaseRequestParamQuery.&End: IGBClientRequest;
begin
  Result := FParent;
end;

constructor TGBClientBaseRequestParamQuery.create(Parent: IGBClientRequest);
begin
  FParent := Parent;
  FParams := TStringList.Create;
end;

destructor TGBClientBaseRequestParamQuery.Destroy;
begin
  FParams.Free;
  inherited;
end;

class function TGBClientBaseRequestParamQuery.New(Parent: IGBClientRequest): IGBClientParamQuery;
begin
  result := Self.create(Parent);
end;

function TGBClientBaseRequestParamQuery.Params: TStrings;
begin
  result := FParams;
end;

end.
