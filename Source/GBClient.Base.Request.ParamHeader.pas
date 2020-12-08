unit GBClient.Base.Request.ParamHeader;

interface

uses
  GBClient.Interfaces,
  GBClient.Helpers,
  System.SysUtils,
  System.Classes;

type TGBClientBaseRequestParamHeader = class(TInterfacedObject, IGBClientParamHeader)

  private
    [Weak]
    FParent: IGBClientRequest;
    FParams: TStrings;

  protected
    function AddOrSet(Key: string; Value: String; bEncode: Boolean = True)   : IGBClientParamHeader; overload;
    function AddOrSet(Key: string; Value: Integer; bEncode: Boolean = True)  : IGBClientParamHeader; overload;
    function AddOrSet(Key: string; Value: Extended; bEncode: Boolean = True) : IGBClientParamHeader; overload;
    function AddOrSet(Key: string; Value: TDateTime; bEncode: Boolean = True): IGBClientParamHeader; overload;

    function &End: IGBClientRequest;

  public
    function Params: TStrings;
    procedure Clear;

    class function New(Parent: IGBClientRequest): IGBClientParamHeader;
    constructor create(Parent: IGBClientRequest);
    destructor  Destroy; override;
end;

implementation

{ TGBClientBaseRequestParamHeader }

function TGBClientBaseRequestParamHeader.AddOrSet(Key, Value: String; bEncode: Boolean): IGBClientParamHeader;
begin
  result := Self;
  FParams.Values[Key] := Value;
end;

function TGBClientBaseRequestParamHeader.AddOrSet(Key: string; Value: Integer; bEncode: Boolean): IGBClientParamHeader;
begin
  result := AddOrSet(Key, Value.ToString);
end;

function TGBClientBaseRequestParamHeader.AddOrSet(Key: string; Value: TDateTime; bEncode: Boolean): IGBClientParamHeader;
begin
  result := AddOrSet(Key, Value.DateTimeToIso8601);
end;

procedure TGBClientBaseRequestParamHeader.Clear;
begin
  FParams.Clear;
end;

function TGBClientBaseRequestParamHeader.AddOrSet(Key: string; Value: Extended; bEncode: Boolean): IGBClientParamHeader;
begin
  result := AddOrSet(Key, Value.ToString)
end;

function TGBClientBaseRequestParamHeader.&End: IGBClientRequest;
begin
  Result := FParent;
end;

constructor TGBClientBaseRequestParamHeader.create(Parent: IGBClientRequest);
begin
  FParent := Parent;
  FParams := TStringList.Create;
end;

destructor TGBClientBaseRequestParamHeader.Destroy;
begin
  FParams.Free;
  inherited;
end;

class function TGBClientBaseRequestParamHeader.New(Parent: IGBClientRequest): IGBClientParamHeader;
begin
  result := Self.create(Parent);
end;

function TGBClientBaseRequestParamHeader.Params: TStrings;
begin
  result := FParams;
end;

end.
