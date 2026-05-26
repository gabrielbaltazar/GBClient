unit GBClient.Core.Request.Param;

interface

{$IFDEF WEAKPACKAGEUNIT}
  {$WEAKPACKAGEUNIT ON}
{$ENDIF}

uses
  System.Generics.Collections;

type
  TGBClientCoreRequestParam = class
  private
    FKey: string;
    FValue: string;
    FEncoding: Boolean;
  public
    constructor Create(const AKey, AValue: string; const AEncoding: Boolean = False);

    class function GetParam(const AList: TObjectList<TGBClientCoreRequestParam>; const AKey: string):
      TGBClientCoreRequestParam;
    class procedure AddOrSet(const AList: TObjectList<TGBClientCoreRequestParam>; const AKey, AValue: string;
      const AEncoding: Boolean = False); overload;
    class procedure AddOrSet(const AList: TObjectList<TGBClientCoreRequestParam>; const AKey: string;
      const AValue: Integer; const AEncoding: Boolean = False); overload;
    class procedure AddOrSet(const AList: TObjectList<TGBClientCoreRequestParam>; const AKey: string;
      const AValue: Extended; AEncoding: Boolean = False); overload;
    class procedure AddOrSet(const AList: TObjectList<TGBClientCoreRequestParam>; const AKey: string;
      const AValue: TDateTime; const AEncoding: Boolean = False); overload;

    property Key: string read FKey;
    property Value: string read FValue;
    property Encoding: Boolean read FEncoding;
  end;

implementation

uses
  GBClient.Core.Helpers,
  System.SysUtils;

{ TGBClientCoreRequestParam }

class procedure TGBClientCoreRequestParam.AddOrSet(const AList: TObjectList<TGBClientCoreRequestParam>;
  const AKey, AValue: string; const AEncoding: Boolean = False);
var
  LParam: TGBClientCoreRequestParam;
begin
  LParam := GetParam(AList, AKey);
  if not Assigned(LParam) then
  begin
    LParam := TGBClientCoreRequestParam.create(AKey, AValue, AEncoding);
    AList.Add(LParam);
    Exit;
  end;

  LParam.FKey := AKey;
  LParam.FValue := AValue;
  LParam.FEncoding := AEncoding;
end;

constructor TGBClientCoreRequestParam.Create(const AKey, AValue: string; const AEncoding: Boolean = False);
begin
  FKey := AKey;
  FValue := AValue;
  FEncoding := AEncoding;
end;

class procedure TGBClientCoreRequestParam.AddOrSet(const AList: TObjectList<TGBClientCoreRequestParam>;
  const AKey: string; const AValue: Integer; const AEncoding: Boolean = False);
begin
  AddOrSet(AList, AKey, AValue.ToString);
end;

class procedure TGBClientCoreRequestParam.AddOrSet(const AList: TObjectList<TGBClientCoreRequestParam>;
  const AKey: string; const AValue: Extended; AEncoding: Boolean = False);
begin
  AddOrSet(AList, AKey, AValue.ToString);
end;

class procedure TGBClientCoreRequestParam.AddOrSet(const AList: TObjectList<TGBClientCoreRequestParam>;
  const AKey: string; const AValue: TDateTime; const AEncoding: Boolean = False);
begin
  AddOrSet(AList, AKey, AValue.DateTimeToIso8601);
end;

class function TGBClientCoreRequestParam.GetParam(const AList: TObjectList<TGBClientCoreRequestParam>;
  const AKey: string): TGBClientCoreRequestParam;
var
  LCount: Integer;
begin
  Result := nil;
  for LCount := 0 to Pred(AList.Count) do
  begin
    if AKey.ToLower.Equals(AList[LCount].Key.ToLower) then
      Exit(AList[LCount]);
  end;
end;

end.
