unit GBClient.Core.Request.Param;

interface

{$IFDEF WEAKPACKAGEUNIT}
  {$WEAKPACKAGEUNIT ON}
{$ENDIF}

uses
  GBClient.Core.Helpers,
  System.SysUtils,
  System.Generics.Collections;

type
  TGBClientCoreRequestParam = class
  private
    FKey: string;
    FValue: string;
    FEncoding: Boolean;
  public
    constructor Create(AKey: string; AValue: string; AEncoding: Boolean = False);

    class function GetParam(AList: TObjectList<TGBClientCoreRequestParam>; AKey: string): TGBClientCoreRequestParam;
    class procedure AddOrSet(AList: TObjectList<TGBClientCoreRequestParam>; AKey: string; AValue: string; AEncoding: Boolean = False); overload;
    class procedure AddOrSet(AList: TObjectList<TGBClientCoreRequestParam>; AKey: string; AValue: Integer; AEncoding: Boolean = False); overload;
    class procedure AddOrSet(AList: TObjectList<TGBClientCoreRequestParam>; AKey: string; AValue: Extended; AEncoding: Boolean = False); overload;
    class procedure AddOrSet(AList: TObjectList<TGBClientCoreRequestParam>; AKey: string; AValue: TDateTime; AEncoding: Boolean = False); overload;

    property Key: string read FKey;
    property Value: string read FValue;
    property Encoding: Boolean read FEncoding;
  end;

implementation

{ TGBClientCoreRequestParam }

class procedure TGBClientCoreRequestParam.AddOrSet(AList: TObjectList<TGBClientCoreRequestParam>; AKey, AValue: string; AEncoding: Boolean);
var
  LParam: TGBClientCoreRequestParam;
begin
  LParam := GetParam(AList, AKey);
  if not Assigned(LParam) then
  begin
    LParam := TGBClientCoreRequestParam.Create(AKey, AValue, AEncoding);
    AList.Add(LParam);
    Exit;
  end;

  LParam.FKey := AKey;
  LParam.FValue := AValue;
  LParam.FEncoding := AEncoding;
end;

constructor TGBClientCoreRequestParam.Create(AKey, AValue: string; AEncoding: Boolean);
begin
  FKey := AKey;
  FValue := AValue;
  FEncoding := AEncoding;
end;

class procedure TGBClientCoreRequestParam.AddOrSet(AList: TObjectList<TGBClientCoreRequestParam>; AKey: string; AValue: Integer; AEncoding: Boolean);
begin
  AddOrSet(AList, AKey, AValue.ToString);
end;

class procedure TGBClientCoreRequestParam.AddOrSet(AList: TObjectList<TGBClientCoreRequestParam>; AKey: string; AValue: Extended; AEncoding: Boolean);
begin
  AddOrSet(AList, AKey, AValue.ToString);
end;

class procedure TGBClientCoreRequestParam.AddOrSet(AList: TObjectList<TGBClientCoreRequestParam>; AKey: string; AValue: TDateTime; AEncoding: Boolean);
begin
  AddOrSet(AList, AKey, AValue.DateTimeToIso8601);
end;

class function TGBClientCoreRequestParam.GetParam(AList: TObjectList<TGBClientCoreRequestParam>; AKey: string): TGBClientCoreRequestParam;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Pred(AList.Count) do
  begin
    if AKey.ToLower.Equals(AList[I].Key.ToLower) then
      Exit(AList[I]);
  end;
end;

end.
