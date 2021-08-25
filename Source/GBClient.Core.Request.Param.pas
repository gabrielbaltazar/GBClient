unit GBClient.Core.Request.Param;

interface

uses
  GBClient.Core.Helpers,
  System.SysUtils,
  System.Generics.Collections;

type TGBClientCoreRequestParam = class
  private
    FKey: String;
    FValue: String;
    FEncoding: Boolean;

  public
    property Key: String read FKey;
    property Value: String read FValue;
    property Encoding: Boolean read FEncoding;

    constructor create(AKey: string; AValue: string; bEncoding: Boolean = False);

    class function GetParam(AList: TObjectList<TGBClientCoreRequestParam>; AKey: String): TGBClientCoreRequestParam;
    class procedure AddOrSet(AList: TObjectList<TGBClientCoreRequestParam>; AKey: String; AValue: string; bEncoding: Boolean = False); overload;
    class procedure AddOrSet(AList: TObjectList<TGBClientCoreRequestParam>; AKey: String; AValue: Integer; bEncoding: Boolean = False); overload;
    class procedure AddOrSet(AList: TObjectList<TGBClientCoreRequestParam>; AKey: String; AValue: Extended; bEncoding: Boolean = False); overload;
    class procedure AddOrSet(AList: TObjectList<TGBClientCoreRequestParam>; AKey: String; AValue: TDateTime; bEncoding: Boolean = False); overload;
end;

implementation

{ TGBClientCoreRequestParam }

class procedure TGBClientCoreRequestParam.AddOrSet(AList: TObjectList<TGBClientCoreRequestParam>; AKey, AValue: string; bEncoding: Boolean);
var
  param: TGBClientCoreRequestParam;
begin
  param := GetParam(AList, AKey);
  if not Assigned(param) then
  begin
    param := TGBClientCoreRequestParam.create(AKey, AValue, bEncoding);
    AList.Add(param);
    Exit;
  end;

  param.FKey := AKey;
  param.FValue := AValue;
  param.FEncoding := bEncoding;
end;

constructor TGBClientCoreRequestParam.create(AKey, AValue: string; bEncoding: Boolean);
begin
  FKey := AKey;
  FValue := AValue;
  FEncoding := bEncoding;
end;

class procedure TGBClientCoreRequestParam.AddOrSet(AList: TObjectList<TGBClientCoreRequestParam>; AKey: String; AValue: Integer; bEncoding: Boolean);
begin
  AddOrSet(AList, AKey, AValue.ToString);
end;

class procedure TGBClientCoreRequestParam.AddOrSet(AList: TObjectList<TGBClientCoreRequestParam>; AKey: String; AValue: Extended; bEncoding: Boolean);
begin
  AddOrSet(AList, AKey, AValue.ToString);
end;

class procedure TGBClientCoreRequestParam.AddOrSet(AList: TObjectList<TGBClientCoreRequestParam>; AKey: String; AValue: TDateTime; bEncoding: Boolean);
begin
  AddOrSet(AList, AKey, AValue.DateTimeToIso8601);
end;

class function TGBClientCoreRequestParam.GetParam(AList: TObjectList<TGBClientCoreRequestParam>; AKey: String): TGBClientCoreRequestParam;
var
  i: Integer;
begin
  result := nil;
  for i := 0 to Pred(AList.Count) do
  begin
    if AKey.ToLower.Equals(AList[i].Key.ToLower) then
      Exit(AList[i]);
  end;
end;

end.
