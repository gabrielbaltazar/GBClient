unit GBClient.Request.Base.Param;

interface

uses
  GBClient.Helpers,
  System.SysUtils,
  System.Generics.Collections;

type TGBClientRequestBaseParam = class
  private
    FKey: String;
    FValue: String;
    FEncoding: Boolean;

  public
    property Key: String read FKey;
    property Value: String read FValue;
    property Encoding: Boolean read FEncoding;

    constructor create(AKey: string; AValue: string; bEncoding: Boolean = False);

    class function GetParam(AList: TObjectList<TGBClientRequestBaseParam>; AKey: String): TGBClientRequestBaseParam;
    class procedure AddOrSet(AList: TObjectList<TGBClientRequestBaseParam>; AKey: String; AValue: string; bEncoding: Boolean = False); overload;
    class procedure AddOrSet(AList: TObjectList<TGBClientRequestBaseParam>; AKey: String; AValue: Integer; bEncoding: Boolean = False); overload;
    class procedure AddOrSet(AList: TObjectList<TGBClientRequestBaseParam>; AKey: String; AValue: Extended; bEncoding: Boolean = False); overload;
    class procedure AddOrSet(AList: TObjectList<TGBClientRequestBaseParam>; AKey: String; AValue: TDateTime; bEncoding: Boolean = False); overload;
end;

implementation

{ TGBClientRequestBaseParam }

class procedure TGBClientRequestBaseParam.AddOrSet(AList: TObjectList<TGBClientRequestBaseParam>; AKey, AValue: string; bEncoding: Boolean);
var
  param: TGBClientRequestBaseParam;
begin
  param := GetParam(AList, AKey);
  if not Assigned(param) then
  begin
    param := TGBClientRequestBaseParam.create(AKey, AValue, bEncoding);
    AList.Add(param);
    Exit;
  end;

  param.FKey := AKey;
  param.FValue := AValue;
  param.FEncoding := bEncoding;
end;

constructor TGBClientRequestBaseParam.create(AKey, AValue: string; bEncoding: Boolean);
begin
  FKey := AKey;
  FValue := AValue;
  FEncoding := bEncoding;
end;

class procedure TGBClientRequestBaseParam.AddOrSet(AList: TObjectList<TGBClientRequestBaseParam>; AKey: String; AValue: Integer; bEncoding: Boolean);
begin
  AddOrSet(AList, AKey, AValue.ToString);
end;

class procedure TGBClientRequestBaseParam.AddOrSet(AList: TObjectList<TGBClientRequestBaseParam>; AKey: String; AValue: Extended; bEncoding: Boolean);
begin
  AddOrSet(AList, AKey, AValue.ToString);
end;

class procedure TGBClientRequestBaseParam.AddOrSet(AList: TObjectList<TGBClientRequestBaseParam>; AKey: String; AValue: TDateTime; bEncoding: Boolean);
begin
  AddOrSet(AList, AKey, AValue.DateTimeToIso8601);
end;

class function TGBClientRequestBaseParam.GetParam(AList: TObjectList<TGBClientRequestBaseParam>; AKey: String): TGBClientRequestBaseParam;
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
