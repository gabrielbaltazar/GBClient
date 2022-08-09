unit GBClient.Core.Helpers;

interface

{$IFDEF WEAKPACKAGEUNIT}
  {$WEAKPACKAGEUNIT ON}
{$ENDIF}

uses
  Data.DB,
  REST.Response.Adapter,
  System.SysUtils,
  System.DateUtils,
  System.JSON;

type
  TGBClientDatetimeHelper = record helper for TDateTime
  private
    function Iso8601ToDateTime(AValue: string): TDateTime;
  public
    function DateTimeToIso8601: string;
    procedure FromIso8601ToDateTime(AValue: string);
  end;

  TGBClientDataSetHelper = class helper for TDataSet
  public
    function ToJSONObject: TJSONObject;
    function ToJSONArray: TJSONArray;
    procedure FromJSON(AJSONValue: TJSONValue); overload;
    procedure FromJSON(AJSONString: string); overload;
  end;

implementation

uses
  REST.Json;

{ TGBClientDatetimeHelper }

function TGBClientDatetimeHelper.DateTimeToIso8601: string;
begin
  if Self = 0 then
    Result := ''
  else
  if Frac(Self) = 0 then
    Result := FormatDateTime('yyyy"-"mm"-"dd', Self)
  else
  if Trunc(Self) = 0 then
    Result := FormatDateTime('"T"hh":"nn":"ss', Self)
  else
    Result := FormatDateTime('yyyy"-"mm"-"dd"T"hh":"nn":"ss', Self);
end;

procedure TGBClientDatetimeHelper.fromIso8601ToDateTime(AValue: string);
begin
  Self := Iso8601ToDateTime(AValue);
end;

function TGBClientDatetimeHelper.Iso8601ToDateTime(AValue: string): TDateTime;
var
  Y, M, D, HH, MI, SS: Cardinal;
begin
  // YYYY-MM-DD   Thh:mm:ss  or  YYYY-MM-DDThh:mm:ss
  // 1234567890   123456789      1234567890123456789
  Result := 0;
  case Length(AValue) of
    9:
      if (AValue[1] = 'T') and (AValue[4] = ':') and (AValue[7] = ':') then
      begin
        HH := Ord(AValue[2]) * 10 + Ord(AValue[3]) - (48 + 480);
        MI := Ord(AValue[5]) * 10 + Ord(AValue[6]) - (48 + 480);
        SS := Ord(AValue[8]) * 10 + Ord(AValue[9]) - (48 + 480);
        if (HH < 24) and (MI < 60) and (SS < 60) then
          Result := EncodeTime(HH, MI, SS, 0);
      end;
    10:
      if (AValue[5] = AValue[8]) and (Ord(AValue[8]) in [Ord('-'), Ord('/')]) then
      begin
        Y := Ord(AValue[1]) * 1000 + Ord(AValue[2]) * 100 + Ord(AValue[3]) * 10 + Ord(AValue[4]) - (48 + 480 + 4800 + 48000);
        M := Ord(AValue[6]) * 10 + Ord(AValue[7]) - (48 + 480);
        D := Ord(AValue[9]) * 10 + Ord(AValue[10]) - (48 + 480);
        if (Y <= 9999) and ((M - 1) < 12) and ((D - 1) < 31) then
          Result := EncodeDate(Y, M, D);
      end;
    19,20,21,22,23,24,25:
      if (AValue[5] = AValue[8]) and
         (Ord(AValue[8]) in [Ord('-'), Ord('/')]) and
         (Ord(AValue[11]) in [Ord(' '), Ord('T')]) and
         (AValue[14] = ':') and
         (AValue[17] = ':') then
      begin
        Y := Ord(AValue[1]) * 1000 + Ord(AValue[2]) * 100 + Ord(AValue[3]) * 10 + Ord(AValue[4]) - (48 + 480 + 4800 + 48000);
        M := Ord(AValue[6]) * 10 + Ord(AValue[7]) - (48 + 480);
        D := Ord(AValue[9]) * 10 + Ord(AValue[10]) - (48 + 480);
        HH := Ord(AValue[12]) * 10 + Ord(AValue[13]) - (48 + 480);
        MI := Ord(AValue[15]) * 10 + Ord(AValue[16]) - (48 + 480);
        SS := Ord(AValue[18]) * 10 + Ord(AValue[19]) - (48 + 480);
        if (Y <= 9999) and ((M - 1) < 12) and ((D - 1) < 31) and (HH < 24) and (MI < 60) and (SS < 60) then
          Result := EncodeDate(Y, M, D) + EncodeTime(HH, MI, SS, 0);
      end;
  end;
end;

{ TGBClientDataSetHelper }

procedure TGBClientDataSetHelper.FromJSON(AJSONValue: TJSONValue);
var
  LAdapter: TCustomJSONDataSetAdapter;
begin
  LAdapter := TCustomJSONDataSetAdapter.Create(nil);
  try
    LAdapter.Dataset := Self;
    LAdapter.UpdateDataSet(AJSONValue);
  finally
    LAdapter.Free;
  end;
end;

procedure TGBClientDataSetHelper.FromJSON(AJSONString: string);
var
  LJSON: TJSONValue;
begin
  LJSON := TJSONObject.ParseJSONValue(AJSONString);
  try
    FromJSON(LJSON);
  finally
    LJSON.Free;
  end;
end;

function TGBClientDataSetHelper.ToJSONArray: TJSONArray;
var
  LBookmark: TBookmark;
begin
  LBookmark := GetBookmark;
  try
    Result := TJSONArray.Create;
    try
      First;
      while not Eof do
      begin
        Result.Add(ToJSONObject);
        Next;
      end;
    except
      Result.Free;
      raise;
    end;
  finally
    GotoBookmark(LBookmark);
  end;
end;

function TGBClientDataSetHelper.ToJSONObject: TJSONObject;
var
  LField: TField;
  LKey: string;
begin
  Result := TJSONObject.Create;
  try
    for LField in Fields do
    begin
      if LField.IsNull then
        Continue;

      LKey := LField.FieldName;
      case LField.DataType of
        ftString: Result.AddPair(LKey, LField.AsString);

        ftAutoInc, ftInteger, ftSmallint, ftShortint, ftCurrency,
        ftSingle, ftFloat, ftLargeint, ftBCD, ftFMTBcd, ftWord,
        ftExtended, ftLongWord: Result.AddPair(LKey, TJSONNumber.Create(LField.AsInteger));

        ftMemo, ftWideMemo, ftWideString: Result.AddPair(LKey, LField.AsWideString);

        ftBoolean: Result.AddPair(LKey, TJSONBool.Create( LField.AsBoolean));

        ftDate, ftTime, ftTimeStamp, ftDateTime: Result.AddPair(LKey, LField.AsDateTime.DateTimeToIso8601);
      end;
    end;
  except
    Result.Free;
    raise;
  end;
end;

end.
