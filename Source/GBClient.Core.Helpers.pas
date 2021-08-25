unit GBClient.Core.Helpers;

interface

uses
  Data.DB,
  REST.Response.Adapter,
  System.SysUtils,
  System.DateUtils,
  System.JSON;

type
  TGBClientDatetimeHelper = record helper for TDateTime
  private
    function Iso8601ToDateTime(AValue: String): TDateTime;

  public
    function DateTimeToIso8601: string;

    procedure fromIso8601ToDateTime(AValue: String);
  end;

  TGBClientDataSetHelper = class helper for TDataSet
  public
    function ToJSONObject: TJSONObject;
    function ToJSONArray : TJSONArray;

    procedure FromJSON(AJSONValue: TJSONValue); overload;
    procedure FromJSON(AJSONString: String); overload;
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

procedure TGBClientDatetimeHelper.fromIso8601ToDateTime(AValue: String);
begin
  Self := Iso8601ToDateTime(AValue);
end;

function TGBClientDatetimeHelper.Iso8601ToDateTime(AValue: String): TDateTime;
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
  adapter: TCustomJSONDataSetAdapter;
begin
  adapter := TCustomJSONDataSetAdapter.Create(nil);
  try
    adapter.Dataset := Self;
    adapter.UpdateDataSet(AJSONValue);
  finally
    adapter.Free;
  end;
end;

procedure TGBClientDataSetHelper.FromJSON(AJSONString: String);
var
  json : TJSONValue;
begin
  json := TJSONObject.ParseJSONValue(AJSONString);
  try
    FromJSON(json);
  finally
    json.Free;
  end;
end;

function TGBClientDataSetHelper.ToJSONArray: TJSONArray;
var
  bookmark: TBookmark;
begin
  bookmark := GetBookmark;
  try
    result := TJSONArray.Create;
    try
      First;
      while not Eof do
      begin
        Result.Add(ToJSONObject);
        Next;
      end;
    except
      result.Free;
      raise;
    end;
  finally
    GotoBookmark(bookmark);
  end;
end;

function TGBClientDataSetHelper.ToJSONObject: TJSONObject;
var
  field: TField;
  key: string;
begin
  result := TJSONObject.Create;
  try
    for field in Fields do
    begin
      if field.IsNull then
        Continue;

      key := field.FieldName;
      case field.DataType of
        ftString: Result.AddPair(key, field.AsString);

        ftAutoInc, ftInteger, ftSmallint, ftShortint, ftCurrency,
        ftSingle, ftFloat, ftLargeint, ftBCD, ftFMTBcd, ftWord,
        ftExtended, ftLongWord: Result.AddPair(key, TJSONNumber.Create(field.AsInteger));

        ftMemo, ftWideMemo, ftWideString: Result.AddPair(key, field.AsWideString);

        ftBoolean: Result.AddPair(key, TJSONBool.Create( field.AsBoolean));

        ftDate, ftTime, ftTimeStamp, ftDateTime: result.AddPair(key, field.AsDateTime.DateTimeToIso8601);
      end;
    end;
  except
    Result.Free;
    raise;
  end;
end;

end.
