unit GBClient.RestClient.Response;

interface

{$IFDEF WEAKPACKAGEUNIT}
  {$WEAKPACKAGEUNIT ON}
{$ENDIF}

uses
  System.JSON,
  System.Generics.Collections,
  System.SysUtils,
  System.Classes,
  Data.DB,
  REST.Client,
  REST.Types,
  GBClient.Interfaces,
  GBClient.Core.Helpers;

type
  TGBClientRestClientResponse = class(TInterfacedObject, IGBClientResponse)
  private
    [Weak]
    FParent: IGBClientRequest;
    FResponse: TRESTResponse;
    FByteStream: TBytesStream;
  protected
    function StatusCode: Integer;
    function StatusText: string;

    function GetText: string;
    function GetJSONObject: TJSONObject;
    function GetJSONArray: TJSONArray;
    function DataSet(const AValue: TDataSet): IGBClientResponse;
    function GetObject(const AValue: TObject): IGBClientResponse;
    function GetList(const AValue: TList<TObject>; const AType: TClass): IGBClientResponse;
    function GetBytes: TBytes;
    function GetStream: TBytesStream;

    function HeaderAsString(const AName: string): string;
    function HeaderAsInteger(const AName: string): Integer;
    function HeaderAsFloat(const AName: string): Double;
    function HeaderAsDateTime(const AName: string): TDateTime;

    function &End: IGBClientRequest;
  public
    constructor Create(const AParent: IGBClientRequest);
    class function New(const AParent: IGBClientRequest): IGBClientResponse;
    destructor Destroy; override;
  end;

implementation

{ TGBClientRestClientResponse }

constructor TGBClientRestClientResponse.Create(const AParent: IGBClientRequest);
begin
  FParent := AParent;
  FResponse := TRESTResponse(TRESTRequest(FParent.Component).Response);
end;

function TGBClientRestClientResponse.DataSet(const AValue: TDataSet): IGBClientResponse;
var
  LParse: TGBOnParseJSONToDataSet;
begin
  Result := Self;
  LParse := FParent.Settings.OnParseJSONToDataSet;
  LParse(GetJSONObject, AValue);
end;

destructor TGBClientRestClientResponse.Destroy;
begin
  FreeAndNil(FByteStream);
  inherited;
end;

function TGBClientRestClientResponse.&End: IGBClientRequest;
begin
  Result := FParent;
end;

function TGBClientRestClientResponse.GetBytes: TBytes;
begin
  Result := FResponse.RawBytes;
end;

function TGBClientRestClientResponse.GetJSONArray: TJSONArray;
begin
  Result := TJSONArray(FResponse.JSONValue);
end;

function TGBClientRestClientResponse.GetJSONObject: TJSONObject;
begin
  Result := TJSONObject(FResponse.JSONValue);
end;

function TGBClientRestClientResponse.GetList(const AValue: TList<TObject>; const AType: TClass): IGBClientResponse;
var
  LParse: TGBOnParseJSONToObject;
  LJsonArray: TJSONArray;
  LObject: TObject;
  LCount: Integer;
begin
  Result := Self;
  LJsonArray := GetJSONArray;
  for LCount := 0 to Pred(LJsonArray.Count) do
  begin
    LParse := FParent.Settings.OnParseJSONToObject;
    if Assigned(LParse) then
    begin
      LObject := AType.Create;
      try
        LParse(TJSONObject(LJsonArray.Items[LCount]), LObject);
        AValue.Add(LObject);
      except
        LObject.Free;
        raise;
      end;
    end;
  end;
end;

function TGBClientRestClientResponse.GetObject(const AValue: TObject): IGBClientResponse;
var
  LParse: TGBOnParseJSONToObject;
begin
  Result := Self;
  LParse := FParent.Settings.OnParseJSONToObject;
  if Assigned(FParent.Settings.OnParseJSONToObject) then
    LParse(GetJSONObject, AValue);
end;

function TGBClientRestClientResponse.GetStream: TBytesStream;
begin
  FreeAndNil(FByteStream);
  FByteStream := TBytesStream.Create(GetBytes);
  Result := FByteStream;
end;

function TGBClientRestClientResponse.GetText: string;
begin
  Result := FResponse.Content;
end;

function TGBClientRestClientResponse.HeaderAsDateTime(const AName: string): TDateTime;
begin
  Result.FromIso8601ToDateTime(HeaderAsString(AName));
end;

function TGBClientRestClientResponse.HeaderAsFloat(const AName: string): Double;
begin
  Result := HeaderAsString(AName).ToDouble;
end;

function TGBClientRestClientResponse.HeaderAsInteger(const AName: string): Integer;
begin
  Result := HeaderAsString(AName).ToInteger;
end;

function TGBClientRestClientResponse.HeaderAsString(const AName: string): string;
begin
  Result := FResponse.Headers.Values[AName];
end;

class function TGBClientRestClientResponse.New(const AParent: IGBClientRequest): IGBClientResponse;
begin
  Result := Self.create(AParent);
end;

function TGBClientRestClientResponse.StatusCode: Integer;
begin
  Result := FResponse.StatusCode;
end;

function TGBClientRestClientResponse.StatusText: string;
begin
  Result := FResponse.StatusText;
end;

end.
