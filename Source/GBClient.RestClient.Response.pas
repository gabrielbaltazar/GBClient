unit GBClient.RestClient.Response;

interface

{$IFDEF WEAKPACKAGEUNIT}
  {$WEAKPACKAGEUNIT ON}
{$ENDIF}

uses
  Data.DB,
  GBClient.Interfaces,
  GBClient.Core.Helpers,
  REST.Client,
  REST.Types,
  System.JSON,
  System.Generics.Collections,
  System.SysUtils,
  System.Classes;

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
    function DataSet(AValue: TDataSet): IGBClientResponse;
    function GetObject(AValue: TObject): IGBClientResponse;
    function GetList(AValue: TList<TObject>; AType: TClass): IGBClientResponse;
    function GetBytes: TBytes;
    function GetStream: TBytesStream;
    function HeaderAsString(AName: string): string;
    function HeaderAsInteger(AName: string): Integer;
    function HeaderAsFloat(AName: string): Double;
    function HeaderAsDateTime(AName: string): TDateTime;
    function &End: IGBClientRequest;
  public
    constructor Create(AParent: IGBClientRequest);
    class function New(AParent: IGBClientRequest): IGBClientResponse;
    destructor Destroy; override;
  end;

implementation

{ TGBClientRestClientResponse }

constructor TGBClientRestClientResponse.create(AParent: IGBClientRequest);
begin
  FParent := AParent;
  FResponse := TRESTResponse(TRESTRequest(FParent.Component).Response);
end;

function TGBClientRestClientResponse.DataSet(AValue: TDataSet): IGBClientResponse;
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

function TGBClientRestClientResponse.GetList(AValue: TList<TObject>; AType: TClass): IGBClientResponse;
var
  LParse: TGBOnParseJSONToObject;
  LJsonArray: TJSONArray;
  LObject: TObject;
  I: Integer;
begin
  Result := Self;
  LJsonArray := GetJSONArray;
  for I := 0 to Pred(LJsonArray.Count) do
  begin
    LParse := FParent.Settings.OnParseJSONToObject;
    if Assigned(LParse) then
    begin
      LObject := AType.Create;
      try
        LParse(TJSONObject( LJsonArray.Items[I] ), LObject);
        AValue.Add(LObject);
      except
        LObject.Free;
        raise;
      end;
    end;
  end;
end;

function TGBClientRestClientResponse.GetObject(AValue: TObject): IGBClientResponse;
var
  LParse: TGBOnParseJSONToObject;
begin
  Result := Self;
  LParse := FParent.Settings.OnParseJSONToObject;
  if Assigned( FParent.Settings.OnParseJSONToObject ) then
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

function TGBClientRestClientResponse.HeaderAsDateTime(AName: string): TDateTime;
begin
  Result.fromIso8601ToDateTime( HeaderAsString(AName));
end;

function TGBClientRestClientResponse.HeaderAsFloat(AName: string): Double;
begin
  Result := HeaderAsString(AName).ToDouble;
end;

function TGBClientRestClientResponse.HeaderAsInteger(AName: string): Integer;
begin
  Result := HeaderAsString(AName).ToInteger;
end;

function TGBClientRestClientResponse.HeaderAsString(AName: string): string;
begin
  Result := FResponse.Headers.Values[AName];
end;

class function TGBClientRestClientResponse.New(AParent: IGBClientRequest): IGBClientResponse;
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
