unit GBClient.RestClient.Response;

interface

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

type TGBClientRestClientResponse = class(TInterfacedObject, IGBClientResponse)

  private
    [Weak]
    FParent: IGBClientRequest;
    FResponse: TRESTResponse;
    FByteStream: TBytesStream;

  protected
    function StatusCode: Integer;
    function StatusText: string;

    function GetText       : string;
    function GetJSONObject : TJSONObject;
    function GetJSONArray  : TJSONArray;
    function DataSet(Value: TDataSet): IGBClientResponse;
    function GetObject(Value: TObject): IGBClientResponse;
    function GetList(Value: TList<TObject>; AType: TClass): IGBClientResponse;
    function GetBytes: TBytes;
    function GetStream: TBytesStream;

    function HeaderAsString   (Name: String): string;
    function HeaderAsInteger  (Name: String): Integer;
    function HeaderAsFloat    (Name: String): Double;
    function HeaderAsDateTime (Name: String): TDateTime;

    function &End: IGBClientRequest;

  public
    constructor create(Parent: IGBClientRequest);
    class function New(Parent: IGBClientRequest): IGBClientResponse;
    destructor Destroy; override;
end;

implementation

{ TGBClientRestClientResponse }

constructor TGBClientRestClientResponse.create(Parent: IGBClientRequest);
begin
  FParent := Parent;
  FResponse := TRESTResponse(TRESTRequest(FParent.Component).Response);
end;

function TGBClientRestClientResponse.DataSet(Value: TDataSet): IGBClientResponse;
var
  parse: TGBOnParseJSONToDataSet;
begin
  result := Self;
  parse := FParent.Settings.OnParseJSONToDataSet;
  parse(GetJSONObject, Value);
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
  result := FResponse.RawBytes;
end;

function TGBClientRestClientResponse.GetJSONArray: TJSONArray;
begin
  result := TJSONArray(FResponse.JSONValue);
end;

function TGBClientRestClientResponse.GetJSONObject: TJSONObject;
begin
  result := TJSONObject(FResponse.JSONValue);
end;

function TGBClientRestClientResponse.GetList(Value: TList<TObject>; AType: TClass): IGBClientResponse;
var
  parse : TGBOnParseJSONToObject;
  jsonArray : TJSONArray;
  LObject : TObject;
  i : Integer;
begin
  result := Self;
  jsonArray := GetJSONArray;

  for i := 0 to Pred(jsonArray.Count) do
  begin
    parse := FParent.Settings.OnParseJSONToObject;
    if Assigned(parse) then
    begin
      LObject := AType.Create;
      try
        parse(TJSONObject( jsonArray.Items[i] ), LObject);
        Value.Add(LObject);
      except
        LObject.Free;
        raise;
      end;
    end;
  end;
end;

function TGBClientRestClientResponse.GetObject(Value: TObject): IGBClientResponse;
var
  parse: TGBOnParseJSONToObject;
begin
  Result := Self;
  parse := FParent.Settings.OnParseJSONToObject;
  if Assigned( FParent.Settings.OnParseJSONToObject ) then
    parse(GetJSONObject, Value);
end;

function TGBClientRestClientResponse.GetStream: TBytesStream;
begin
  FreeAndNil(FByteStream);
  FByteStream := TBytesStream.Create(GetBytes);
  result := FByteStream;
end;

function TGBClientRestClientResponse.GetText: string;
begin
  result := FResponse.Content;
end;

function TGBClientRestClientResponse.HeaderAsDateTime(Name: String): TDateTime;
begin
  result.fromIso8601ToDateTime( HeaderAsString(Name));
end;

function TGBClientRestClientResponse.HeaderAsFloat(Name: String): Double;
begin
  result := HeaderAsString(Name).ToDouble;
end;

function TGBClientRestClientResponse.HeaderAsInteger(Name: String): Integer;
begin
  result := HeaderAsString(Name).ToInteger;
end;

function TGBClientRestClientResponse.HeaderAsString(Name: String): string;
begin
  result := FResponse.Headers.Values[Name];
end;

class function TGBClientRestClientResponse.New(Parent: IGBClientRequest): IGBClientResponse;
begin
  result := Self.create(Parent);
end;

function TGBClientRestClientResponse.StatusCode: Integer;
begin
  result := FResponse.StatusCode;
end;

function TGBClientRestClientResponse.StatusText: string;
begin
  result := FResponse.StatusText;
end;

end.
