unit GBClient.RestClient.Response;

interface

uses
  Data.DB,
  GBClient.Interfaces,
  GBClient.Helpers,
  REST.Json,
  REST.Client,
  REST.Types,
  System.JSON,
  System.Generics.Collections,
  System.SysUtils;

type TGBClientRestClientResponse = class(TInterfacedObject, IGBClientResponse)

  private
    [Weak]
    FParent   : IGBClientRequest;
    FResponse : TRESTResponse;

  protected
    function StatusCode: Integer;
    function StatusText: string;

    function GetText       : string;
    function GetJSONObject : TJSONObject;
    function GetJSONArray  : TJSONArray;
    function GetObject(Value: TObject): IGBClientResponse;
    function GetList(Value: TList<TObject>; AType: TClass): IGBClientResponse;
    function DataSet(Value: TDataSet): IGBClientResponse;

    function HeaderAsString   (Name: String): string;
    function HeaderAsInteger  (Name: String): Integer;
    function HeaderAsFloat    (Name: String): Double;
    function HeaderAsDateTime (Name: String): TDateTime;

    function &End: IGBClientRequest;

  public
    class function New(Parent: IGBClientRequest; Response: TRESTResponse): IGBClientResponse;
    constructor create(Parent: IGBClientRequest; Response: TRESTResponse);
    destructor Destroy; override;
end;

implementation

{ TGBClientRestClientResponse }

constructor TGBClientRestClientResponse.create(Parent: IGBClientRequest; Response: TRESTResponse);
begin
  FParent   := Parent;
  FResponse := Response;
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

  inherited;
end;

function TGBClientRestClientResponse.&End: IGBClientRequest;
begin
  result := FParent;
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
  parse     : TGBOnParseJSONToObject;
  jsonArray : TJSONArray;
  LObject   : TObject;
  i         : Integer;
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
  parse  := FParent.Settings.OnParseJSONToObject;
  if Assigned( FParent.Settings.OnParseJSONToObject ) then
    parse(GetJSONObject, Value);
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

class function TGBClientRestClientResponse.New(Parent: IGBClientRequest; Response: TRESTResponse): IGBClientResponse;
begin
  result := Self.create(Parent, Response);
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
