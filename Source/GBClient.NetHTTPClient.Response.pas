unit GBClient.NetHTTPClient.Response;

interface

uses
  Data.DB,
  GBClient.Interfaces,
  GBClient.Types,
  GBClient.Helpers,
  REST.Json,
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.Generics.Collections,
  System.NetEncoding,
  System.Net.URLClient,
  System.Net.HttpClient,
  System.Net.HttpClientComponent;

type TGBClientNetHTTPClientResponse = class(TInterfacedObject, IGBClientResponse)

  private
    [Weak]
    FParent : IGBClientRequest;

    FResponse   : IHTTPResponse;
    FJSONArray  : TJSONArray;
    FJSONObject : TJSONObject;
  protected
    // Response
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
    class function New(Parent: IGBClientRequest; Response: IHTTPResponse): IGBClientResponse;
    constructor create(Parent: IGBClientRequest; Response: IHTTPResponse);
    destructor  Destroy; override;
end;

implementation

uses
  REST.Response.Adapter;

{ TGBClientNetHTTPClientResponse }

function TGBClientNetHTTPClientResponse.&End: IGBClientRequest;
begin
  result := FParent;
end;

constructor TGBClientNetHTTPClientResponse.create(Parent: IGBClientRequest; Response: IHTTPResponse);
begin
  FParent := Parent;
  FResponse := Response;
end;

function TGBClientNetHTTPClientResponse.DataSet(Value: TDataSet): IGBClientResponse;
begin
  result := Self;
  Value.FromJSON(GetText);
end;

destructor TGBClientNetHTTPClientResponse.Destroy;
begin
  FJSONArray.Free;
  FJSONObject.Free;
  inherited;
end;

function TGBClientNetHTTPClientResponse.GetJSONArray: TJSONArray;
begin
  FreeAndNil(FJSONArray);
  FJSONArray := TJSONObject.ParseJSONValue(GetText) as TJSONArray;
  result := FJSONArray;
end;

function TGBClientNetHTTPClientResponse.GetJSONObject: TJSONObject;
begin
  FreeAndNil(FJSONArray);
  FJSONObject := TJSONObject.ParseJSONValue(GetText) as TJSONObject;
  result := FJSONObject;
end;

function TGBClientNetHTTPClientResponse.GetList(Value: TList<TObject>; AType: TClass): IGBClientResponse;
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

function TGBClientNetHTTPClientResponse.GetObject(Value: TObject): IGBClientResponse;
begin
  result := Self;
  TJson.JsonToObject(Value, GetJSONObject);
end;

function TGBClientNetHTTPClientResponse.GetText: string;
begin
  result := FResponse.ContentAsString;
end;

function TGBClientNetHTTPClientResponse.HeaderAsDateTime(Name: String): TDateTime;
begin
  result.fromIso8601ToDateTime(HeaderAsString(Name));
end;

function TGBClientNetHTTPClientResponse.HeaderAsFloat(Name: String): Double;
begin
  result := StrToFloatDef(HeaderAsString(Name), 0);
end;

function TGBClientNetHTTPClientResponse.HeaderAsInteger(Name: String): Integer;
begin
  result := StrToIntDef(HeaderAsString(Name), 0);
end;

function TGBClientNetHTTPClientResponse.HeaderAsString(Name: String): string;
begin
  if FResponse.ContainsHeader(Name) then
    result := FResponse.HeaderValue[Name];
end;

class function TGBClientNetHTTPClientResponse.New(Parent: IGBClientRequest; Response: IHTTPResponse): IGBClientResponse;
begin
  result := Self.create(Parent, Response);
end;

function TGBClientNetHTTPClientResponse.StatusCode: Integer;
begin
  result := FResponse.StatusCode;
end;

function TGBClientNetHTTPClientResponse.StatusText: string;
begin
  result := FResponse.StatusText;
end;

end.
