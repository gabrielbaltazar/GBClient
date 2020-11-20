unit GBClient.IdHTTP.Response;

interface

uses
  Data.DB,
  GBClient.Interfaces,
  GBClient.Types,
  GBClient.Helpers,
  REST.Json,
  IdHTTP,
  System.SysUtils,
  System.Generics.Collections,
  System.Classes,
  System.JSON;

type TGBClientIdHTTPResponse = class(TInterfacedObject, IGBClientResponse)

  private
    [Weak]
    FParent : IGBClientRequest;

    FIdHTTP     : TIdHTTP;
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
    class function New(Parent: IGBClientRequest; IdHTTP: TIdHTTP): IGBClientResponse;
    constructor create(Parent: IGBClientRequest; IdHTTP: TIdHTTP);
    destructor  Destroy; override;
end;

implementation

uses
  REST.Response.Adapter;

{ TGBClientIdHTTPResponse }

function TGBClientIdHTTPResponse.&End: IGBClientRequest;
begin
  result := FParent;
end;

constructor TGBClientIdHTTPResponse.create(Parent: IGBClientRequest; IdHTTP: TIdHTTP);
begin
  FParent := Parent;
  FIdHTTP := IdHTTP;
end;

function TGBClientIdHTTPResponse.DataSet(Value: TDataSet): IGBClientResponse;
begin
  result := Self;
  Value.FromJSON(GetText);
end;

destructor TGBClientIdHTTPResponse.Destroy;
begin
  FJSONArray.Free;
  FJSONObject.Free;
  inherited;
end;

function TGBClientIdHTTPResponse.GetJSONArray: TJSONArray;
begin
  FreeAndNil(FJSONArray);
  FJSONArray := TJSONObject.ParseJSONValue(GetText) as TJSONArray;
  result := FJSONArray;
end;

function TGBClientIdHTTPResponse.GetJSONObject: TJSONObject;
begin
  FreeAndNil(FJSONArray);
  FJSONObject := TJSONObject.ParseJSONValue(GetText) as TJSONObject;
  result := FJSONObject;
end;

function TGBClientIdHTTPResponse.GetList(Value: TList<TObject>; AType: TClass): IGBClientResponse;
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

function TGBClientIdHTTPResponse.GetObject(Value: TObject): IGBClientResponse;
begin
  result := Self;
  TJson.JsonToObject(Value, GetJSONObject);
end;

function TGBClientIdHTTPResponse.GetText: string;
var
  stringStream: TStringStream;
begin
  stringStream := TStringStream.Create;
  try
    stringStream.LoadFromStream(FIdHTTP.Response.ContentStream);
    result := stringStream.DataString;
  finally
    stringStream.Free;
  end;
end;

function TGBClientIdHTTPResponse.HeaderAsDateTime(Name: String): TDateTime;
begin
  result.fromIso8601ToDateTime(HeaderAsString(Name));
end;

function TGBClientIdHTTPResponse.HeaderAsFloat(Name: String): Double;
begin
  result := StrToFloatDef(HeaderAsString(Name), 0);
end;

function TGBClientIdHTTPResponse.HeaderAsInteger(Name: String): Integer;
begin
  result := StrToIntDef(HeaderAsString(Name), 0);
end;

function TGBClientIdHTTPResponse.HeaderAsString(Name: String): string;
begin
  result := FIdHTTP.Response.CustomHeaders.Values[Name];
end;

class function TGBClientIdHTTPResponse.New(Parent: IGBClientRequest; IdHTTP: TIdHTTP): IGBClientResponse;
begin
  result := Self.create(Parent, IdHTTP);
end;

function TGBClientIdHTTPResponse.StatusCode: Integer;
begin
  result := FIdHTTP.ResponseCode;
end;

function TGBClientIdHTTPResponse.StatusText: string;
begin
  result := FIdHTTP.ResponseText;
end;

end.
