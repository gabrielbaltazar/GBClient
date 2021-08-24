unit GBClient.Settings.Default;

interface

uses
  Data.DB,
  GBClient.Interfaces,
  GBClient.Helpers,
  REST.Json,
  System.JSON;

type TGBClientSettingsDefault = class(TInterfacedObject, IGBClientSettings)

  private
    [Weak]
    FParent : IGBClientRequest;

    FOnParseJSONToObject: TGBOnParseJSONToObject;
    FOnParseObjectToJSON: TGBOnParseObjectToJSON;

    FOnParseJSONToDataSet: TGBOnParseJSONToDataSet;
    FOnParseDataSetToJSONObject: TGBOnParseDataSetToJSONObject;
    FOnParseDataSetToJSONArray : TGBOnParseDataSetToJSONArray;

  protected
    function OnParseJSONToObject(Value: TGBOnParseJSONToObject): IGBClientSettings; overload;
    function OnParseJSONToObject: TGBOnParseJSONToObject; overload;

    function OnParseObjectToJSON(Value: TGBOnParseObjectToJSON): IGBClientSettings; overload;
    function OnParseObjectToJSON: TGBOnParseObjectToJSON; overload;

    function OnParseJSONToDataSet(Value: TGBOnParseJSONToDataSet): IGBClientSettings; overload;
    function OnParseJSONToDataSet: TGBOnParseJSONToDataSet; overload;

    function OnParseDataSetToJSONObject(Value: TGBOnParseDataSetToJSONObject): IGBClientSettings; overload;
    function OnParseDataSetToJSONObject: TGBOnParseDataSetToJSONObject; overload;

    function OnParseDataSetToJSONArray(Value: TGBOnParseDataSetToJSONArray): IGBClientSettings; overload;
    function OnParseDataSetToJSONArray: TGBOnParseDataSetToJSONArray; overload;

    function &End: IGBClientRequest;

  public
    constructor create(Parent: IGBClientRequest);
    class function New(Parent: IGBClientRequest): IGBClientSettings;
    destructor  Destroy; override;
end;

procedure ParseJSONToObject(JSON: TJSONObject; AObject: TObject);
function ParseObjectToJSON(AObject: TObject): TJSONObject;

procedure ParseJSONToDataSet(AJSON: TJSONObject; ADataSet: TDataSet);
function  ParseDataSetToJSONObject(ADataSet: TDataSet): TJSONObject;
function  ParseDataSetToJSONArray (ADataSet: TDataSet): TJSONArray;

implementation

procedure ParseJSONToObject(JSON: TJSONObject; AObject: TObject);
begin
  if (Assigned(JSON)) and (Assigned(AObject)) then
    TJson.JsonToObject(AObject, JSON);
end;

function ParseObjectToJSON(AObject: TObject): TJSONObject;
begin
  result := TJson.ObjectToJsonObject(AObject);
end;

procedure ParseJSONToDataSet(AJSON: TJSONObject; ADataSet: TDataSet);
begin
  ADataSet.FromJSON(AJSON);
end;

function ParseDataSetToJSONObject(ADataSet: TDataSet): TJSONObject;
begin
  result := ADataSet.ToJSONObject;
end;

function ParseDataSetToJSONArray (ADataSet: TDataSet): TJSONArray;
begin
  result := ADataSet.ToJSONArray;
end;

{ TGBClientSettingsDefault }

constructor TGBClientSettingsDefault.create(Parent: IGBClientRequest);
begin
  FParent := Parent;

  FOnParseJSONToObject := ParseJSONToObject;
  FOnParseObjectToJSON := ParseObjectToJSON;
  FOnParseJSONToDataSet := ParseJSONToDataSet;
  FOnParseDataSetToJSONObject := ParseDataSetToJSONObject;
  FOnParseDataSetToJSONArray := ParseDataSetToJSONArray;
end;

destructor TGBClientSettingsDefault.Destroy;
begin

  inherited;
end;

function TGBClientSettingsDefault.&End: IGBClientRequest;
begin
  result := FParent;
end;

class function TGBClientSettingsDefault.New(Parent: IGBClientRequest): IGBClientSettings;
begin
  result := Self.create(Parent);
end;

function TGBClientSettingsDefault.OnParseDataSetToJSONArray: TGBOnParseDataSetToJSONArray;
begin
  result := FOnParseDataSetToJSONArray;
end;

function TGBClientSettingsDefault.OnParseDataSetToJSONArray(Value: TGBOnParseDataSetToJSONArray): IGBClientSettings;
begin
  result := Self;
  if Assigned(Value) then
    FOnParseDataSetToJSONArray := Value;
end;

function TGBClientSettingsDefault.OnParseDataSetToJSONObject(Value: TGBOnParseDataSetToJSONObject): IGBClientSettings;
begin
  result := Self;
  if Assigned(Value) then
    FOnParseDataSetToJSONObject := Value;
end;

function TGBClientSettingsDefault.OnParseDataSetToJSONObject: TGBOnParseDataSetToJSONObject;
begin
  result := FOnParseDataSetToJSONObject;
end;

function TGBClientSettingsDefault.OnParseJSONToDataSet(Value: TGBOnParseJSONToDataSet): IGBClientSettings;
begin
  result := Self;
  if Assigned(Value) then
    FOnParseJSONToDataSet := Value;
end;

function TGBClientSettingsDefault.OnParseJSONToDataSet: TGBOnParseJSONToDataSet;
begin
  result := FOnParseJSONToDataSet;
end;

function TGBClientSettingsDefault.OnParseJSONToObject: TGBOnParseJSONToObject;
begin
  result := FOnParseJSONToObject;
end;

function TGBClientSettingsDefault.OnParseObjectToJSON: TGBOnParseObjectToJSON;
begin
  result := FOnParseObjectToJSON;
end;

function TGBClientSettingsDefault.OnParseObjectToJSON(Value: TGBOnParseObjectToJSON): IGBClientSettings;
begin
  result := Self;
  if Assigned(Value) then
    FOnParseObjectToJSON := Value;
end;

function TGBClientSettingsDefault.OnParseJSONToObject(Value: TGBOnParseJSONToObject): IGBClientSettings;
begin
  result := Self;
  if Assigned(Value) then
    FOnParseJSONToObject := Value;
end;

end.
