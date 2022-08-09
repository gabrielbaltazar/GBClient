unit GBClient.Core.Settings;

interface

{$IFDEF WEAKPACKAGEUNIT}
	{$WEAKPACKAGEUNIT ON}
{$ENDIF}

uses
  Data.DB,
  GBClient.Interfaces,
  GBClient.Core.Helpers,
  REST.Json,
  System.JSON;

type TGBClientCoreSettings = class(TInterfacedObject, IGBClientSettings)

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

constructor TGBClientCoreSettings.create(Parent: IGBClientRequest);
begin
  FParent := Parent;

  FOnParseJSONToObject := ParseJSONToObject;
  FOnParseObjectToJSON := ParseObjectToJSON;
  FOnParseJSONToDataSet := ParseJSONToDataSet;
  FOnParseDataSetToJSONObject := ParseDataSetToJSONObject;
  FOnParseDataSetToJSONArray := ParseDataSetToJSONArray;
end;

destructor TGBClientCoreSettings.Destroy;
begin

  inherited;
end;

function TGBClientCoreSettings.&End: IGBClientRequest;
begin
  result := FParent;
end;

class function TGBClientCoreSettings.New(Parent: IGBClientRequest): IGBClientSettings;
begin
  result := Self.create(Parent);
end;

function TGBClientCoreSettings.OnParseDataSetToJSONArray: TGBOnParseDataSetToJSONArray;
begin
  result := FOnParseDataSetToJSONArray;
end;

function TGBClientCoreSettings.OnParseDataSetToJSONArray(Value: TGBOnParseDataSetToJSONArray): IGBClientSettings;
begin
  result := Self;
  if Assigned(Value) then
    FOnParseDataSetToJSONArray := Value;
end;

function TGBClientCoreSettings.OnParseDataSetToJSONObject(Value: TGBOnParseDataSetToJSONObject): IGBClientSettings;
begin
  result := Self;
  if Assigned(Value) then
    FOnParseDataSetToJSONObject := Value;
end;

function TGBClientCoreSettings.OnParseDataSetToJSONObject: TGBOnParseDataSetToJSONObject;
begin
  result := FOnParseDataSetToJSONObject;
end;

function TGBClientCoreSettings.OnParseJSONToDataSet(Value: TGBOnParseJSONToDataSet): IGBClientSettings;
begin
  result := Self;
  if Assigned(Value) then
    FOnParseJSONToDataSet := Value;
end;

function TGBClientCoreSettings.OnParseJSONToDataSet: TGBOnParseJSONToDataSet;
begin
  result := FOnParseJSONToDataSet;
end;

function TGBClientCoreSettings.OnParseJSONToObject: TGBOnParseJSONToObject;
begin
  result := FOnParseJSONToObject;
end;

function TGBClientCoreSettings.OnParseObjectToJSON: TGBOnParseObjectToJSON;
begin
  result := FOnParseObjectToJSON;
end;

function TGBClientCoreSettings.OnParseObjectToJSON(Value: TGBOnParseObjectToJSON): IGBClientSettings;
begin
  result := Self;
  if Assigned(Value) then
    FOnParseObjectToJSON := Value;
end;

function TGBClientCoreSettings.OnParseJSONToObject(Value: TGBOnParseJSONToObject): IGBClientSettings;
begin
  result := Self;
  if Assigned(Value) then
    FOnParseJSONToObject := Value;
end;

end.
