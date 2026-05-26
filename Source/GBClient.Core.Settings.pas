unit GBClient.Core.Settings;

interface

{$IFDEF WEAKPACKAGEUNIT}
  {$WEAKPACKAGEUNIT ON}
{$ENDIF}

uses
  Data.DB,
  REST.Json,
  System.JSON,
  GBClient.Interfaces,
  GBClient.Core.Helpers;

type
  TGBClientCoreSettings = class(TInterfacedObject, IGBClientSettings)
  private
    [Weak]
    FParent: IGBClientRequest;
    FOnParseJSONToObject: TGBOnParseJSONToObject;
    FOnParseObjectToJSON: TGBOnParseObjectToJSON;
    FOnParseJSONToDataSet: TGBOnParseJSONToDataSet;
    FOnParseDataSetToJSONObject: TGBOnParseDataSetToJSONObject;
    FOnParseDataSetToJSONArray: TGBOnParseDataSetToJSONArray;
  protected
    function OnParseJSONToObject(const AValue: TGBOnParseJSONToObject): IGBClientSettings; overload;
    function OnParseJSONToObject: TGBOnParseJSONToObject; overload;

    function OnParseObjectToJSON(const AValue: TGBOnParseObjectToJSON): IGBClientSettings; overload;
    function OnParseObjectToJSON: TGBOnParseObjectToJSON; overload;

    function OnParseJSONToDataSet(const AValue: TGBOnParseJSONToDataSet): IGBClientSettings; overload;
    function OnParseJSONToDataSet: TGBOnParseJSONToDataSet; overload;

    function OnParseDataSetToJSONObject(const AValue: TGBOnParseDataSetToJSONObject): IGBClientSettings; overload;
    function OnParseDataSetToJSONObject: TGBOnParseDataSetToJSONObject; overload;

    function OnParseDataSetToJSONArray(const AValue: TGBOnParseDataSetToJSONArray): IGBClientSettings; overload;
    function OnParseDataSetToJSONArray: TGBOnParseDataSetToJSONArray; overload;

    function &End: IGBClientRequest;
  public
    constructor Create(const AParent: IGBClientRequest);
    class function New(const AParent: IGBClientRequest): IGBClientSettings;
  end;

procedure ParseJSONToObject(const AJSON: TJSONObject; const AObject: TObject);
function ParseObjectToJSON(const AObject: TObject): TJSONObject;

procedure ParseJSONToDataSet(const AJSON: TJSONObject; const ADataSet: TDataSet);
function ParseDataSetToJSONObject(const ADataSet: TDataSet): TJSONObject;
function ParseDataSetToJSONArray(const ADataSet: TDataSet): TJSONArray;

implementation

procedure ParseJSONToObject(const AJSON: TJSONObject; const AObject: TObject);
begin
  if (Assigned(AJSON)) and (Assigned(AObject)) then
    TJson.JsonToObject(AObject, AJSON);
end;

function ParseObjectToJSON(const AObject: TObject): TJSONObject;
begin
  Result := TJson.ObjectToJsonObject(AObject);
end;

procedure ParseJSONToDataSet(const AJSON: TJSONObject; const ADataSet: TDataSet);
begin
  ADataSet.FromJSON(AJSON);
end;

function ParseDataSetToJSONObject(const ADataSet: TDataSet): TJSONObject;
begin
  Result := ADataSet.ToJSONObject;
end;

function ParseDataSetToJSONArray(const ADataSet: TDataSet): TJSONArray;
begin
  Result := ADataSet.ToJSONArray;
end;

{ TGBClientSettingsDefault }

constructor TGBClientCoreSettings.Create(const AParent: IGBClientRequest);
begin
  FParent := AParent;
  FOnParseJSONToObject := ParseJSONToObject;
  FOnParseObjectToJSON := ParseObjectToJSON;
  FOnParseJSONToDataSet := ParseJSONToDataSet;
  FOnParseDataSetToJSONObject := ParseDataSetToJSONObject;
  FOnParseDataSetToJSONArray := ParseDataSetToJSONArray;
end;

function TGBClientCoreSettings.&End: IGBClientRequest;
begin
  Result := FParent;
end;

class function TGBClientCoreSettings.New(const AParent: IGBClientRequest): IGBClientSettings;
begin
  Result := Self.create(AParent);
end;

function TGBClientCoreSettings.OnParseDataSetToJSONArray: TGBOnParseDataSetToJSONArray;
begin
  Result := FOnParseDataSetToJSONArray;
end;

function TGBClientCoreSettings.OnParseDataSetToJSONArray(const AValue: TGBOnParseDataSetToJSONArray): IGBClientSettings;
begin
  Result := Self;
  if Assigned(AValue) then
    FOnParseDataSetToJSONArray := AValue;
end;

function TGBClientCoreSettings.OnParseDataSetToJSONObject(const AValue: TGBOnParseDataSetToJSONObject):
  IGBClientSettings;
begin
  Result := Self;
  if Assigned(AValue) then
    FOnParseDataSetToJSONObject := AValue;
end;

function TGBClientCoreSettings.OnParseDataSetToJSONObject: TGBOnParseDataSetToJSONObject;
begin
  Result := FOnParseDataSetToJSONObject;
end;

function TGBClientCoreSettings.OnParseJSONToDataSet(const AValue: TGBOnParseJSONToDataSet): IGBClientSettings;
begin
  Result := Self;
  if Assigned(AValue) then
    FOnParseJSONToDataSet := AValue;
end;

function TGBClientCoreSettings.OnParseJSONToDataSet: TGBOnParseJSONToDataSet;
begin
  Result := FOnParseJSONToDataSet;
end;

function TGBClientCoreSettings.OnParseJSONToObject: TGBOnParseJSONToObject;
begin
  Result := FOnParseJSONToObject;
end;

function TGBClientCoreSettings.OnParseObjectToJSON: TGBOnParseObjectToJSON;
begin
  Result := FOnParseObjectToJSON;
end;

function TGBClientCoreSettings.OnParseObjectToJSON(const AValue: TGBOnParseObjectToJSON): IGBClientSettings;
begin
  Result := Self;
  if Assigned(AValue) then
    FOnParseObjectToJSON := AValue;
end;

function TGBClientCoreSettings.OnParseJSONToObject(const AValue: TGBOnParseJSONToObject): IGBClientSettings;
begin
  Result := Self;
  if Assigned(AValue) then
    FOnParseJSONToObject := AValue;
end;

end.
