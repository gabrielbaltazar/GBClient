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

type
  TGBClientCoreSettings = class(TInterfacedObject, IGBClientSettings)
  private
    [Weak]
    FParent : IGBClientRequest;
    FOnParseJSONToObject: TGBOnParseJSONToObject;
    FOnParseObjectToJSON: TGBOnParseObjectToJSON;
    FOnParseJSONToDataSet: TGBOnParseJSONToDataSet;
    FOnParseDataSetToJSONObject: TGBOnParseDataSetToJSONObject;
    FOnParseDataSetToJSONArray : TGBOnParseDataSetToJSONArray;
  protected
    function OnParseJSONToObject(AValue: TGBOnParseJSONToObject): IGBClientSettings; overload;
    function OnParseJSONToObject: TGBOnParseJSONToObject; overload;

    function OnParseObjectToJSON(AValue: TGBOnParseObjectToJSON): IGBClientSettings; overload;
    function OnParseObjectToJSON: TGBOnParseObjectToJSON; overload;

    function OnParseJSONToDataSet(AValue: TGBOnParseJSONToDataSet): IGBClientSettings; overload;
    function OnParseJSONToDataSet: TGBOnParseJSONToDataSet; overload;

    function OnParseDataSetToJSONObject(AValue: TGBOnParseDataSetToJSONObject): IGBClientSettings; overload;
    function OnParseDataSetToJSONObject: TGBOnParseDataSetToJSONObject; overload;

    function OnParseDataSetToJSONArray(AValue: TGBOnParseDataSetToJSONArray): IGBClientSettings; overload;
    function OnParseDataSetToJSONArray: TGBOnParseDataSetToJSONArray; overload;

    function &End: IGBClientRequest;
  public
    constructor Create(AParent: IGBClientRequest);
    class function New(AParent: IGBClientRequest): IGBClientSettings;
    destructor  Destroy; override;
end;

procedure ParseJSONToObject(AJSON: TJSONObject; AObject: TObject);
function ParseObjectToJSON(AObject: TObject): TJSONObject;

procedure ParseJSONToDataSet(AJSON: TJSONObject; ADataSet: TDataSet);
function  ParseDataSetToJSONObject(ADataSet: TDataSet): TJSONObject;
function  ParseDataSetToJSONArray (ADataSet: TDataSet): TJSONArray;

implementation

procedure ParseJSONToObject(AJSON: TJSONObject; AObject: TObject);
begin
  if (Assigned(AJSON)) and (Assigned(AObject)) then
    TJson.JsonToObject(AObject, AJSON);
end;

function ParseObjectToJSON(AObject: TObject): TJSONObject;
begin
  Result := TJson.ObjectToJsonObject(AObject);
end;

procedure ParseJSONToDataSet(AJSON: TJSONObject; ADataSet: TDataSet);
begin
  ADataSet.FromJSON(AJSON);
end;

function ParseDataSetToJSONObject(ADataSet: TDataSet): TJSONObject;
begin
  Result := ADataSet.ToJSONObject;
end;

function ParseDataSetToJSONArray (ADataSet: TDataSet): TJSONArray;
begin
  Result := ADataSet.ToJSONArray;
end;

{ TGBClientSettingsDefault }

constructor TGBClientCoreSettings.Create(AParent: IGBClientRequest);
begin
  FParent := AParent;
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
  Result := FParent;
end;

class function TGBClientCoreSettings.New(AParent: IGBClientRequest): IGBClientSettings;
begin
  Result := Self.Create(AParent);
end;

function TGBClientCoreSettings.OnParseDataSetToJSONArray: TGBOnParseDataSetToJSONArray;
begin
  Result := FOnParseDataSetToJSONArray;
end;

function TGBClientCoreSettings.OnParseDataSetToJSONArray(AValue: TGBOnParseDataSetToJSONArray): IGBClientSettings;
begin
  Result := Self;
  if Assigned(AValue) then
    FOnParseDataSetToJSONArray := AValue;
end;

function TGBClientCoreSettings.OnParseDataSetToJSONObject(AValue: TGBOnParseDataSetToJSONObject): IGBClientSettings;
begin
  Result := Self;
  if Assigned(AValue) then
    FOnParseDataSetToJSONObject := AValue;
end;

function TGBClientCoreSettings.OnParseDataSetToJSONObject: TGBOnParseDataSetToJSONObject;
begin
  Result := FOnParseDataSetToJSONObject;
end;

function TGBClientCoreSettings.OnParseJSONToDataSet(AValue: TGBOnParseJSONToDataSet): IGBClientSettings;
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

function TGBClientCoreSettings.OnParseObjectToJSON(AValue: TGBOnParseObjectToJSON): IGBClientSettings;
begin
  Result := Self;
  if Assigned(AValue) then
    FOnParseObjectToJSON := AValue;
end;

function TGBClientCoreSettings.OnParseJSONToObject(AValue: TGBOnParseJSONToObject): IGBClientSettings;
begin
  Result := Self;
  if Assigned(AValue) then
    FOnParseJSONToObject := AValue;
end;

end.
