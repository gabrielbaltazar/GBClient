unit GBClient.RestClient.Request.Body;

interface

uses
  Data.DB,
  GBClient.Interfaces,
  GBClient.Helpers,
  REST.Client,
  REST.Types,
  System.Generics.Collections,
  System.JSON,
  System.SysUtils;

type TGBClientRestClientRequestBody = class(TInterfacedObject, IGBClientBodyRequest)

  private
    [Weak]
    FParent : IGBClientRequest;
    FRequest: TRESTRequest;

    FContentType : TRESTContentType;

    function ContentTypeFromString(Value: String): TRESTContentType;

  protected
    function AddOrSet(Value : String)                              : IGBClientBodyRequest; overload;
    function AddOrSet(Value : TJSONObject; AOwner: Boolean = False): IGBClientBodyRequest; overload;
    function AddOrSet(Value : TJSONArray;  AOwner: Boolean = False): IGBClientBodyRequest; overload;
    function AddOrSet(Value : TObject;  AOwner: Boolean = False): IGBClientBodyRequest; overload;
    function AddOrSet(Value : TDataSet; ACurrent: Boolean = True): IGBClientBodyRequest; overload;
    function AddOrSet(Value : TList<TObject>; AOwner: Boolean = False): IGBClientBodyRequest; overload;
    function AddOrSet(Name, Value: String): IGBClientBodyRequest; overload;

    function &End: IGBClientRequest;

  public
    class function New(Parent: IGBClientRequest; Request: TRESTRequest): IGBClientBodyRequest;
    constructor create(Parent: IGBClientRequest; Request: TRESTRequest);
    destructor Destroy; override;

end;

implementation

uses
  REST.Json;

{ TGBClientRestClientRequestBody }

function TGBClientRestClientRequestBody.AddOrSet(Value: String): IGBClientBodyRequest;
begin
  if FRequest.Client.ContentType <> EmptyStr then
    FContentType := ContentTypeFromString(FRequest.Client.ContentType);
  result := Self;
  FRequest.Body.Add(Value, FContentType);
end;

function TGBClientRestClientRequestBody.AddOrSet(Value: TJSONObject; AOwner: Boolean): IGBClientBodyRequest;
begin
  result := Self;
  if Assigned(Value) then
    FRequest.Body.Add(Value);

  if AOwner then
    Value.Free;
end;

function TGBClientRestClientRequestBody.AddOrSet(Value: TJSONArray; AOwner: Boolean): IGBClientBodyRequest;
begin
  result := Self;
  if Assigned(Value) then
    FRequest.Body.Add(Value.ToString, TRESTContentType.ctAPPLICATION_JSON);

  if AOwner then
    Value.Free;
end;

function TGBClientRestClientRequestBody.AddOrSet(Value: TObject; AOwner: Boolean): IGBClientBodyRequest;
var
  parse : TGBOnParseObjectToJSON;
begin
  result := Self;
  parse  := FParent.Settings.OnParseObjectToJSON;

  AddOrSet(parse(Value), True);

  if AOwner then
    FreeAndNil(Value);
end;

constructor TGBClientRestClientRequestBody.create(Parent: IGBClientRequest; Request: TRESTRequest);
begin
  FParent  := Parent;
  FRequest := Request;
  FContentType := ctAPPLICATION_JSON;
end;

destructor TGBClientRestClientRequestBody.Destroy;
begin

  inherited;
end;

function TGBClientRestClientRequestBody.&End: IGBClientRequest;
begin
  result := FParent;
end;

class function TGBClientRestClientRequestBody.New(Parent: IGBClientRequest; Request: TRESTRequest): IGBClientBodyRequest;
begin
  result := Self.create(Parent, Request);
end;

function TGBClientRestClientRequestBody.AddOrSet(Value: TDataSet; ACurrent: Boolean): IGBClientBodyRequest;
begin
  result := Self;
  if ACurrent then
    AddOrSet(Value.ToJSONObject, True)
  else
    AddOrSet(Value.ToJSONArray, True);
end;

function TGBClientRestClientRequestBody.ContentTypeFromString(Value: String): TRESTContentType;
var
  auxContentType: string;
begin
  result := ctAPPLICATION_JSON;
  auxContentType := Value.ToLower;
  try
    if auxContentType.Equals('application/atom+xml') then
      Exit( ctAPPLICATION_ATOM_XML );

    if auxContentType.Equals('application/ecmascript') then
      Exit( ctAPPLICATION_ECMASCRIPT );

    if auxContentType.Equals('application/json') then
      Exit( ctAPPLICATION_JSON );

    if auxContentType.Equals('application/javascript') then
      Exit( ctAPPLICATION_JAVASCRIPT );

    if auxContentType.Equals('application/octet-stream') then
      Exit( ctAPPLICATION_OCTET_STREAM );

    if auxContentType.Equals('application/ogg') then
      Exit( ctAPPLICATION_OGG );

    if auxContentType.Equals('application/pdf') then
      Exit( ctAPPLICATION_PDF );

    if auxContentType.Equals('application/postscript') then
      Exit( ctAPPLICATION_POSTSCRIPT );

    if auxContentType.Equals('application/rdf+xml') then
      Exit( ctAPPLICATION_RDF_XML );

    if auxContentType.Equals('application/rss+xml') then
      Exit( ctAPPLICATION_RSS_XML );

    if auxContentType.Equals('application/soap+xml') then
      Exit( ctAPPLICATION_SOAP_XML );

    if auxContentType.Equals('application/font-woff') then
      Exit( ctAPPLICATION_FONT_WOFF );

    if auxContentType.Equals('application/xhtml+xml') then
      Exit( ctAPPLICATION_XHTML_XML );

    if auxContentType.Equals('application/xml') then
      Exit( ctAPPLICATION_XML );

    if auxContentType.Equals('application/xml-dtd') then
      Exit( ctAPPLICATION_XML_DTD );

    if auxContentType.Equals('application/xop+xml') then
      Exit( ctAPPLICATION_XOP_XML );

    if auxContentType.Equals('application/zip') then
      Exit( ctAPPLICATION_ZIP );

    if auxContentType.Equals('application/gzip') then
      Exit( ctAPPLICATION_GZIP );

    if auxContentType.Equals('text/cmd') then
      Exit( ctTEXT_CMD );

    if auxContentType.Equals('text/css') then
      Exit( ctTEXT_CSS );

    if auxContentType.Equals('text/csv') then
      Exit( ctTEXT_CSV );

    if auxContentType.Equals('text/html') then
      Exit( ctTEXT_HTML );

    if auxContentType.Equals('text/javascript') then
      Exit( ctTEXT_JAVASCRIPT );

    if auxContentType.Equals('text/plain') then
      Exit( ctTEXT_PLAIN );

    if auxContentType.Equals('text/vcard') then
      Exit( ctTEXT_VCARD );

    if auxContentType.Equals('text/xml') then
      Exit( ctTEXT_XML );

    if auxContentType.Equals('image/gif') then
      Exit( ctIMAGE_GIF );

    if auxContentType.Equals('image/jpeg') then
      Exit( ctIMAGE_JPEG );

    if auxContentType.Equals('image/pjpeg') then
      Exit( ctIMAGE_PJPEG );

    if auxContentType.Equals('image/png') then
      Exit( ctIMAGE_PNG );

    if auxContentType.Equals('image/svg+xml') then
      Exit( ctIMAGE_SVG_XML );

    if auxContentType.Equals('image/tiff') then
      Exit( ctIMAGE_TIFF );

    if auxContentType.Equals('message/http') then
      Exit( ctMESSAGE_HTTP );

    if auxContentType.Equals('message/imdn+xml') then
      Exit( ctMESSAGE_IMDN_XML );

    if auxContentType.Equals('message/partial') then
      Exit( ctMESSAGE_PARTIAL );

    if auxContentType.Equals('message/rfc822') then
      Exit( ctMESSAGE_RFC822 );

    if auxContentType.Equals('multipart/mixed') then
      Exit( ctMULTIPART_MIXED );

    if auxContentType.Equals('multipart/alternative') then
      Exit( ctMULTIPART_ALTERNATIVE );

    if auxContentType.Equals('multipart/related') then
      Exit( ctMULTIPART_RELATED );

    if auxContentType.Equals('multipart/form-data') then
      Exit( ctMULTIPART_FORM_DATA );

    if auxContentType.Equals('multipart/signed') then
      Exit( ctMULTIPART_SIGNED );

    if auxContentType.Equals('multipart/encrypted') then
      Exit( ctMULTIPART_ENCRYPTED );
  finally
    FContentType := Result;
  end;
end;

function TGBClientRestClientRequestBody.AddOrSet(Value: TList<TObject>; AOwner: Boolean): IGBClientBodyRequest;
var
  parse: TGBOnParseObjectToJSON;
  jsonArray: TJSONArray;
  i: Integer;
begin
  parse  := FParent.Settings.OnParseObjectToJSON;
  result := Self;

  jsonArray := TJSONArray.Create;
  try
    for i := 0 to Pred(Value.Count) do
      jsonArray.AddElement(parse(Value[i]));

    AddOrSet(jsonArray);
  finally
    jsonArray.Free;
    if AOwner then
      FreeAndNil(Value);
  end;
end;

function TGBClientRestClientRequestBody.AddOrSet(Name, Value: String): IGBClientBodyRequest;
var
  parameter: TRESTRequestParameter;
begin
  result := Self;
  parameter := FRequest.Params.AddItem;
  parameter.Name := Name;
  parameter.Value := Value;
  parameter.Kind := TRESTRequestParameterKind.pkREQUESTBODY;
end;

end.
