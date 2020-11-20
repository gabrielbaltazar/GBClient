unit GBClient.IdHTTP.Exceptions;

interface

uses
  GBClient.Exceptions,
  System.SysUtils,
  System.Classes,
  IdHTTP;

type
  EGBIdHTTPException = class(EGBRestException)
  protected
    FIdHTTP: TIdHTTP;

    function GetStatusCode: Integer; override;
    function GetStatusText: String; override;
    function GetContent: string; override;

  public
    constructor create(IdHTTP: TIdHTTP); overload;
  end;

implementation

{ EGBIdHTTPException }

constructor EGBIdHTTPException.create(IdHTTP: TIdHTTP);
begin
  FIdHTTP := IdHTTP;
  Self.Message := FIdHTTP.ResponseCode.ToString + ': ' + FIdHTTP.ResponseText;
end;

function EGBIdHTTPException.GetContent: string;
var
  stream: TStringStream;
begin
  stream := TStringStream.Create;
  try
    stream.LoadFromStream(FIdHTTP.Response.ContentStream);
    result := stream.DataString;
  finally
    stream.Free;
  end;
end;

function EGBIdHTTPException.GetStatusCode: Integer;
begin
  result := FIdHTTP.ResponseCode;
end;

function EGBIdHTTPException.GetStatusText: String;
begin
  result := FIdHTTP.ResponseText;
end;

end.
