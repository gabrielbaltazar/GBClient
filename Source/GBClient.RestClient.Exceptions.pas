unit GBClient.RestClient.Exceptions;

interface

uses
  REST.Client,
  System.SysUtils,
  GBClient.Exceptions;

type
  EGBRestRequestException = class(EGBRestException)
  protected
    FRequest: TRESTRequest;

    function GetStatusCode: Integer; override;
    function GetStatusText: String; override;
    function GetContent: string; override;

  public
    constructor create(Request: TRESTRequest);
  end;

implementation

{ EGBRestRequestException }

constructor EGBRestRequestException.create(Request: TRESTRequest);
begin
  FRequest := Request;
  Self.Message := FRequest.Response.StatusCode.ToString + ': ' + FRequest.Response.StatusText;
end;

function EGBRestRequestException.GetContent: string;
begin
  result := FRequest.Response.Content;
end;

function EGBRestRequestException.GetStatusCode: Integer;
begin
  result := FRequest.Response.StatusCode;
end;

function EGBRestRequestException.GetStatusText: String;
begin
  result := FRequest.Response.StatusText;
end;

end.
