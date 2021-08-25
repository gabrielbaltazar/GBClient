unit GBClient.NetHTTPClient.Exceptions;

interface

uses
  GBClient.Core.Exceptions,
  System.SysUtils,
  System.Net.HttpClient;

type
  EGBNetHTTPClientException = class(EGBRestException)
  protected
    FResponse: IHTTPResponse;

    function GetStatusCode: Integer; override;
    function GetStatusText: String; override;
    function GetContent: string; override;

  public
    constructor create(Response: IHTTPResponse);
  end;

implementation

{ EGBNetHTTPClientException }

constructor EGBNetHTTPClientException.create(Response: IHTTPResponse);
begin
  FResponse := Response;
  Self.Message := Format('%s %s: %s', [Response.StatusCode.ToString,
                                       Response.StatusText,
                                       Response.ContentAsString]);
end;

function EGBNetHTTPClientException.GetContent: string;
begin
  result := FResponse.ContentAsString;
end;

function EGBNetHTTPClientException.GetStatusCode: Integer;
begin
  result := FResponse.StatusCode;
end;

function EGBNetHTTPClientException.GetStatusText: String;
begin
  result := FResponse.StatusText;
end;

end.
