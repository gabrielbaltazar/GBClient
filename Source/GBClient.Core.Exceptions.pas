unit GBClient.Core.Exceptions;

interface

uses
  System.SysUtils,
  System.JSON;

type
  EGBRestExceptionTimeout = class(Exception)
  end;

  EGBRestException = class(Exception)
  protected
    FStatusCode: Integer;
    FStatusText: string;
    FContent: string;

  public
    property statusCode: Integer read FStatusCode;
    property statusText: String read FStatusText;
    property content: string read FContent;

    constructor create(AStatusCode: Integer;
                       AStatusText: string;
                       AContent: string;
                       AJSON: TJSONObject = nil);
  end;

implementation

{ EGBRestException }

constructor EGBRestException.create(AStatusCode: Integer; AStatusText, AContent: string; AJSON: TJSONObject);
begin
  FStatusCode := AStatusCode;
  FStatusText := AStatusText;
  FContent := AContent;

  if AJSON <> nil then
    FContent := AJSON.ToString;

  Self.Message := Format('%s %s: %s', [FStatusCode.ToString,
                                       FStatusText,
                                       FContent]);
end;

end.
