unit GBClient.Core.Exceptions;

interface

{$IFDEF WEAKPACKAGEUNIT}
  {$WEAKPACKAGEUNIT ON}
{$ENDIF}

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
    constructor Create(const AStatusCode: Integer; const AStatusText: string; const AContent: string;
      const AJSON: TJSONObject = nil);

    property StatusCode: Integer read FStatusCode;
    property StatusText: string read FStatusText;
    property Content: string read FContent;
  end;

implementation

{ EGBRestException }

constructor EGBRestException.Create(const AStatusCode: Integer; const AStatusText: string;
  const AContent: string; const AJSON: TJSONObject = nil);
begin
  FStatusCode := AStatusCode;
  FStatusText := AStatusText;
  FContent := AContent;

  if AJSON <> nil then
    FContent := AJSON.ToString;

  Self.Message := Format('%s %s: %s', [FStatusCode.ToString, FStatusText, FContent]);
end;

end.
