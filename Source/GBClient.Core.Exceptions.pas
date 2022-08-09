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
    FJSON: TJSONObject;
  public
    constructor Create(AStatusCode: Integer; AStatusText: string;
      AContent: string; AJSON: TJSONObject = nil);
    destructor Destroy; override;

    property StatusCode: Integer read FStatusCode;
    property StatusText: string read FStatusText;
    property Content: string read FContent;
    property JSON: TJSONObject read FJSON;
  end;

implementation

{ EGBRestException }

constructor EGBRestException.Create(AStatusCode: Integer; AStatusText, AContent: string; AJSON: TJSONObject);
begin
  FStatusCode := AStatusCode;
  FStatusText := AStatusText;
  FContent := AContent;

  if AJSON <> nil then
  begin
    FContent := AJSON.ToString;
    FJSON := AJSON.Clone as TJSONObject;
  end;

  Self.Message := Format('%s %s: %s',
    [FStatusCode.ToString, FStatusText, FContent]);
end;

destructor EGBRestException.Destroy;
begin
  if Assigned(FJSON) then
    FJSON.Free;
  inherited;
end;

end.
