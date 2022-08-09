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
    property statusCode: Integer read FStatusCode;
    property statusText: String read FStatusText;
    property content: string read FContent;
    property JSON: TJSONObject read FJSON;

    constructor create(AStatusCode: Integer;
                       AStatusText: string;
                       AContent: string;
                       AJSON: TJSONObject = nil);
    destructor Destroy; override;
  end;

implementation

{ EGBRestException }

constructor EGBRestException.create(AStatusCode: Integer; AStatusText, AContent: string; AJSON: TJSONObject);
begin
  FStatusCode := AStatusCode;
  FStatusText := AStatusText;
  FContent := AContent;

  if AJSON <> nil then
  begin
    FContent := AJSON.ToString;
    FJSON := AJSON.Clone as TJSONObject;
  end;

  Self.Message := Format('%s %s: %s', [FStatusCode.ToString,
                                       FStatusText,
                                       FContent]);
end;

destructor EGBRestException.Destroy;
begin
  if Assigned(FJSON) then
    FJSON.Free;
  inherited;
end;

end.
