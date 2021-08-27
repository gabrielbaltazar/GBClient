unit GBClient.Core.Exceptions;

interface

uses
  REST.Client,
  System.SysUtils;

type
  EGBRestExceptionTimeout = class(Exception)
  end;

  EGBRestException = class(Exception)
  protected
    function GetStatusCode: Integer; virtual; abstract;
    function GetStatusText: String; virtual; abstract;
    function GetContent: string; virtual; abstract;

  public
    property statusCode: Integer read GetStatusCode;
    property statusText: String read GetStatusText;
    property content: string read GetContent;
  end;

implementation

end.
