unit GBClient.Core.Request.Auth.AWS;

interface

{$IFDEF WEAKPACKAGEUNIT}
  {$WEAKPACKAGEUNIT ON}
{$ENDIF}

uses
  System.Net.URLClient,
  System.Generics.Collections,
  System.DateUtils,
  System.SysUtils,
  System.Classes,
  System.TimeSpan,
  System.Hash,
  GBClient.Interfaces;

//////////////////////////////////////////
// Based on Wallace Oliveira function
// https://github.com/w0ll
// Thanks a lot
//////////////////////////////////////////

type
  TGBClientCoreRequestAuthAWS = class(TInterfacedObject, IGBClientAuthAWSv4)
  private
    const
      ALGORITHM = 'AWS4-HMAC-SHA256';

    procedure Initialize;

    function HashSHA256(const AHashString: string): string;

    function URLEncodeValue(const AValue: string): string;
    function URLEncode(const AStr: string; const AEncodeChars: array of Char): string;
  protected
    [Weak]
    FParent: IGBClientRequest;
    FHeaders: TDictionary<string, string>;
    FQueries: TDictionary<string, string>;

    FHost: string;
    FAmzDate: string;
    FAuthorization: string;
    FDateStamp: string;
    FSignedHeader: string;
    FPayload: string;
    FHTTPVerb: string;
    FUtcOffSet: Integer;
    FAccessKey: string;
    FSecretKey: string;
    FRegion: string;
    FService: string;

    function AccessKey(const AValue: string): IGBClientAuthAWSv4;
    function SecretKey(const AValue: string): IGBClientAuthAWSv4;
    function Region(const AValue: string): IGBClientAuthAWSv4;
    function Service(const AValue: string): IGBClientAuthAWSv4;

    function HTTPVerb(const AValue: string): IGBClientAuthAWSv4;
    function Host(const AValue: string): IGBClientAuthAWSv4;

    function HeaderAddOrSet(const AKey, AValue: string): IGBClientAuthAWSv4;
    function QueryAddOrSet(const AKey, AValue: string): IGBClientAuthAWSv4;

    function Payload(const AValue: string): IGBClientAuthAWSv4; overload;
    function Payload(const AValue: TStream): IGBClientAuthAWSv4; overload;

    function XAmzDate: string;
    function Authorization: string;

    function &End: IGBClientRequest;

    function GetCannonicalURI: string;
    function GetCannonicalQuery: string;
    function GetCannonicalHeader: string;
    function GetCannonicalBody: string;

    function GetCredentialScope: string;

    // https://docs.aws.amazon.com/pt_br/general/latest/gr/sigv4-signed-request-examples.html
    function GetCannonicalRequest: string;
    function GetStringToSignin(const ACannonicalRequest: string): string;
    function CalculateSignature(const AStringToSignin: string): string;
    function GetAuthorizationHeader(const ASignature: string): string;
  public
    constructor Create(const AParent: IGBClientRequest);
    class function New(const AParent: IGBClientRequest): IGBClientAuthAWSv4;
    destructor Destroy; override;

    function Apply: IGBClientAuthAWSv4;
  end;

implementation

{ TGBClientCoreRequestAuthAWS }

function TGBClientCoreRequestAuthAWS.AccessKey(const AValue: string):
    IGBClientAuthAWSv4;
begin
  Result := Self;
  FAccessKey := AValue;
end;

function TGBClientCoreRequestAuthAWS.Region(const AValue: string):
    IGBClientAuthAWSv4;
begin
  Result := Self;
  FRegion := AValue;
end;

constructor TGBClientCoreRequestAuthAWS.Create(const AParent: IGBClientRequest);
begin
  FParent := AParent;
  FRegion := 'us-east-1';
  FUtcOffSet := TTimeZone.Local.GetUtcOffset(now).Hours * -1;
  FQueries := TDictionary<string, string>.Create;
  FHeaders := TDictionary<string, string>.Create;
end;

destructor TGBClientCoreRequestAuthAWS.Destroy;
begin
  FQueries.Free;
  FHeaders.Free;
  inherited;
end;

function TGBClientCoreRequestAuthAWS.&End: IGBClientRequest;
begin
  Result := FParent;
end;

function TGBClientCoreRequestAuthAWS.GetAuthorizationHeader(const ASignature: string): string;
begin
  Result := Format('%s Credential=%s/%s, SignedHeaders=%s, Signature=%s',
    [ALGORITHM, FAccessKey, GetCredentialScope, FSignedHeader, ASignature]);
end;

function TGBClientCoreRequestAuthAWS.GetCannonicalBody: string;
begin
  Result := HashSHA256(FPayload);
end;

function TGBClientCoreRequestAuthAWS.GetCannonicalHeader: string;
var
  LHost: string;
begin
  FSignedHeader := 'host;x-amz-date';
  LHost := FHost;

  if not LHost.StartsWith('http') then
    LHost := 'https://' + LHost;

  LHost := TURI.Create(LHost).Host;
  Result := Format('host:%s'#10'x-amz-date:%s'#10#10'%s', [LHost, FAmzDate, FSignedHeader]);
end;

function TGBClientCoreRequestAuthAWS.GetCannonicalQuery: string;
var
  LName: string;
  LValue: string;
  LQueries: TArray<string>;
  LCount: Integer;
begin
  Result := EmptyStr;
  LQueries:= FQueries.Keys.ToArray;
  TArray.Sort<string>(LQueries);
  for LCount := 0 to Pred(Length(LQueries)) do
  begin
    LName := LQueries[LCount];
    LValue := URLEncodeValue( FQueries.Items[LName] );
    if not Result.IsEmpty then
      Result := Result + '&';
    Result := Result + Format('%s=%s', [LName, LValue]);
  end;
end;

function TGBClientCoreRequestAuthAWS.GetCannonicalRequest: string;
var
  LCannonicalURI: string;
  LCannonicalQuery: string;
  LCannonicalHeader: string;
  LCannonicalBody: string;
begin
  LCannonicalURI := GetCannonicalURI;
  LCannonicalQuery := GetCannonicalQuery;
  LCannonicalHeader := GetCannonicalHeader;
  LCannonicalBody := GetCannonicalBody;

  Result := FHTTPVerb + #10 +
    LCannonicalURI + #10 +
    LCannonicalQuery + #10 +
    LCannonicalHeader + #10 +
    LCannonicalBody;
end;

function TGBClientCoreRequestAuthAWS.GetCannonicalURI: string;
begin
  Result := TURI.Create(FHost).Path;
end;

function TGBClientCoreRequestAuthAWS.GetCredentialScope: string;
begin
  Result := Format('%s/%s/%s/aws4_request', [FDateStamp, FRegion, FService]);
end;

function TGBClientCoreRequestAuthAWS.Apply: IGBClientAuthAWSv4;
var
  LCannonicalRequest: string;
  LStringToSignin: string;
  LSignature: string;
begin
  Result := Self;
  Initialize;
  LCannonicalRequest := GetCannonicalRequest;
  LStringToSignin := GetStringToSignin(LCannonicalRequest);
  LSignature := CalculateSignature(LStringToSignin);
  FAuthorization := GetAuthorizationHeader(LSignature);
end;

function TGBClientCoreRequestAuthAWS.Authorization: string;
begin
  Result := FAuthorization;
end;

function TGBClientCoreRequestAuthAWS.CalculateSignature(const AStringToSignin: string): string;
var
  LKDate: TBytes;
  LKRegion: TBytes;
  LKService: TBytes;
  LKSigningKey: TBytes;
begin
  LKDate := THashSHA2.GetHMACAsBytes(FDateStamp, 'AWS4'+ FSecretKey);
  LKRegion := THashSHA2.GetHMACAsBytes(FRegion, LKDate);
  LKService := THashSHA2.GetHMACAsBytes(FService, LKRegion);
  LKSigningKey := THashSHA2.GetHMACAsBytes('aws4_request', LKService);
  Result := THash.DigestAsString(THashSHA2.GetHMACAsBytes(AStringToSignin, LKSigningKey));;
end;

function TGBClientCoreRequestAuthAWS.GetStringToSignin(const ACannonicalRequest: string): string;
var
  LCredentialScope: string;
  LHashRequest: string;
begin
  LCredentialScope := Format('%s/%s/%s/aws4_request', [FDateStamp, FRegion, FService]);
  LHashRequest := HashSHA256(ACannonicalRequest);
  Result := Format('%s'#10'%s'#10'%s'#10'%s', [ALGORITHM, FAmzDate, LCredentialScope, LHashRequest]);
end;

function TGBClientCoreRequestAuthAWS.HashSHA256(const AHashString: string): string;
begin
  Result := THashSHA2.GetHashString(AHashString);
end;

function TGBClientCoreRequestAuthAWS.HeaderAddOrSet(const AKey, AValue: string): IGBClientAuthAWSv4;
begin
  Result := Self;
  FHeaders.AddOrSetValue(AKey, AValue);
end;

function TGBClientCoreRequestAuthAWS.Host(const AValue: string): IGBClientAuthAWSv4;
var
  LContentHost: TArray<string>;
begin
  Result := Self;
  FHost := AValue;
  LContentHost := FHost.Split(['.']);
  if (FService = EmptyStr) and (Length(LContentHost) >= 3) then
    FService := LContentHost[2];
end;

function TGBClientCoreRequestAuthAWS.HTTPVerb(const AValue: string): IGBClientAuthAWSv4;
begin
  Result := Self;
  FHTTPVerb := AValue.ToUpper;
end;

procedure TGBClientCoreRequestAuthAWS.Initialize;
begin
  FSignedHeader := EmptyStr;
  FAmzDate := FormatDateTime('YYYYMMDD''T''HHMMSS''Z''', IncHour(NOW, FUtcOffSet));
  FDateStamp := FormatDateTime('YYYYMMDD', IncHour(NOW, FUtcOffSet));
end;

class function TGBClientCoreRequestAuthAWS.New(const AParent: IGBClientRequest): IGBClientAuthAWSv4;
begin
  Result := Self.Create(AParent);
end;

function TGBClientCoreRequestAuthAWS.Payload(const AValue: TStream): IGBClientAuthAWSv4;
var
  LStream: TStringStream;
begin
  Result := Self;
  if Assigned(AValue) then
  begin
    LStream := TStringStream.Create;
    try
      LStream.LoadFromStream(AValue);
      LStream.Position := 0;
      Payload(LStream.DataString);
    finally
      LStream.Free;
    end;
  end;
end;

function TGBClientCoreRequestAuthAWS.Payload(const AValue: string): IGBClientAuthAWSv4;
begin
  Result := Self;
  FPayload := AValue;
end;

function TGBClientCoreRequestAuthAWS.QueryAddOrSet(const AKey, AValue: string): IGBClientAuthAWSv4;
begin
  Result := Self;
  FQueries.AddOrSetValue(AKey, AValue);
end;

function TGBClientCoreRequestAuthAWS.SecretKey(const AValue: string): IGBClientAuthAWSv4;
begin
  Result := Self;
  FSecretKey := AValue;
end;

function TGBClientCoreRequestAuthAWS.Service(const AValue: string): IGBClientAuthAWSv4;
begin
  Result := Self;
  FService := AValue;
end;

function TGBClientCoreRequestAuthAWS.URLEncode(const AStr: string; const AEncodeChars: array of Char): string;

  function IsHexChar(C: Byte): Boolean;
  begin
    case Char(C) of
      '0'..'9', 'a'..'f', 'A'..'F':  Result := True;
    else
      Result := False;
    end;
  end;

const
  // Safe characters from on TIdURL.ParamsEncode
  // '*<>#%"{}|\^[]`'
  DefaultUnsafeChars: array[0..13] of Byte = (Ord('*'), Ord('<'), Ord('>'), Ord('#'),
    Ord('%'), Ord('"'), Ord('{'), Ord('}'), Ord('|'), Ord('\'), Ord('^'), Ord('['), Ord(']'), Ord('`'));
  XD: array[0..15] of char = ('0', '1', '2', '3', '4', '5', '6', '7',
                              '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');

var
  Buff: TBytes;
  I, J: Integer;
  IsUnsafe: Boolean;
begin

  Result := '';
  if AStr <> '' then
  begin
    Buff := TEncoding.UTF8.GetBytes(AStr);
    I := 0;
    while I < Length(Buff) do
    begin
      if (Buff[I] < 33) or (Buff[I] > 127) then
        IsUnsafe := True
      else
      begin
        IsUnsafe := False;
        for J := 0 to Length(DefaultUnsafeChars) - 1 do
          if Buff[I] = DefaultUnsafeChars[J] then
          begin
            IsUnsafe := True;
            break;
          end;
        if not IsUnsafe then
          for J := 0 to Length(AEncodeChars) - 1 do
            if Char(Buff[I]) = AEncodeChars[J] then
            begin
              IsUnsafe := True;
              break;
            end;
      end;
      if IsUnsafe then
        Result := Result + '%' + XD[(Buff[I] shr 4) and $0F] + XD[Buff[I] and $0F]
      else
        Result := Result + Char(Buff[I]);
      Inc(I);
    end;
  end;
end;

function TGBClientCoreRequestAuthAWS.URLEncodeValue(const AValue: string): string;
begin
  Result := URLEncode(AValue, ['=', ':', '/', '+', '(', ')', '/', '!', '"', '$', '@', '&', ',', '''', '?', ';']);
end;

function TGBClientCoreRequestAuthAWS.XAmzDate: string;
begin
  Result := FAmzDate;
end;

end.
