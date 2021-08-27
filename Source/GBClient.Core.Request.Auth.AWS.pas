unit GBClient.Core.Request.Auth.AWS;

interface

uses
  GBClient.Interfaces,
  System.Net.URLClient,
  System.Generics.Collections,
  System.DateUtils,
  System.SysUtils,
  System.Classes,
  System.TimeSpan,
  System.Hash;

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

    function HashSHA256(hashString: string): string;

    procedure SortDictionary(Value: TDictionary<String, String>);

    function URLEncodeValue(const Value: String): string;
    function URLEncode(const Str: string; const EncodeChars: array of Char): string;

  protected
    [Weak]
    FParent: IGBClientRequest;
    FHeaders: TDictionary<String, String>;
    FQueries: TDictionary<String, String>;

    FHost: String;
    FAmzDate: String;
    FDateStamp: string;
    FSignedHeader: string;
    FPayload: string;
    FHTTPVerb: string;
    FUtcOffSet: Integer;
    FAccessKey: string;
    FSecretKey: String;
    FRegion: string;
    FService: string;
    FOnAWSSignature: TGBOnAWSSignature;

    function OnAWSSignature(Value: TGBOnAWSSignature): IGBClientAuthAWSv4;

    function AccessKey(Value: String): IGBClientAuthAWSv4;
    function SecretKey(Value: String): IGBClientAuthAWSv4;
    function Region(Value: String): IGBClientAuthAWSv4;
    function Service(Value: String): IGBClientAuthAWSv4;

    function HTTPVerb(Value: String): IGBClientAuthAWSv4;
    function Host(Value: String): IGBClientAuthAWSv4;

    function HeaderAddOrSet(Key, Value: String): IGBClientAuthAWSv4;
    function QueryAddOrSet(Key, Value: String): IGBClientAuthAWSv4;

    function Payload(Value: String): IGBClientAuthAWSv4; overload;
    function Payload(Value: TStream): IGBClientAuthAWSv4; overload;

    function &End: IGBClientRequest;

    function GetCannonicalURI: string;
    function GetCannonicalQuery: string;
    function GetCannonicalHeader: string;
    function GetCannonicalBody: string;

    function GetCredentialScope: String;

    // https://docs.aws.amazon.com/pt_br/general/latest/gr/sigv4-signed-request-examples.html
    function GetCannonicalRequest: string;
    function GetStringToSignin(ACannonicalRequest: String): String;
    function CalculateSignature(AStringToSignin: String): string;
    function GetAuthorizationHeader(Signature: String): String;

  public

    procedure Apply;

    constructor create(Parent: IGBClientRequest);
    class function New(Parent: IGBClientRequest): IGBClientAuthAWSv4;
    destructor Destroy; override;
end;

implementation

{ TGBClientCoreRequestAuthAWS }

function TGBClientCoreRequestAuthAWS.AccessKey(Value: String): IGBClientAuthAWSv4;
begin
  result := Self;
  FAccessKey := Value;
end;

function TGBClientCoreRequestAuthAWS.Region(Value: String): IGBClientAuthAWSv4;
begin
  result := Self;
  FRegion := Value;
end;

constructor TGBClientCoreRequestAuthAWS.create(Parent: IGBClientRequest);
begin
  FParent := Parent;
  FRegion := 'us-east-1';
  FUtcOffSet := TTimeZone.Local.GetUtcOffset(now).Hours * -1;
  FQueries := TDictionary<String, String>.create;
  FHeaders := TDictionary<String, String>.create;
end;

destructor TGBClientCoreRequestAuthAWS.Destroy;
begin
  FQueries.Free;
  FHeaders.Free;
  inherited;
end;

function TGBClientCoreRequestAuthAWS.&End: IGBClientRequest;
begin
  result := FParent;
end;

function TGBClientCoreRequestAuthAWS.GetAuthorizationHeader(Signature: String): String;
begin
  result := Format('%s Credential=%s/%s, SignedHeaders=%s, Signature=%s',
              [ALGORITHM, FAccessKey, GetCredentialScope, FSignedHeader, Signature]);
end;

function TGBClientCoreRequestAuthAWS.GetCannonicalBody: string;
begin
  result := HashSHA256(FPayload);
end;

function TGBClientCoreRequestAuthAWS.GetCannonicalHeader: string;
var
  host: string;
begin
  FSignedHeader := 'host;x-amz-date';
  host := FHost;

  if not host.StartsWith('http') then
    host := 'https://' + host;

  host := TURI.Create(host).Host;

  result := Format('host:%s'#10'x-amz-date:%s'#10#10'%s',
                  [host, FAmzDate, FSignedHeader]);
end;

function TGBClientCoreRequestAuthAWS.GetCannonicalQuery: string;
var
  name: string;
  value: string;
begin
  result := EmptyStr;
  SortDictionary(FQueries);

  for name in FQueries.Keys do
  begin
    value := URLEncodeValue( FQueries.Items[name] );
    if not Result.IsEmpty then
      result := result + '&';
    result := result + Format('%s=%s', [name, value]);
  end;
end;

function TGBClientCoreRequestAuthAWS.GetCannonicalRequest: string;
var
  cannonicalURI: string;
  cannonicalQuery: String;
  cannonicalHeader: String;
  cannonicalBody: string;
begin

  cannonicalURI := GetCannonicalURI;
  cannonicalQuery := GetCannonicalQuery;
  cannonicalHeader := GetCannonicalHeader;
  cannonicalBody := GetCannonicalBody;

  result := FHTTPVerb + #10 +
            cannonicalURI + #10 +
            cannonicalQuery + #10 +
            cannonicalHeader + #10 +
            cannonicalBody;
end;

function TGBClientCoreRequestAuthAWS.GetCannonicalURI: string;
begin
  Result := TURI.Create(FHost).Path;
end;

function TGBClientCoreRequestAuthAWS.GetCredentialScope: String;
begin
  result := Format('%s/%s/%s/aws4_request', [FDateStamp, FRegion, FService]);
end;

procedure TGBClientCoreRequestAuthAWS.Apply;
var
  cannonicalRequest: String;
  stringToSignin: string;
  signature: string;
  authorization: string;
begin
  Initialize;
  cannonicalRequest := GetCannonicalRequest;
  stringToSignin := GetStringToSignin(cannonicalRequest);
  signature := CalculateSignature(stringToSignin);
  authorization := GetAuthorizationHeader(signature);

  if Assigned(FOnAWSSignature) then
    FOnAWSSignature(authorization, FAmzDate);
end;

function TGBClientCoreRequestAuthAWS.CalculateSignature(AStringToSignin: String): string;
var
  kDate: TBytes;
  kRegion: TBytes;
  kService: TBytes;
  kSigningKey: TBytes;
begin
  kDate := THashSHA2.GetHMACAsBytes(FDateStamp, 'AWS4'+ FSecretKey);
  kRegion := THashSHA2.GetHMACAsBytes(FRegion, kDate);
  kService := THashSHA2.GetHMACAsBytes(FService, kRegion);
  kSigningKey := THashSHA2.GetHMACAsBytes('aws4_request', kService);

  result := THash.DigestAsString(THashSHA2.GetHMACAsBytes(AStringToSignin, kSigningKey));;
end;

function TGBClientCoreRequestAuthAWS.GetStringToSignin(ACannonicalRequest: String): String;
var
  credentialScope: String;
  hashRequest: string;
begin
  credentialScope := Format('%s/%s/%s/aws4_request',
                        [FDateStamp, FRegion, FService]);

  hashRequest := HashSHA256(ACannonicalRequest);

  result := Format('%s'#10'%s'#10'%s'#10'%s',
              [ALGORITHM, FAmzDate, credentialScope, hashRequest]);
end;

function TGBClientCoreRequestAuthAWS.HashSHA256(hashString: string): string;
begin
  Result := THashSHA2.GetHashString(hashString);
end;

function TGBClientCoreRequestAuthAWS.HeaderAddOrSet(Key, Value: String): IGBClientAuthAWSv4;
begin
  result := Self;
  FHeaders.AddOrSetValue(Key, Value);
end;

function TGBClientCoreRequestAuthAWS.Host(Value: String): IGBClientAuthAWSv4;
var
  contentHost: TArray<String>;
begin
  result := Self;
  FHost := Value;

  contentHost := FHost.Split(['.']);
  if (FService = EmptyStr) and (Length(contentHost) >= 3) then
    FService := contentHost[2];
end;

function TGBClientCoreRequestAuthAWS.HTTPVerb(Value: String): IGBClientAuthAWSv4;
begin
  result := Self;
  FHTTPVerb := Value.ToUpper;
end;

procedure TGBClientCoreRequestAuthAWS.Initialize;
begin
  FSignedHeader := EmptyStr;
  FAmzDate := FormatDateTime('YYYYMMDD''T''HHMMSS''Z''', IncHour(NOW, FUtcOffSet));
  FDateStamp := FormatDateTime('YYYYMMDD', IncHour(NOW, FUtcOffSet));
end;

class function TGBClientCoreRequestAuthAWS.New(Parent: IGBClientRequest): IGBClientAuthAWSv4;
begin
  result := Self.create(Parent);
end;

function TGBClientCoreRequestAuthAWS.OnAWSSignature(Value: TGBOnAWSSignature): IGBClientAuthAWSv4;
begin
  result := Self;
  FOnAWSSignature := Value;
end;

function TGBClientCoreRequestAuthAWS.Payload(Value: TStream): IGBClientAuthAWSv4;
var
  stream: TStringStream;
begin
  result := Self;
  if Assigned(Value) then
  begin
    stream := TStringStream.Create;
    try
      stream.LoadFromStream(Value);
      stream.Position := 0;
      Payload(stream.DataString);
    finally
      stream.Free;
    end;
  end;
end;

function TGBClientCoreRequestAuthAWS.Payload(Value: String): IGBClientAuthAWSv4;
begin
  result := Self;
  FPayload := Value;
end;

function TGBClientCoreRequestAuthAWS.QueryAddOrSet(Key, Value: String): IGBClientAuthAWSv4;
begin
  result := Self;
  FQueries.AddOrSetValue(Key, Value);
end;

function TGBClientCoreRequestAuthAWS.SecretKey(Value: String): IGBClientAuthAWSv4;
begin
  result := Self;
  FSecretKey := Value;
end;

function TGBClientCoreRequestAuthAWS.Service(Value: String): IGBClientAuthAWSv4;
begin
  result := Self;
  FService := Value;
end;

procedure TGBClientCoreRequestAuthAWS.SortDictionary(Value: TDictionary<String, String>);
begin

end;

function TGBClientCoreRequestAuthAWS.URLEncode(const Str: string; const EncodeChars: array of Char): string;

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
  if Str <> '' then
  begin
    Buff := TEncoding.UTF8.GetBytes(Str);
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
          for J := 0 to Length(EncodeChars) - 1 do
            if Char(Buff[I]) = EncodeChars[J] then
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

function TGBClientCoreRequestAuthAWS.URLEncodeValue(const Value: String): string;
begin
  Result := URLEncode(Value, ['=', ':', '/', '+', '(', ')', '/', '!', '"', '$', '@', '&', ',',
                              '''', '?', ';']);
end;

end.
