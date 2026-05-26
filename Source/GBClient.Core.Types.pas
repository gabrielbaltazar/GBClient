unit GBClient.Core.Types;

interface

{$IFDEF WEAKPACKAGEUNIT}
  {$WEAKPACKAGEUNIT ON}
{$ENDIF}

type
  TGBMethodType = (gmtGET, gmtPOST, gmtPUT, gmtDELETE, gmtPATCH);

  TGBContentType = (ctApplicationJson, ctApplicationXml, ctApplication_x_www_form_urlencoded);

  TGBContentTypeHelper = record helper for TGBContentType
  public
    function Value: string;
  end;

  TGBMethodTypeHelper = record helper for TGBMethodType
  public
    function Value: string;
  end;

implementation

{ TGBContentTypeHelper }

function TGBContentTypeHelper.Value: string;
begin
  case Self of
    ctApplicationJson:
      Result := 'application/json';
    ctApplicationXml:
      Result := 'application/xml';
    ctApplication_x_www_form_urlencoded:
      Result := 'application/x-www-form-urlencoded';
  end;
end;

{ TGBMethodTypeHelper }

function TGBMethodTypeHelper.Value: string;
begin
  Result := 'GET';
  case Self of
    gmtGET:
      Result := 'GET';
    gmtPOST:
      Result := 'POST';
    gmtPUT:
      Result := 'PUT';
    gmtDELETE:
      Result := 'DELETE';
    gmtPATCH:
      Result := 'PATCH';
  end;
end;

end.
