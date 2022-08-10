unit GBClient.Core.Types;

interface

{$IFDEF WEAKPACKAGEUNIT}
  {$WEAKPACKAGEUNIT ON}
{$ENDIF}

type
  TGBMethodType = (gmtGET, gmtPOST, gmtPUT, gmtDELETE, gmtPATCH);

  TGBContentType = (ctApplicationJson, ctApplicationXml, ctApplication_x_www_form_urlencoded,
    ctMultipart_form_data);

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

function TGBContentTypeHelper.value: string;
begin
  case Self of
    ctApplicationJson: Result := 'application/json';
    ctApplicationXml: Result := 'application/xml';
    ctApplication_x_www_form_urlencoded: Result := 'application/x-www-form-urlencoded';
    ctMultipart_form_data: Result := 'multipart/form-data';
  end;
end;

{ TGBMethodTypeHelper }

function TGBMethodTypeHelper.value: string;
begin
  Result := 'GET';
  case Self of
    gmtGET: Result := 'GET';
    gmtPOST: Result := 'POST';
    gmtPUT: Result := 'PUT';
    gmtDELETE: Result := 'DELETE';
    gmtPATCH: Result := 'PATCH';
  end;
end;

end.
