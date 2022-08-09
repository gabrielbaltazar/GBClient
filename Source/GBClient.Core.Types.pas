unit GBClient.Core.Types;

interface

{$IFDEF WEAKPACKAGEUNIT}
	{$WEAKPACKAGEUNIT ON}
{$ENDIF}

type
  TGBMethodType = (gmtGET, gmtPOST, gmtPUT, gmtDELETE, gmtPATCH);

  TGBContentType = (ctApplicationJson,
                    ctApplicationXml,
                    ctApplication_x_www_form_urlencoded,
                    ctMultipart_form_data);

  TGBContentTypeHelper = record helper for TGBContentType
    public
      function value: string;
  end;

  TGBMethodTypeHelper = record helper for TGBMethodType
  public
    function value: string;
  end;

implementation

{ TGBContentTypeHelper }

function TGBContentTypeHelper.value: string;
begin
  case Self of
    ctApplicationJson : result := 'application/json';
    ctApplicationXml  : result := 'application/xml';
    ctApplication_x_www_form_urlencoded : result := 'application/x-www-form-urlencoded';
    ctMultipart_form_data: result := 'multipart/form-data';
  end;
end;

{ TGBMethodTypeHelper }

function TGBMethodTypeHelper.value: string;
begin
  result := 'GET';
  case Self of
    gmtGET    : result := 'GET';
    gmtPOST   : result := 'POST';
    gmtPUT    : result := 'PUT';
    gmtDELETE : result := 'DELETE';
    gmtPATCH  : result := 'PATCH';
  end;
end;

end.
