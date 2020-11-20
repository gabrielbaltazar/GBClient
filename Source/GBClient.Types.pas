unit GBClient.Types;

interface

type
  TGBMethodType = (gmtGET, gmtPOST, gmtPUT, gmtDELETE, gmtPATCH);

  TGBContentType = (ctApplicationJson, ctApplicationXml);

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
