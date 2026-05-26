unit GBClient.Synapse.Util;

interface

{$IFDEF WEAKPACKAGEUNIT}
  {$WEAKPACKAGEUNIT ON}
{$ENDIF}


uses
  System.SysUtils;

type
  TCompressType = ( ctUnknown, ctZLib, ctGZip, ctZipFile );

function FastStringReplace(const S, OldPattern, NewPattern: string; Flags: TReplaceFlags): string;

function ParseText( const Texto : AnsiString; const Decode : Boolean = True;
                    const IsUTF8: Boolean = True ) : String;

function DecodeToString(const ABinaryString: AnsiString; const StrIsUTF8: Boolean): String;
function UTF8ToNativeString(const AUTF8String: AnsiString): String;
function AnsiToNativeString(const AAnsiString: AnsiString): String;

implementation

function ParseText( const Texto : AnsiString; const Decode : Boolean = True;
                    const IsUTF8: Boolean = True ) : String;
var
  AStr: String;

  function InternalStringReplace(const S, OldPatern, NewPattern: String ): String;
  begin
    if pos(OldPatern, S) > 0 then
      Result := FastStringReplace(S, OldPatern, String(NewPattern), [rfReplaceAll])
    else
      Result := S;
  end;
begin
  if Decode then
  begin
    Astr := DecodeToString( Texto, IsUTF8 ) ;

    Astr := InternalStringReplace(AStr, '&amp;'   , '&');
    AStr := InternalStringReplace(AStr, '&lt;'    , '<');
    AStr := InternalStringReplace(AStr, '&gt;'    , '>');
    AStr := InternalStringReplace(AStr, '&quot;'  , '"');
    AStr := InternalStringReplace(AStr, '&#39;'   , #39);
    AStr := InternalStringReplace(AStr, '&aacute;', 'á');
    AStr := InternalStringReplace(AStr, '&Aacute;', 'Á');
    AStr := InternalStringReplace(AStr, '&acirc;' , 'â');
    AStr := InternalStringReplace(AStr, '&Acirc;' , 'Â');
    AStr := InternalStringReplace(AStr, '&atilde;', 'ã');
    AStr := InternalStringReplace(AStr, '&Atilde;', 'Ã');
    AStr := InternalStringReplace(AStr, '&agrave;', 'à');
    AStr := InternalStringReplace(AStr, '&Agrave;', 'À');
    AStr := InternalStringReplace(AStr, '&eacute;', 'é');
    AStr := InternalStringReplace(AStr, '&Eacute;', 'É');
    AStr := InternalStringReplace(AStr, '&ecirc;' , 'ê');
    AStr := InternalStringReplace(AStr, '&Ecirc;' , 'Ê');
    AStr := InternalStringReplace(AStr, '&iacute;', 'í');
    AStr := InternalStringReplace(AStr, '&Iacute;', 'Í');
    AStr := InternalStringReplace(AStr, '&oacute;', 'ó');
    AStr := InternalStringReplace(AStr, '&Oacute;', 'Ó');
    AStr := InternalStringReplace(AStr, '&otilde;', 'õ');
    AStr := InternalStringReplace(AStr, '&Otilde;', 'Õ');
    AStr := InternalStringReplace(AStr, '&ocirc;' , 'ô');
    AStr := InternalStringReplace(AStr, '&Ocirc;' , 'Ô');
    AStr := InternalStringReplace(AStr, '&uacute;', 'ú');
    AStr := InternalStringReplace(AStr, '&Uacute;', 'Ú');
    AStr := InternalStringReplace(AStr, '&uuml;'  , 'ü');
    AStr := InternalStringReplace(AStr, '&Uuml;'  , 'Ü');
    AStr := InternalStringReplace(AStr, '&ccedil;', 'ç');
    AStr := InternalStringReplace(AStr, '&Ccedil;', 'Ç');
    AStr := InternalStringReplace(AStr, '&apos;'  , '''');
  end
  else
  begin
    AStr := string(Texto);
    AStr := StringReplace(AStr, '&', '&amp;' , [rfReplaceAll]);
    AStr := StringReplace(AStr, '<', '&lt;'  , [rfReplaceAll]);
    AStr := StringReplace(AStr, '>', '&gt;'  , [rfReplaceAll]);
    AStr := StringReplace(AStr, '"', '&quot;', [rfReplaceAll]);
    AStr := StringReplace(AStr, #39, '&#39;' , [rfReplaceAll]);
    AStr := StringReplace(AStr, '''','&apos;', [rfReplaceAll]);
  end;

  Result := AStr;
end;

function FastStringReplace(const S, OldPattern, NewPattern: string; Flags: TReplaceFlags): string;
const
  Size_of_Char = SizeOf(Char);
var
  Str: string;
  xOldPattern: string;
  i, j: Integer;
  SourceIdx,
  DestIdx,
  p: Integer;
  FindCount: Integer;
  PosArray: array of Integer;
  LenOP, LenNP, ArrLen: Integer;
begin
  if S = '' then begin
    Result := '';
    Exit;
  end;

  if rfIgnoreCase in Flags then begin
    xOldPattern := AnsiUpperCase(OldPattern);
    {$IFDEF FPC_OR_LEGACY24}
    if (CompareStr(xOldPattern,AnsiLowerCase(OldPattern)) = 0) then begin   // if AnsiUpperCase(OldPattern) = AnsiLowerCase(OldPattern) we don't need to use UpperCase() in the whole string
    {$ELSE}
    if SameStr(xOldPattern, AnsiLowerCase(OldPattern)) then begin   // if AnsiUpperCase(OldPattern) = AnsiLowerCase(OldPattern) we don't need to use UpperCase() in the whole string
    {$ENDIF}
      Str := S;
    end else begin
      Str := AnsiUpperCase(S);
    end;
  end else begin
    xOldPattern := OldPattern;
    Str := S;
  end;

  LenOP := Length(OldPattern);
  LenNP := Length(NewPattern);

  i := 1;
  FindCount := 0;
  ArrLen := 0;
  repeat
    //In x64 we call FastPos() directly. In XE3 or below, we call PosEx(). If IDE >= XE3, then call Pos()
    i := {$IFDEF USE_FASTPOS}FastPos{$ELSE}
         {$IFDEF FPC_OR_LEGACY24}PosEx{$ELSE}
         Pos{$ENDIF}
         {$ENDIF}(xOldPattern, Str, i);
    if i = 0 then begin
      Break;
    end;
    Inc(FindCount);
    if ArrLen < FindCount then begin
      if ArrLen = 0 then begin
        ArrLen := 32;
      end else begin
        ArrLen := ArrLen * 2;
      end;
      SetLength(PosArray, ArrLen);   // call SetLength less frequently makes a huge difference when replacing multiple occurrences
    end;
    PosArray[FindCount - 1] := i;
    Inc(i, LenOP);
  until Flags * [rfReplaceAll] = [];

  if FindCount > 0 then begin
    if LenNP = LenOP then begin   // special case where Length(OldPattern) = Length(NewPattern)
      Result := S;                // in this case, we can optimize it even further
      for j := 0 to FindCount - 1 do begin
        DestIdx := PosArray[j];
        if LenNP > 0 then begin
          Move(NewPattern[1], Result[DestIdx], LenNP * Size_of_Char);
        end;
      end;
    end else begin
      SetLength(Result, Length(S) + (LenNP - LenOP) * FindCount);
      SourceIdx := 1;
      DestIdx := 1;
      for j := 0 to FindCount - 1 do begin
        p := PosArray[j] - SourceIdx;
        if p > 0 then begin
          Move(S[SourceIdx], Result[DestIdx], p * Size_of_Char);
          Inc(SourceIdx, p);
          Inc(DestIdx, p);
        end;
        if LenNP > 0 then begin
          Move(NewPattern[1], Result[DestIdx], LenNP * Size_of_Char);
        end;
        Inc(SourceIdx, LenOP);
        Inc(DestIdx, LenNP);
      end;
      p := Length(S) + 1 - SourceIdx;
      if p > 0 then begin
        Move(S[SourceIdx], Result[DestIdx], p * Size_of_Char);
      end;
    end;
  end else begin
    Result := S;
  end;
end;

function DecodeToString(const ABinaryString: AnsiString; const StrIsUTF8: Boolean
  ): String;
begin
  if StrIsUTF8 then
    Result := UTF8ToNativeString(ABinaryString)
  else
    Result := AnsiToNativeString(ABinaryString);
end;

function UTF8ToNativeString(const AUTF8String: AnsiString): String;
begin
  {$IfDef USE_UTF8}
   Result := AUTF8String;  // FPC, DELPHI LINUX e NEXTGEN usam UTF8 de forma nativa
  {$Else}
   {$IfDef UNICODE}
     Result := UTF8ToString(AUTF8String);
   {$Else}
    Result := Utf8ToAnsi(AUTF8String) ;
   {$EndIf}

   if Result = '' then
     Result := String(AUTF8String);
  {$EndIf}
end;

function AnsiToNativeString(const AAnsiString: AnsiString): String;
begin
  {$IfDef USE_UTF8}
    Result := ACBrAnsiToUTF8(AAnsiString);
  {$Else}
    Result := String(AAnsiString);
  {$EndIf}
end;

end.
