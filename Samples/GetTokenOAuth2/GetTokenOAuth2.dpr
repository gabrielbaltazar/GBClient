program GetTokenOAuth2;

uses
  Vcl.Forms,
  FRequestToken in 'FRequestToken.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  ReportMemoryLeaksOnShutdown := True;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
