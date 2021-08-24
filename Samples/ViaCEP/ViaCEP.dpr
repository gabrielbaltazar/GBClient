program ViaCEP;

uses
  Vcl.Forms,
  FMain in 'FMain.pas' {Form1},
  GBClient.RestClient.Request in '..\..\Source\GBClient.RestClient.Request.pas',
  GBClient.Request.Base in '..\..\Source\GBClient.Request.Base.pas',
  GBClient.Interfaces in '..\..\Source\GBClient.Interfaces.pas',
  GBClient.Request.Base.Param in '..\..\Source\GBClient.Request.Base.Param.pas',
  GBClient.Settings.Default in '..\..\Source\GBClient.Settings.Default.pas',
  GBClient.Helpers in '..\..\Source\GBClient.Helpers.pas',
  GBClient.Types in '..\..\Source\GBClient.Types.pas',
  GBClient.Request.Base.Auth in '..\..\Source\GBClient.Request.Base.Auth.pas',
  GBClient.RestClient.Auth in '..\..\Source\GBClient.RestClient.Auth.pas',
  GBClient.Exceptions in '..\..\Source\GBClient.Exceptions.pas',
  GBClient.RestClient.Exceptions in '..\..\Source\GBClient.RestClient.Exceptions.pas';

{$R *.res}

begin
  Application.Initialize;
  ReportMemoryLeaksOnShutdown := True;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
