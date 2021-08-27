program ViaCEP;

uses
  Vcl.Forms,
  FMain in 'FMain.pas' {Form1},
  GBClient.Interfaces in '..\..\Source\GBClient.Interfaces.pas',
  GBClient.Core.Exceptions in '..\..\Source\GBClient.Core.Exceptions.pas',
  GBClient.Core.Helpers in '..\..\Source\GBClient.Core.Helpers.pas',
  GBClient.Core.Request.Auth in '..\..\Source\GBClient.Core.Request.Auth.pas',
  GBClient.Core.Request.Param in '..\..\Source\GBClient.Core.Request.Param.pas',
  GBClient.Core.Request in '..\..\Source\GBClient.Core.Request.pas',
  GBClient.Core.Settings in '..\..\Source\GBClient.Core.Settings.pas',
  GBClient.Core.Types in '..\..\Source\GBClient.Core.Types.pas',
  GBClient.IdHTTP.Auth in '..\..\Source\GBClient.IdHTTP.Auth.pas',
  GBClient.IdHTTP.Exceptions in '..\..\Source\GBClient.IdHTTP.Exceptions.pas',
  GBClient.IdHTTP in '..\..\Source\GBClient.IdHTTP.pas',
  GBClient.NetHTTPClient.Auth in '..\..\Source\GBClient.NetHTTPClient.Auth.pas',
  GBClient.NetHTTPClient.Exceptions in '..\..\Source\GBClient.NetHTTPClient.Exceptions.pas',
  GBClient.NetHTTPClient in '..\..\Source\GBClient.NetHTTPClient.pas',
  GBClient.RestClient.Auth in '..\..\Source\GBClient.RestClient.Auth.pas',
  GBClient.RestClient.Exceptions in '..\..\Source\GBClient.RestClient.Exceptions.pas',
  GBClient.RestClient in '..\..\Source\GBClient.RestClient.pas',
  GBClient.Core.Request.Auth.AWS in '..\..\Source\GBClient.Core.Request.Auth.AWS.pas';

{$R *.res}

begin
  Application.Initialize;
  ReportMemoryLeaksOnShutdown := True;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
