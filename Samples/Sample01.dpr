program Sample01;

uses
  Vcl.Forms,
  FMain in 'FMain.pas' {Form1},
  GBClient.Helpers in '..\Source\GBClient.Helpers.pas',
  GBClient.Interfaces in '..\Source\GBClient.Interfaces.pas',
  GBClient.RestClient.Request.Body in '..\Source\GBClient.RestClient.Request.Body.pas',
  GBClient.RestClient.Request.Header in '..\Source\GBClient.RestClient.Request.Header.pas',
  GBClient.RestClient.Request.ParamPath in '..\Source\GBClient.RestClient.Request.ParamPath.pas',
  GBClient.RestClient.Request in '..\Source\GBClient.RestClient.Request.pas',
  GBClient.RestClient.Request.Query in '..\Source\GBClient.RestClient.Request.Query.pas',
  GBClient.RestClient.Response in '..\Source\GBClient.RestClient.Response.pas',
  GBClient.Exceptions in '..\Source\GBClient.Exceptions.pas',
  GBClient.RestClient.Request.Auth.Basic in '..\Source\GBClient.RestClient.Request.Auth.Basic.pas',
  GBClient.RestClient.Request.Auth.Bearer in '..\Source\GBClient.RestClient.Request.Auth.Bearer.pas',
  GBClient.RestClient.Request.Auth in '..\Source\GBClient.RestClient.Request.Auth.pas',
  GBClient.Types in '..\Source\GBClient.Types.pas',
  GBClient.NetHTTPClient.Request in '..\Source\GBClient.NetHTTPClient.Request.pas',
  GBClient.Base.Request.ParamPath in '..\Source\GBClient.Base.Request.ParamPath.pas',
  GBClient.IdHTTP.Request in '..\Source\GBClient.IdHTTP.Request.pas',
  GBClient.IdHTTP.Response in '..\Source\GBClient.IdHTTP.Response.pas',
  GBClient.IdHTTP.Exceptions in '..\Source\GBClient.IdHTTP.Exceptions.pas',
  GBClient.Settings.Default in '..\Source\GBClient.Settings.Default.pas';

{$R *.res}

begin
  Application.Initialize;
  ReportMemoryLeaksOnShutdown := True;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
