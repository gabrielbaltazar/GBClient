program AWSSQS;

uses
  Vcl.Forms,
  FAWSSQSMain in 'FAWSSQSMain.pas' {Form1},
  GBClient.Interfaces in '..\..\Source\GBClient.Interfaces.pas',
  GBClient.Request.Base in '..\..\Source\GBClient.Request.Base.pas',
  GBClient.Exceptions in '..\..\Source\GBClient.Exceptions.pas',
  GBClient.Types in '..\..\Source\GBClient.Types.pas',
  GBClient.Request.Param in '..\..\Source\GBClient.Request.Param.pas',
  GBClient.Settings.Default in '..\..\Source\GBClient.Settings.Default.pas',
  GBClient.Helpers in '..\..\Source\GBClient.Helpers.pas',
  GBClient.RestClient.Request in '..\..\Source\GBClient.RestClient.Request.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
