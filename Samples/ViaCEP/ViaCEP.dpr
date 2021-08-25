program ViaCEP;

uses
  Vcl.Forms,
  FMain in 'FMain.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  ReportMemoryLeaksOnShutdown := True;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
