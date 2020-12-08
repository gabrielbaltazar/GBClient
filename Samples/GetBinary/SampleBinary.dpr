program SampleBinary;

uses
  Vcl.Forms,
  FGetBinaryMain in 'FGetBinaryMain.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
