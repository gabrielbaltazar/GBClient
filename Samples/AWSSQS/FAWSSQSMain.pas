unit FAWSSQSMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  GBClient.Interfaces;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    edtAWSID: TEdit;
    edtSecretKey: TEdit;
    lbl1: TLabel;
    edtAWSRegion: TEdit;
    lbl2: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

end.
