unit FMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    pnlTop: TPanel;
    edtCEP: TEdit;
    lbl1: TLabel;
    btnConsulta: TButton;
    mmoReposta: TMemo;
    procedure btnConsultaClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  GBClient.Interfaces;

procedure TForm1.btnConsultaClick(Sender: TObject);
var
  LClient: IGBClientRequest;
begin
  LClient := NewClientRequest;
  LClient
    .GET
    .BaseURL('https://viacep.com.br/ws/{cep}/json/')
    .Params
      .PathAddOrSet('cep', edtCEP.Text)
    .&End
    .Send;

  mmoReposta.Lines.Text := LClient.Response.GetText;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;
end;

end.
