unit FMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  System.Net.URLClient, System.Net.HttpClient,
  System.Net.HttpClientComponent;

type
  TForm1 = class(TForm)
    pnlTop: TPanel;
    edtCEP: TEdit;
    lbl1: TLabel;
    btnConsulta: TButton;
    mmoReposta: TMemo;
    procedure btnConsultaClick(Sender: TObject);
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
  client: IGBClientRequest;
begin
  client := NewClientRequest;
  client
    .GET
    .BaseURL('https://viacep.com.br/ws/{cep}/json')
    .ParamPath
      .AddOrSet('cep', edtCEP.Text)
    .&End
    .Execute;

  mmoReposta.Lines.Text := client.Response.GetText;
end;

end.
