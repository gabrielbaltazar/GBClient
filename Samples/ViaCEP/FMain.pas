unit FMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  GBClient.Interfaces;

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
    FClient: IGBClientRequest;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  GBClient.RestClient.Request;

procedure TForm1.btnConsultaClick(Sender: TObject);
begin
  FClient
    .GET
//    .BaseURL('https://webhook.site/3ac39100-30f8-4a74-bd83-5261db165529')
    .BaseURL('http://127.0.0.1:9000/ping')
    .Params
      .HeaderAddOrSet('h1', 'açãoÇã')
      .HeaderAddOrSet('h2', 'çã aa')
      .QueryAddOrSet('cep', 'a1 as')
      .QueryAddOrSet('cep2', 'a2 as')
    .&End
    .Authorization
      .Bearer
        .Token('1234')
      .&End
    .Send;

  mmoReposta.Lines.Text := FClient.Response.GetText;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;
  FClient := TGBClientRestClientRequest.New;
end;

end.
