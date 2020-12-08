unit FGetBinaryMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  GBClient.Interfaces;

type
  TForm1 = class(TForm)
    edtBaseURL: TEdit;
    Label1: TLabel;
    btnSend: TButton;
    procedure btnSendClick(Sender: TObject);
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

procedure TForm1.btnSendClick(Sender: TObject);
var
  Request: IGBClientRequest;
begin
  Request := NewClientRequest;
  Request
    .GET
    .BaseURL(edtBaseURL.Text)
    .Send
    .GetStream
    .SaveToFile('imagem.png');
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := true;
end;

end.
