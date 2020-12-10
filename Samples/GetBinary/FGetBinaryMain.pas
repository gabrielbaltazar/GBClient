unit FGetBinaryMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  GBClient.Interfaces;

type
  TForm1 = class(TForm)
    edtURLDownloadFile: TEdit;
    Label1: TLabel;
    btnDownloadFile: TButton;
    Label2: TLabel;
    edtURLSendFile: TEdit;
    btnSendFile: TButton;
    edtFileName: TEdit;
    FileName: TLabel;
    btnSendFileStream: TButton;
    procedure btnDownloadFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnSendFileClick(Sender: TObject);
    procedure btnSendFileStreamClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btnDownloadFileClick(Sender: TObject);
var
  Request: IGBClientRequest;
begin
  Request := NewClientRequest;
  Request
    .GET
    .BaseURL(edtURLDownloadFile.Text)
    .Send
    .GetStream
    .SaveToFile(edtFileName.Text);
end;

procedure TForm1.btnSendFileClick(Sender: TObject);
var
  Request: IGBClientRequest;
begin
  Request := NewClientRequest;
  Request
    .POST
    .BaseURL(edtURLSendFile.Text)
    .Body
      .Binary(edtFileName.Text)
    .&End
    .Send;
end;

procedure TForm1.btnSendFileStreamClick(Sender: TObject);
var
  Request: IGBClientRequest;
  stream : TFileStream;
begin
  stream  := TFileStream.Create(edtFileName.Text, fmOpenRead);
  Request := NewClientRequest;
  Request
    .POST
    .BaseURL(edtURLSendFile.Text)
    .Body
      .Binary(stream, True)
    .&End
    .Send;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := true;

  edtFileName.Text := ExtractFilePath(GetModuleName(HInstance)) + 'image.png';
end;

end.
