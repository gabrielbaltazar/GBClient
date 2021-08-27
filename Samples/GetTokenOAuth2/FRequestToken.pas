unit FRequestToken;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, REST.Types,
  REST.Client, Data.Bind.Components, Data.Bind.ObjectScope;

type
  TForm2 = class(TForm)
    edtBaseURL: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    edtClientId: TEdit;
    Label3: TLabel;
    edtGrantType: TEdit;
    edtClientSecret: TEdit;
    Label4: TLabel;
    btnSend: TButton;
    mmoResponse: TMemo;
    procedure btnSendClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

uses
  GBClient.Interfaces;

procedure TForm2.btnSendClick(Sender: TObject);
var
  Request: IGBClientRequest;
begin
  Request := NewClientRequest;
  Request
    .POST
    .BaseURL(edtBaseURL.Text)
    .Params
      .BodyAddOrSet('client_id', edtClientId.Text)
      .BodyAddOrSet('client_secret', edtClientSecret.Text)
      .BodyAddOrSet('grant_type', edtGrantType.Text)
    .&End
    .Send;

  mmoResponse.Lines.Text := Request.Response.GetJSONObject.ToString;
end;

end.
