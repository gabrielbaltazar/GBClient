object Form1: TForm1
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Form1'
  ClientHeight = 114
  ClientWidth = 685
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 24
    Width = 45
    Height = 13
    Caption = 'Base URL'
  end
  object edtBaseURL: TEdit
    Left = 24
    Top = 39
    Width = 633
    Height = 21
    TabOrder = 0
    Text = 
      'https://chart.apis.google.com/chart?chs=400x400&cht=qr&chld=M&ch' +
      'l=www.google.com'
  end
  object btnSend: TButton
    Left = 24
    Top = 66
    Width = 75
    Height = 25
    Caption = 'Send'
    TabOrder = 1
    OnClick = btnSendClick
  end
end
