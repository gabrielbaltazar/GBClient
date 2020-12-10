object Form1: TForm1
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Form1'
  ClientHeight = 225
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
  object Label2: TLabel
    Left = 24
    Top = 120
    Width = 45
    Height = 13
    Caption = 'Base URL'
  end
  object FileName: TLabel
    Left = 215
    Top = 120
    Width = 46
    Height = 13
    Caption = 'File Name'
  end
  object edtURLDownloadFile: TEdit
    Left = 24
    Top = 39
    Width = 633
    Height = 21
    TabOrder = 0
    Text = 
      'https://chart.apis.google.com/chart?chs=400x400&cht=qr&chld=M&ch' +
      'l=www.google.com'
  end
  object btnDownloadFile: TButton
    Left = 24
    Top = 66
    Width = 105
    Height = 25
    Caption = 'Download File'
    TabOrder = 1
    OnClick = btnDownloadFileClick
  end
  object edtURLSendFile: TEdit
    Left = 24
    Top = 135
    Width = 185
    Height = 21
    TabOrder = 2
    Text = 'http://localhost:9001/stream'
  end
  object btnSendFile: TButton
    Left = 24
    Top = 162
    Width = 105
    Height = 25
    Caption = 'Send File'
    TabOrder = 3
    OnClick = btnSendFileClick
  end
  object edtFileName: TEdit
    Left = 215
    Top = 135
    Width = 442
    Height = 21
    TabOrder = 4
  end
  object btnSendFileStream: TButton
    Left = 135
    Top = 162
    Width = 105
    Height = 25
    Caption = 'Send File Stream'
    TabOrder = 5
    OnClick = btnSendFileStreamClick
  end
end
