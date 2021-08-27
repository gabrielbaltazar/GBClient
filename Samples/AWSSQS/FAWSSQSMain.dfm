object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 393
  ClientWidth = 739
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 40
    Width = 37
    Height = 13
    Caption = 'AWS ID'
  end
  object lbl1: TLabel
    Left = 271
    Top = 40
    Width = 78
    Height = 13
    Caption = 'AWS Secret Key'
  end
  object lbl2: TLabel
    Left = 518
    Top = 40
    Width = 59
    Height = 13
    Caption = 'AWS Region'
  end
  object edtAWSID: TEdit
    Left = 24
    Top = 57
    Width = 241
    Height = 21
    TabOrder = 0
  end
  object edtSecretKey: TEdit
    Left = 271
    Top = 57
    Width = 241
    Height = 21
    TabOrder = 1
  end
  object edtAWSRegion: TEdit
    Left = 518
    Top = 57
    Width = 211
    Height = 21
    TabOrder = 2
    Text = 'us-east-1'
  end
end
