object Form2: TForm2
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Form2'
  ClientHeight = 309
  ClientWidth = 552
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
    Top = 24
    Width = 19
    Height = 13
    Caption = 'URL'
  end
  object Label2: TLabel
    Left = 24
    Top = 72
    Width = 39
    Height = 13
    Caption = 'client_id'
  end
  object Label3: TLabel
    Left = 374
    Top = 72
    Width = 51
    Height = 13
    Caption = 'GrantType'
  end
  object Label4: TLabel
    Left = 199
    Top = 72
    Width = 61
    Height = 13
    Caption = 'client_secret'
  end
  object edtBaseURL: TEdit
    Left = 24
    Top = 39
    Width = 505
    Height = 21
    TabOrder = 0
  end
  object edtClientId: TEdit
    Left = 24
    Top = 87
    Width = 169
    Height = 21
    TabOrder = 1
  end
  object edtGrantType: TEdit
    Left = 374
    Top = 87
    Width = 155
    Height = 21
    TabOrder = 2
    Text = 'client_credentials'
  end
  object edtClientSecret: TEdit
    Left = 199
    Top = 87
    Width = 169
    Height = 21
    TabOrder = 3
  end
  object btnSend: TButton
    Left = 24
    Top = 114
    Width = 81
    Height = 25
    Caption = 'btnSend'
    TabOrder = 4
    OnClick = btnSendClick
  end
  object mmoResponse: TMemo
    Left = 24
    Top = 164
    Width = 505
    Height = 137
    Lines.Strings = (
      'mmoResponse')
    TabOrder = 5
  end
end
