object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 290
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 635
    Height = 73
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object lbl1: TLabel
      Left = 32
      Top = 16
      Width = 19
      Height = 13
      Caption = 'CEP'
    end
    object edtCEP: TEdit
      Left = 32
      Top = 35
      Width = 209
      Height = 21
      TabOrder = 0
      Text = '24240670'
    end
    object btnConsulta: TButton
      Left = 256
      Top = 32
      Width = 75
      Height = 25
      Caption = 'Consulta'
      TabOrder = 1
      OnClick = btnConsultaClick
    end
  end
  object mmoReposta: TMemo
    Left = 0
    Top = 73
    Width = 635
    Height = 217
    Align = alClient
    Lines.Strings = (
      '')
    TabOrder = 1
  end
end
