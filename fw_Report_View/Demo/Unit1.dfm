object Form1: TForm1
  Left = 231
  Top = 124
  Width = 506
  Height = 449
  Caption = 'fw Report View'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 16
  object FWReportView1: TFWReportView
    Left = 0
    Top = 0
    Width = 498
    Height = 421
    Align = alClient
    Columns = <>
    Items = <>
    ParentColor = True
    TabOrder = 0
    TabStop = False
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 16
    Top = 24
  end
end
