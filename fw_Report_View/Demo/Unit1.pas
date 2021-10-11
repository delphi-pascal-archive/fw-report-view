unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, FWReportView, ExtCtrls;

type
  TForm1 = class(TForm)
    FWReportView1: TFWReportView;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses Math;

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  with FWReportView1.Columns.Add do
  begin
    Text := '������ �������';
    Width := 250;
  end;
  with FWReportView1.Columns.Add do
  begin
    Text := '������ �������';
    Width := 230;
  end;
  with FWReportView1.Items.Add do
  begin
    Caption := '����� ���������� �������';
    ShowCheckBox := True;
    Checked := True;
    Subitems.Add('... �� ������� ��� ����');
  end;
  with FWReportView1.Items.Add do
  begin
    Caption := '����� ���������� �������� ���';
    ShowProgress := True;
    ProgressCount := 75;
    ProgressColumn := 1;
  end;
  with FWReportView1.Items.Add do
  begin
    Subitems.Add('... ������ � ����� �������');
    ShowProgress := True;
    ProgressCount := 25;
    ProgressColumn := 0;
  end;
  with FWReportView1.Items.Add do
  begin
    Caption := '����� ���������� �����������';
    ItemStyle := isSeparator;
  end;
  with FWReportView1.Items.Add do
  begin
    Caption := '... � ����� ������� ����� �� �������� ��������� �������';
    ItemStyle := isBlank;
  end;
  with FWReportView1.Items.Add do
  begin
    Caption := '�� � �����������...';
    Subitems.Add('... ��� ��� ������ � ������ Read Only :)');
  end;
  FWReportView1.Items.Add.ItemStyle := isBlank;
  FWReportView1.Items.Add.ItemStyle := isBlank;
  with FWReportView1.Items.Add do
  begin
    Caption := '� ������ ������� ��������� �������� ����� � ����������...';
    ItemStyle := isSeparator;
  end;
  for I := 0 to 10 do
    with FWReportView1.Items.Add do
    begin
      ShowProgress := True;
      ShowCheckBox := True;
      ProgressColumn := (I mod 2);
    end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  I, A: Integer;
begin
  Randomize;
  for I := 0 to 10 do
  begin
    A := Random(100);
    FWReportView1.Items[I + 9].ProgressCount := A;
    FWReportView1.Items[I + 9].Checked := Boolean(A mod 2);
  end;
end;

end.
