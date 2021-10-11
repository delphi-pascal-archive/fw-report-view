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
    Text := 'Первая колонка';
    Width := 250;
  end;
  with FWReportView1.Columns.Add do
  begin
    Text := 'Вторая колонка';
    Width := 230;
  end;
  with FWReportView1.Items.Add do
  begin
    Caption := 'Можно отображать чекбокс';
    ShowCheckBox := True;
    Checked := True;
    Subitems.Add('... но щелкать его низя');
  end;
  with FWReportView1.Items.Add do
  begin
    Caption := 'Можно отображать прогресс бар';
    ShowProgress := True;
    ProgressCount := 75;
    ProgressColumn := 1;
  end;
  with FWReportView1.Items.Add do
  begin
    Subitems.Add('... причем в любой колонке');
    ShowProgress := True;
    ProgressCount := 25;
    ProgressColumn := 0;
  end;
  with FWReportView1.Items.Add do
  begin
    Caption := 'Можно отображать разделитель';
    ItemStyle := isSeparator;
  end;
  with FWReportView1.Items.Add do
  begin
    Caption := '... а также длинный текст не разбитый границами колонок';
    ItemStyle := isBlank;
  end;
  with FWReportView1.Items.Add do
  begin
    Caption := 'ну и естественно...';
    Subitems.Add('... все это только в режиме Read Only :)');
  end;
  FWReportView1.Items.Add.ItemStyle := isBlank;
  FWReportView1.Items.Add.ItemStyle := isBlank;
  with FWReportView1.Items.Add do
  begin
    Caption := 'А сейчас добавим несколько прогресс баров с чекбоксами...';
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
