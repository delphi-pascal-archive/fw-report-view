////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Unit Name : FWReportView
//  * Purpose   : Класс для отображения статистики выполнения.
//  * Author    : Александр (Rouse_) Багель
//  * Copyright : © Fangorn Wizards Lab 1999 - 2007.
//  * Version   : 1.01
//  * Home Page : http://rouse.drkb.ru
//  ****************************************************************************
//

unit FWReportView;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Types,
  Classes,
  Controls,
  ComCtrls,
  CommCtrl,
  StdCtrls,
  Forms,
  Graphics,
  Themes;

const
  WM_ITEMCHANGE = WM_USER;
  WM_NEED_VSCROLL = WM_USER + 1;

type
  TFWItemStyle = (isData, isSeparator, isBlank);

  TFWReportView = class;

  TFWHeaderControl = class(THeaderControl)
  protected
    BlockAlign: Boolean;
    TotalWidth: Integer;
    procedure RequestAlign; override;
  end;            

  TFWReportViewItem = class(TCollectionItem)
  private
    FCaption: String;
    FSubItems: TStringList;
    FShowCheckBox: Boolean;
    FChecked: Boolean;
    FShowProgress: Boolean;
    FProgressColumn: Byte;
    FProgressCount: Byte;
    FObject: Pointer;
    FItemStyle: TFWItemStyle;
    FItemHeight, FItemTop: Integer;
    FBlockUpdate: Boolean;
    procedure SetItemHeight(const Value: Integer);
    procedure SetItemStyle(const Value: TFWItemStyle);
    procedure SetByte(const Index: Integer; const Value: Byte);
    procedure SetBoolean(const Index: Integer; const Value: Boolean);
    procedure SetSubItems(const Value: TStringList);
    procedure SetCaption(const Value: String);
  protected
    procedure DoChange(IsResized: Boolean = False); virtual;
    procedure SubItemsChange(Sender: TObject); virtual;
    procedure DrawProgressBar(ACanvas: TCanvas; lpRect: TRect); virtual;
    procedure DoDrawItem(ACanvas: TCanvas; lpRect: TRect); virtual;
    property ItemTop: Integer read FItemTop write FItemTop;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    property Data: Pointer read FObject write FObject;
  published
    property Caption: String read FCaption write SetCaption;
    property Checked: Boolean index 0 read FChecked write SetBoolean default False;
    property ItemHeight: Integer read FItemHeight write SetItemHeight default 19;
    property ItemStyle: TFWItemStyle read FItemStyle write SetItemStyle default isData;
    property ProgressColumn: Byte index 0 read FProgressColumn write SetByte  default 0;
    property ProgressCount: Byte index 1 read FProgressCount write SetByte default 0;
    property ShowCheckBox: Boolean index 1 read FShowCheckBox write SetBoolean default False;
    property ShowProgress: Boolean index 2 read FShowProgress write SetBoolean default False;
    property Subitems: TStringList read FSubItems write SetSubItems;
  end;

  TFWReportViewItems = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TFWReportViewItem;
    procedure SetItem(Index: Integer; const Value: TFWReportViewItem);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    function Add: TFWReportViewItem;
    property Item[Index: Integer]: TFWReportViewItem read GetItem write SetItem; default;
  end;

  TFWCustomReportView = class(TCustomControl)
  private
    FBorderStyle: TBorderStyle;
    FHeader: TFWHeaderControl;
    FScrollBars: array [TScrollBarKind] of TScrollBar;
    FItems: TFWReportViewItems;
    FBackGround: TBitmap;
    TotalHeight, TotalWidth: Integer;
    ScrollBarWidth, ScrollBarHeight, HeaderHeight: Integer;
    procedure SetBorderStyle(const Value: TBorderStyle);
    procedure SetItems(const Value: TFWReportViewItems);
    function GetScrollBarVisible(Index: TScrollBarKind): Boolean;
    procedure SetScrollBarVisible(Index: TScrollBarKind; const Value: Boolean);
    function GetSections: THeaderSections;
    procedure SetSections(const Value: THeaderSections);
  protected
    property ScrollBarVisible[Index: TScrollBarKind]: Boolean
      read GetScrollBarVisible write SetScrollBarVisible;
    function GetReportViewItemClass: TCollectionItemClass; virtual;
    procedure DoPaintItems(FromIndex: Integer; RepaintOnlyThisItem: Boolean);
    procedure WMItemChange(var Msg: TMessage); message WM_ITEMCHANGE;
    procedure WMNeedVScroll(var Msg: TMessage); message WM_NEED_VSCROLL;
    procedure WMMouseWheel(var Msg: TMessage); message WM_MOUSEWHEEL;
    procedure CheckHorzScrollBar;
    procedure CheckVertScrollBar;
    procedure RealignSubControls;
    function IntToPercent(const Base, Value: Integer): Integer;
    function PercentToInt(const Value, Percent: Integer): Integer;
    procedure ScrollBarsOnEnter(Sender: TObject);
  protected
    procedure OnHeaderResize(HeaderControl: THeaderControl;
      Section: THeaderSection);
    procedure OnHBarChange(Sender: TObject);
    procedure OnVBarChange(Sender: TObject);
    procedure Loaded; override;
    procedure Resize; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Scroll(Position: TAlign);
  published
    property Align;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property Columns: THeaderSections read GetSections write SetSections;
    property Items: TFWReportViewItems read FItems write SetItems;
  end;

  TFWReportView = class(TFWCustomReportView)
  published
    property Action;
    property Align;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind default bkNone;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle;
    property BorderWidth;
    property Color;
    property Columns;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property Items;
    property ParentBiDiMode;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Visible;
  end;

procedure Register;

implementation

const
  SmallScrollPercent = 10;

procedure Register;
begin
  RegisterComponents('Fangorn Wizards Lab', [TFWReportView]);
end;

function Light(Col: TColor; Percentage: Byte): TColor;
var
  R, G, B: Byte;
begin
  Col := ColorToRGB(Col);
  R := GetRValue(Col); G := GetGValue(Col); B := GetBValue(Col);
  R := Round(R * Percentage / 100) + Round(255 - Percentage / 100 * 255);
  G := Round(G * Percentage / 100) + Round(255 - Percentage / 100 * 255);
  B := Round(B * Percentage / 100) + Round(255 - Percentage / 100 * 255);
  Light := RGB(R, G, B);
end;

function GetRealScrollBarPosition(Bar: TScrollBar): Integer;
var
  CurrentPosition, ValidMax: Integer;
begin
  CurrentPosition := Bar.Position;
  if CurrentPosition = 0 then
  begin
    Result := 0;
    Exit;
  end;
  if CurrentPosition + Bar.PageSize >= 100 then
  begin
    Result := 100;
    Exit;
  end;
  ValidMax := 100 - Bar.PageSize;
  if CurrentPosition > ValidMax then
    CurrentPosition := ValidMax;
  Result := Trunc(CurrentPosition / (ValidMax / 100));
end;      

{ TFWHeaderControl }

procedure TFWHeaderControl.RequestAlign;
begin
  Width := TotalWidth; 
end;

{ TFWReportViewItem }

procedure TFWReportViewItem.BeginUpdate;
begin
  FBlockUpdate := True;
end;

constructor TFWReportViewItem.Create(Collection: TCollection);
begin
  inherited;
  FSubItems := TStringList.Create;
  FSubItems.OnChange := SubItemsChange;
  FShowCheckBox := False;
  FChecked := False;
  FShowProgress := False;
  FProgressColumn := 0;
  FProgressCount := 0;
  FObject := nil;
  if Index = 0 then
    FItemTop := 0
  else
    FItemTop := TFWReportViewItem(Collection.Items[Index - 1]).FItemTop +
      TFWReportViewItem(Collection.Items[Index - 1]).FItemHeight;
  FItemHeight := 19;
//  if FItemHeight + FItemTop > TWinControl(Collection.Owner).Height then
    SendMessage(TWinControl(Collection.Owner).Handle, WM_NEED_VSCROLL, 0, 0);
end;

destructor TFWReportViewItem.Destroy;
begin
  FSubItems.Free;
  inherited;
end;

procedure TFWReportViewItem.DoChange(IsResized: Boolean);
begin
  if not FBlockUpdate then Changed(IsResized);
end;

procedure TFWReportViewItem.DoDrawItem(ACanvas: TCanvas; lpRect: TRect);
const
  ThemedChecked: array[Boolean] of
    TThemedButton = (tbCheckBoxUncheckedNormal, tbCheckBoxCheckedNormal);
  NoThemedChecked: array[Boolean] of Integer = (DFCS_CHECKED, 0);
var
  Header: TCustomHeaderControl;
  I, AlignFlag: Integer;
  R: TRect;
  TextData: String;
  Details: TThemedElementDetails;
begin
  Header := TCustomHeaderControl(TFWReportView(Collection.Owner).FHeader);

//  if (Index mod 2) <> 0 then
//    ACanvas.Brush.Color := Light(ColorToRGB(clHighlight), 80)
//  else
//    ACanvas.Brush.Color := ColorToRGB(clWindow);

  ACanvas.FillRect(lpRect);
  ACanvas.Font.Style := [];
  ACanvas.Font.Color := clWindowText;

  if ItemStyle = isBlank then
  begin
    lpRect.Top := lpRect.Top + (FItemHeight - ACanvas.TextHeight('Wg')) div 2;
    Inc(lpRect.Left, 4);
    Dec(lpRect.Right, 4);
    DrawText(ACanvas.Handle, PChar(Caption), Length(Caption),
      lpRect, DT_END_ELLIPSIS or DT_WORD_ELLIPSIS or DT_NOPREFIX);
    Exit;
  end;

  if ItemStyle = isSeparator then
  begin
    lpRect.Top := lpRect.Top + (FItemHeight - ACanvas.TextHeight('Wg')) div 2;
    Inc(lpRect.Left, 20);
    Dec(lpRect.Right, 4);
    ACanvas.Font.Style := [fsBold];
    DrawText(ACanvas.Handle, PChar(Caption), Length(Caption),
      lpRect, DT_END_ELLIPSIS or DT_WORD_ELLIPSIS or DT_NOPREFIX);
    ACanvas.Font.Style := [];
    ACanvas.Pen.Color := Light(ColorToRGB(clHighlight), 40);
    ACanvas.Pen.Width := 2;
    ACanvas.Pen.Style := psDash;
    ACanvas.MoveTo(3, lpRect.Bottom - 2);
    ACanvas.LineTo(lpRect.Right - 3, lpRect.Bottom - 2);
    ACanvas.Pen.Width := 1;
    Exit;
  end;

  if ShowCheckBox then
  begin
    R := Rect(lpRect.Left + 2, lpRect.Top + ((FItemHeight - 14) div 2) + 1,
      lpRect.Left + 16, 0);
    R.Bottom := R.Top + 14;
    if ThemeServices.ThemesEnabled then
    begin
      Details := ThemeServices.GetElementDetails(ThemedChecked[Checked]);
      ThemeServices.DrawElement(ACanvas.Handle, Details, R);
    end
    else
      DrawFrameControl(ACanvas.Handle, R, DFC_BUTTON,
        DFCS_BUTTONCHECK or NoThemedChecked[Checked]);
  end;

  for I := 0 to Header.Sections.Count - 1 do
  begin
    R := Rect(lpRect.Left + Header.Sections[I].Left, lpRect.Top,
      lpRect.Left + Header.Sections[I].Right, lpRect.Bottom);
    TextData := '';
    if I = 0 then
    begin
      if ShowCheckBox then Inc(R.Left, 18);
      TextData := Caption;
    end
    else
      if FSubItems.Count >= I then
        TextData := FSubItems.Strings[I - 1];

    AlignFlag := DT_LEFT;
    case Header.Sections[I].Alignment of
      taRightJustify: AlignFlag := DT_RIGHT;
      taCenter: AlignFlag := DT_CENTER;
    end;
    
    R.Top := R.Top + (FItemHeight - ACanvas.TextHeight('Wg')) div 2;
    Inc(R.Left, 4);
    Dec(R.Right, 4);
    if R.Left >= R.Right then Continue;

    if ShowProgress then
      if ProgressColumn = I then
      begin
        R.Top := lpRect.Top + 2;
        R.Bottom := lpRect.Bottom - 2;
        DrawProgressBar(ACanvas, R);
        Continue;
      end;

    DrawText(ACanvas.Handle, PChar(TextData), Length(TextData),
      R, DT_END_ELLIPSIS or DT_WORD_ELLIPSIS or DT_NOPREFIX or AlignFlag);
  end;
end;

procedure TFWReportViewItem.DrawProgressBar(ACanvas: TCanvas; lpRect: TRect);

  procedure GetChunkRect;
  begin
    Inc(lpRect.Left, 4);
    Inc(lpRect.Top, 2);
    Dec(lpRect.Right, 2);
    Dec(lpRect.Bottom, 2);
    lpRect.Right := lpRect.Left +
      Trunc(((lpRect.Right - lpRect.Left) / 100) * ProgressCount);
  end;

var
  Details: TThemedElementDetails;
  ChunkPos: Integer;
  ChunkRect: TRect;
  CurrentColor: TColor;
begin
  if ThemeServices.ThemesEnabled then
  begin
    Details := ThemeServices.GetElementDetails(tpBar);
    ThemeServices.DrawElement(ACanvas.Handle, Details, lpRect);
    GetChunkRect;
    Details := ThemeServices.GetElementDetails(tpChunk);
    ThemeServices.DrawElement(ACanvas.Handle, Details, lpRect);
  end
  else
  begin
    DrawFrameControl(ACanvas.Handle, lpRect, DFC_BUTTON,
      DFCS_BUTTONPUSH or DFCS_PUSHED);
    GetChunkRect;
    Dec(lpRect.Right, 2);
    Dec(lpRect.Left, 1);
    ChunkPos := 0;
    CurrentColor := ACanvas.Brush.Color;
    ACanvas.Brush.Color := ColorToRGB(clHighlight);
    while ChunkPos < lpRect.Right - lpRect.Left do
    begin
      ChunkRect := lpRect;
      Inc(ChunkRect.Left, ChunkPos);
      ChunkRect.Right := ChunkRect.Left + 8;
      if ChunkRect.Right > lpRect.Right then
        ChunkRect.Right := lpRect.Right;
      ACanvas.FillRect(ChunkRect);
      Inc(ChunkPos, 10);
    end;
    ACanvas.Brush.Color := CurrentColor;
  end;
end;

procedure TFWReportViewItem.EndUpdate;
begin
  FBlockUpdate := False;
  DoChange;
end;

procedure TFWReportViewItem.SetBoolean(const Index: Integer;
  const Value: Boolean);
begin
  case Index of
    0:
    begin
      if FChecked = Value then Exit;
      FChecked := Value;
      if not FShowCheckBox then Exit;
    end;
    1:
    begin
      if FShowCheckBox = Value then Exit;
      FShowCheckBox := Value;
    end;
    2:
    begin
      if FShowProgress = Value then Exit;
      FShowProgress := Value;
    end;
  end;
  DoChange;
end;

procedure TFWReportViewItem.SetByte(const Index: Integer; const Value: Byte);
begin
  case Index of
    0:
    begin
      if FProgressColumn = Value then Exit;
      FProgressColumn := Value;
    end;
    1:
    begin
      if FProgressCount = Value then Exit;
      FProgressCount := Value;
    end;
  end;
  if not ShowProgress then Exit;
  DoChange;
end;

procedure TFWReportViewItem.SetCaption(const Value: String);
begin
  if FCaption = Value then Exit;
  FCaption := Value;
  DoChange;
end;

procedure TFWReportViewItem.SetItemHeight(const Value: Integer);
begin
  if FItemHeight = Value then Exit;
  FItemHeight := Value;
  DoChange(True);
end;

procedure TFWReportViewItem.SetItemStyle(const Value: TFWItemStyle);
begin
  if FItemStyle = Value then Exit;
  FItemStyle := Value;
  DoChange;
end;

procedure TFWReportViewItem.SetSubItems(const Value: TStringList);
begin
  FSubItems.Assign(Value);
  DoChange;
end;

procedure TFWReportViewItem.SubItemsChange(Sender: TObject);
begin
  DoChange;
end;

{ TFWReportViewItems }

function TFWReportViewItems.Add: TFWReportViewItem;
begin
  Result := TFWReportViewItem(inherited Add);
end;

function TFWReportViewItems.GetItem(Index: Integer): TFWReportViewItem;
begin
  Result := TFWReportViewItem(inherited GetItem(Index));
end;

procedure TFWReportViewItems.SetItem(Index: Integer;
  const Value: TFWReportViewItem);
begin
  inherited SetItem(Index, Value);
end;

procedure TFWReportViewItems.Update(Item: TCollectionItem);
begin
  if Item <> nil then
    PostMessage(TWinControl(Owner).Handle, WM_ITEMCHANGE, Item.Index, 1)
  else
    PostMessage(TWinControl(Owner).Handle, WM_ITEMCHANGE, 0, 0);
end;

{ TFWReportView }

procedure TFWCustomReportView.CheckHorzScrollBar;
var
  I: Integer;
  Percent: Integer;
  CliWidth: Integer;
  BorderCorrection: Integer;
begin

  // 26.02.2007 START
  if BorderStyle = bsSingle then
    BorderCorrection := 2
  else
    BorderCorrection := 0;
  Inc(BorderCorrection, BorderWidth * 2);
  // 26.02.2007 END
  
  TotalWidth := 0;
  for I := 0 to FHeader.Sections.Count - 1 do
    Inc(TotalWidth, FHeader.Sections[I].Width);
  CliWidth := Width - BorderCorrection;
  if FScrollBars[sbVertical] <> nil then
    Dec(CliWidth, ScrollBarWidth);
  if TotalWidth > CliWidth then
  begin
    SetScrollBarVisible(sbHorizontal, True);
    FScrollBars[sbHorizontal].Max := 100;
    Percent := IntToPercent(TotalWidth, CliWidth);
    if Percent >= 100 then Percent := 99;
    FScrollBars[sbHorizontal].PageSize := Percent;
    FScrollBars[sbHorizontal].LargeChange := Percent;
    FScrollBars[sbHorizontal].SmallChange := Percent div SmallScrollPercent;
  end
  else
    SetScrollBarVisible(sbHorizontal, False);
end;

procedure TFWCustomReportView.CheckVertScrollBar;
var
  Percent: Integer;
  CliHeight: Integer;
  BorderCorrection: Integer;
begin

  // 26.02.2007 START
  if BorderStyle = bsSingle then
    BorderCorrection := 2
  else
    BorderCorrection := 0;
  Inc(BorderCorrection, BorderWidth * 2);
  // 26.02.2007 END

  if FItems.Count = 0 then Exit;
  TotalHeight := FItems[FItems.Count - 1].ItemTop +
    FItems[FItems.Count - 1].ItemHeight;
  CliHeight := Height - FHeader.Height - BorderCorrection;
  if FScrollBars[sbHorizontal] <> nil then
    Dec(CliHeight, ScrollBarHeight);
  if TotalHeight > CliHeight then
  begin
    SetScrollBarVisible(sbVertical, True);
    FScrollBars[sbVertical].Max := 100;
    Percent := IntToPercent(TotalHeight, CliHeight);
    if Percent >= 100 then Percent := 99;
    FScrollBars[sbVertical].PageSize := Percent;
    FScrollBars[sbVertical].LargeChange := Percent;
    FScrollBars[sbVertical].SmallChange := Percent div SmallScrollPercent;
  end
  else
    SetScrollBarVisible(sbVertical, False);
end;

constructor TFWCustomReportView.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := [csCaptureMouse, csClickEvents, csOpaque];
  // 26.02.2007 START
  BorderStyle := bsSingle;
  BorderWidth := 0;
  // 26.02.2007 END
  Width := 120;
  Height := 120;
  FHeader := TFWHeaderControl.Create(Self);
  FHeader.Parent := Self;
  if ThemeServices.ThemesEnabled then
    HeaderHeight := 20
  else
    HeaderHeight := 17;
  FHeader.Height := HeaderHeight;
  FHeader.Top := 0;
  FHeader.Left := -100;
  TotalWidth := 0;
  FHeader.Width := TotalWidth + Width;
  FHeader.TotalWidth := TotalWidth + Width;
  FHeader.OnSectionResize := OnHeaderResize;
  FHeader.DoubleBuffered := True;

  FScrollBars[sbVertical] := nil;
  FScrollBars[sbHorizontal] := nil;
  FItems := TFWReportViewItems.Create(Self, GetReportViewItemClass);
  FBackGround := TBitmap.Create;
  FBackGround.Width := Width;
  FBackGround.Height := Height;
  FBackGround.PixelFormat := pf24bit;
  DoubleBuffered := True;

  ScrollBarWidth := GetSystemMetrics(SM_CXHSCROLL);
  ScrollBarHeight := GetSystemMetrics(SM_CXVSCROLL);
end;

procedure TFWCustomReportView.CreateParams(var Params: TCreateParams);
const
  BorderStyles: array[TBorderStyle] of DWORD = (0, WS_BORDER);
begin
  inherited CreateParams(Params);
  // 26.02.2007 START
  //with Params do
  //  Style := Style or WS_CLIPCHILDREN;
  if not (csDesigning in ComponentState) then
    CreateSubClass(Params, 'LISTBOX');
  with Params do
  begin
    Style := (Style or WS_CLIPCHILDREN) or BorderStyles[BorderStyle];
    if NewStyleControls and Ctl3D and (BorderStyle = bsSingle) then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;
  end;
  // 26.02.2007 END
end;

destructor TFWCustomReportView.Destroy;
begin
  FItems.Free;
  FHeader.Free;
  FBackGround.Free;
  ScrollBarVisible[sbVertical] := False;
  ScrollBarVisible[sbHorizontal] := False;
  inherited;
end;

procedure TFWCustomReportView.DoPaintItems(FromIndex: Integer;
  RepaintOnlyThisItem: Boolean);
var
  I, ItemSize, LeftDecrement, TopDecrement: Integer;
  lpRect: TRect;
  CliWidth, CliHeight: Integer;
  BorderCorrection: Integer;
begin

  // 26.02.2007 START
  if BorderStyle = bsSingle then
    BorderCorrection := 2
  else
    BorderCorrection := 0;
  Inc(BorderCorrection, BorderWidth * 2);
  // 26.02.2007 END

  CliWidth := Width - BorderCorrection;
  CliHeight := Height - FHeader.Height - BorderCorrection;
  if FScrollBars[sbHorizontal] <> nil then
  begin
    Dec(CliHeight, ScrollBarHeight);
    if FScrollBars[sbVertical] <> nil then
      Dec(CliWidth, ScrollBarWidth);
    LeftDecrement := PercentToInt(TotalWidth - CliWidth,
      GetRealScrollBarPosition(FScrollBars[sbHorizontal]));
  end
  else
    LeftDecrement := 0;

  if FScrollBars[sbVertical] <> nil then
    TopDecrement := PercentToInt(TotalHeight - CliHeight,
      GetRealScrollBarPosition(FScrollBars[sbVertical]))
  else
    TopDecrement := 0;

  FHeader.TotalWidth := LeftDecrement + Width;
  FHeader.Left := LeftDecrement * -1;


  FBackGround.Canvas.Brush.Color := ColorToRGB(clWindow);
  FBackGround.Canvas.Pen.Color := ColorToRGB(clWindowText);
  FBackGround.Canvas.Font.Style := [];
  FBackGround.Canvas.Font.Color := clWindowText;
  if RepaintOnlyThisItem then
  begin
    ItemSize := FItems.Item[FromIndex].ItemTop + FItems.Item[FromIndex].ItemHeight;
    Dec(ItemSize, TopDecrement);
    if (ItemSize > 0) and (ItemSize < Height) then
    begin
      lpRect := Rect(0, FItems.Item[FromIndex].ItemTop + FHeader.Height,
        Width, ItemSize + FHeader.Height);
      Dec(lpRect.Left, LeftDecrement);
      Dec(lpRect.Top, TopDecrement);
      FItems.Item[FromIndex].DoDrawItem(FBackGround.Canvas, lpRect);
    end;
  end
  else
  begin
    FBackGround.Canvas.FillRect(Rect(0, 0, Width, Height));
    for I := FromIndex to FItems.Count - 1 do
    begin
      ItemSize := FItems.Item[I].ItemTop + FItems.Item[I].ItemHeight;
      Dec(ItemSize, TopDecrement);
      if (ItemSize > 0) and (ItemSize < Height) then
      begin
        lpRect := Rect(0, FItems.Item[I].ItemTop + FHeader.Height,
          Width, ItemSize + FHeader.Height);
        Dec(lpRect.Left, LeftDecrement);
        Dec(lpRect.Top, TopDecrement);
        FItems.Item[I].DoDrawItem(FBackGround.Canvas, lpRect);
      end;
    end;
  end;
  if FScrollBars[sbHorizontal] <> nil then
    if FScrollBars[sbVertical] <> nil then
    begin
      FBackGround.Canvas.Brush.Color := ColorToRGB(clBtnFace);
      // 26.02.2007
      FBackGround.Canvas.FillRect(
        Rect(Width - ScrollBarWidth - BorderCorrection - 2,
        Height - ScrollBarHeight - BorderCorrection - 2, Width, Height));
//      DrawFrameControl(FBackGround.Canvas.Handle,
//        Rect(Width - ScrollBarWidth - BorderCorrection - 2,
//        Height - ScrollBarHeight - BorderCorrection - 2, Width, Height),
//        DFC_BUTTON, DFCS_BUTTONPUSH or DFCS_PUSHED);
      //FBackGround.Canvas.FillRect();
      FBackGround.Canvas.Brush.Color := ColorToRGB(clWindow);
    end;
  BitBlt(Canvas.Handle, 0, 0, Width, Height, FBackGround.Canvas.Handle, 0, 0, SRCCOPY);
end;

function TFWCustomReportView.GetReportViewItemClass: TCollectionItemClass;
begin
  Result := TFWReportViewItem;
end;

function TFWCustomReportView.GetScrollBarVisible(Index: TScrollBarKind): Boolean;
begin
  Result := FScrollBars[Index] <> nil;
end;

function TFWCustomReportView.GetSections: THeaderSections;
begin
  Result := FHeader.Sections;
end;

function TFWCustomReportView.IntToPercent(const Base, Value: Integer): Integer;
var
  AResult: Real;
begin
  AResult := Value / (Base / 100);
  Result := Trunc(AResult);
end;

procedure TFWCustomReportView.Loaded;
begin
  inherited;
  Resize;
  DoPaintItems(0, False);
  Invalidate;
end;

procedure TFWCustomReportView.OnHBarChange(Sender: TObject);
begin
  DoPaintItems(0, False);
end;

procedure TFWCustomReportView.OnHeaderResize(HeaderControl: THeaderControl;
  Section: THeaderSection);
begin
  if FHeader.BlockAlign then Exit;
  FHeader.BlockAlign := True;
  CheckHorzScrollBar;
  RealignSubControls;
  DoPaintItems(0, False);
  FHeader.BlockAlign := False;
end;

procedure TFWCustomReportView.OnVBarChange(Sender: TObject);
begin
  DoPaintItems(0, False);
end;

procedure TFWCustomReportView.Paint;
begin
  BitBlt(Canvas.Handle, 0, 0, Width, Height, FBackGround.Canvas.Handle, 0, 0, SRCCOPY);
end;

function TFWCustomReportView.PercentToInt(const Value, Percent: Integer): Integer;
begin
  Result := Trunc(Value / 100 * Percent);
end;

procedure TFWCustomReportView.RealignSubControls;
var
  CliWidth, CliHeight, BorderCorrection: Integer;
begin
  // 26.02.2007 START
  //CliWidth := Width
  if BorderStyle = bsSingle then
    BorderCorrection := 4
  else
    BorderCorrection := 0;
  Inc(BorderCorrection, BorderWidth * 2);
  CliWidth := Width - BorderCorrection;
  // 26.02.2007 END
  if FScrollBars[sbVertical] <> nil then
  begin
    FScrollBars[sbVertical].Top := 0;
    // 26.02.2007
    //FScrollBars[sbVertical].Left := Width - ScrollBarWidth;
    FScrollBars[sbVertical].Left := Width - ScrollBarWidth - BorderCorrection;
    Dec(CliWidth, ScrollBarWidth);
  end;

  // 26.02.2007
  //CliHeight := Height;
  CliHeight := Height - BorderCorrection;
  if FScrollBars[sbHorizontal] <> nil then
  begin
    Dec(CliHeight, ScrollBarHeight);
    FScrollBars[sbHorizontal].Left := 0;
    // 26.02.2007
    //FScrollBars[sbHorizontal].Top := Height - ScrollBarHeight;
    FScrollBars[sbHorizontal].Top := Height - ScrollBarHeight - BorderCorrection;
    FScrollBars[sbHorizontal].Width := CliWidth;
  end;

  if FScrollBars[sbVertical] <> nil then
    FScrollBars[sbVertical].Height := CliHeight;

  FBackGround.Width := Width;
  FBackGround.Height := Height;
  FHeader.TotalWidth := TotalWidth + CliWidth;
  FHeader.Width := TotalWidth + CliWidth;
end;

procedure TFWCustomReportView.Resize;
begin
  inherited Resize;
  RealignSubControls;
  CheckHorzScrollBar;
  CheckVertScrollBar;
  DoPaintItems(0, False);
end;

procedure TFWCustomReportView.Scroll(Position: TAlign);
begin
  if Position = alTop then
    if FScrollBars[sbVertical] <> nil then
    begin
      CheckVertScrollBar;
      FScrollBars[sbVertical].Position := 0;
      DoPaintItems(0, False);
    end;
  if Position = alBottom then
    if FScrollBars[sbVertical] <> nil then
    begin
      CheckVertScrollBar;
      FScrollBars[sbVertical].Position := 99;
      FScrollBars[sbVertical].Position := 100;
      DoPaintItems(0, False);
    end;
end;

procedure TFWCustomReportView.ScrollBarsOnEnter(Sender: TObject);
begin
  SetFocus;
end;

procedure TFWCustomReportView.SetBorderStyle(const Value: TBorderStyle);
begin
  if BorderStyle <> Value then
  begin
    FBorderStyle := Value;
    if HandleAllocated then
    begin
      RecreateWnd;
      Invalidate;
    end;
  end;
end;

procedure TFWCustomReportView.SetItems(const Value: TFWReportViewItems);
begin
  FItems.Assign(Value);
end;

procedure TFWCustomReportView.SetScrollBarVisible(Index: TScrollBarKind;
  const Value: Boolean);
begin
  if Value then
  begin
    if FScrollBars[Index] <> nil then Exit;
    FScrollBars[Index] := TScrollBar.Create(Self);
    FScrollBars[Index].Parent := Self;
    FScrollBars[Index].Kind := Index;
    if Index = sbHorizontal then
    begin
      FScrollBars[Index].OnChange := OnHBarChange;
      FScrollBars[Index].Height := ScrollBarHeight;
    end
    else
    begin
      FScrollBars[Index].OnChange := OnVBarChange;
      FScrollBars[Index].Width := ScrollBarWidth;
    end;
    FScrollBars[Index].SetSubComponent(True);
    FScrollBars[Index].DoubleBuffered := True;
    RealignSubControls;
    FScrollBars[Index].Visible := True;
    FScrollBars[Index].OnEnter := ScrollBarsOnEnter;
  end
  else
    if FScrollBars[Index] <> nil then
      FreeAndNil(FScrollBars[Index]);       
end;

procedure TFWCustomReportView.SetSections(const Value: THeaderSections);
begin
  if Value <> nil then
    FHeader.Sections.Assign(Value);
end;

procedure TFWCustomReportView.WMItemChange(var Msg: TMessage);
begin
  DoPaintItems(Msg.WParam, Boolean(Msg.LParam));
end;

procedure TFWCustomReportView.WMMouseWheel(var Msg: TMessage);
var
  NewPosition: Integer;
  SBKind: TScrollBarKind;
  WheelCount: Integer;
begin
  SystemParametersInfo(SPI_GETWHEELSCROLLLINES, 0, @WheelCount, 0);
  if WheelCount = -1 then
    WheelCount := SmallScrollPercent;
  with Msg do
  begin
    if WParamLo = MK_SHIFT then
      SBKind := sbHorizontal
    else
      SBKind := sbVertical;
    if Assigned(FScrollBars[SBKind]) then
    begin
      NewPosition := FScrollBars[SBKind].Position;
      if WParamHi < UD_MAXVAL then
        Dec(NewPosition, FScrollBars[SBKind].SmallChange * WheelCount)
      else
        Inc(NewPosition, FScrollBars[SBKind].SmallChange * WheelCount);
      FScrollBars[SBKind].Position := NewPosition;
    end;
  end;
end;

procedure TFWCustomReportView.WMNeedVScroll(var Msg: TMessage);
begin
  CheckVertScrollBar;
  DoPaintItems(0, False);
end;

end.