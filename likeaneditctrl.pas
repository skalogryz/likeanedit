unit likeaneditctrl;

{$mode delphi}{$H+}

interface

uses
  LCLType,
  Classes, SysUtils, Controls, StdCtrls, Graphics;

type
  // I will pretend I'm an edit. But I'm not, I only host one (or two... or more)

  { TCustomLikeAnEdit }

  TCustomLikeAnEdit = class(TCustomControl)
  private
    fLeftPad: Integer;
    FOnChange: TNotifyEvent;
    FPasswordChar: Char;
    fRightPad: Integer;
    function GetAlignment: TAlignment;
    function GetAutoSelect: Boolean;
    function GetAutoSelected: Boolean;
    function GetCanUndo: Boolean;
    function GetCharCase: TEditCharCase;
    function GetEchoMode: TEchoMode;
    function GetHideSelection: Boolean;
    function GetMaxLength: Integer;
    function GetModified: Boolean;
    procedure SetAlignment(AValue: TAlignment);
    procedure SetAutoSelect(AValue: Boolean);
    procedure SetAutoSelected(AValue: Boolean);
    procedure SetCharCase(AValue: TEditCharCase);
    procedure SetHideSelection(AValue: Boolean);
    procedure SetLeftPad(AValue: Integer);
    procedure SetMaxLength(AValue: Integer);
    procedure SetModified(AValue: Boolean);
    procedure SetPasswordChar(AValue: Char);
    procedure SetRightPad(AValue: Integer);
  protected
    fEdit : TCustomEdit;
    procedure EditChange(Sender: TObject);
    function EditAlloc: TCustomEdit; virtual;
    procedure EditPrepare; virtual;
    procedure EditUpdateBounds; virtual;
    procedure EditExit(Sender: TObject);
    procedure EditEnter(Sender: TObject);
  protected
    procedure Change; virtual;
    function GetCaretPos: TPoint; virtual;
    function GetNumbersOnly: Boolean; virtual;
    function GetReadOnly: Boolean; virtual;
    function GetSelLength: integer; virtual;
    function GetSelStart: integer; virtual;
    function GetSelText: string; virtual;
    function GetTextHint: TTranslateString; virtual;
    procedure SetCaretPos(const Value: TPoint); virtual;
    procedure SetEchoMode(Val: TEchoMode); virtual;
    procedure SetNumbersOnly(Value: Boolean); virtual;
    procedure SetReadOnly(Value: Boolean); virtual;
    procedure SetSelLength(Val: integer); virtual;
    procedure SetSelStart(Val: integer); virtual;
    procedure SetSelText(const Val: string); virtual;
    procedure SetTextHint(const AValue: TTranslateString); virtual;

    procedure SetBounds(ALeft, ATop, AWidth, AHeight: integer); override;

    property AutoSelect: Boolean read GetAutoSelect write SetAutoSelect;
    property AutoSelected: Boolean read GetAutoSelected write SetAutoSelected;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Clear;
    procedure SelectAll;
    procedure ClearSelection; virtual;
    procedure CopyToClipboard; virtual;
    procedure CutToClipboard; virtual;
    procedure PasteFromClipboard; virtual;
    procedure Undo; virtual;

    function GetTextBuf(Buffer: PChar; BufSize: Integer): Integer; override;
    function GetTextLen: Integer; override;
    procedure SetTextBuf(Buffer: PChar); override;

    procedure GetPreferredSize(var PreferredWidth, PreferredHeight: integer;
                               Raw: boolean = false;
                               WithThemeSpace: boolean = true); override;
  public
    property Alignment: TAlignment read GetAlignment write SetAlignment;
    property AutoSize default True;
    property BorderStyle default bsSingle;
    property CanUndo: Boolean read GetCanUndo;
    property CaretPos: TPoint read GetCaretPos write SetCaretPos;
    property CharCase: TEditCharCase read GetCharCase write SetCharCase;
    property EchoMode: TEchoMode read GetEchoMode write SetEchoMode;
    property HideSelection: Boolean read GetHideSelection write SetHideSelection;
    property MaxLength: Integer read GetMaxLength write SetMaxLength;
    property Modified: Boolean read GetModified write SetModified;
    property NumbersOnly: Boolean read GetNumbersOnly write SetNumbersOnly;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property PasswordChar: Char read FPasswordChar write SetPasswordChar;
    property PopupMenu;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property SelLength: integer read GetSelLength write SetSelLength;
    property SelStart: integer read GetSelStart write SetSelStart;
    property SelText: String read GetSelText write SetSelText;
    property TextHint: TTranslateString read GetTextHint write SetTextHint;
    property Text;
    property LeftPadding: Integer read fLeftPad write SetLeftPad;
    property RightPadding: Integer read fRightPad write SetRightPad;
  end;

  TLikeAnEdit = class(TCustomLikeAnEdit)
  public
    property AutoSelected;
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize default true;
    property AutoSelect;
    property BidiMode;
    property BorderSpacing;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property EchoMode;
    property Enabled;
    property Font;
    property HideSelection;
    property MaxLength;
    property NumbersOnly;
    property ParentBidiMode;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditingDone;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property ParentColor;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabStop;
    property TabOrder;
    property Text;
    property TextHint;
    property Visible;
    property LeftPadding: Integer read fLeftPad write SetLeftPad;
    property RightPadding: Integer read fRightPad write SetRightPad;
  end;

function IsEditOrLikeEdit(c: TControl): Boolean; inline;

implementation

function IsEditOrLikeEdit(c: TControl): Boolean; inline;
begin
  Result:=(c is TCustomEdit) or (c is TCustomLikeAnEdit);
end;

type
  TProtectedEdit = class(TCustomEdit);

{ TCustomLikeAnEdit }

function TCustomLikeAnEdit.GetAlignment: TAlignment;
begin
  Result:=fEdit.Alignment;
end;

function TCustomLikeAnEdit.GetAutoSelect: Boolean;
begin
  Result:=TProtectedEdit(fEdit).Autoselect;
end;

function TCustomLikeAnEdit.GetAutoSelected: Boolean;
begin
  Result:=TProtectedEdit(fEdit).Autoselected;
end;

function TCustomLikeAnEdit.GetCanUndo: Boolean;
begin
  Result:=fEdit.CanUndo;
end;

function TCustomLikeAnEdit.GetCharCase: TEditCharCase;
begin
  Result:=fEdit.CharCase;
end;

function TCustomLikeAnEdit.GetEchoMode: TEchoMode;
begin
  Result:=fEdit.EchoMode;
end;

function TCustomLikeAnEdit.GetHideSelection: Boolean;
begin
  Result:=fEdit.HideSelection;
end;

function TCustomLikeAnEdit.GetMaxLength: Integer;
begin
  Result:=fEdit.MaxLength;
end;

function TCustomLikeAnEdit.GetModified: Boolean;
begin
  Result:=fEdit.Modified;
end;

procedure TCustomLikeAnEdit.SetAlignment(AValue: TAlignment);
begin
  fEdit.Alignment:=AValue;
end;

procedure TCustomLikeAnEdit.SetAutoSelect(AValue: Boolean);
begin
  TProtectedEdit(fEdit).AutoSelect := AValue;
end;

procedure TCustomLikeAnEdit.SetAutoSelected(AValue: Boolean);
begin
  TProtectedEdit(fEdit).AutoSelected := AValue;
end;

procedure TCustomLikeAnEdit.SetCharCase(AValue: TEditCharCase);
begin
  fEdit.CharCase:=AValue;
end;

procedure TCustomLikeAnEdit.SetHideSelection(AValue: Boolean);
begin
  FEdit.HideSelection:=AValue;
end;

procedure TCustomLikeAnEdit.SetLeftPad(AValue: Integer);
begin
  AValue:=abs(AValue);
  if fLeftPad=AValue then Exit;
  fLeftPad:=AValue;
  EditUpdateBounds;
end;

procedure TCustomLikeAnEdit.SetMaxLength(AValue: Integer);
begin
  FEDit.MaxLength:=AValue;
end;

procedure TCustomLikeAnEdit.SetModified(AValue: Boolean);
begin
  FEdit.Modified:=AValue;
end;

procedure TCustomLikeAnEdit.SetPasswordChar(AValue: Char);
begin
  if FPasswordChar=AValue then Exit;
  FPasswordChar:=AValue;
end;

procedure TCustomLikeAnEdit.SetRightPad(AValue: Integer);
begin
  AValue:=abs(AValue);
  if fRightPad=AValue then Exit;
  fRightPad:=AValue;
  EditUpdateBounds;
end;

procedure TCustomLikeAnEdit.EditChange(Sender: TObject);
begin
  Change;
end;

function TCustomLikeAnEdit.EditAlloc: TCustomEdit;
begin
  Result:=TEdit.Create(Self);
end;

procedure TCustomLikeAnEdit.EditPrepare;
begin
  fEdit.Parent:=self;
  fEdit.Visible:=true;
  fEdit.OnChange:=EditChange;
  fEdit.OnEnter:=EditEnter;
  fEdit.OnExit:=EditExit;
  fEdit.AutoSize:=false;
  fEdit.OnExit:=EditExit;
  fEdit.OnEnter:=EditEnter;
end;

procedure TCustomLikeAnEdit.EditUpdateBounds;
begin
  fEdit.BoundsRect:=Bounds(LeftPadding,0, Self.Width - LeftPadding-RightPadding, Self.Height);
end;

procedure TCustomLikeAnEdit.EditExit(Sender: TObject);
begin
  DoExit;
end;

procedure TCustomLikeAnEdit.EditEnter(Sender: TObject);
begin
  DoEnter;
end;

constructor TCustomLikeAnEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fEdit:=EditAlloc;
  EditPrepare;
  EditUpdateBounds;
end;

procedure TCustomLikeAnEdit.Change;
begin
  Changed;
  if Assigned(FOnChange) then FOnChange(Self);
  //todo:
  //if Assigned(FOnChangeHandler) then
  //  FOnChangeHandler.CallNotifyEvents(Self);
end;

function TCustomLikeAnEdit.GetCaretPos: TPoint;
begin
  Result:=fEdit.CaretPos;
end;

function TCustomLikeAnEdit.GetNumbersOnly: Boolean;
begin
  Result:=fEdit.NumbersOnly;
end;

function TCustomLikeAnEdit.GetReadOnly: Boolean;
begin
  Result:=fEdit.ReadOnly;
end;

function TCustomLikeAnEdit.GetSelLength: integer;
begin
  Result:=fEdit.SelLength;
end;

function TCustomLikeAnEdit.GetSelStart: integer;
begin
  Result:=fEdit.SelStart;
end;

function TCustomLikeAnEdit.GetSelText: string;
begin
  Result:=fEdit.SelText;
end;

function TCustomLikeAnEdit.GetTextHint: TTranslateString;
begin
  Result:=fEdit.TextHint;
end;

procedure TCustomLikeAnEdit.SetCaretPos(const Value: TPoint);
begin
  fEdit.CaretPos:=Value;
end;

procedure TCustomLikeAnEdit.SetEchoMode(Val: TEchoMode);
begin
  fEdit.EchoMode:=Val;
end;

procedure TCustomLikeAnEdit.SetNumbersOnly(Value: Boolean);
begin
  fEdit.NumbersOnly:=Value;
end;

procedure TCustomLikeAnEdit.SetReadOnly(Value: Boolean);
begin
  fEdit.ReadOnly:=Value;
end;

procedure TCustomLikeAnEdit.SetSelLength(Val: integer);
begin
  fEdit.SelLength:=Val;
end;

procedure TCustomLikeAnEdit.SetSelStart(Val: integer);
begin
  fEdit.SelStart:=Val;
end;

procedure TCustomLikeAnEdit.SetSelText(const Val: string);
begin
  fEdit.SelText:=Val;
end;

procedure TCustomLikeAnEdit.SetTextHint(const AValue: TTranslateString);
begin
  fEdit.TextHint:=AValue;
end;

procedure TCustomLikeAnEdit.SetBounds(ALeft, ATop, AWidth, AHeight: integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  EditUpdateBounds;
end;

procedure TCustomLikeAnEdit.Clear;
begin
  fEdit.Clear;
end;

procedure TCustomLikeAnEdit.SelectAll;
begin
  fEdit.SelectAll;
end;

procedure TCustomLikeAnEdit.ClearSelection;
begin
  fEdit.ClearSelection;
end;

procedure TCustomLikeAnEdit.CopyToClipboard;
begin
  fEdit.CopyToClipboard;
end;

procedure TCustomLikeAnEdit.CutToClipboard;
begin
  fEdit.CutToClipboard;
end;

procedure TCustomLikeAnEdit.PasteFromClipboard;
begin
  fEdit.PasteFromClipboard;
end;

procedure TCustomLikeAnEdit.Undo;
begin
  fEdit.Undo;
end;

function TCustomLikeAnEdit.GetTextBuf(Buffer: PChar; BufSize: Integer): Integer;
begin
  Result:=fEdit.GetTextBuf(Buffer, BufSize);
end;

function TCustomLikeAnEdit.GetTextLen: Integer;
begin
  Result:=fEdit.GetTextLen;
end;

procedure TCustomLikeAnEdit.SetTextBuf(Buffer: PChar);
begin
  fEdit.SetTextBuf(Buffer);
end;

procedure TCustomLikeAnEdit.GetPreferredSize(var PreferredWidth,
  PreferredHeight: integer; Raw: boolean; WithThemeSpace: boolean);
begin
  fEdit.GetPreferredSize(PreferredWidth, PreferredHeight, Raw, WithThemeSpace);
  PreferredWidth:=PreferredWidth+LeftPadding+RightPadding;
end;

end.

