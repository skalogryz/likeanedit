unit likeaneditctrl;

{$mode delphi}{$H+}

interface

uses
  LCLType, WSLCLClasses, WSControls, WSStdCtrls,
  Classes, SysUtils, Controls, StdCtrls, Graphics
  {$ifdef mswindows}
  , Win32WSControls, Win32WSStdCtrls
  {$endif}
  ;

type

  { TWSPretendCustomEdit }

  TWSPretendCustomEdit = class(TWSCustomEdit)
    class function CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): TLCLIntfHandle; override;

    class function  GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;
    class function  GetTextLen(const AWinControl: TWinControl; var ALength: Integer): Boolean; override;
    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;

    class function GetCanUndo(const ACustomEdit: TCustomEdit): Boolean; override;
    class function GetCaretPos(const ACustomEdit: TCustomEdit): TPoint; override;
    class function GetSelStart(const ACustomEdit: TCustomEdit): integer; override;
    class function GetSelLength(const ACustomEdit: TCustomEdit): integer; override;

    class procedure SetAlignment(const ACustomEdit: TCustomEdit; const AAlignment: TAlignment); override;
    class procedure SetCaretPos(const ACustomEdit: TCustomEdit; const NewPos: TPoint); override;
    class procedure SetCharCase(const ACustomEdit: TCustomEdit; NewCase: TEditCharCase); override;
    class procedure SetEchoMode(const ACustomEdit: TCustomEdit; NewMode: TEchoMode); override;
    class procedure SetHideSelection(const ACustomEdit: TCustomEdit; NewHideSelection: Boolean); override;
    class procedure SetMaxLength(const ACustomEdit: TCustomEdit; NewLength: integer); override;
    class procedure SetNumbersOnly(const ACustomEdit: TCustomEdit; NewNumbersOnly: Boolean); override;
    class procedure SetPasswordChar(const ACustomEdit: TCustomEdit; NewChar: char); override;
    class procedure SetReadOnly(const ACustomEdit: TCustomEdit; NewReadOnly: boolean); override;
    class procedure SetSelStart(const ACustomEdit: TCustomEdit; NewStart: integer); override;
    class procedure SetSelLength(const ACustomEdit: TCustomEdit; NewLength: integer); override;
    class procedure SetSelText(const ACustomEdit: TCustomEdit; const NewSelText: string); override;
    class procedure SetTextHint(const ACustomEdit: TCustomEdit; const ATextHint: string); override;
    class function CreateEmulatedTextHintFont(const ACustomEdit: TCustomEdit): TFont; override;

    class procedure Cut(const ACustomEdit: TCustomEdit); override;
    class procedure Copy(const ACustomEdit: TCustomEdit); override;
    class procedure Paste(const ACustomEdit: TCustomEdit); override;
    class procedure Undo(const ACustomEdit: TCustomEdit); override;
  end;

  // I will pretend I'm an edit. But I'm not, I only host one (or two... or more)

  { TCustomLikeAnEdit }

  TCustomLikeAnEdit = class(TCustomEdit)
  private
    fLeftPad: Integer;
    fRightPad: Integer;
    procedure SetLeftPad(AValue: Integer);
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
    // this is LCL by-pass
    class procedure WSRegisterClass; override;
    function ChildClassAllowed(ChildClass: TClass): boolean; override;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure GetPreferredSize(var PreferredWidth, PreferredHeight: integer;
                               Raw: boolean = false;
                               WithThemeSpace: boolean = true); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: integer); override;
    procedure SetFocus; override;
  public
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

{ TWSPretendCustomEdit }

class function TWSPretendCustomEdit.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
begin
  {$ifdef mswindows}
  Result:=TWin32WSWinControl.CreateHandle(AWinControl, Aparams);
  {$endif}
end;

class function TWSPretendCustomEdit.GetText(const AWinControl: TWinControl;
  var AText: String): Boolean;
var
  c : TCustomLikeAnEdit absolute AWinControl;
begin
  if not (AWinControl is TCustomLikeAnEdit) then begin
    AText :='';
    Result := false;
    Exit;
  end;
  AText := c.fEdit.Text;
  Result := true;
end;

class function TWSPretendCustomEdit.GetTextLen(const AWinControl: TWinControl;
  var ALength: Integer): Boolean;
var
  c : TCustomLikeAnEdit absolute AWinControl;
begin
  if not (AWinControl is TCustomLikeAnEdit) then begin
    ALength := 0;
    Result := false;
    Exit;
  end;
  ALength := c.fEdit.GetTextLen;
  Result := true;
end;

class procedure TWSPretendCustomEdit.SetText(const AWinControl: TWinControl;
  const AText: String);
var
  c : TCustomLikeAnEdit absolute AWinControl;
begin
  if not (AWinControl is TCustomLikeAnEdit) then Exit;
  c.fEdit.Text := AText;
end;

class function TWSPretendCustomEdit.GetCanUndo(const ACustomEdit: TCustomEdit
  ): Boolean;
var
  c : TCustomLikeAnEdit absolute ACustomEdit;
begin
  if not (ACustomEdit is TCustomLikeAnEdit) then begin
    Result := false;
    Exit;
  end;
  Result := c.fEdit.CanUndo;
end;

class function TWSPretendCustomEdit.GetCaretPos(const ACustomEdit: TCustomEdit
  ): TPoint;
var
  c : TCustomLikeAnEdit absolute ACustomEdit;
begin
  if not (ACustomEdit is TCustomLikeAnEdit) then Exit;
  Result := c.fEdit.CaretPos;
end;

class function TWSPretendCustomEdit.GetSelStart(const ACustomEdit: TCustomEdit
  ): integer;
var
  c : TCustomLikeAnEdit absolute ACustomEdit;
begin
  if not (ACustomEdit is TCustomLikeAnEdit) then Exit;
  Result := c.fEdit.SelStart;
end;

class function TWSPretendCustomEdit.GetSelLength(const ACustomEdit: TCustomEdit
  ): integer;
var
  c : TCustomLikeAnEdit absolute ACustomEdit;
begin
  if not (ACustomEdit is TCustomLikeAnEdit) then Exit;
  Result := c.fEdit.SelLength;
end;

class procedure TWSPretendCustomEdit.SetAlignment(
  const ACustomEdit: TCustomEdit; const AAlignment: TAlignment);
var
  c : TCustomLikeAnEdit absolute ACustomEdit;
begin
  if not (ACustomEdit is TCustomLikeAnEdit) then Exit;
  c.fEdit.Alignment:=AAlignment;
end;

class procedure TWSPretendCustomEdit.SetCaretPos(
  const ACustomEdit: TCustomEdit; const NewPos: TPoint);
var
  c : TCustomLikeAnEdit absolute ACustomEdit;
begin
  if not (ACustomEdit is TCustomLikeAnEdit) then Exit;
  c.fEdit.CaretPos := NewPos;
end;

class procedure TWSPretendCustomEdit.SetCharCase(
  const ACustomEdit: TCustomEdit; NewCase: TEditCharCase);
var
  c : TCustomLikeAnEdit absolute ACustomEdit;
begin
  if not (ACustomEdit is TCustomLikeAnEdit) then Exit;
  c.fEdit.CharCase:=NewCase;
end;

class procedure TWSPretendCustomEdit.SetEchoMode(
  const ACustomEdit: TCustomEdit; NewMode: TEchoMode);
var
  c : TCustomLikeAnEdit absolute ACustomEdit;
begin
  if not (ACustomEdit is TCustomLikeAnEdit) then Exit;
  c.fEdit.EchoMode:=NewMode;
end;

class procedure TWSPretendCustomEdit.SetHideSelection(
  const ACustomEdit: TCustomEdit; NewHideSelection: Boolean);
var
  c : TCustomLikeAnEdit absolute ACustomEdit;
begin
  if not (ACustomEdit is TCustomLikeAnEdit) then Exit;
  c.fEdit.HideSelection:=NewHideSelection;
end;

class procedure TWSPretendCustomEdit.SetMaxLength(
  const ACustomEdit: TCustomEdit; NewLength: integer);
var
  c : TCustomLikeAnEdit absolute ACustomEdit;
begin
  if not (ACustomEdit is TCustomLikeAnEdit) then Exit;
  c.fEdit.MaxLength:=NewLength;
end;

class procedure TWSPretendCustomEdit.SetNumbersOnly(
  const ACustomEdit: TCustomEdit; NewNumbersOnly: Boolean);
var
  c : TCustomLikeAnEdit absolute ACustomEdit;
begin
  if not (ACustomEdit is TCustomLikeAnEdit) then Exit;
  c.fEdit.NumbersOnly:=NewNumbersOnly;
end;

class procedure TWSPretendCustomEdit.SetPasswordChar(
  const ACustomEdit: TCustomEdit; NewChar: char);
var
  c : TCustomLikeAnEdit absolute ACustomEdit;
begin
  if not (ACustomEdit is TCustomLikeAnEdit) then Exit;
  c.fEdit.PasswordChar:=NewChar;
end;

class procedure TWSPretendCustomEdit.SetReadOnly(
  const ACustomEdit: TCustomEdit; NewReadOnly: boolean);
var
  c : TCustomLikeAnEdit absolute ACustomEdit;
begin
  if not (ACustomEdit is TCustomLikeAnEdit) then Exit;
  c.fEdit.ReadOnly:=NewReadOnly;
end;

class procedure TWSPretendCustomEdit.SetSelStart(
  const ACustomEdit: TCustomEdit; NewStart: integer);
var
  c : TCustomLikeAnEdit absolute ACustomEdit;
begin
  if not (ACustomEdit is TCustomLikeAnEdit) then Exit;
  c.fEdit.SelStart:=NewStart;
end;

class procedure TWSPretendCustomEdit.SetSelLength(
  const ACustomEdit: TCustomEdit; NewLength: integer);
var
  c : TCustomLikeAnEdit absolute ACustomEdit;
begin
  if not (ACustomEdit is TCustomLikeAnEdit) then Exit;
  c.fEdit.SelLength:=NewLength;
end;

class procedure TWSPretendCustomEdit.SetSelText(const ACustomEdit: TCustomEdit;
  const NewSelText: string);
var
  c : TCustomLikeAnEdit absolute ACustomEdit;
begin
  if not (ACustomEdit is TCustomLikeAnEdit) then Exit;
  c.fEdit.SelText:=NewSelText;
end;

class procedure TWSPretendCustomEdit.SetTextHint(
  const ACustomEdit: TCustomEdit; const ATextHint: string);
var
  c : TCustomLikeAnEdit absolute ACustomEdit;
begin
  if not (ACustomEdit is TCustomLikeAnEdit) then Exit;
  c.fEdit.Hint:=ATextHint;
end;

class function TWSPretendCustomEdit.CreateEmulatedTextHintFont(
  const ACustomEdit: TCustomEdit): TFont;
var
  c : TCustomLikeAnEdit absolute ACustomEdit;
begin
  if not (ACustomEdit is TCustomLikeAnEdit) then Exit;
  Result := TProtectedEdit(c.fEdit).CreateEmulatedTextHintFont;
end;

class procedure TWSPretendCustomEdit.Cut(const ACustomEdit: TCustomEdit);
var
  c : TCustomLikeAnEdit absolute ACustomEdit;
begin
  if not (ACustomEdit is TCustomLikeAnEdit) then Exit;
  c.fEdit.CutToClipboard;
end;

class procedure TWSPretendCustomEdit.Copy(const ACustomEdit: TCustomEdit);
var
  c : TCustomLikeAnEdit absolute ACustomEdit;
begin
  if not (ACustomEdit is TCustomLikeAnEdit) then Exit;
  c.fEdit.CopyToClipboard;
end;

class procedure TWSPretendCustomEdit.Paste(const ACustomEdit: TCustomEdit);
var
  c : TCustomLikeAnEdit absolute ACustomEdit;
begin
  if not (ACustomEdit is TCustomLikeAnEdit) then Exit;
  c.fEdit.PasteFromClipboard;
end;

class procedure TWSPretendCustomEdit.Undo(const ACustomEdit: TCustomEdit);
var
  c : TCustomLikeAnEdit absolute ACustomEdit;
begin
  if not (ACustomEdit is TCustomLikeAnEdit) then Exit;
  c.fEdit.Undo;
end;

{ TCustomLikeAnEdit }

procedure TCustomLikeAnEdit.SetLeftPad(AValue: Integer);
begin
  AValue:=abs(AValue);
  if fLeftPad=AValue then Exit;
  fLeftPad:=AValue;
  EditUpdateBounds;
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

class procedure TCustomLikeAnEdit.WSRegisterClass;
begin
  RegisterWSComponent(TCustomLikeAnEdit, TWSPretendCustomEdit);
end;

function TCustomLikeAnEdit.ChildClassAllowed(ChildClass: TClass): boolean;
begin
  Result:=true;
end;

constructor TCustomLikeAnEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csAcceptsControls, csCaptureMouse,
      csClickEvents, csSetCaption, csDoubleClicks, csReplicatable,
      csNoFocus, csAutoSize0x0, csParentBackground];
  fEdit:=EditAlloc;
  EditPrepare;
  EditUpdateBounds;
end;

procedure TCustomLikeAnEdit.SetBounds(ALeft, ATop, AWidth, AHeight: integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  EditUpdateBounds;
end;

procedure TCustomLikeAnEdit.SetFocus;
begin
  fEdit.SetFocus;
end;

procedure TCustomLikeAnEdit.GetPreferredSize(var PreferredWidth,
  PreferredHeight: integer; Raw: boolean; WithThemeSpace: boolean);
begin
  fEdit.GetPreferredSize(PreferredWidth, PreferredHeight, Raw, WithThemeSpace);
  PreferredWidth:=PreferredWidth+LeftPadding+RightPadding;
end;

end.

