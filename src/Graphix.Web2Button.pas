{
  Author: William Yang
  Website: http://www.pockhero.com

}
unit Graphix.Web2Button;

interface

uses
  System.SysUtils, System.Classes, System.Types, System.UITypes, System.UIConsts,
    FMX.Types, FMX.Controls, FMX.StdCtrls, FMX.Objects,
    Graphix.Helpers, FMX.Layouts;

type
  TWeb2BaseButton = class(TSpeedButton)
  private
    FText: TText;
    FImage: TImage;
    FUpdateButtonCount: Integer;
    FButton, FButtonOver, FButtonDown: TBitmap;

    FTextMargin: TBounds;
    FButtonActiveBitmap: TBitmap;
    FHoverActive: Boolean;
    FTextLift: Boolean;

    procedure SetButtonBitmap(const Value: TBitmap);
    procedure SetButtonDownBitmap(const Value: TBitmap);
    procedure SetButtonOverBitmap(const Value: TBitmap);

    procedure SetTextMargin(const Value: TBounds);
    procedure TextMarginChange(ASender: TObject);
    procedure SetButtonActiveBitmap(const Value: TBitmap);
    procedure SetHoverActive(const Value: Boolean);
    procedure SetTextLift(const Value: Boolean);

  protected
    FDefault: Boolean;
    FCancel: Boolean;

    procedure TrySetImage(const AImages: array of TBitmap);
    procedure SetIsPressed(const Value: Boolean); override;
    procedure DialogKey(var Key: Word; Shift: TShiftState); override;
    procedure Resize; override;
    procedure Loaded; override;
    procedure ApplyStyle; override;
    function GetStyleObject: TFmxObject; override;
    function FindTextObject: TFmxObject; override;

    procedure RecreateButton; virtual;
    procedure UpdateImage; virtual;
    procedure BeginUpdateButton; virtual;
    procedure EndUpdateButton; virtual;

    procedure DoMouseEnter;override;
    procedure DoMouseLeave;override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;

    property ButtonBitmap: TBitmap read fButton write SetButtonBitmap;
    property ButtonOverBitmap: TBitmap read FButtonOver write SetButtonOverBitmap;
    property ButtonDownBitmap: TBitmap read FButtonDown write SetButtonDownBitmap;
    property ButtonActiveBitmap: TBitmap read FButtonActiveBitmap write SetButtonActiveBitmap;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadBitmaps(AFilename: String); overload;
    procedure LoadBitmaps(AFilename, AFilenameOver, AFilenameDown, AFilenameActive: String); overload;
    procedure LoadBitmaps(ABitmap: TBitmap;
      const ABitmapOver: TBitmap = nil;
      const ABitmapDown: TBitmap = nil;
      const ABitmapActive: TBitmap = nil); overload;
  published

    property HoverActive: Boolean read FHoverActive write SetHoverActive;
    property TextMargin: TBounds read FTextMargin write SetTextMargin;
    property TextLift: Boolean read FTextLift write SetTextLift;
    property StaysPressed default False;
    property Action;
    property Align default TAlignLayout.alNone;
    property Anchors;
    property AutoTranslate default True;
    property Cancel: Boolean read FCancel write FCancel default False;
    property CanFocus default True;
    property CanParentFocus;
    property ClipChildren default False;
    property ClipParent default False;
    property Cursor default crDefault;
    property Default: Boolean read FDefault write FDefault default False;
    property DesignVisible default True;
    property DisableFocusEffect;
    property DragMode default TDragMode.dmManual;
    property EnableDragHighlight default True;
    property Enabled default True;
    property Font;
    property StyledSettings;
    property Height;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property HitTest default True;
    property IsPressed default False;
    property Locked default False;
    property Padding;
    property ModalResult default mrNone;
    property Opacity;
    property Margins;
    property PopupMenu;
    property Position;
    property RepeatClick default False;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property StyleLookup;
    property TabOrder;
    property Text;
    property Trimming;
    property TextAlign default TTextAlign.taCenter;
    property TouchTargetExpansion;
    property Visible default True;
    property Width;
    property WordWrap default False;
    property OnApplyStyleLookup;
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    property OnKeyDown;
    property OnKeyUp;
    property OnCanFocus;
    property OnClick;
    property OnDblClick;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnPainting;
    property OnPaint;
    property OnResize;


  end;

  TWeb2ImageButton = class(TWeb2BaseButton)
  private
    FAutosize: Boolean;
    procedure SetAutosize(const Value: Boolean);
  published
    property Autosize: Boolean read FAutosize write SetAutosize;
    property ButtonBitmap;
    property ButtonOverBitmap;
    property ButtonDownBitmap;
    property ButtonActiveBitmap;
  end;

  TWeb2Button = class(TWeb2BaseButton)
  private
    { Private declarations }
    FUpdateButtonCount: Integer;
    FCancel: Boolean;
    FImage: TImage;
    FButtonBackground: TBitmap;
    FButtonLightOverlay: TBitmap;

    FColor: TAlphaColor;
    FXRadius: Single;
    FYRadius: Single;
    FButtonMask: TBitmap;
    FCorners: TCorners;
    FSides: TSides;
    procedure SetColor(const Value: TAlphaColor);
    procedure SetXRadius(const Value: Single);
    procedure SetYRadius(const Value: Single);
    procedure SetCorners(const Value: TCorners);
    procedure SetSides(const Value: TSides);


  protected
    { Protected declarations }
    procedure RecreateButton; override;
    procedure CreateButton; virtual;
    procedure CreateButtonMask; virtual;
    procedure CreateDownEffect; virtual;
    procedure CreateOverEffect; virtual;
    procedure RecreateButtonBackground; virtual;
    procedure RecreateLightOverlay; virtual;
    procedure UpdateImage; override;

    property Sides: TSides read FSides write SetSides;
    property Corners: TCorners read FCorners write SetCorners;
  public
    { Public declarations }

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published

    property Color: TAlphaColor read FColor write SetColor;
    property XRadius: Single read FXRadius write SetXRadius;
    property YRadius: Single read FYRadius write SetYRadius;

  end;

  TWeb2TabButton = class(TWeb2Button)
  private
    FTabPosition: TSide;
    procedure SetTabPosition(const Value: TSide);
  protected
    procedure RecreateButton; override;
    procedure CreateButton; override;
    procedure CreateButtonMask; override;
    procedure CreateDownEffect; override;
    procedure CreateOverEffect; override;
    procedure RecreateButtonBackground; override;
    procedure RecreateLightOverlay; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published

    property TabPosition: TSide read FTabPosition write SetTabPosition;

  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Graphix', [TWeb2ImageButton, TWeb2Button, TWeb2TabButton]);
end;

function BrightColor(Start, Stop: TAlphaColor): TAlphaColor;
begin
  if TAlphaColorRec(Start).A + TAlphaColorRec(Stop).A < $FF then
    TAlphaColorRec(Result).A := TAlphaColorRec(Start).A + TAlphaColorRec(Stop).A
  else
    TAlphaColorRec(Result).A := $FF;
  if TAlphaColorRec(Start).R + TAlphaColorRec(Stop).R < $FF then
    TAlphaColorRec(Result).R := TAlphaColorRec(Start).R + TAlphaColorRec(Stop).R
  else
    TAlphaColorRec(Result).R := $FF;
  if TAlphaColorRec(Start).G + TAlphaColorRec(Stop).G < $FF then
    TAlphaColorRec(Result).G := TAlphaColorRec(Start).G + TAlphaColorRec(Stop).G
  else
    TAlphaColorRec(Result).G := $FF;
  if TAlphaColorRec(Start).B + TAlphaColorRec(Stop).B < $FF then
    TAlphaColorRec(Result).B := TAlphaColorRec(Start).B + TAlphaColorRec(Stop).B
  else
    TAlphaColorRec(Result).B := $FF;
end;

{ TWeb2Button }
constructor TWeb2Button.Create(AOwner: TComponent);
begin
  inherited;
  FButtonBackground := TBitmap.Create;
  FButtonLightOverlay := TBitmap.Create;
  FButtonMask := TBitmap.Create;
  FXRadius := 12;
  FYRadius := 12;
  FColor := $FF7fe0f8;
  FCorners := AllCorners;
  FSides := AllSides;
end;

destructor TWeb2Button.Destroy;
begin
  FButtonMask.Free;
  FButtonBackground.Free;
  FButtonLightOverlay.Free;
  inherited;
end;

procedure TWeb2Button.CreateButtonMask;
var
  r: TRectF;
begin
  R := RectF(0, 0, Width, Height);
  FButtonMask.SetSize(Trunc(Width), Trunc(Height));
//    FButtonMask.PixelFormat :=
  FButtonMask.Canvas.BeginScene();
  try
    with FButtonMask.Canvas do
    begin
      Clear($FF000000);
      Fill.Kind := TBrushKind.bkSolid;
      Fill.Color := $FFFF0000;
      FillRect(r, FXRadius, YRadius, FCorners, 1);
    end;
  finally
    FButtonMask.Canvas.EndScene();
  end;

end;

procedure TWeb2Button.RecreateLightOverlay;
var
  r: TRectF;
begin
  FButtonLightOverlay.SetSize(Trunc(Width), Trunc(Height));
  with FButtonLightOverlay.Canvas do
  begin
    BeginScene();
    try
      ClearRect(RectF(0, 0, Width, Height), $00000000);
      r.Top := -Height*0.5;
      r.Left :=  -Width*1.1*0.5;
      r.Right := Width*1.1;
      r.Bottom := Height*0.6;
      Fill.Kind := TBrushKind.bkSolid;
      Fill.Color := $50FFFFFF;
      Stroke.Kind := TBrushKind.bkNone;
      FillEllipse(r, 1);
    finally
      EndScene();
    end;
  end;
//  FButtonLightOverlay.ApplyMask(FButtonMask);
end;

procedure TWeb2Button.RecreateButtonBackground;
var
  r: TRectF;
begin
  FButtonBackground.SetSize(Trunc(Width), Trunc(Height));
//  FButtonBackground.Clear(0);
  with FButtonBackground.Canvas do
  begin
    BeginScene();
    try
      R := RectF(0, 0, Width, Height);
      Fill.Kind := TBrushKind.bkGradient;
      Fill.Gradient.Color := fColor.Brighten(20);
      Fill.Gradient.Color1 := fColor;
      FillRect(r, FXRadius, YRadius, FCorners, 1);
      if fDefault then
      begin
        Stroke.Kind := TBrushKind.bkSolid;
        Stroke.Color := $C0FFFFFF;
        Stroke.Thickness := 2;
        DrawRectSides(r, FXRadius, YRadius, FCorners, 1, FSides);
      end else
      begin
        Stroke.Kind := TBrushKind.bkSolid;
        Stroke.Color := $C0000000;
        Stroke.Thickness := 1;
        DrawRectSides(r, FXRadius, YRadius, FCorners, 1, FSides);
//
//        Stroke.Color := $E0FFFFFF;
//        r.Inflate(-1, -1);
//        DrawRectSides(r, FXRadius, YRadius, FCorners, 1, FSides);

      end;
    finally
      EndScene();
    end;
  end;
//  FButtonBackground.SaveToFile('button-back.png');
end;


procedure TWeb2Button.CreateButton;
var
  r: TRectF;
  tmp: TBitmap;
begin
  FButton.SetSize(Trunc(Width), Trunc(Height));
  with FButton.Canvas do
  begin
    BeginScene();
    try
      R := RectF(0, 0, Width, Height);
      Clear($00000000);
      DrawBitmap(FButtonBackground, R, R, 1);
      DrawBitmap(FButtonLightOverlay, R, R, 1);

    finally
      EndScene();
    end;
  end;
//  FButtonMask.SaveToFile('button-mask.png');
  FButton.ApplyMask(FButtonMask);

end;

procedure TWeb2Button.CreateOverEffect;
var
  r: TRectF;
begin
  FButtonOver.SetSize(Trunc(Width), Trunc(Height));
  with FButtonOver.Canvas do
  begin
    BeginScene();
    try
      R := RectF(0, 0, Width, Height);
      Clear($00000000);
      DrawBitmap(FButtonBackground, R, R, 1);
      DrawBitmap(FButtonLightOverlay, R, R, 1);
      Fill.Kind := TBrushKind.bkSolid;
      Fill.Color := $40FFFFFF;
      FillRect(r, FXRadius, YRadius, FCorners, 1);

      Stroke.Kind := TBrushKind.bkSolid;
      Stroke.Color := FColor.Darken(40);
      Stroke.Thickness := 1;
      DrawRectSides(r, FXRadius, YRadius, FCorners, 1, FSides);

      r.Inflate(-1, -1, -1, -1);
      Stroke.Color := FColor.Brighten(60);
      DrawRectSides(r, FXRadius, YRadius, FCorners, 1, FSides);

//      Stroke.Color := FColor.Brighten(20);
//      r.Inflate(-1, -1, -1, -1);
//      DrawRectSides(r, FXRadius, YRadius, FCorners, 1, FSides);

    finally
      EndScene();
    end;
  end;
  FButtonOver.ApplyMask(FButtonMask);
end;

procedure TWeb2Button.CreateDownEffect;
var
  r: TRectF;
begin
  FButtonDown.SetSize(Trunc(Width), Trunc(Height));
  with FButtonDown.Canvas do
  begin
    BeginScene();
    try
      R := RectF(0, 0, Width, Height);
      Clear($00000000);
      DrawBitmap(FButtonBackground, R, R, 1);
      DrawBitmap(FButtonLightOverlay, R, R, 1);
      Fill.Kind := TBrushKind.bkSolid;
      Fill.Color := $20000000;
      FillRect(r, FXRadius, YRadius, FCorners, 1);

      Stroke.Kind := TBrushKind.bkSolid;
      Stroke.Color := FColor.Darken(40);
      Stroke.Thickness := 1;
      DrawRectSides(r, FXRadius, YRadius, FCorners, 1, FSides);

      Stroke.Color := FColor.Darken(20);
      r.Inflate(-1, -1);
      DrawRectSides(r, FXRadius, YRadius, FCorners, 1, FSides);
    finally
      EndScene();
    end;
  end;
  FButtonDown.ApplyMask(FButtonMask);
end;


procedure TWeb2Button.RecreateButton;
begin
  if (csLoading in ComponentState) or (FUpdateButtonCount > 0) then
  begin
    Exit;
  end;
  CreateButtonMask;
  RecreateLightOverlay;
  RecreateButtonBackground;
  CreateButton;
  CreateOverEffect;
  CreateDownEffect;

  UpdateImage;
end;

procedure TWeb2Button.SetColor(const Value: TAlphaColor);
begin
  FColor := Value;

  RecreateButton;
end;

procedure TWeb2Button.SetCorners(const Value: TCorners);
begin
  FCorners := Value;
  RecreateButton;
end;

procedure TWeb2Button.SetSides(const Value: TSides);
begin
  FSides := Value;
  RecreateButton;
end;

procedure TWeb2Button.SetXRadius(const Value: Single);
begin
  FXRadius := Value;
  RecreateButton;
end;

procedure TWeb2Button.SetYRadius(const Value: Single);
begin
  FYRadius := Value;
  RecreateButton;
end;


procedure TWeb2Button.UpdateImage;
begin
  inherited;
  if FText <> nil then
  begin
    if FColor.Brightness > 0.5 then
      FText.Color := TAlphaColorRec.Black
    else
      FText.Color := TAlphaColorRec.White;
  end;
end;

{ TWeb2TabButton }

constructor TWeb2TabButton.Create(AOwner: TComponent);
begin
  inherited;
  FCorners := [TCorner.crTopLeft, TCorner.crTopRight];
  FSides := [TSide.sdTop, TSide.sdLeft, TSide.sdRight];
end;

procedure TWeb2TabButton.CreateButton;
begin
  inherited;

end;

procedure TWeb2TabButton.CreateButtonMask;
begin
  inherited;

end;

procedure TWeb2TabButton.CreateDownEffect;
begin
  inherited;

end;

procedure TWeb2TabButton.CreateOverEffect;
begin
  inherited;

end;

destructor TWeb2TabButton.Destroy;
begin

  inherited;
end;

procedure TWeb2TabButton.RecreateButton;
begin
  inherited RecreateButton;
end;

procedure TWeb2TabButton.RecreateButtonBackground;
var
  r: TRectF;
  mask: TBitmap;
begin
  inherited;
  r := RectF(0, 0, Width, Height);
  mask := TBitmap.Create( Trunc(Width), Trunc(Height));
  with mask.Canvas do
  begin
    BeginScene();
    try
      Fill.Kind := TBrushKind.bkGradient;
      Fill.Gradient.Color := $FFFFFFFF;
      Fill.Gradient.Color1 := $FFD00000;
      r.Bottom := Height*0.9;
      FillRect(r, 0, 0, fCorners, 1);

      Fill.Kind := TBrushKind.bkGradient;
      Fill.Gradient.Color := $FFD00000;
      Fill.Gradient.Color1 := $FF000000;
      r.Top := r.Bottom;
      r.Bottom := Height;
      FillRect(r, 0, 0, fCorners, 1);

    finally
      EndScene();
    end;

  end;
//  mask.SaveToFile('button-back-mask.png');
  FButtonBackground.ApplyMask(mask);
  mask.Free;
//  FButtonBackground.SaveToFile('button-back-masked.png');
end;

procedure TWeb2TabButton.RecreateLightOverlay;
begin
  inherited;

end;

procedure TWeb2TabButton.SetTabPosition(const Value: TSide);
begin
  FTabPosition := Value;
  case FTabPosition of
  TSide.sdTop:
  begin
    FSides := AllSides - [TSide.sdBottom];
    FCorners := AllCorners - [TCorner.crBottomLeft, TCorner.crBottomRight];
  end;
  TSide.sdLeft:
  begin
    FSides := AllSides - [TSide.sdRight];
    FCorners := AllCorners - [TCorner.crTopRight, TCorner.crBottomRight];
  end;
  TSide.sdRight:
  begin
    FSides := AllSides - [TSide.sdLeft];
    FCorners := AllCorners - [TCorner.crTopLeft, TCorner.crBottomLeft];
  end;
  TSide.sdBottom:
  begin
    FSides := AllSides - [TSide.sdTop];
    FCorners := AllCorners - [TCorner.crTopLeft, TCorner.crTopRight];
  end;
  end;

  RecreateButton;
end;

{ TWeb2BaseButton }

procedure TWeb2BaseButton.ApplyStyle;
begin
  inherited;
  FImage := TImage(FindStyleResource('image'));
  FText := TText(FindStyleResource('text'));
  UpdateImage;
end;

procedure TWeb2BaseButton.BeginUpdateButton;
begin
  Inc(FUpdateButtonCount);
end;

procedure TWeb2BaseButton.TextMarginChange(ASender: TObject);
begin
  UpdateImage;
end;

constructor TWeb2BaseButton.Create(AOwner: TComponent);
begin
  inherited;
  Cursor := crHandPoint;
  FTextMargin := TBounds.Create(RectF(0, 0, 0, 0));
  FTextMargin.OnChange := TextMarginChange;
  FUpdateButtonCount := 0;
  FButton := TBitmap.Create;
  FButtonOver := TBitmap.Create;
  FButtonDown := TBitmap.Create;
  FButtonActiveBitmap := TBitmap.Create;
  FTextLift := False;
end;

destructor TWeb2BaseButton.Destroy;
begin
  FButton.Free;
  FButtonOver.Free;
  FButtonDown.Free;
  FButtonActiveBitmap.Free;
  FTextMargin.Free;
  inherited;
end;

procedure TWeb2BaseButton.DialogKey(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if Default and (Key = vkReturn) then
  begin
    Click;
    Key := 0;
  end;
  if Cancel and (Key = vkEscape) then
  begin
    Click;
    Key := 0;
  end;
end;

procedure TWeb2BaseButton.DoMouseEnter;
begin
  inherited;
  if FHoverActive and IsMouseOver then
  begin
    Click;
    IsPressed := True;
  end;
  UpdateImage;
end;

procedure TWeb2BaseButton.DoMouseLeave;
begin
  inherited;
  UpdateImage;
end;

procedure TWeb2BaseButton.EndUpdateButton;
begin
  Dec(FUpdateButtonCount);
  RecreateButton;

end;

function TWeb2BaseButton.FindTextObject: TFmxObject;
begin
  if fText = nil then
  begin
    FText := TText(FindStyleResource('text'));
  end;
  Result := fText;
end;

function TWeb2BaseButton.GetStyleObject: TFmxObject;
var
  image: TImage;
begin

  Result := TLayout.Create(nil);
  image := TImage.Create(Result);
  with image do
  begin
    Align := TAlignLayout.alClient;
    Parent := Result;
    HitTest := False;
    StyleName := 'image';
  end;

  with TText.Create(Result) do
  begin
    StyleName := 'text';
    HitTest := False;
    Parent := image;
    AutoSize := True;
    Align := TAlignLayout.alCenter;
  end;

end;

procedure TWeb2BaseButton.LoadBitmaps(AFilename: String);
begin
  LoadBitmaps(AFilename, ChangeFileExt(AFilename, '-over.png'),
    ChangeFileExt(AFilename, '-down.png'),
    ChangeFileExt(AFilename, '-active.png'));
end;

procedure TWeb2BaseButton.LoadBitmaps(AFilename, AFilenameOver, AFilenameDown,
  AFilenameActive: String);
begin
  if FileExists(AFilename) then
    FButton.LoadFromFile(AFilename);

  if FileExists(AFilenameOver) then
    FButtonOver.LoadFromFile(AFilenameOver);

  if FileExists(AFilenameDown) then
    FButtonDown.LoadFromFile(AFilenameDown);

  if FileExists(AFilenameActive) then
    FButtonActiveBitmap.LoadFromFile(AFilenameActive);

end;

procedure TWeb2BaseButton.LoadBitmaps(ABitmap: TBitmap; const ABitmapOver,
  ABitmapDown, ABitmapActive: TBitmap);
begin
  if Assigned(ABitmap) and (not ABitmap.IsEmpty) then
    FButton.Assign(ABitmap);

  if Assigned(ABitmapOver) and (not ABitmapOver.IsEmpty) then
    FButtonOver.Assign(ABitmapOver);

  if Assigned(ABitmapDown) and (not ABitmapDown.IsEmpty) then
    FButtonDown.Assign(ABitmapDown);

  if Assigned(ABitmapActive) and (not ABitmapActive.IsEmpty) then
    FButtonActiveBitmap.Assign(ABitmapActive);

end;

procedure TWeb2BaseButton.Loaded;
begin
  inherited;
  RecreateButton;
end;

procedure TWeb2BaseButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;
  UpdateImage;
end;

procedure TWeb2BaseButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;
  UpdateImage;
end;

procedure TWeb2BaseButton.RecreateButton;
begin
  // Do nothing
end;

procedure TWeb2BaseButton.Resize;
begin
  inherited;
  RecreateButton;
end;

procedure TWeb2BaseButton.SetButtonActiveBitmap(const Value: TBitmap);
begin
  FButtonActiveBitmap.Assign(Value);
end;

procedure TWeb2BaseButton.SetButtonBitmap(const Value: TBitmap);
begin
  fButton.Assign(Value);
  UpdateImage;
end;

procedure TWeb2BaseButton.SetButtonDownBitmap(const Value: TBitmap);
begin
  FButtonDown.Assign(Value);
  UpdateImage;
end;

procedure TWeb2BaseButton.SetButtonOverBitmap(const Value: TBitmap);
begin
  FButtonOver.Assign(Value);
  UpdateImage;
end;

procedure TWeb2BaseButton.SetHoverActive(const Value: Boolean);
begin
  FHoverActive := Value;
  if IsMouseOver then
  begin
    Click;
    IsPressed := True;
  end;
end;

procedure TWeb2BaseButton.SetIsPressed(const Value: Boolean);
begin
  inherited;
  UpdateImage;
end;

procedure TWeb2BaseButton.SetTextLift(const Value: Boolean);
begin
  FTextLift := Value;
end;

procedure TWeb2BaseButton.SetTextMargin(const Value: TBounds);
begin
  FTextMargin.Assign(Value);
end;

procedure TWeb2BaseButton.TrySetImage(const AImages: array of TBitmap);
var
  i: TBitmap;
begin
  if FImage <> nil then
  begin
    for i in AImages do
    begin
      if (i <> nil) and (not i.IsEmpty) then
      begin
        fImage.Bitmap.PlatformCopy(i);
        Break;
      end;
    end;
  end;
end;

procedure TWeb2BaseButton.UpdateImage;
begin
  Self.BeginUpdate;
  try
    if FImage <> nil then
    begin
      if (IsPressed and StaysPressed) and (not IsMouseOver) then
      begin
        TrySetImage([FButtonActiveBitmap, FButtonDown, FButton]);
      end else if IsPressed then
      begin
        TrySetImage([FButtonDown, FButton]);
      end else if IsMouseOver then
      begin
        TrySetImage([FButtonOver, FButton]);
      end
      else
      begin
        TrySetImage([FButton]);
      end;
    end;


    if FTextLift and (FText <> nil) then
    begin
      FText.Margins.Assign(FTextMargin);
      if IsPressed then
        FText.Margins.Top := FTextMargin.Top
      else if IsMouseOver then
        FText.Margins.Top := FTextMargin.Top-1
      else
        FText.Margins.Top := FTextMargin.Top;
    end;

  finally
    Self.EndUpdate;
    Self.Repaint;
  end;
end;

{ TWeb2ImageButton }

procedure TWeb2ImageButton.SetAutosize(const Value: Boolean);
begin
  FAutosize := Value;
  if FAutoSize and (not FButton.IsEmpty) then
  begin
    Width := FButton.Width;
    Height := FButton.Height;
  end;
end;

end.
