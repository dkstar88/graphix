{
  Author: William Yang
  Website: http://www.pockhero.com
  Last Update: 2013/07/23
  A clickable label opens URL in system's Internet browser.

}
///	<summary>
///	  A clickable label opens URL in system's Internet browser.
///	</summary>
///	<remarks>
///	  A clickable label opens URL in system's Internet browser with full Mouse
///	  Hover, Mouse Down effect.
///	</remarks>
unit Graphix.WebLabel;

interface

uses System.Classes, System.UIConsts, System.Types, System.UITypes, FMX.Types, FMX.Controls,
  FMX.Objects;

type
  ///	<summary>
  ///	  TextSettings for Font
  ///	</summary>
  ///	<remarks>
  ///	  TTextSettings with Font and FontColor exposed as published properties.
  ///	</remarks>
  TFontTextSettings = class(TTextSettings)
  published
    property Font;
    property FontColor;
  end;

  TWebLabel = class(TText)
  private
    FAutoOpen: Boolean;
    FOnOpenURL: TNotifyEvent;
    FURL: String;
    FFontActive: TFontTextSettings;
    FFontHover: TFontTextSettings;
    FFontDown: TFontTextSettings;
    FFontNormal: TFontTextSettings;
    FIsPressed: Boolean;
    FFontSettings: TStyledSettings;
    procedure SetURL(const Value: String);
    procedure SetFontActive(const Value: TFontTextSettings);
    procedure SetFontDown(const Value: TFontTextSettings);
    procedure SetFontHover(const Value: TFontTextSettings);
    procedure SetFontNormal(const Value: TFontTextSettings);
    procedure SetIsPressed(const Value: Boolean);
    function GetColor: TAlphaColor;
    function GetFont: TFont;
    procedure SetColor(const Value: TAlphaColor);
    procedure SetFont(const Value: TFont);
    procedure SetFontSettings(const Value: TStyledSettings);

  protected
    procedure Click; override;
    procedure FontChanged(Sender: TObject); override;
    procedure UpdateFont;
    procedure DoMouseEnter; override;
    procedure DoMouseLeave; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Single); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published

    ///	<remarks>
    ///	  Sets URLLabel's Font Color, alias to FontNormal.FontColor
    ///	</remarks>
    property Color: TAlphaColor read GetColor write SetColor;

    ///	<remarks>
    ///	  Sets URLLabel's Font, alias to FontNormal.Font
    ///	</remarks>
    property Font: TFont read GetFont write SetFont;

    ///	<summary>
    ///	  AutoOpens url in browser
    ///	</summary>
    property AutoOpen: Boolean read FAutoOpen write FAutoOpen default True;

    ///	<summary>
    ///	  Is URLLabel pressed
    ///	</summary>
    property IsPressed: Boolean read FIsPressed write SetIsPressed
      default False;

    ///	<summary>
    ///	  On URL Open event
    ///	</summary>
    property OnOpenURL: TNotifyEvent read FOnOpenURL write FOnOpenURL;

    ///	<summary>
    ///	  URL to be opened
    ///	</summary>
    property URL: String read FURL write SetURL;

    ///	<summary>
    ///	  Default font for all states
    ///	</summary>
    property FontNormal: TFontTextSettings read FFontNormal write SetFontNormal;

    ///	<summary>
    ///	  Font for mouse over effect
    ///	</summary>
    property FontHover: TFontTextSettings read FFontHover write SetFontHover;

    ///	<summary>
    ///	  Font for mouse down effect
    ///	</summary>
    property FontDown: TFontTextSettings read FFontDown write SetFontDown;

    ///	<summary>
    ///	  Font for active effect
    ///	</summary>
//    property FontActive: TFontTextSettings read FFontActive write SetFontActive;

    ///	<summary>
    ///	  When URLLabel effect changes, what font settings to be applied.
    ///	</summary>
    ///	<remarks>
    ///	  <para>
    ///	    Default is [TStyledSetting.ssFamily, TStyledSetting.ssStyle,
    ///	    TStyledSetting.ssFontColor]
    ///	  </para>
    ///	  <para>
    ///	    If you need to change font size on mouse over, then add ssSize.
    ///	  </para>
    ///	</remarks>
    property FontSettings: TStyledSettings read FFontSettings
      write SetFontSettings;
  end;

procedure Register;

implementation

uses
{$IFDEF MSWINDOWS}
  Winapi.ShellAPI, Winapi.Windows;
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
Posix.Stdlib;
{$ENDIF POSIX}

procedure Register;
begin
  RegisterComponents('Graphix', [TWebLabel]);
end;

{ TWebLabel }
procedure TWebLabel.Click;
begin
  inherited;
  if AutoOpen then
  begin
{$IFDEF MSWINDOWS}
    ShellExecute(0, 'OPEN', PChar(FURL), '', '', SW_SHOWNORMAL);
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
    _system(PAnsiChar('open ' + AnsiString(FURL)));
{$ENDIF POSIX}
  end;
  if Assigned(OnOpenURL) then
    OnOpenURL(Self);
end;

constructor TWebLabel.Create(AOwner: TComponent);
begin
  inherited;
  AutoCapture := True;
  CanFocus := True;
  Cursor := crHandpoint;
  FAutoOpen := True;
  FFontNormal := TFontTextSettings.Create(Self);
  FFontHover := TFontTextSettings.Create(Self);
  FFontDown := TFontTextSettings.Create(Self);

  FFontSettings := [TStyledSetting.ssFamily, TStyledSetting.ssStyle,
    TStyledSetting.ssFontColor];

  // FFontActive := TFontTextSettings.Create;
  FFontNormal.Assign((Self as ITextSettings).TextSettings);
  FFontNormal.FontColor := claBlue;
  // FFontNormal.Font.Style := Font.Style+[TFontStyle.fsUnderline];

  FFontHover.Assign(FFontNormal);
  FFontHover.FontColor := claDarkcyan;
  FFontHover.Font.Style := Font.Style + [TFontStyle.fsUnderline];

  FFontDown.Assign(FFontHover);
  FFontDown.FontColor := claDarkGray;
  // StyledSettings := StyledSettings-[TStyledSetting.ssFontColor, TStyledSetting.ssStyle];
end;

destructor TWebLabel.Destroy;
begin
  // FFontActive.Free;
  FFontHover.Free;
  FFontDown.Free;
  FFontNormal.Free;
  inherited;
end;

procedure TWebLabel.DoMouseEnter;
begin
  inherited;
  UpdateFont;
end;

procedure TWebLabel.DoMouseLeave;
begin
  inherited;
  UpdateFont;
end;

procedure TWebLabel.FontChanged(Sender: TObject);
begin
  inherited;
  // UpdateFont;
end;

function TWebLabel.GetColor: TAlphaColor;
begin
  Result := FFontNormal.FontColor;
end;

function TWebLabel.GetFont: TFont;
begin
  Result := FFontNormal.Font;
end;

procedure TWebLabel.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Single);
begin
  inherited;
  FIsPressed := True;
  UpdateFont;
end;

procedure TWebLabel.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if (ssLeft in Shift) and IsPressed then
  begin
    IsPressed := LocalRect.Contains(PointF(X, Y));
  end;
end;

procedure TWebLabel.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Single);
begin
  inherited;
  if Button = TMouseButton.mbLeft then
  begin
    FIsPressed := False;
    UpdateFont;
  end;
end;

procedure TWebLabel.SetColor(const Value: TAlphaColor);
begin
  FFontNormal.FontColor := Value;
  UpdateFont;
end;

procedure TWebLabel.SetFont(const Value: TFont);
begin
  FFontNormal.Font := Value;
  UpdateFont;
end;

procedure TWebLabel.SetFontActive(const Value: TFontTextSettings);
begin
  FFontActive.Assign(Value);
  UpdateFont;
end;

procedure TWebLabel.SetFontDown(const Value: TFontTextSettings);
begin
  FFontDown.Assign(Value);
  UpdateFont;
end;

procedure TWebLabel.SetFontHover(const Value: TFontTextSettings);
begin
  FFontHover.Assign(Value);
  UpdateFont;
end;

procedure TWebLabel.SetFontNormal(const Value: TFontTextSettings);
begin
  FFontNormal.Assign(Value);
  UpdateFont;
end;

procedure TWebLabel.SetFontSettings(const Value: TStyledSettings);
begin
  FFontSettings := Value;
end;

procedure TWebLabel.SetIsPressed(const Value: Boolean);
begin
  FIsPressed := Value;
end;

procedure TWebLabel.SetURL(const Value: String);
begin
  FURL := Value;
end;

procedure TWebLabel.UpdateFont;
begin
  if IsPressed then
    (Self as ITextSettings).TextSettings.AssignNoStyled(FFontDown,
      AllStyledSettings - FFontSettings)
  else if IsMouseOver then
    (Self as ITextSettings).TextSettings.AssignNoStyled(FFontHover,
      AllStyledSettings - FFontSettings)
  else
    (Self as ITextSettings).TextSettings.AssignNoStyled(FFontNormal,
      AllStyledSettings - FFontSettings);
end;

initialization
  RegisterFMXClasses([TWebLabel]);

end.
