{
  Author: William Yang
  Website: http://www.pockhero.com

}
unit Graphix.Helpers;

interface

uses System.UITypes, System.Classes, System.Types, System.UIConsts, FMX.Types;

type

  TColorHelper = record helper for TAlphaColor
  public
    function Brighten(AValue: Single): TAlphaColor;
    function Darken(AValue: Single): TAlphaColor;
    function Brightness: Single;
  end;

  TBitmapHelper = class helper for TBitmap
  public
    class function Create: TBitmap; overload;
    function ClientRectF: TRectF;
    function ClientRect: TRect;
    procedure ApplyMask(const Mask: TBitmap); overload;
    procedure PlatformCopy(ASrc: TBitmap);
    procedure Clear; overload;
  end;

  function DarkColorChannel(const Channel: Byte; const Pct: Single): Byte; inline;
  function BrightColorChannel(const Channel: Byte; const Pct: Single): Byte; inline;
  function BrightColor(const Color: TAlphaColor; const Pct: Single): TAlphaColor; inline;
  function DarkColor(const Color: TAlphaColor; const Pct: Single): TAlphaColor; inline;

implementation

uses Math;

const
  MaxBytePercent = 2.55;

function BrightColor(const Color: TAlphaColor; const Pct: Single): TAlphaColor; inline;
begin
  Result := Color;
  with TAlphaColorRec(Result) do
  begin
    R := BrightColorChannel(R, Pct);
    G := BrightColorChannel(G, Pct);
    B := BrightColorChannel(B, Pct);
  end;
end;

function BrightColorChannel(const Channel: Byte; const Pct: Single): Byte; inline;
var
  Temp: Integer;
begin
  if Pct < 0 then
    Result := DarkColorChannel(Channel, -Pct)
  else
  begin
    Temp := Round(Channel + Pct * MaxBytePercent);
    if Temp > High(Result) then
      Result := High(Result)
    else
      Result := Temp;
  end;
end;

function DarkColor(const Color: TAlphaColor; const Pct: Single): TAlphaColor; inline;
begin
  Result := Color;
  with TAlphaColorRec(Result) do
  begin
    R := DarkColorChannel(R, Pct);
    G := DarkColorChannel(G, Pct);
    B := DarkColorChannel(B, Pct);
  end;
end;
function DarkColorChannel(const Channel: Byte; const Pct: Single): Byte; inline;
var
  Temp: Integer;
begin
  if Pct < 0 then
    Result := BrightColorChannel(Channel, -Pct)
  else
  begin
    Temp := Round(Channel - Pct * MaxBytePercent);
    if Temp < Low(Result) then
      Result := Low(Result)
    else
      Result := Temp;
  end;
end;

{ TColorHelper }
//class operator TColorHelper.Add(a, b: TAlphaColor): TAlphaColor;
//begin
//
//  TAlphaColorRec(Result).A := (TAlphaColorRec(A).A + TAlphaColorRec(B).A) div 2;
//  TAlphaColorRec(Result).R := TAlphaColorRec(A).R + TAlphaColorRec(B).R;
//  TAlphaColorRec(Result).G := TAlphaColorRec(A).G + TAlphaColorRec(B).G;
//  TAlphaColorRec(Result).B := TAlphaColorRec(A).B + TAlphaColorRec(B).B;
//end;

function TColorHelper.Brighten(AValue: Single): TAlphaColor;
begin
  Result := BrightColor(Self, AValue);
end;

function TColorHelper.Brightness: Single;
var
  h, s, l: Single;
begin
  RGBtoHSL(Self, h, s, l);
  Result := l;
end;

function TColorHelper.Darken(AValue: Single): TAlphaColor;
begin
  Result := DarkColor(Self, AValue);
end;


//class operator TColorHelper.Subtract(a, b: TAlphaColor): TAlphaColor;
//begin
//  TAlphaColorRec(Result).A := (TAlphaColorRec(A).A - TAlphaColorRec(B).A) div 2;
//  TAlphaColorRec(Result).R := TAlphaColorRec(A).R - TAlphaColorRec(B).R;
//  TAlphaColorRec(Result).G := TAlphaColorRec(A).G - TAlphaColorRec(B).G;
//  TAlphaColorRec(Result).B := TAlphaColorRec(A).B - TAlphaColorRec(B).B;
//end;

{ TBitmapHelper }
procedure TBitmapHelper.ApplyMask(const Mask: TBitmap);
var
  I, J: Integer;
  D, B, M: TBitmapData;
  C, MC: TAlphaColor;
  tmp: TBitmap;
begin
  if (Self.Width <> Mask.Width) or (Self.Height <> Mask.Height) then
    Exit;

  tmp := TBitmap.Create(Self.Width, Self.Height);
//  tmp.Canvas.Clear($00000000);

  if tmp.Map(TMapAccess.maWrite, D) and Self.Map(TMapAccess.maRead, B)
    and Mask.Map(TMapAccess.maRead, M) then
  begin
    try
      for I := 0 to Self.Width - 1 do
        for J := 0 to Self.Height - 1 do
        begin
          C := B.GetPixel(I, J);
          MC := M.GetPixel(I, j);
          if TAlphaColorRec(MC).R > 0 then
          begin
            TAlphaColorRec(C).A := Trunc(TAlphaColorRec(C).A/255 * TAlphaColorRec(MC).R);
          end else
          begin
            TAlphaColorRec(C).A := TAlphaColorRec(MC).R;
          end;
//          TAlphaColorRec(C).A := TAlphaColorRec(MC).R;
//          TAlphaColorRec(C).A := Max(TAlphaColorRec(MC).R - TAlphaColorRec(C).A, 0);
          D.SetPixel(I, J, C);
        end;
    finally
      tmp.Unmap(D);
      Self.Unmap(B);
      Mask.Unmap(M);
      Assign(tmp);
      tmp.Free;
    end;
  end;
end;

procedure TBitmapHelper.Clear;
begin
  Self.Canvas.Clear(0);
end;
//var
//  n: TBitmap;
//begin
//  n := TBitmap.Create(Width, Height);
//  Assign(n);
//  n.Free;
//end;

function TBitmapHelper.ClientRect: TRect;
begin
  Result := Rect(0, 0, Width, Height);
end;

function TBitmapHelper.ClientRectF: TRectF;
begin
  Result := RectF(0, 0, Width, Height);
end;

class function TBitmapHelper.Create: TBitmap;
begin
  Result := TBitmap.Create(0, 0);
end;

// -------------------
//  ** Fixes alpha value lost during copy
// --------------------
procedure TBitmapHelper.PlatformCopy(ASrc: TBitmap);
var
  m: TMemoryStream;
begin
  m := TMemoryStream.Create;
  try
    if GlobalUseDX10 then
    begin
      ASrc.SaveToStream(m);
      m.Seek(0, 0);
      LoadFromStream(m);
    end else
    begin

      Assign(ASrc);
    end;
  finally
    m.Free;
  end;
end;
//begin
//  Assign(ASrc);
//end;


end.
