unit web2buttontest;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, Graphix.Web2Button, FMX.Objects, Graphix.WebLabel;

type
  TForm2 = class(TForm)
    Web2Button4: TWeb2Button;
    Information: TWeb2TabButton;
    Contact: TWeb2TabButton;
    Web2TabButton3: TWeb2TabButton;
    Web2Button1: TWeb2Button;
    Web2Button2: TWeb2Button;
    Web2Button3: TWeb2Button;
    Web2Button5: TWeb2Button;
    Web2Button6: TWeb2Button;
    Web2Button7: TWeb2Button;
    Web2Button8: TWeb2Button;
    Text1: TText;
    Text2: TText;
    Rectangle1: TRectangle;
    Web2TabButton1: TWeb2TabButton;
    Web2TabButton2: TWeb2TabButton;
    Web2TabButton4: TWeb2TabButton;
    Web2TabButton5: TWeb2TabButton;
    Web2TabButton6: TWeb2TabButton;
    Web2TabButton7: TWeb2TabButton;
    Web2TabButton8: TWeb2TabButton;
    WebLabel1: TWebLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

procedure TForm2.FormCreate(Sender: TObject);
begin
  Rectangle1.Fill.Bitmap.Bitmap.LoadFromFile('chilipepper2.jpg');
  Rectangle1.Fill.Kind := TBrushKind.bkBitmap;
end;

initialization
  ReportMemoryLeaksOnShutdown := True;
//  GlobalUseDX10 := False;
//  GlobalUseDirect2D := False;

end.
