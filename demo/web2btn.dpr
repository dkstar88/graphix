program web2btn;

uses
  FMX.Forms,
  web2buttontest in 'web2buttontest.pas' {Form2},
  Graphix.Web2Button in '..\src\Graphix.Web2Button.pas',
  Graphix.Helpers in '..\src\Graphix.Helpers.pas',
  logUtils in 'logUtils.pas';

{$R *.res}

begin

  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
