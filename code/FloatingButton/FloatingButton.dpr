program FloatingButton;

uses
  System.StartUpCopy,
  FMX.Forms,
  UFormFloatingButton in 'UFormFloatingButton.pas' {FormFloatingButton},
  UFrameFloatingButton in 'UFrameFloatingButton.pas' {FrameFloatingButton: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormFloatingButton, FormFloatingButton);
  Application.Run;
end.
