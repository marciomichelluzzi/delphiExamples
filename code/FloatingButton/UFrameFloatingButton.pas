unit UFrameFloatingButton;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Ani, FMX.Objects, FMX.Layouts, FMX.Controls.Presentation;

type
  TFrameFloatingButton = class(TFrame)
    CircleMainButton: TCircle;
    LabelButtonIcon: TLabel;
    LayoutMenu: TLayout;
    RectangleBackground: TRectangle;
    ImageButtonOne: TImage;
    ImageButtonThree: TImage;
    ImageButtonTwo: TImage;
    AnimationMenu: TFloatAnimation;
    procedure CircleMainButtonClick(Sender: TObject);
    procedure AnimationMenuFinish(Sender: TObject);
    procedure ImageButtonHideClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

procedure TFrameFloatingButton.AnimationMenuFinish(Sender: TObject);
begin
  if AnimationMenu.Inverse = true then
    LayoutMenu.Visible := false;
end;

procedure TFrameFloatingButton.CircleMainButtonClick(Sender: TObject);
begin
  LayoutMenu.Position.Y := Self.Height + 20;
  LayoutMenu.Visible := true;

  AnimationMenu.Inverse := false;
  AnimationMenu.StartValue := Self.Height + 20;
  AnimationMenu.StopValue := 0;
  AnimationMenu.Start;
end;

procedure TFrameFloatingButton.ImageButtonHideClick(Sender: TObject);
begin
  AnimationMenu.Inverse := true;
  AnimationMenu.Start;
end;

end.
