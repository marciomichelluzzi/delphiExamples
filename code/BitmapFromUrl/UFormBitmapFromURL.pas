unit UFormBitmapFromURL;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Objects,
  FMX.StdCtrls,
  FMX.Controls.Presentation,
  FMX.Edit;

type
  TFormBitmapFromURL = class(TForm)
    ImageBitmap: TImage;
    ButtonLoad: TButton;
    EditLink: TEdit;
    procedure ButtonLoadClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormBitmapFromURL: TFormBitmapFromURL;

implementation

uses
  FMX.Devgear.Extentions;

{$R *.fmx}
{ TImageHelper }

procedure TFormBitmapFromURL.ButtonLoadClick(Sender: TObject);
begin
  ImageBitmap.Bitmap.LoadThumbnailFromURL
    (EditLink.Text,
    100, 100);
//  TButton(Sender).Text := IntToStr(ImageBitmap.Bitmap.Height);
end;

end.
