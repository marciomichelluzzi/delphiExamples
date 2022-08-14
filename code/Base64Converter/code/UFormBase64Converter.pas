unit UFormBase64Converter;

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
  FMX.StdCtrls,
  FMX.Controls.Presentation,
  FMX.Edit, FMX.Layouts,
  FMX.Memo, FMX.Objects,
  UnitBase64,
  FMX.ScrollBox,
  FMX.Memo.Types;

type
  TFormBase64Converter = class(TForm)
    MemoBase64: TMemo;
    EditFilePath: TEdit;
    ButtonToBase64: TButton;
    ButtonToImage: TButton;
    ImageSource: TImage;
    ButtonClearImage: TButton;
    procedure EditFilePathChange(Sender: TObject);
    procedure ButtonToBase64Click(Sender: TObject);
    procedure ButtonClearImageClick(Sender: TObject);
    procedure ButtonToImageClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormBase64Converter: TFormBase64Converter;

implementation

{$R *.fmx}

procedure TFormBase64Converter.EditFilePathChange(Sender: TObject);
begin
  ImageSource.Bitmap.LoadFromFile(EditFilePath.Text);
end;

procedure TFormBase64Converter.ButtonClearImageClick(Sender: TObject);
begin
  ImageSource.Bitmap := nil;
end;

procedure TFormBase64Converter.ButtonToBase64Click(Sender: TObject);
var
  LMemoryStream: TMemoryStream;
  LBase64: TBase64;
begin
  LMemoryStream := TMemoryStream.Create;
  ImageSource.Bitmap.SaveToStream(LMemoryStream);
  LBase64 := TBase64.Create();
  MemoBase64.Lines.Add(LBase64.encodeToString(LMemoryStream));
end;

procedure TFormBase64Converter.ButtonToImageClick(Sender: TObject);
var
  base: TBase64;
  string64: String;
begin
  base := TBase64.Create();
  string64 := MemoBase64.Lines.Text;
  ImageSource.Bitmap.LoadFromStream(base.decodeToStream(string64));
end;

end.
