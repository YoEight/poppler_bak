module Graphics.UI.Gtk.Poppler.Annotation (
    Annot,
    AnnotClass,
    AnnotType (..),
    AnnotFlag (..),
    annotGetAnnotType,
    annotGetAnnotFlag,
    annotTextNew
    ) where

import Control.Monad
import Data.Typeable
import System.Glib.FFI
import System.Glib.Flags
import System.Glib.GError
import System.Glib.GObject
import System.Glib.GList
import System.Glib.UTFString
import Graphics.UI.Gtk.Poppler.Enums
import Graphics.UI.Gtk.Abstract.Widget (Rectangle (..), Color (..))
{#import Graphics.UI.GtkInternals#}
{#import Graphics.Rendering.Cairo.Types#}
{#import Graphics.UI.Gtk.Poppler.Types#}
import Graphics.UI.Gtk.Poppler.Structs

{# context lib="poppler" prefix="poppler" #}

annotGetAnnotType :: AnnotClass annot => annot -> IO AnnotType
annotGetAnnotType annot =
  liftM (toEnum . fromIntegral) $
  {# call poppler_annot_get_annot_type #} (toAnnot annot)

annotGetAnnotFlag :: AnnotClass annot => annot -> IO AnnotFlag
annotGetAnnotFlag annot =
  liftM (toEnum . fromIntegral) $
  {# call poppler_annot_get_flags #} (toAnnot annot)

annotTextNew :: DocumentClass doc => doc -> PopplerRectangle -> IO Annot
annotTextNew doc selection =
  wrapNewGObject mkAnnot $
  with selection $ \selectionPtr ->
    {# call poppler_annot_text_new #} (toDocument doc) (castPtr selectionPtr)
