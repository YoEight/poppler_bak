{-# LANGUAGE CPP, DeriveDataTypeable #-}
module Graphics.UI.Gtk.Poppler.Annotation (
    Annot,
    AnnotClass,
    AnnotType (..),
    AnnotFlag (..),
    AnnotMarkup,
    AnnotMarkupClass,
    annotGetAnnotType,
    annotGetAnnotFlags,
    annotGetName,
    annotGetPageIndex,
    annotGetColor,
    annotSetColor,
    annotGetContents,
    annotSetContents,
    annotGetModified,
    annotMarkupGetLabel,
    annotMarkupSetLabel,
    annotMarkupGetSubject,
    annotMarkupGetOpacity,
    annotMarkupSetOpacity,
    annotMarkupHasPopup,
    annotMarkupSetPopup,
    annotMarkupGetPopupIsOpen,
    annotMarkupSetPopupIsOpen,
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

annotGetAnnotFlags :: AnnotClass annot => annot -> IO AnnotFlag
annotGetAnnotFlags annot =
  liftM (toEnum . fromIntegral) $
  {# call poppler_annot_get_flags #} (toAnnot annot)

annotGetName :: AnnotClass annot => annot -> IO String
annotGetName annot =
  {# call poppler_annot_get_name #} (toAnnot annot) >>= peekUTFString

annotGetPageIndex :: AnnotClass annot => annot -> IO Int
annotGetPageIndex annot =
  liftM fromIntegral $
  {# call poppler_annot_get_page_index #} (toAnnot annot)

annotGetColor :: AnnotClass annot => annot -> IO PopplerColor
annotGetColor annot =
  (peekPopplerColor . castPtr) =<<
  {# call poppler_annot_get_color #} (toAnnot annot)

annotSetColor :: AnnotClass annot => annot -> PopplerColor -> IO ()
annotSetColor annot color =
  with color $ \colorPtr ->
    {# call poppler_annot_set_color #} (toAnnot annot) (castPtr colorPtr)

annotGetContents :: AnnotClass annot => annot -> IO String
annotGetContents annot =
  {# call poppler_annot_get_contents #} (toAnnot annot) >>= peekUTFString

annotSetContents :: AnnotClass annot => annot -> String -> IO ()
annotSetContents annot content =
  withUTFString content $ \contentPtr ->
    {# call poppler_annot_set_contents #} (toAnnot annot) contentPtr

annotGetModified :: AnnotClass annot => annot -> IO String
annotGetModified annot =
  {# call poppler_annot_get_modified #} (toAnnot annot) >>= peekUTFString

annotMarkupGetLabel :: AnnotMarkupClass mark => mark -> IO String
annotMarkupGetLabel mark =
  peekUTFString =<<
  {# call poppler_annot_markup_get_label #} (toAnnotMarkup mark)

annotMarkupSetLabel :: AnnotMarkupClass mark => mark -> String -> IO ()
annotMarkupSetLabel mark label =
  withUTFString label $ \labelPtr ->
    {# call poppler_annot_markup_set_label #}
    (toAnnotMarkup mark)
    (castPtr labelPtr)

annotMarkupGetSubject :: AnnotMarkupClass mark => mark -> IO String
annotMarkupGetSubject mark =
  peekUTFString =<<
  {# call poppler_annot_markup_get_subject #} (toAnnotMarkup mark)

annotMarkupGetOpacity :: AnnotMarkupClass mark => mark -> IO Double
annotMarkupGetOpacity mark =
  liftM realToFrac $
  {# call poppler_annot_markup_get_opacity #} (toAnnotMarkup mark)

annotMarkupSetOpacity :: AnnotMarkupClass mark => mark -> Double -> IO ()
annotMarkupSetOpacity mark opacity =
  {# call poppler_annot_markup_set_opacity #}
  (toAnnotMarkup mark)
  (realToFrac opacity)

annotMarkupHasPopup :: AnnotMarkupClass mark => mark -> IO Bool
annotMarkupHasPopup mark =
  liftM toBool $
  {# call poppler_annot_markup_has_popup #} (toAnnotMarkup mark)

annotMarkupSetPopup :: AnnotMarkupClass mark
                    => mark
                    -> PopplerRectangle
                    -> IO ()
annotMarkupSetPopup mark rect =
  with rect $ \rectPtr ->
    {# call poppler_annot_markup_set_popup #} (toAnnotMarkup mark) (castPtr rectPtr)

annotMarkupGetPopupIsOpen :: AnnotMarkupClass mark => mark -> IO Bool
annotMarkupGetPopupIsOpen mark =
  liftM toBool $
  {# call poppler_annot_markup_get_popup_is_open #} (toAnnotMarkup mark)

annotMarkupSetPopupIsOpen :: AnnotMarkupClass mark => mark -> Bool -> IO ()
annotMarkupSetPopupIsOpen mark bool =
  {# call poppler_annot_markup_set_popup_is_open #}
  (toAnnotMarkup mark)
  (fromBool bool)

annotTextNew :: DocumentClass doc => doc -> PopplerRectangle -> IO Annot
annotTextNew doc selection =
  wrapNewGObject mkAnnot $
  with selection $ \selectionPtr ->
    {# call poppler_annot_text_new #} (toDocument doc) (castPtr selectionPtr)
