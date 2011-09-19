module X3D.Grouping where

import Text.XML.HXT.Core

import X3D.ChildNode

getChildNode
  :: (ArrowXml cat, ArrowIO cat, ArrowChoice cat) =>
     cat XmlTree X3DChildNode
