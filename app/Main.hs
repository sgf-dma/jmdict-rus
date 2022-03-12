import Text.XML.HXT.Core

engRus :: String -> [String]
engRus l
  | l == "eng" || l == "rus" || l == "" = [l]
  | otherwise = []

-- 'getAttrValue' always succeeds in terms of 'ifA' (returns not a zeroArrow),
-- regardless of whether an attribute was found or not.
--
-- The reason is because it uses 'xshow' internally, and 'xshow' uses '>.'.
-- The second argument of '>.' can't return zeroArrow, because it has type
-- '[c] -> d' and zeroArrow is '[]'. So, even 'zeroArrow >.' will result in
-- non-zeroArrow:
--
--  *Main> flip runLA 3 $ zeroArrow >. (\_ -> "")
--  [""]
--

-- I need to filter out empty 'sense' elements (not containing any 'gloss'-es)
-- too. Otherwise, i'll get empty numbers in word card.
filterLang :: ArrowXml a => a XmlTree XmlTree
filterLang =
    ( filterA $
        getChildren
            >>> filterA (getAttrValue "xml:lang" >>> arrL engRus)
            >>> hasName "gloss"
    )
    `when`
    (getName `containing` isA (== "sense"))

main = do
    runX $ readDocument [] "JMDict"
    --res <- runX $ readDocument [] "jmtest"
        >>> processTopDown filterLang
        -- //> ffL2
        -- >>> indentDoc
        -- >>> writeDocument [] "jmresult"
        >>> writeDocument [] "JMDict-rus"
    --mapM_ (\x -> print "---------" >> print x) res
    return ()
