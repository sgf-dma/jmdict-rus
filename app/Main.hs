import Text.XML.HXT.Core

filterLang :: String -> [a]
filterLang l
  | l == "eng" || l == "rus" || l == "" = [undefined]
  | otherwise = []

main = do
    runX $ readDocument [] "JMDict"
        -- 'getAttrValue' always succeeds in terms of 'ifA' (returns
        -- not a zeroArrow), regardless of whether an attribute was
        -- found or not.
        --
        -- The reason is because it uses 'xshow' internally, and
        -- 'xshow' uses '>.'. The second argument of '>.' can't return
        -- zeroArrow, because it has type '[c] -> d' and zeroArrow is
        -- '[]'. So, even 'zeroArrow >.' will result in non-zeroArrow:
        --
        --  *Main> flip runLA 3 $ zeroArrow >. (\_ -> "")
        --  [""]
        --
        >>> processTopDown
            ( filterA (getAttrValue "xml:lang" >>> arrL filterLang) )
        -- >>> indentDoc
        >>> writeDocument [] "JMDict-rus"
    return ()
