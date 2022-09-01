import Html

main :: IO ()
main = putStrLn (render myhtml)

myhtml :: Html
myhtml =
  html_
    "Page title"
    ( (<>)
        (h_ 1 "I'm a heading")
        ( (<>)
            (p_ "I'm a paragraph one")
            (p_ "I'm a paragraph two")
        )
    )
