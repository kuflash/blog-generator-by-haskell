import Html

main :: IO ()
main = putStrLn (render myhtml)

myhtml :: Html
myhtml =
  html_
    "Page title"
    ( append_
        (h1_ "I'm a heading")
        ( append_
            (p_ "I'm a paragraph one")
            (p_ "I'm a paragraph two")
        )
    )
