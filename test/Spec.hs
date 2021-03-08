import Test.Tasty (defaultMain)
import Test.Tasty.Golden

goldenTests :: IO TestTree
goldenTests = testGroup "all" <$> sequence [parsing]

main :: IO ()
main = defaultMain =<< goldenTests

parsing :: IO TestTree
parsing = do
  files <- concat <$> mapM (findByExtension [".pl"]) ["tests/pass", "tests/fail"]
  fmap (testGroup "parsing") $ forM files $ \file -> do
    input      <- T.readFile file
    combinator <- pure $ runParser programP file input
    generator  <- try @IOError . evaluate . parse . alexScanTokens $ cs input
    pure . testCase file $ case (combinator, generator) of
      (Right ast, Right ast') -> assertEqual file ast ast'
      (Left  _  , Left _    ) -> pure ()
      _                       -> assertFailure file

