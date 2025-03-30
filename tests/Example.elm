module Example exposing (suite)

import Expect
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Test Something" [ Test.test "Something" <| \_ -> Expect.equal 1 1 ]
