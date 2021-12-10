module Spec.Plutus.Contract.Oracle where

import Hedgehog (Property, forAll, property)
import Hedgehog qualified
import Ledger.Crypto (generateFromSeed, toPublicKey)
import Ledger.Generators qualified as Gen
import Plutus.Contract.Oracle
import PlutusTx.Prelude (isRight, toBuiltin)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests =
    testGroup
        "Plutus.Contract.Oracle"
        [ testProperty "Oracle signed payloads verify with oracle public key offchain" oracleSignOffChainProp,
          testProperty "Oracle signed payloads verify with on-chain constraint" oracleSignContrastraintProp
        ]

oracleSignOffChainProp :: Property
oracleSignOffChainProp = property $ do
  seed <- forAll Gen.genSeed
  pass <- forAll Gen.genPassphrase
  msg <- forAll $ toBuiltin <$> Gen.genSizedByteString 128

  let
    privKey = generateFromSeed seed pass
    pubKey = toPublicKey privKey

  Hedgehog.assert $ isRight $ verifySignedMessageOffChain pubKey $ signMessage msg privKey pass

oracleSignContrastraintProp :: Property
oracleSignContrastraintProp = property $ do
  seed <- forAll Gen.genSeed
  pass <- forAll Gen.genPassphrase
  msg <- forAll $ toBuiltin <$> Gen.genSizedByteString 128

  let
    privKey = generateFromSeed seed pass
    pubKey = toPublicKey privKey

  Hedgehog.assert $ isRight $ verifySignedMessageConstraints pubKey $ signMessage msg privKey pass