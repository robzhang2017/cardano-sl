module Test.Pos.Wallet.Web.AddressSpec
       ( spec
       ) where

import           Universum

import           Data.Default                 (def)
import           Formatting                   (sformat, (%))
import           Serokell.Data.Memory.Units   (memory)
import           Test.Hspec                   (Spec, describe)
import           Test.Hspec.QuickCheck        (prop)
import           Test.QuickCheck              (arbitrary)
import           Test.QuickCheck.Monadic      (pick)

import           Pos.Binary                   (biSize)
import           Pos.Client.Txp.Addresses     (getFakeChangeAddress, getNewAddress)
import           Pos.Core.Address             (Address)
import           Pos.Crypto                   (PassPhrase)
import           Pos.Launcher                 (HasConfigurations)
import           Pos.Util.CompileInfo         (withCompileInfo)

import           Pos.Util.CompileInfo         (HasCompileInfo)
import           Pos.Wallet.Web.Account       (GenSeed (..), genUniqueAddress)
import           Pos.Wallet.Web.ClientTypes   (AccountId, CAccountInit (..), caId, cwamId)
import           Pos.Wallet.Web.Methods.Logic (newAccount)
import           Pos.Wallet.Web.State         (getWalletAddresses)
import           Pos.Wallet.Web.Util          (decodeCTypeOrFail)
import           Test.Pos.Util                (assertProperty, expectedOne,
                                               withDefConfigurations)
import           Test.Pos.Wallet.Web.Mode     (WalletProperty)
import           Test.Pos.Wallet.Web.Util     (importSingleWallet, mostlyEmptyPassphrases)

spec :: Spec
spec = withCompileInfo def $
       withDefConfigurations $
    describe "Fake address had maximal possible size" $ do
        prop "getNewAddress" $
            fakeAddressHasMaxSizeTest changeAddressGenerator
        prop "genUniqueAddress" $
            fakeAddressHasMaxSizeTest commonAddressGenerator

type AddressGenerator = AccountId -> PassPhrase -> WalletProperty Address

fakeAddressHasMaxSizeTest
    :: (HasConfigurations, HasCompileInfo)
    => AddressGenerator -> Word32 -> WalletProperty ()
fakeAddressHasMaxSizeTest generator accSeed = do
    passphrase <- importSingleWallet mostlyEmptyPassphrases
    wid <- expectedOne "wallet addresses" =<< getWalletAddresses
    accId <- lift $ decodeCTypeOrFail . caId
         =<< newAccount (DeterminedSeed accSeed) passphrase (CAccountInit def wid)
    address <- generator accId passphrase

    largeAddress <- lift getFakeChangeAddress

    assertProperty
        (biSize largeAddress >= biSize address)
        (sformat (memory%" < "%memory) (biSize largeAddress) (biSize address))

-- | Addresses generator used in 'MonadAddresses' to create change addresses.
-- Unfortunatelly, its randomness doesn't depend on QuickCheck seed,
-- so another proper generator is helpful.
changeAddressGenerator :: HasConfigurations => AddressGenerator
changeAddressGenerator accId passphrase = lift $ getNewAddress (accId, passphrase)

-- | Generator which is directly used in endpoints.
commonAddressGenerator :: HasConfigurations => AddressGenerator
commonAddressGenerator accId passphrase = do
    addrSeed <- pick arbitrary
    lift $ decodeCTypeOrFail . cwamId
       =<< genUniqueAddress (DeterminedSeed addrSeed) passphrase accId
