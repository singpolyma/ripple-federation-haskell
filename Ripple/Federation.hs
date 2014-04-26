module Ripple.Federation where

import Control.Applicative ((<$>), (<*>))
import Data.Maybe (fromMaybe)
import Data.List (span)
import Data.Word (Word32)
import Control.Error (readZ)
import Data.Base58Address (RippleAddress)
import Data.Text (Text)
import qualified Data.Text as T

import Network.HTTP.Types.QueryLike (QueryLike(..))
import Data.Aeson (FromJSON(..), ToJSON(..), object, (.=), (.:), (.:?))
import qualified Data.Aeson as Aeson

-- * Errors and stuff

data ErrorType = NoSuchUser | NoSupported | NoSuchDomain | InvalidParams | Unavailable

data Error = Error {
		errorType :: ErrorType,
		errorMessage :: String
	}

instance ToJSON Error where
	toJSON (Error typ message) = object [
			T.pack "result" .= "error",
			T.pack "error" .= typ,
			T.pack "error_message" .= message
		]

instance FromJSON Error where
	parseJSON (Aeson.Object o) =
		Error                 <$>
		(o .: T.pack "error") <*>
		(o .: T.pack "error_message")
	parseJSON _ = fail "Ripple federation errors are always objects."

instance ToJSON ErrorType where
	toJSON NoSuchUser = toJSON "noSuchUser"
	toJSON NoSupported = toJSON "noSupported"
	toJSON NoSuchDomain = toJSON "noSuchDomain"
	toJSON InvalidParams = toJSON "invalidParams"
	toJSON Unavailable = toJSON "unavailable"

instance FromJSON ErrorType where
	parseJSON (Aeson.String s) =
		fromMaybe (fail "Unknown Ripple federation error type.") $ fmap return $
		lookup s [
			(T.pack "noSuchUser", NoSuchUser),
			(T.pack "noSupported", NoSupported),
			(T.pack "noSuchDomain", NoSuchDomain),
			(T.pack "invalidParams", InvalidParams),
			(T.pack "unavailable", Unavailable)
		]
	parseJSON _ = fail "Ripple federation error type is always a string."

-- * Aliases: user@domain.tld

data Alias = Alias {
		destination :: Text,
		domain      :: Text
	}

instance Show Alias where
	show (Alias dest domain) = T.unpack dest ++ "@" ++ T.unpack domain

instance Read Alias where
	readsPrec _ = readParen False go
		where
		domainchars = ['a'..'z']++['A'..'Z']++['0'..'9']++['-','.']
		whitespace = [' ', '\t', '\n', '\r']
		go s = case span (/='@') (dropWhile (`elem` whitespace) s) of
			(dest, ('@':rest)) ->
				let (domain, end) = span (`elem` domainchars) rest in
				[(Alias (T.pack dest) (T.pack domain), end)]
			_ -> []

instance QueryLike Alias where
	toQuery (Alias dest domain) =
		toQuery [("destination", dest), ("domain", domain)]

data ResolvedAlias = ResolvedAlias {
		alias  :: Alias,
		ripple :: RippleAddress,
		dt     :: Maybe Word32
	}

instance ToJSON ResolvedAlias where
	toJSON (ResolvedAlias (Alias dest domain) ripple dt) = object [
		T.pack "federation_json" .= (object $ [
				T.pack "type" .= "federation_record",
				T.pack "destination" .= dest,
				T.pack "domain" .= domain,
				T.pack "destination_address" .= show ripple
			] ++ maybe [] (\x -> [T.pack "dt" .= x]) dt)
		]

instance FromJSON ResolvedAlias where
	parseJSON (Aeson.Object o) = do
		o' <- o .: T.pack "federation_json"
		ResolvedAlias <$> (
				Alias                        <$>
				(o' .: T.pack "destination") <*>
				(o' .: T.pack "domain")
			)                                    <*>
			(o' .: T.pack "destination_address" >>= readZ) <*>
			(o' .:? T.pack "dt")
	parseJSON _ = fail "Ripple federation records are always objects."
