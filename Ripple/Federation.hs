module Ripple.Federation where

import Control.Applicative ((<$>), (<*>), (*>), (<*), (<|>), many, some)
import Control.Monad (guard)
import Data.Maybe (fromMaybe)
import Data.Either (rights)
import Data.List (span)
import Data.Monoid (Monoid(..))
import Data.Word (Word32)
import Control.Error (readZ, fmapLT, throwT, runEitherT, EitherT(..), hoistEither)
import UnexceptionalIO (fromIO, runUnexceptionalIO)
import Control.Exception (fromException)
import Data.Base58Address (RippleAddress)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8 -- eww

import Blaze.ByteString.Builder (Builder)
import Network.URI (URI(..), URIAuth(..))
import Network.HTTP.Types.Status (Status)
import System.IO.Streams (OutputStream, InputStream, fromLazyByteString)
import System.IO.Streams.Attoparsec (parseFromStream, ParseException(..))
import Network.Http.Client (withConnection, establishConnection, sendRequest, buildRequest, http, setAccept, setContentType, Response, receiveResponse, RequestBuilder, inputStreamBody, getStatusCode, setAuthorizationBasic, setContentLength, getHeader)
import qualified Network.Http.Client as HttpStreams

import Network.HTTP.Types.QueryLike (QueryLike(..))
import Network.HTTP.Types.URI (renderQuery)
import Data.Aeson (FromJSON(..), ToJSON(..), object, (.=), (.:), (.:?))
import qualified Data.Aeson as Aeson
import qualified Data.Attoparsec.ByteString as Attoparsec
import qualified Data.Attoparsec.ByteString.Char8 as Attoparsec hiding (takeTill)
import qualified Data.Attoparsec.Combinator as Attoparsec

-- * Errors and stuff

data ErrorType = NoSuchUser | NoSupported | NoSuchDomain | InvalidParams | Unavailable

data Error = Error {
		errorType :: ErrorType,
		errorMessage :: Text
	} deriving (Show)

instance Monoid Error where
	mempty = Error Unavailable (T.pack "mempty")
	mappend _ y = y

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

-- * Resolve aliases

getRippleTxt :: Text -> IO (Either Error [(Text, [Text])])
getRippleTxt domain = runUnexceptionalIO $ runEitherT $
	tryOne (uri domain')             <|>
	tryOne (uri ("www." ++ domain')) <|>
	tryOne (uri ("ripple." ++ domain'))
	where
	domain' = T.unpack domain
	uri d = URI "https:" (Just $ URIAuth "" d "") "/ripple.txt" "" ""
	tryOne uri =
		(hoistEither =<<) $
		fmapLT (const $ Error Unavailable (T.pack "Network error")) $ fromIO $
		oneShotHTTP HttpStreams.GET uri
		(setContentLength 0 >> setAccept (BS8.pack "text/plain"))
		HttpStreams.emptyBody (parseResponse rippleTxtParser)

rippleTxtParser :: Attoparsec.Parser [(Text, [Text])]
rippleTxtParser = some section
	where
	section = do
		h <- header >>= utf8
		ls <- many (Attoparsec.eitherP comment line)
		ls' <- mapM utf8 (rights ls)
		return (h, ls')
	utf8 bs = case T.decodeUtf8' bs of
		Right r -> return r
		Left e -> fail $ show e
	header = Attoparsec.skipSpace *>
		Attoparsec.char '[' *> Attoparsec.takeTill(==0x5D) <* Attoparsec.char ']'
	line = Attoparsec.skipSpace *> do
		c <- Attoparsec.peekChar
		guard (c /= Just '[')
		Attoparsec.takeTill Attoparsec.isEndOfLine <* Attoparsec.endOfLine
	comment = Attoparsec.skipSpace *> Attoparsec.char '#' *>
		Attoparsec.takeTill Attoparsec.isEndOfLine <* Attoparsec.endOfLine

-- * Internal Helpers

get :: (QueryLike a, FromJSON b) => URI -> a -> IO (Either Error b)
get uri payload = oneShotHTTP HttpStreams.GET uri'
	(setContentLength 0 >> setAccept (BS8.pack "application/json"))
	HttpStreams.emptyBody safeJSONresponse
	where
	uri' = uri { uriQuery = BS8.unpack $ renderQuery True (toQuery payload)}

safeJSONresponse :: (Aeson.FromJSON a) => Response -> InputStream ByteString -> IO (Either Error a)
safeJSONresponse resp i = runEitherT $ do
	v <- EitherT $ parseResponse Aeson.json' resp i
	case Aeson.fromJSON v of
		Aeson.Success a -> return a
		Aeson.Error e -> throwT $ Error Unavailable $
			T.pack $ "JSON parser error: " ++ e

parseResponse :: Attoparsec.Parser a -> Response -> InputStream ByteString -> IO (Either Error a)
parseResponse parser resp i = runUnexceptionalIO $ runEitherT $ do
	fmapLT (\e -> handle e (fromException e)) $ fromIO $
		parseFromStream parser i
	where
	parseError = Error Unavailable (T.pack "Parse error")
	handle _ (Just (ParseException _)) = parseError
	handle e _ = Error Unavailable (T.pack $ "Exception: " ++ show e)

oneShotHTTP :: HttpStreams.Method -> URI -> RequestBuilder () -> (OutputStream Builder -> IO ()) -> (Response -> InputStream ByteString -> IO b) -> IO b
oneShotHTTP method uri req body handler = do
	req' <- buildRequest $ do
		http method (BS8.pack $ uriPath uri)
		req
	withConnection (establishConnection url) $ \conn -> do
		sendRequest conn req' body
		receiveResponse conn handler
	where
	url = BS8.pack $ show uri -- URI can only have ASCII, so should be safe
