# Twitter Streams Example in Erlang

A simple Erlang/OTP gen_server example that connects to Twitters public streaming API https://stream.twitter.com/1.1/statuses/filter.json

## Dependencies

You will need Rebar 2 (I haven't tried Rebar 3 yet) to run this example.  Make
sure you have it installed.  Read more here https://github.com/rebar/rebar

You will need an existing or create a new Twitter app https://apps.twitter.com/.  Make sure you have access to your app's four keys: **Consumer Key**, **Consumer Secret**, **Access Token** and **Access Token Secret**

## Installation

    git clone git://github.com/fishoutawata/twitter_streams
    cd twitter_streams
    rebar get-deps

## Configuration

Edit src/twitter_streams_client.erl and enter your four Twitter keys

    -define(CONSUMER_KEY, "XXXXXXXXX").
    -define(CONSUMER_SECRET, "XXXXXXXXX").
    -define(ACCESS_TOKEN, "XXXXXXXXX").
    -define(ACCESS_TOKEN_SECRET, "XXXXXXXXX").

## Compile

From the project root directory run

    rebar compile

## Running

From the project root directory run

    erl -pa ebin -pa deps/oauth/ebin -s ssl -s inets -s crypto

Once in the Erlang shell run

    1> application:start(oauth).
    ok
    2> application:start(twitter_streams).
    ok

You should see a bunch of tweets scrolling away on your screen at this point.

## Contributing

1. Fork it!
2. Create your feature branch: `git checkout -b my-new-feature`
3. Commit your changes: `git commit -am 'Add some feature'`
4. Push to the branch: `git push origin my-new-feature`
5. Submit a pull request :D

## Credits

Martin Logan, Eric Merritt and Richard Carlsson for their book **Erlang and OTP in Action**

Tim Fletcher for his OAuth module for Erlang **https://github.com/tim/erlang-oauth**

## License

MIT
