# echoes

This is a redis-backed message inbox with an HTTP JSON API. Messages
stored are always set to expire, and they're deleted after being
consumed by their intended reader.

## How to use it

**Note**: This application should absolutely not be used for
anything. Use [Redis channels](https://redis.io/topics/pubsub), or any
[AMQP](https://www.rabbitmq.com/) or [MQTT](https://mosquitto.org/)
broker for message handling.

If used, there's three main endpoints to POST to:

- `/send` receives a single message with multiple destinations, and
  stores a copy of it for each destination until it expires or said
  destination consumes it. The message format is, in
  [jsonschema](http://json-schema.org/):

  ```json
  {
    "type": "object",
    "properties": {
      "idempotencyKey": {"type": "string", "pattern": "^[A-Za-z0-9\\-_=]{12,64}$"},
      "destination": {"type": "array", "items": {"type": "string"}},
      "body": {},
      "expectsAck": {"type": "boolean"}
    },
    "required": ["idempotencyKey", "destination", "body"]
  }
  ```

- `/receive` returns to the request author all the messages that
  target her, and expires them all afterwards. Each message returned
  is formatted depending on the type of message it is: ACK or actual
  message. The ACK is formatted like this:

  ```json
  {
    "type": "object",
    "properties": {
      "type": {"const": "ack"},
      "idempotencyKey": {"type": "string", "pattern": "^[A-Za-z0-9\\-_=]{12,64}$"},
      "destination": {"type": "string"},
      "receivedAt": {"type": "string", "format": "date-time"}
    },
    "required": ["type", "idempotencyKey", "destination", "receivedAt"]
  }
  ```

  While the actual message returned is formatted like this:

  ```json
  {
    "type": "object",
    "properties": {
      "type": {"const": "msg"},
      "idempotencyKey": {"type": "string", "pattern": "^[A-Za-z0-9\\-_=]{12,64}$"},
      "origin": {"type": "string"},
      "body": {},
      "expectsAck": {"type": "boolean"},
      "sentAt": {"type": "string", "format": "date-time"}
    },
    "required": ["type", "idempotencyKey", "origin", "body", "expectsAck", "sentAt"]
  }
  ```

- `/exchange` is the sequencing of `/send` and `/receive`, and should
  be the only endpoint used since it's more efficient. The other two
  endpoints exist for completion's sake.

### Idempotency

_echoes_ doesn't generate unique keys for messages, and instead
expects the user to provide an _idempotency key_, which is a token
which should be unique for every specific message coming from one
sender. Sending a message with an already-used idempotency key risks
overwriting the previous message from the inbox, if the previous one
wasn't yet consumed or didn't yet expire.

If a user doesn't want to keep track of idempotency keys, they may
simply use a random url-safe base64-encoded token, like this:

```bash
token=$(openssl rand -base64 32 | tr '/+' '_-')
```

Or the equivalent for any other programming language.

### Authentication

This application doesn't authorize a request in any way. If that's
needed, a separate identity management application should be used,
which forwards authorized requests to _echoes_.

To make a request to _echoes_ use the
[From](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/From)
header. It doesn't need to contain an email address: any non-empty
string will do. An authorization application put in front of _echoes_
would need to compare the `From` contents with e.g. whatever is sent
in the `Authorization` header; _echoes_ doesn't read the
`Authorization` header at all.

## Example

This is a toy scenario which illustrates almost everything there is to _echoes_:

```bash
# echoes server running at localhost:8000

# Olivia sends a message to Maurice and George expecting an ACK from both
curl -H "From: olivia" localhost:8000/exchange -d '{
  "idempotencyKey": "000000000000",
  "destination": ["maurice", "george"],
  "body": "Hey there, are you ok?",
  "expectsAck": true
}' | jq .
# Response from the server:
# []

# Maurice sends a message to Olivia and George
curl -H "From: maurice" localhost:8000/exchange -d '{
  "idempotencyKey": "111111111111",
  "destination": ["olivia", "george"],
  "body": {"message": "Yep fine check this loremipsum", "lorem": "ipsum"}
}' | jq .
# Response from the server:
# [
#   {
#     "type": "msg",
#     "idempotencyKey": "000000000000",
#     "origin": "olivia",
#     "body": "Hey there, are you ok?",
#     "expectsAck": true,
#     "sentAt": "2020-05-09T20:20:39Z"
#   }
# ]

# George checks his messages
curl -X POST -H "From: george" localhost:8000/receive | jq .
# Response from the server:
# [
#   {
#     "type": "msg",
#     "idempotencyKey": "111111111111",
#     "origin": "maurice",
#     "body": {
#       "message": "Yep fine check this loremipsum",
#       "lorem": "ipsum"
#     },
#     "expectsAck": false,
#     "sentAt": "2020-05-09T20:20:39Z"
#   },
#   {
#     "type": "msg",
#     "idempotencyKey": "000000000000",
#     "origin": "olivia",
#     "body": "Hey there, are you ok?",
#     "expectsAck": true,
#     "sentAt": "2020-05-09T20:20:39Z"
#   }
# ]

# George checks his messages again
curl -X POST -H "From: george" localhost:8000/receive | jq .
# Response from the server:
# []

# Olivia checks her messages
curl -X POST -H "From: olivia" localhost:8000/receive | jq .
# Response from the server:
# [
#   {
#     "type": "ack",
#     "idempotencyKey": "000000000000",
#     "destination": "maurice",
#     "receivedAt": "2020-05-09T20:20:39Z"
#   },
#   {
#     "type": "msg",
#     "idempotencyKey": "111111111111",
#     "origin": "maurice",
#     "body": {
#       "message": "Yep fine check this loremipsum",
#       "lorem": "ipsum"
#     },
#     "expectsAck": false,
#     "sentAt": "2020-05-09T20:20:39Z"
#   },
#   {
#     "type": "ack",
#     "idempotencyKey": "000000000000",
#     "destination": "george",
#     "receivedAt": "2020-05-09T20:20:39Z"
#   }
# ]
```
