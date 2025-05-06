# Build the engine
FROM rust:alpine AS builder

RUN apk update && apk add --no-cache git musl-dev

WORKDIR /usr/src/myapp
COPY . .
RUN cargo build --release


# Create the lichess-bot image
FROM python:latest

WORKDIR /app
RUN git clone https://github.com/lichess-bot-devs/lichess-bot.git .
RUN python -m pip install -r requirements.txt
COPY ./lichess-bot/config.yml .

WORKDIR /app/engines
COPY --from=builder /usr/src/myapp/target/release/oxide9 .

WORKDIR /app
CMD ["python", "lichess-bot.py"]