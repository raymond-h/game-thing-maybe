FROM debian:stretch

WORKDIR /app

RUN apt-get update && apt-get -y install libgmp10 netbase ca-certificates

COPY main /app/main

CMD /app/main
