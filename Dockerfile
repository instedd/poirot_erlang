FROM debian:wheezy

RUN \
  apt-get update && \
  apt-get install -y curl git build-essential && \
  curl http://packages.erlang-solutions.com/debian/erlang_solutions.asc | apt-key add - && \
  echo "deb http://packages.erlang-solutions.com/debian wheezy contrib" > /etc/apt/sources.list.d/esl.list && \
  apt-get update && \
  apt-get install -y esl-erlang && \
  apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

RUN curl -L -o /usr/local/bin/rebar https://github.com/rebar/rebar/wiki/rebar
RUN chmod +x /usr/local/bin/rebar

ADD . /poirot_erlang
WORKDIR /poirot_erlang

RUN rebar get-deps
RUN rebar compile

ADD docker/poirot.config /poirot_erlang/poirot.config

CMD ["make", "run-prod"]
