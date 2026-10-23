FROM eclipse-temurin:21-jdk-jammy AS build

RUN apt-get update \
 && apt-get install -y --no-install-recommends curl ca-certificates \
 && rm -rf /var/lib/apt/lists/* \
 && curl -fsSL -o /tmp/sbt.tgz https://github.com/sbt/sbt/releases/download/v1.10.6/sbt-1.10.6.tgz \
 && tar -xzf /tmp/sbt.tgz -C /usr/local \
 && ln -s /usr/local/sbt/bin/sbt /usr/local/bin/sbt \
 && rm /tmp/sbt.tgz

WORKDIR /src
COPY project/build.properties project/plugins.sbt project/
COPY build.sbt .
RUN sbt -batch game/update

COPY core core
COPY game game
RUN sbt -batch game/fullLinkJS

FROM nginx:1.27-alpine

RUN printf '%s\n' \
  'server {' \
  '  listen 8080;' \
  '  server_name _;' \
  '  root /usr/share/nginx/html;' \
  '  location /levels/ {' \
  '    autoindex on;' \
  '  }' \
  '  location / {' \
  '    try_files $uri $uri/ =404;' \
  '  }' \
  '}' > /etc/nginx/conf.d/default.conf

COPY index.html /usr/share/nginx/html/index.html
COPY paperfold_title_final.png /usr/share/nginx/html/paperfold_title_final.png
COPY web/style.css /usr/share/nginx/html/web/style.css
COPY levels /usr/share/nginx/html/levels
COPY --from=build /src/web/js /usr/share/nginx/html/web/js

EXPOSE 8080
