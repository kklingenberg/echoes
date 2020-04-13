module Log = Dolog.Log;

type t = {
  redis_host: string,
  redis_port: int,
  redis_key_prefix: string,
  redis_expiration_seconds: int,
  server_port: int,
  log_level: Log.log_level,
  sealed: bool,
};

/* Default settings */
let defaults: t = {
  redis_host: "localhost",
  redis_port: 6379,
  redis_key_prefix: "echoes:",
  redis_expiration_seconds: 60 * 60 * 24 * 7,
  server_port: 8000,
  log_level: Log.INFO,
  sealed: true,
};

let current = ref({...defaults, sealed: false});

/* Initialize logger */
Log.set_output(stdout);
Log.set_log_level(current^.log_level);

module StringMap = Map.Make(String);

let initialize = (cli_args: StringMap.t(string)) =>
  if (current^.sealed) {
    Log.warn("Settings are already initialized! Ignoring initialize() call...");
  } else {
    let variable = (cli_args_key, env_key, mapfn, default, predicates) => {
      let fromenv = () =>
        switch (mapfn(Sys.getenv(env_key))) {
        | v when List.for_all(p => p(v), predicates) => v
        | exception Not_found => default
        | exception _
        | _ =>
          Log.warn("Invalid %s value: %s", env_key, Sys.getenv(env_key));
          default;
        };
      switch (mapfn(StringMap.find(cli_args_key, cli_args))) {
      | v when List.for_all(p => p(v), predicates) => v
      | exception Not_found => fromenv()
      | exception _
      | _ =>
        Log.warn(
          "Invalid %s option: %s",
          cli_args_key,
          StringMap.find(cli_args_key, cli_args),
        );
        default;
      };
    };

    current :=
      {
        redis_host:
          variable(
            "--redis-host",
            "REDIS_HOST",
            x => x,
            defaults.redis_host,
            [h => String.length(h) > 0],
          ),
        redis_port:
          variable(
            "--redis-port",
            "REDIS_PORT",
            int_of_string,
            defaults.redis_port,
            [p => p >= 1 && p <= 65535],
          ),
        redis_key_prefix:
          variable(
            "--redis-key-prefix",
            "REDIS_KEY_PREFIX",
            x => x,
            defaults.redis_key_prefix,
            [],
          ),
        redis_expiration_seconds:
          variable(
            "--redis-expiration-seconds",
            "REDIS_EXPIRATION_SECONDS",
            int_of_string,
            defaults.redis_expiration_seconds,
            [t => t > 0],
          ),
        server_port:
          variable(
            "--server-port",
            "SERVER_PORT",
            int_of_string,
            defaults.server_port,
            [p => p >= 1 && p <= 65535],
          ),
        log_level:
          variable(
            "--log-level",
            "LOG_LEVEL",
            Log.level_of_string,
            defaults.log_level,
            [],
          ),
        sealed: true,
      };

    Log.set_log_level(current^.log_level);
  };
