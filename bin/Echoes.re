open Lwt;
module Log = Dolog.Log;
open Echoes.Settings;

let collect_options: unit => CliOptionMap.t(string) =
  opts => {
    let filled_opts = ref(CliOptionMap.empty);
    Arg.parse(
      [
        (
          "--version",
          Arg.Unit(
            () =>
              filled_opts := CliOptionMap.add("--version", "requested", filled_opts^),
          ),
          "
                 Show version and exit",
        ),
        (
          "--redis-host",
          Arg.String(
            host => filled_opts := CliOptionMap.add("--redis-host", host, filled_opts^),
          ),
          Printf.sprintf(
            "
                 The host of the redis backend
                 (default: REDIS_HOST env variable, or '%s')",
            defaults.redis_host,
          ),
        ),
        (
          "--redis-port",
          Arg.Int(
            port =>
              filled_opts :=
                CliOptionMap.add("--redis-port", string_of_int(port), filled_opts^),
          ),
          Printf.sprintf(
            "
                 The port used by the redis backend
                 (default: REDIS_PORT env variable, or '%d')",
            defaults.redis_port,
          ),
        ),
        (
          "--redis-key-prefix",
          Arg.String(
            prefix =>
              filled_opts :=
                CliOptionMap.add("--redis-key-prefix", prefix, filled_opts^),
          ),
          Printf.sprintf(
            "
                 The prefix used for redis keys
                 (default: REDIS_KEY_PREFIX env variable, or '%s')",
            defaults.redis_key_prefix,
          ),
        ),
        (
          "--redis-expiration-seconds",
          Arg.Int(
            seconds =>
              filled_opts :=
                CliOptionMap.add(
                  "--redis-expiration-seconds",
                  string_of_int(seconds),
                  filled_opts^,
                ),
          ),
          Printf.sprintf(
            "
                 The expiration setting for all redis keys
                 (default: REDIS_EXPIRATION_SECONDS env variable, or '%d')",
            defaults.redis_expiration_seconds,
          ),
        ),
        (
          "--redis-page-size",
          Arg.Int(
            size =>
              filled_opts :=
                CliOptionMap.add(
                  "--redis-page-size",
                  string_of_int(size),
                  filled_opts^,
                ),
          ),
          Printf.sprintf(
            "
                 The page size used by the SCAN redis command
                 (default: REDIS_PAGE_SIZE env variable, or '%d')",
            defaults.redis_page_size,
          ),
        ),
        (
          "--server-listen",
          Arg.String(
            address =>
              filled_opts := CliOptionMap.add("--server-listen", address, filled_opts^),
          ),
          Printf.sprintf(
            "
                 The listening address used by the echoes server
                 (default: SERVER_LISTEN env variable, or '%s')",
            Unix.string_of_inet_addr(defaults.server_listen),
          ),
        ),
        (
          "--server-port",
          Arg.Int(
            port =>
              filled_opts :=
                CliOptionMap.add("--server-port", string_of_int(port), filled_opts^),
          ),
          Printf.sprintf(
            "
                 The port used by the echoes server
                 (default: SERVER_PORT env variable, or '%d')",
            defaults.server_port,
          ),
        ),
        (
          "--log-level",
          Arg.String(
            level =>
              filled_opts := CliOptionMap.add("--log-level", level, filled_opts^),
          ),
          Printf.sprintf(
            "
                 The dolog level used
                 (default: LOG_LEVEL env variable, or '%s')",
            String.trim(Log.string_of_level(defaults.log_level)),
          ),
        ),
      ],
      _ => (),
      "Usage: echoes [OPTION]...",
    );
    filled_opts^;
  };

let main = () => {
  let opts = collect_options();
  if (CliOptionMap.mem("--version", opts)) {
    print_endline(
      Printf.sprintf(
        "Echoes version %s build %s sha1 %s",
        Echoes.Version.version,
        Echoes.Version.build,
        Echoes.Version.sha1,
      ),
    );
  } else {
    initialize(opts);
    let server =
      Echoes.Service.server(
        Echoes.Settings.current^.server_listen,
        Echoes.Settings.current^.server_port,
      );
    Lwt_main.run(server);
  };
};

let () = main();
